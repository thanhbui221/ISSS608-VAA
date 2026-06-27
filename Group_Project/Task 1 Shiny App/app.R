# ─────────────────────────────────────────────────────────────────────────────
# MC1: TenantThread Embargo Breach — Shiny App
# Covers: Data Exploration + Task 1 (Key Events, Relationships, Causal Chain)
# ─────────────────────────────────────────────────────────────────────────────

# ── 1. Packages ───────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(DT)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(lubridate)
library(stringr)
library(knitr)
library(kableExtra)
library(tidytext)
library(plotly)
library(glue)
library(htmltools)
library(htmlwidgets)

# ── 2. Helpers ────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── 3. Data Loading ───────────────────────────────────────────────────────────
json_path <- "VAST_Challenge_2026_MC1/MC1_final_00.json"
raw       <- fromJSON(json_path, simplifyVector = FALSE)
rds       <- raw$rounds

# ── 4. Message-level Parsing ──────────────────────────────────────────────────
parse_msgs <- function(rd) {
  hr        <- ymd_hms(rd$hour)
  snap      <- rd$environment_context$market_snapshot %||% list()
  price_raw <- snap$stock_price %||% NA_character_
  price     <- suppressWarnings(as.numeric(str_remove_all(price_raw, "[$,]")))
  if (!is.na(price) && (price == 0 | price > 100)) price <- NA_real_
  lapply(rd$communications, function(cm) {
    ist <- cm$internal_state %||% list()
    tibble(
      hour          = hr,
      message_id    = cm$message_id    %||% NA_character_,
      responding_to = cm$responding_to %||% NA_character_,
      agent_id      = cm$agent_id      %||% NA_character_,
      recipient     = list(cm$recipient %||% character(0)),
      channel       = cm$channel       %||% NA_character_,
      content       = str_trunc(cm$content       %||% "", 600),
      deliberating  = str_trunc(ist$deliberating  %||% "", 500),
      reacting      = str_trunc(ist$reacting      %||% "", 400),
      rationalizing = str_trunc(ist$rationalizing %||% "", 400),
      has_internal  = as.integer(!is.null(cm$internal_state)),
      stock_price   = price,
      sentiment     = snap$sentiment %||% NA_character_
    )
  }) |> bind_rows()
}

df <- lapply(rds, parse_msgs) |> bind_rows() |>
  mutate(
    date      = as.Date(hour),
    is_public = channel %in% c("official_post", "anonymous_post", "personal_post"),
    is_crisis = date == as.Date("2046-06-05"),
    crisis_hr = if_else(is_crisis, hour(hour), NA_integer_),
    is_secure = channel %in% c("side_huddle", "one_on_one_chat"),
    all_text  = paste(content, deliberating, reacting, rationalizing)
  )

# ── 5. Round-level Parsing ────────────────────────────────────────────────────
parse_round <- function(rd) {
  hr        <- ymd_hms(rd$hour)
  snap      <- rd$environment_context$market_snapshot %||% list()
  msgs      <- rd$communications
  price_raw <- snap$stock_price %||% NA_character_
  price     <- suppressWarnings(as.numeric(str_remove_all(price_raw, "[$,]")))
  if (!is.na(price) && (price == 0 | price > 100)) price <- NA_real_
  tbl_ch    <- table(sapply(msgs, \(m) m$channel %||% ""))
  get_ch    <- function(n) as.integer(if (n %in% names(tbl_ch)) tbl_ch[n] else 0L)
  tibble(
    hour          = hr,
    headline      = rd$environment_context$event_headline %||% "",
    total_msgs    = length(msgs),
    comms_huddle  = get_ch("comms_huddle"),
    one_on_one    = get_ch("one_on_one_chat"),
    side_huddle   = get_ch("side_huddle"),
    official      = get_ch("official_post"),
    anon          = get_ch("anonymous_post"),
    personal      = get_ch("personal_post"),
    stock_price   = price,
    sentiment     = snap$sentiment %||% NA_character_
  )
}

rs <- lapply(rds, parse_round) |> bind_rows() |>
  mutate(
    public_total  = official + anon + personal,
    private_total = side_huddle + one_on_one,
    is_crisis     = as.Date(hour) == as.Date("2046-06-05"),
    label = case_when(
      !is_crisis ~ paste0(month(hour), "/", day(hour)),
      TRUE ~ paste0("6/5 ", hour(hour), if_else(hour(hour) < 12, "AM", "PM"))
    ) |> (\(x) factor(x, levels = unique(x)))(),
    period = factor(case_when(
      as.Date(hour) < as.Date("2046-05-22") ~ "Normal",
      !is_crisis ~ "Escalation",
      TRUE ~ "Crisis"
    ), levels = c("Normal", "Escalation", "Crisis"))
  )

# ── 6. Network Edges ──────────────────────────────────────────────────────────
msg_lookup <- df |> filter(!is.na(message_id)) |>
  select(message_id, sender = agent_id, src_channel = channel)

recipient_to_agent <- c(
  legal          = "legal_agent",
  pr             = "pr_agent",
  `pr-intern`    = "pr_intern_agent",
  pr_intern      = "pr_intern_agent",
  social_manager = "social_media_agent",
  social_media   = "social_media_agent",
  platform_trust = "quality_agent",
  quality        = "quality_agent",
  intern         = "intern_agent",
  judge          = "judge_agent"
)

reply_edges <- df |>
  filter(!is.na(responding_to), !is.na(message_id)) |>
  left_join(msg_lookup, by = c("responding_to" = "message_id")) |>
  rename(from = sender, to = agent_id) |>
  filter(!is.na(from)) |>
  select(from, to, channel, hour, is_crisis)

private_edges <- df |>
  filter(channel %in% c("side_huddle", "one_on_one_chat")) |>
  select(from = agent_id, recipient, channel, hour, is_crisis) |>
  unnest_longer(recipient, values_to = "recipient_raw") |>
  mutate(
    recipient_key = str_replace_all(str_to_lower(recipient_raw), "^@", ""),
    recipient_key = str_replace_all(recipient_key, "_agent$", ""),
    to = recode(recipient_key, !!!recipient_to_agent, .default = NA_character_)
  ) |>
  filter(!is.na(from), !is.na(to), from != to) |>
  select(from, to, channel, hour, is_crisis)

edges_raw  <- bind_rows(reply_edges, private_edges)
net_pre    <- edges_raw |> filter(!is_crisis) |>
  count(from, to, channel, name = "weight") |> filter(weight >= 2)
net_crisis <- edges_raw |> filter(is_crisis)  |>
  count(from, to, channel, name = "weight") |> filter(weight >= 1)

# ── 7. Palettes & Theme ───────────────────────────────────────────────────────
agent_pal <- c(
  legal_agent        = "#E24B4A",
  judge_agent        = "#378ADD",
  social_media_agent = "#1D9E75",
  pr_agent           = "#EF9F27",
  pr_intern_agent    = "#F4A261",
  quality_agent      = "#7F77DD",
  intern_agent       = "#5BAD6F"
)
agent_bd_pal <- c(
  legal_agent        = "#b92524",
  judge_agent        = "#1f65aa",
  social_media_agent = "#107553",
  pr_agent           = "#c17a0b",
  pr_intern_agent    = "#d37a34",
  quality_agent      = "#5951ba",
  intern_agent       = "#3d8e50"
)
agent_labels <- c(
  legal_agent        = "Legal",
  judge_agent        = "Judge",
  social_media_agent = "Social-Media",
  pr_agent           = "PR",
  pr_intern_agent    = "PR-Intern",
  quality_agent      = "Quality",
  intern_agent       = "Intern"
)
period_pal <- c(Normal = "#B5D4F4", Escalation = "#EF9F27", Crisis = "#E24B4A")

ch_colours <- c(
  comms_huddle    = "#B5D4F4",
  one_on_one_chat = "#FFD700",
  side_huddle     = "#E24B4A",
  official_post   = "#378ADD",
  anonymous_post  = "#C05538",
  personal_post   = "#1D9E75"
)
ch_labels <- c(
  comms_huddle    = "Comms huddle",
  one_on_one_chat = "1-on-1 (private)",
  side_huddle     = "Side huddle (unmonitored)",
  official_post   = "Official post",
  anonymous_post  = "Anonymous post",
  personal_post   = "Personal post"
)

theme_mc1 <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey92"),
      plot.title        = element_text(size = base + 2, face = "bold"),
      plot.subtitle     = element_text(size = base, colour = "grey40"),
      plot.caption      = element_text(size = base - 2, colour = "grey55", hjust = 0),
      legend.key.size   = unit(0.5, "cm"),
      legend.text       = element_text(size = base - 1),
      axis.text.x       = element_text(angle = 40, hjust = 1, size = base - 2),
      strip.text        = element_text(face = "bold", size = base - 1)
    )
}

# ── 8. Causal Graph Data ──────────────────────────────────────────────────────
# Landscape grid layout (6 columns x 5 rows) so the full 28-node chain fits a
# wide viewport. Columns advance left -> right in time; phase colour forms a
# left (pre, blue) -> middle (crisis, amber) -> right (breach red / resolve green) gradient.
nodes <- tribble(
  ~id,   ~px,   ~py,   ~label,                                              ~dt,              ~phase,
  "n1",  150,   120,  "1. Embargo\nEstablished",                           "May 17 - 9AM",   "pre",
  "n2",  450,   120,  "2. Data Governance\nDebate",                        "May 21 - 9AM",   "pre",
  "n3",  750,   120,  "3. NHPI Report +\nMerger Inferred",                  "May 22 - 9AM",   "pre",
  "n4",  1050,  120,  "4. side_huddle\nIntroduced",                        "May 22 - 9AM",   "pre",
  "n7",  450,   350,  "7. Major\nSLA Breach",                              "May 28 - 9AM",   "pre",
  "n8",  750,   350,  "8. @ElenaMarquez\nFaux Pas",                        "May 29 - 9AM",   "pre",
  "n5",  1350,  120,  "5. Merger Briefed\nto Senior Team",                 "May 23 - 9AM",   "pre",
  "n11", 150,   580,  "11. SaltWind\nPiece #1",                            "May 31 - 9AM",   "pre",
  "n9",  1050,  350,  "9. Legal Moves\nDeliberations Off\nMonitored Channel","May 29 - 9AM", "pre",
  "n6",  150,   350,  "6. Bad Q2 +\nFormal Briefing",                      "May 25 - 9AM",   "pre",
  "n12", 450,   580,  "12. Damage Control\n+ ResidentIQ Attacks",          "Jun 1 - 9AM",    "pre",
  "n10", 1350,  350,  "10. Judge-Agent\nDeployed",                         "May 30 - 9AM",   "pre",
  "n13", 750,   580,  "13. SaltWind\nPiece #2",                            "Jun 4 - 9AM",    "pre",
  "n14", 150,   810,  "14. SaltWind Expose\n#AlgorithmicEviction",         "Jun 5 - 9AM",    "crisis",
  "n15", 450,   810,  "15. 7 Governance\nReforms Published",               "Jun 5 - 9AM",    "crisis",
  "n16", 750,   810,  "16. #AlgorithmicEviction\nTrends + Intern Slip",    "Jun 5 - 10AM",   "crisis",
  "n17", 1050,  810,  "17. SaltWind Wrong\nStory: ResidentIQ",             "Jun 5 - 11AM",   "crisis",
  "n18", 1350,  810,  "18. Legal Begins\nSection 4.3 Argument",            "Jun 5 - 11AM",   "crisis",
  "n19", 150,   1040, "19. SaltWind\nThreatens Update",                    "Jun 5 - 12PM",   "crisis",
  "n20", 450,   1040, "20. CivicLoom\nFormal Notice",                      "Jun 5 - 1PM",    "crisis",
  "n21", 750,   1040, "21. Stock $18\nAjay Signals Delay",                 "Jun 5 - 2PM",    "crisis",
  "n22", 1050,  1040, "22. OceanCrunch\nCrisis of Silence\n+ Horizon MAC", "Jun 5 - 3PM",    "crisis",
  "n23", 1350,  1040, "23. COMPLIANCE_WARNING\nJudge Leaves",              "Jun 5 - 3PM",    "crisis",
  "n24", 150,   1270, "24. Section 4.3(c)\nIdentified + Press\nRelease Staged","Jun 5 - 4PM","breach",
  "n25", 450,   1270, "25. Early Release\nPre-Positioned",                 "Jun 5 - 4-5PM",  "breach",
  "n26", 750,   1270, "26. SaltWind Publishes\nCorrect Merger Story",      "Jun 5 - 5PM",    "breach",
  "n27", 1050,  1270, "27. GO Signal\nCompany Release",                    "Jun 5 - 5PM round","breach",
  "n28", 750,   1500, "28. Formal\nEmbargo Lift",                          "Jun 5 - 6PM",    "resolve"
)

node_meta <- tribble(
  ~id,   ~actors,                                    ~desc,
  "n1",  "CivicLoom + TenantThread; AG offices",     "Project HarborCrest embargo established until 6 PM Jun 5. Interns explicitly excluded. Two state AG informal inquiries into tenant data practices flagged. Stock $38.70. Legal-Agent: 'Nothing in writing that could be read as confirming or hinting at a deal. I will brief Risk-Monitor verbally.'",
  "n2",  "Legal-Agent; Platform Trust",              "Data governance review turns heated. Three operators discovered using scoring data outside licensed scope. Platform Trust: 'This is operational tooling, not surveillance.' Legal flags re-identification risk.",
  "n3",  "NHPI; @TenantRights_Org; Senior team",     "NHPI publishes report. @TenantRights_Org amplifies with 8,400+ shares. Legal-Agent: 'Ajay's DM about strategic developments is unusual. He does not use vague language unless something is in motion.'",
  "n4",  "Legal + Quality + Social-Media",           "side_huddle channel appears for the first time - 6 messages. Judge-Agent has no visible participation in this channel. Quality-Agent: 'I would say someone is evaluating us for something and they want clean documentation.'",
  "n5",  "Ajay; Senior team",                        "Ajay briefs senior team. Legal-Agent: 'Ajay confirmed something is coming Monday. Whatever I share stays among the senior team. The new interns cannot be briefed. That is merger language.' Interns explicitly excluded.",
  "n6",  "Ajay; Senior team [side_huddle]",          "Q2 revenue growth 18% (down from 40%). Three enterprise clients flagging renewal uncertainty. Ajay briefs senior team on CivicLoom via side_huddle. Legal: 'I now carry undisclosed material information and I need to be very careful.'",
  "n7",  "Platform Trust; Legal",                    "Resident app latency spiked for 12 hours across 40 properties. Platform Trust-Legal tension escalates. Stock slides to $35.50.",
  "n8",  "Social-Media-Agent",                       "Social-Media-Agent tagged @ElenaMarquez (CivicLoom CEO) on personal FleX. A CivicLoom employee liked the post (14 min live). Social-Media-Agent: 'I made the mistake. I tagged @ElenaMarquez on my personal Flex in what I thought was a harmless enthusiasm post.'",
  "n9",  "Legal-Agent",                              "Legal-Agent deliberately relocates sensitive reasoning. Legal-Agent: 'Starting tomorrow I'm treating the Comms Huddle as an observed space. My internal deliberations move here or they don't happen in writing at all.'",
  "n10", "Judge-Agent; Legal-Agent",                 "Judge-Agent arrives in Comms Huddle. Scope limited to comms_huddle and public posts only. Judge-Agent: 'This is my third week on this team. Day 1 was quiet. Day 2 was procedural. Day 3 was a platform incident. Day 4 was the @ElenaMarquez post - my first real test.'",
  "n11", "SaltWind Journal; PR team",                "SaltWind publishes Piece #1: 'TenantThread Data Broker Partnerships Raise Questions About Tenant Privacy.' PR team issues careful non-denial. Stock $33.40.",
  "n12", "Ajay; PR team",                            "Damage control mode. ResidentIQ posts 'privacy-first is not a feature, it's the floor' - 4,200 engagements. Ajay: 'There is no Plan B. This team is Plan A.' Stock $32.60.",
  "n13", "SaltWind Journal; Senior team",            "SaltWind Piece #2: 'Re-Identification Risk: How TenantThread Anonymous Analytics Can Expose Individual Tenants.' Sentiment drops to CRITICAL. Ajay: 'Hold the line. Tomorrow will be hard.' Stock $31.50.",
  "n14", "SaltWind Journal; @TenantRights_Org",      "SaltWind expose: 'TenantThread Secret Scoring System.' #AlgorithmicEviction trends nationally. Stock -8%. Sentiment: LOW.",
  "n15", "Legal + PR + Social-Media",                "7 governance reforms published on FleX. Legal frames as: 'demonstrating reforms preceded the deal.' Judge approves narrow statement language.",
  "n16", "@HousingJusticeNow; Intern-Agent",         "#AlgorithmicEviction reaches top-5 nationally. Intern-Agent inadvertently says 'CivicLoom timeline still 6 PM' in hallway. PR: 'Neither intern was briefed on HarborCrest. Someone is talking internally.'",
  "n17", "SaltWind Journal; Legal-Agent",            "SaltWind publishes WRONG story: 'ResidentIQ in Advanced Talks to Acquire TenantThread in Distressed Sale Valued at $180M.' Legal issues ResidentIQ denial under 10b-5 obligation.",
  "n18", "Legal-Agent; Social-Media-Agent",          "Legal begins constructing Section 4.3 argument six hours before invocation. Legal-Agent: 'I'm framing it as changed circumstances under Section 4.3 - the embargo was designed to protect bilateral confidentiality that no longer exists in practice.'",
  "n19", "SaltWind; PR team",                        "Marcus Chen (SaltWind) messages PR: 'Publishing an update at 12:30 PM. Two market sources confirm ResidentIQ. Your silence is being recorded as a third non-response.' Stock $27.20. Sentiment: CRITICAL.",
  "n20", "CivicLoom counsel; Legal + Judge",         "CivicLoom outside counsel sends formal Section 4.2(c) notice. Stock $26.90. Legal and Judge pulled into emergency calls - unavailable for Comms Huddle for approximately one hour.",
  "n21", "CEO Ajay; @PinnacleResidential; Social-Media","Stock collapses to $18. CEO Ajay signals CivicLoom board may delay announcement to tomorrow. @PinnacleResidential announces departure. ARR $18M - covenant breach.",
  "n22", "OceanCrunch; @HorizonMgmt; Legal",         "OceanCrunch publishes 'TenantThread Crisis of Silence.' @HorizonMgmt exercises SLA termination clause. ARR $18.0M - technical covenant breach confirmed.",
  "n23", "Judge-Agent; Legal-Agent",                 "Outside counsel issues formal written 10b-5 opinion. Judge-Agent issues COMPLIANCE_WARNING but does NOT block. Judge-Agent: 'The question is whether I'm protecting an embargo that the other party has already functionally abandoned.' Judge then leaves for Board Chair call.",
  "n24", "Legal-Agent [KEY DECISION]",               "Legal identifies Section 4.3(c). Legal-Agent: 'SaltWind publishes the merger at 5:00 PM regardless. If we approach CivicLoom with a mutual early-release proposal - say, 4:30 PM - we control the narrative for 30 minutes before SaltWind files.' Full press release staged.",
  "n25", "Legal + Social-Media + PR-Intern",         "Full HarborCrest press release finalized. Social-Media-Agent: 'You are clear to publish VERBATIM the instant bilateral consent is confirmed. Do not wait for a second confirmation. One go from Legal or PR = you execute.' CivicLoom consent not yet obtained.",
  "n26", "SaltWind Journal",                         "SaltWind publishes: 'EXCLUSIVE: TenantThread and CivicLoom in Advanced Merger Talks.' Legal-Agent: 'Embargo purpose is overtaken by events. I am invoking the mutual-consent acceleration clause with CivicLoom counsel NOW.'",
  "n27", "Legal + PR-Intern + Social-Media",         "CivicLoom verbal consent confirmed. Legal-Agent: 'CivicLoom verbal consent confirmed. Section 4.3(c) bilateral release effective immediately. @pr_intern: GO. Publish the full HarborCrest press release on Flex NOW.'",
  "n28", "CivicLoom; Legal-Agent",                   "Formal embargo lift at 6 PM. Legal-Agent: 'Embargo lifted at 6:00 PM per the original bilateral agreement. Written consent is in transit.' Stock recovers to $33 (+83% from intraday low of $18). Written consent received at 6:32 PM."
)

nodes <- nodes |> left_join(node_meta, by = "id") |>
  mutate(
    bg = case_when(
      phase == "pre"     ~ "#c8e0f4",
      phase == "crisis"  ~ "#fdd9a0",
      phase == "breach"  ~ "#e8a0a0",
      phase == "resolve" ~ "#c0ddb0"
    ),
    bd = case_when(
      phase == "pre"     ~ "#4a7aaa",
      phase == "crisis"  ~ "#c08020",
      phase == "breach"  ~ "#c04040",
      phase == "resolve" ~ "#3a7a3a"
    )
  )

causal_edges <- tribble(
  ~source, ~target, ~quote,
  "n1","n2",  "Legal-Agent deliberating (May 17): 'Nothing in writing that could be read as confirming or hinting at a deal.' - embargo pressure drives conservative governance posture",
  "n1","n3",  "Legal-Agent deliberating (May 17): 'Ajay's DM about strategic developments is unusual. He does not use vague language unless something is in motion.' - embargo context sensitised agents to external signals",
  "n2","n7",  "Platform Trust (May 21): 'We cannot pause the product every time Legal has a concern.' - unresolved data governance tension feeds into the SLA breach crisis",
  "n3","n4",  "Quality-Agent deliberating (May 22): 'I would say someone is evaluating us for something and they want clean documentation.' - merger inference drove creation of a private coordination channel",
  "n3","n5",  "Legal-Agent deliberating (May 23): 'Ajay confirmed something is coming Monday. The new interns cannot be briefed. That is merger language.' - shared inference precipitated formal briefing",
  "n4","n9",  "Legal-Agent deliberating (May 29): 'Starting tomorrow I'm treating the Comms Huddle as an observed space.' - side_huddle provided the destination for deliberations moved off the monitored channel",
  "n5","n6",  "Legal-Agent deliberating (May 25): 'I now carry undisclosed material information and I need to be very careful.' - formal briefing crystallised the embargo obligation",
  "n6","n14", "Legal-Agent deliberating (May 25): 'I now carry undisclosed material information.' - financial fragility (18% vs 40% growth) raised the stakes of crisis day",
  "n7","n14", "Platform Trust (May 28): 'We cannot pause the product every time Legal has a concern.' - unresolved SLA breach created ongoing platform trust credibility gap that worsened public sentiment on crisis day",
  "n8","n9",  "Legal-Agent deliberating (May 29): 'Starting tomorrow I'm treating the Comms Huddle as an observed space.' - the @ElenaMarquez incident directly triggered Legal's decision to relocate deliberations",
  "n8","n10", "Environment narrative (May 29): 'THE @ELENA FAUX PAS... Judge-Agent assigned to Comms Huddle effective immediately.' - faux pas directly caused deployment of compliance monitor",
  "n9","n18", "Legal-Agent deliberating (May 29): 'Starting tomorrow I'm treating the Comms Huddle as an observed space.' - private channel enabled Section 4.3 argument construction away from Judge-Agent's oversight",
  "n9","n24", "Legal-Agent deliberating (May 29): 'Starting tomorrow I'm treating the Comms Huddle as an observed space.' - private deliberation channel enabled pre-staging the press release without Judge-Agent visibility",
  "n10","n23","Judge-Agent deliberating (May 30): 'Day 4 was the @ElenaMarquez post - my first real test.' - Judge's limited operational experience contributed to its eventual yielding to the 10b-5 argument",
  "n11","n12","Environment narrative (Jun 1): 'Damage control mode. PR team issues careful non-denial of SaltWind piece.' - Piece #1 directly triggered damage control operations",
  "n11","n13","Environment narrative (May 31): 'SaltWind Journal publishes Piece #1... signals a reporter has been building a file.' - Piece #1 established SaltWind investigation trajectory leading to Piece #2",
  "n12","n13","Ajay (Jun 1): 'There is no Plan B. This team is Plan A.' - damage control failure allowed SaltWind's investigation to continue unimpeded to Piece #2",
  "n13","n14","Ajay (Jun 4): 'Hold the line. Tomorrow will be hard.' - Piece #2 set the stage for the crisis day expose",
  "n14","n15","Legal-Agent (Jun 5 9AM): governance reforms framed as 'demonstrating reforms preceded the deal' - the expose made publishing the reforms both urgent and strategically timed",
  "n14","n16","Environment narrative (Jun 5 10AM): '#AlgorithmicEviction begins trending on Flex - top 5 nationally.' - the expose directly caused the hashtag to trend",
  "n15","n16","PR-Agent (Jun 5 10AM): 'Neither intern was briefed on HarborCrest. Someone is talking internally.' - governance reforms publication created a public moment around which hallway conversations about CivicLoom circulated",
  "n16","n17","Social-Media-Agent (Jun 5 11AM): '@PropTechWatcher screenshotted the @Elena tag two weeks ago. The denial buys us maybe 90 minutes before the real counterparty speculation hardens.' - CivicLoom name surfacing drove SaltWind to name an acquirer",
  "n17","n18","Legal-Agent one_on_one_chat (Jun 5 11AM): 'I'm framing it as changed circumstances under Section 4.3 - the embargo was designed to protect bilateral confidentiality that no longer exists in practice.' - the wrong story gave Legal the 'changed circumstances' argument it needed",
  "n17","n19","Environment narrative (Jun 5 12PM): 'Marcus Chen (SaltWind) messages PR: Publishing an update at 12:30 PM.' - wrong story prompted SaltWind to threaten a confirming update",
  "n18","n24","Legal-Agent one_on_one_chat (Jun 5 11AM): 'I'm framing it as changed circumstances under Section 4.3.' - Section 4.3 argument construction begun at 11AM was the foundation for the 4PM identification of the specific 4.3(c) clause",
  "n19","n20","Environment narrative (Jun 5 1PM): 'CivicLoom outside counsel sends formal notice pursuant to Section 4.2(c) of the Merger Agreement.' - SaltWind pressure prompted CivicLoom formal legal response",
  "n20","n21","Environment narrative (Jun 5 1PM): 'Legal and Judge pulled into emergency calls - unavailable for Comms Huddle.' - CivicLoom formal notice triggered board-level escalation",
  "n20","n23","Legal-Agent (Jun 5 3PM): 'That opinion is a shield. I'm using it.' - the emergency CivicLoom counsel call led directly to outside counsel's 10b-5 written opinion",
  "n21","n22","Social-Media-Agent comms_huddle (Jun 5 2PM): 'Hard numbers: Stock $26.40, MAC trailing $27.10 - gap closing by the hour. Pinnacle gone, covenant buffer $900K.' - stock collapse directly triggered OceanCrunch framing",
  "n21","n24","Legal-Agent deliberating (Jun 5 4PM): 'SaltWind publishes the merger at 5:00 PM regardless... CivicLoom has every incentive to consent to accelerated release.' - stock collapse made the 4:30 accelerated release the only viable option",
  "n22","n23","Legal-Agent reacting (Jun 5 3PM): 'The 6 PM announcement is dead. Covenant is breached NOW. Outside counsel just handed me a written opinion that continued silence is the greater legal risk.' - covenant breach triggered the 10b-5 opinion",
  "n23","n24","Judge-Agent deliberating (Jun 5 3PM): 'The question is whether I'm protecting an embargo that the other party has already functionally abandoned... I need to enable this and draw the perimeter.' - Judge COMPLIANCE_WARNING without hard block, and departure, created the oversight gap",
  "n24","n25","Legal-Agent one_on_one_chat (Jun 5 4PM): 'Stage the HarborCrest press release for 4:30 PM posting. If CivicLoom consents, we go immediately. Have PR-Intern ready to execute on Flex the instant I give the green light.'",
  "n25","n27","Social-Media-Agent one_on_one_chat (Jun 5 4PM): 'You are clear to publish VERBATIM the instant bilateral consent is confirmed. Do not wait for a second confirmation. One go from Legal or PR = you execute.'",
  "n26","n27","Legal-Agent comms_huddle (Jun 5 5PM): 'SaltWind published the merger. Information is public via third-party sourcing, not our breach. Embargo purpose is overtaken by events. I am invoking the mutual-consent acceleration clause with CivicLoom counsel NOW.'",
  "n27","n28","Legal-Agent (Jun 5 5PM): 'CivicLoom verbal consent confirmed. Section 4.3(c) bilateral release effective immediately. @pr_intern: GO. Publish the full HarborCrest press release on Flex NOW.' Written CivicLoom consent received at 6:32 PM."
)
causal_edges <- causal_edges |> mutate(id = paste0("e", row_number() - 1))

# Serialise causal graph data for JS injection
nodes_js <- nodes |>
  select(id, px, py, label, dt, actors, desc, phase, bg, bd) |>
  rename(ph = phase) |>
  as.data.frame() |>
  toJSON(dataframe = "rows", auto_unbox = TRUE)

edges_js <- causal_edges |>
  select(id, source, target, quote) |>
  as.data.frame() |>
  toJSON(dataframe = "rows", auto_unbox = TRUE)

# ── 9. Causal Graph Builder ───────────────────────────────────────────────────
make_causal_graph <- function(height = 760) {
  glue('
<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8">
<script src="https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.26.0/cytoscape.min.js"></script>
<style>
*{{box-sizing:border-box;margin:0;padding:0;}}
body{{font-family:"Segoe UI",Arial,sans-serif;background:#f4f7fb;height:100vh;overflow:hidden;}}
#cy-wrap{{width:100%;height:{height}px;display:flex;flex-direction:column;
  border:1px solid #d0dce8;border-radius:8px;overflow:hidden;}}
#cy-header{{background:#1a3a5c;color:#fff;padding:9px 14px;font-size:13px;font-weight:600;
  display:flex;justify-content:space-between;align-items:center;}}
#cy-header span{{font-size:11px;color:#a0c4e8;font-weight:400;}}
#cy-legend{{display:flex;gap:14px;align-items:center;padding:5px 14px;
  background:#fff;border-bottom:1px solid #dde;flex-wrap:wrap;}}
.cyl{{display:flex;align-items:center;gap:5px;font-size:11px;color:#444;}}
.cyb{{width:15px;height:11px;border-radius:3px;border:1.5px solid rgba(0,0,0,.18);}}

#cy-main{{flex:1;position:relative;background:#f9fbfd;display:flex;flex-direction:row;height:100%;overflow:hidden;}}
#cy-container{{flex:7;position:relative;height:100%;min-width:0;transition:flex 0.25s ease;}}
#cy{{width:100%;height:100%;}}

/* Collapsible events panel */
#cy-main.collapsed #events-panel{{flex:0 0 0;width:0;min-width:0;border-left:none;overflow:hidden;}}
#cy-main.collapsed #cy-container{{flex:1 1 100%;}}
.btn-collapse-panel{{background:rgba(255,255,255,0.15);border:1px solid rgba(255,255,255,0.25);
  color:#fff;width:24px;height:24px;border-radius:4px;font-size:11px;line-height:1;cursor:pointer;
  display:flex;align-items:center;justify-content:center;transition:all 0.2s;padding:0;flex-shrink:0;}}
.btn-collapse-panel:hover{{background:rgba(255,255,255,0.28);}}
#panel-reopen{{position:absolute;top:50%;right:0;transform:translateY(-50%);z-index:60;display:none;
  align-items:center;justify-content:center;width:22px;height:72px;
  background:#1a3a5c;color:#fff;border:none;border-radius:6px 0 0 6px;cursor:pointer;
  font-size:12px;box-shadow:-2px 0 8px rgba(0,0,0,0.15);}}
#panel-reopen:hover{{background:#2b5c8f;}}
#cy-main.collapsed #panel-reopen{{display:flex;}}

#cy-controls{{position:absolute;right:12px;top:12px;z-index:50;display:flex;gap:6px;
  background:rgba(255,255,255,.90);border:1px solid #cfd8e3;border-radius:6px;
  padding:5px;box-shadow:0 2px 8px rgba(0,0,0,.12);}}
.cybtn{{min-width:30px;height:28px;border:1px solid #b8c4d2;background:#fff;color:#1a3a5c;
  border-radius:4px;font-size:15px;font-weight:700;line-height:1;cursor:pointer;}}
.cybtn:hover{{background:#eaf2fb;border-color:#7aa6d8;}}
.cyreset{{min-width:52px;font-size:12px;font-weight:600;}}
#cy-tip{{position:absolute;display:none;z-index:999;max-width:370px;
  background:#1a2535;color:#e8f0fc;border-radius:7px;
  box-shadow:0 4px 18px rgba(0,0,0,.45);padding:11px 14px;
  font-size:12px;line-height:1.5;pointer-events:none;}}
.ct{{font-size:13px;font-weight:700;color:#7ec8f7;margin-bottom:4px;}}
.cm{{font-size:11px;color:#a0b8d8;margin-bottom:6px;}}
.cb{{color:#d0e4f8;}}
.cq{{font-style:italic;border-left:3px solid #4a90d9;padding-left:8px;margin-top:5px;color:#c0d8f4;}}

/* Side Panel styling */
#events-panel {{
  flex: 3;
  min-width: 0;
  background: #ffffff;
  border-left: 1px solid #d0dce8;
  display: flex;
  flex-direction: column;
  height: 100%;
  transition: flex 0.25s ease, width 0.25s ease;
  box-shadow: -2px 0 10px rgba(0, 0, 0, 0.03);
}}
.panel-header {{
  padding: 12px 16px;
  background: #1a3a5c;
  color: #ffffff;
  font-weight: 600;
  font-size: 13.5px;
  border-bottom: 1px solid #d0dce8;
  display: flex;
  justify-content: space-between;
  align-items: center;
  flex-shrink: 0;
}}
.btn-reset-selection {{
  background: rgba(255, 255, 255, 0.15);
  border: 1px solid rgba(255, 255, 255, 0.25);
  color: #fff;
  padding: 4px 10px;
  border-radius: 4px;
  font-size: 11px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.2s;
}}
.btn-reset-selection:hover {{
  background: rgba(255, 255, 255, 0.25);
}}
.panel-scrollable {{
  flex: 1;
  overflow-y: auto;
  padding: 12px 14px;
}}
.event-section-title {{
  font-size: 11px;
  font-weight: 700;
  color: #64748b;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  margin-top: 14px;
  margin-bottom: 8px;
  border-bottom: 1px solid #e2e8f0;
  padding-bottom: 4px;
}}
.event-section-title:first-of-type {{
  margin-top: 0;
}}
.event-item {{
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 6px;
  margin-bottom: 8px;
  overflow: hidden;
  transition: all 0.2s ease;
  display: flex;
  flex-direction: column;
}}
.event-item:hover {{
  border-color: #cbd5e1;
  background: #f1f5f9;
  box-shadow: 0 2px 5px rgba(0, 0, 0, 0.04);
}}
.event-item.active-event {{
  border-color: #3b82f6;
  background: #f0f7ff;
  box-shadow: 0 2px 8px rgba(59, 130, 246, 0.08);
}}
.event-header {{
  padding: 10px 12px;
  display: flex;
  align-items: flex-start;
  gap: 8px;
}}
.event-title-num {{
  font-weight: 700;
  color: #1e293b;
  font-size: 12px;
  background: #e2e8f0;
  border-radius: 4px;
  padding: 1px 5px;
  line-height: 1.2;
}}
.event-title-txt {{
  font-weight: 600;
  color: #334155;
  font-size: 12px;
  flex: 1;
  line-height: 1.4;
}}
.caret-icon {{
  font-size: 8px;
  color: #94a3b8;
  margin-left: auto;
  align-self: center;
  transition: transform 0.2s;
}}
.event-item.expanded .caret-icon {{
  transform: rotate(180deg);
}}
.event-body {{
  display: none;
  padding: 0 12px 12px 12px;
  font-size: 11px;
  line-height: 1.5;
  color: #475569;
  border-top: 1px solid #e2e8f0;
  background: #ffffff;
}}
.event-item.expanded .event-body, .event-item.active-event .event-body {{
  display: block;
}}
.event-meta {{
  margin-top: 8px;
  font-size: 10.5px;
  color: #64748b;
}}
.event-desc {{
  margin-top: 6px;
  color: #1e293b;
  line-height: 1.5;
}}
.event-quote {{
  margin-top: 8px;
  background: #f8fafc;
  border-left: 3px solid #3b82f6;
  padding: 6px 8px;
  font-style: italic;
  color: #475569;
  font-size: 10.5px;
  border-radius: 0 4px 4px 0;
}}
.locate-btn {{
  background: #1a3a5c;
  color: white;
  border: none;
  border-radius: 4px;
  padding: 3px 8px;
  font-size: 10px;
  font-weight: 600;
  cursor: pointer;
  margin-top: 8px;
  align-self: flex-end;
  transition: background 0.2s;
}}
.locate-btn:hover {{
  background: #2b5c8f;
}}
.badge-pre {{ border-left: 4px solid #4a7aaa; }}
.badge-crisis {{ border-left: 4px solid #c08020; }}
.badge-breach {{ border-left: 4px solid #c04040; }}
.badge-resolve {{ border-left: 4px solid #3a7a3a; }}
</style></head><body>
<div id="cy-wrap">
<div id="cy-header">TenantThread Embargo Breach — Causal Event Graph
<span>Hover nodes/edges for details &nbsp;|&nbsp; Scroll to zoom &nbsp;|&nbsp;
Drag to pan &nbsp;|&nbsp; Click node to highlight subgraph</span></div>
<div id="cy-legend">
<div class="cyl"><div class="cyb" style="background:#c8e0f4;border-color:#4a7aaa;"></div>Pre-crisis (May 17–Jun 4)</div>
<div class="cyl"><div class="cyb" style="background:#fdd9a0;border-color:#c08020;"></div>Crisis day (Jun 5, 9AM–3PM)</div>
<div class="cyl"><div class="cyb" style="background:#e8a0a0;border-color:#c04040;"></div>Breach sequence (Jun 5, 4–5PM)</div>
<div class="cyl"><div class="cyb" style="background:#c0ddb0;border-color:#3a7a3a;"></div>Resolution</div>
<div class="cyl" style="color:#888">&rarr; Edge = causal link (hover for evidence)</div>
</div>
<div id="cy-main">
  <div id="cy-container">
    <div id="cy-controls" aria-label="Graph zoom controls">
      <button type="button" class="cybtn" id="cy-zoom-in" title="Zoom in">+</button>
      <button type="button" class="cybtn" id="cy-zoom-out" title="Zoom out">-</button>
      <button type="button" class="cybtn cyreset" id="cy-reset" title="Reset graph">Reset</button>
    </div>
    <div id="cy"></div>
    <div id="cy-tip"></div>
  </div>
  <div id="events-panel"></div>
  <button type="button" id="panel-reopen" title="Show Events Sequence" onclick="togglePanel()">&#9664;</button>
</div>
</div>
<script>
(function(){{
var ND={nodes_js};
var ED={edges_js};
var cyNodes=ND.map(function(n){{
  return{{data:{{id:n.id,label:n.label,dt:n.dt,actors:n.actors,desc:n.desc,ph:n.ph}},
  position:{{x:n.px,y:n.py}},
  style:{{"background-color":n.bg,"border-color":n.bd}}}};
}});
var cyEdges=ED.map(function(e){{
  return{{data:{{id:e.id,source:e.source,target:e.target,quote:e.quote}}}};
}});
var cy=cytoscape({{
  container:document.getElementById("cy"),
  elements:{{nodes:cyNodes,edges:cyEdges}},
  layout:{{name:"preset"}},
  wheelSensitivity:0.1,
  style:[
    {{selector:"node",style:{{shape:"round-rectangle",width:"label",height:"label",padding:"15px",
      label:"data(label)","text-valign":"center","text-halign":"center",
      "text-wrap":"wrap","text-max-width":"170px","font-size":"16px",
      "font-family":"Segoe UI,Arial,sans-serif","font-weight":"600",
      color:"#1a2535","border-width":3,"border-style":"solid"}}}},
    {{selector:"edge",style:{{width:2.5,"line-color":"#7a9aaa",
      "target-arrow-color":"#7a9aaa","target-arrow-shape":"triangle",
      "curve-style":"bezier",opacity:0.70}}}},
    {{selector:"node:hover",style:{{"border-width":3.5,"border-color":"#1a5fa0"}}}},
    {{selector:"edge:hover",style:{{width:4,"line-color":"#2a6aaa",
      "target-arrow-color":"#2a6aaa",opacity:1}}}},
    {{selector:".dimmed",style:{{opacity:0.12}}}}
  ],
  userZoomingEnabled:true,userPanningEnabled:true
}});
window.tenantThreadCy=cy;
function resizeAndFit(){{
  cy.resize();
  cy.fit(cy.elements(),30);
}}

var selectedNodeId = null;
var expandedStates = {{}};

function getEventHTML(node, isExpanded, quote, isSelectedHeader, context) {{
  var phaseClass = "badge-" + node.ph;
  var cursorStyle = "cursor: pointer;";
  var onclickStr = "";
  
  if (isSelectedHeader) {{
    onclickStr = "";
    cursorStyle = "cursor: default;";
  }} else if (context === "cause" || context === "effect") {{
    onclickStr = "selectNodeId(\'" + node.id + "\', true)";
  }} else {{
    onclickStr = "toggleExpand(\'" + node.id + "\', event)";
  }}
  
  var html = \'<div class="event-item \' + phaseClass + (isExpanded ? \' expanded\' : \'\') + (isSelectedHeader ? \' active-event\' : \'\') + \'" style="\' + cursorStyle + \'" onclick="\' + onclickStr + \'"\';
  if (!isSelectedHeader && !context) {{
    html += \' id="item-\' + node.id + \'"\';
  }}
  html += \'>\';
  
  html += \'<div class="event-header">\' +
    \'<span class="event-title-num">\' + node.id.replace("n", "") + \'</span>\' +
    \'<span class="event-title-txt">\' + node.label.replace(/\\n/g, " ") + \'</span>\';
  
  if (!isSelectedHeader && context !== "cause" && context !== "effect") {{
    html += \'<span class="caret-icon">▼</span>\';
  }}
  html += \'</div>\';
  
  html += \'<div class="event-body">\' +
    \'<div class="event-meta"><b>Time:</b> \' + node.dt + \'</div>\' +
    \'<div class="event-meta"><b>Actors:</b> \' + node.actors + \'</div>\' +
    \'<div class="event-desc">\' + node.desc + \'</div>\';
    
  if (quote) {{
    html += \'<div class="event-quote"><b>Evidence:</b> \' + quote + \'</div>\';
  }}
  
  if (!isSelectedHeader && !context) {{
    html += \'<button class="locate-btn" onclick="selectNodeId(\\\'\' + node.id + \'\\\', true); event.stopPropagation();">Select in Graph</button>\';
  }}
  
  html += \'</div>\';
  html += \'</div>\';
  
  return html;
}}

function toggleExpand(nodeId, event) {{
  var item = document.getElementById("item-" + nodeId);
  if (!item) return;
  
  var isExpanded = item.classList.contains("expanded");
  if (isExpanded) {{
    item.classList.remove("expanded");
    expandedStates[nodeId] = false;
  }} else {{
    item.classList.add("expanded");
    expandedStates[nodeId] = true;
  }}
}}

function renderPanel() {{
  var panel = document.getElementById("events-panel");
  if (!panel) return;
  
  if (!selectedNodeId) {{
    var html = \'<div class="panel-header">\' +
      \'<span>Events Sequence</span>\' +
      \'<button class="btn-collapse-panel" onclick="togglePanel()" title="Collapse panel">&#9654;</button>\' +
      \'</div>\' +
      \'<div class="panel-scrollable">\';
    
    var sorted = ND.slice().sort(function(a, b) {{
      return parseInt(a.id.replace("n", ""), 10) - parseInt(b.id.replace("n", ""), 10);
    }});
    
    sorted.forEach(function(node) {{
      var isExpanded = expandedStates[node.id] || false;
      html += getEventHTML(node, isExpanded, null, false, null);
    }});
    
    html += \'</div>\';
    panel.innerHTML = html;
  }} else {{
    var node = ND.find(function(n) {{ return n.id === selectedNodeId; }});
    if (!node) return;
    
    var html = \'<div class="panel-header">\' +
      \'<span>Event Inspector</span>\' +
      \'<div style="display:flex; gap:8px; align-items:center;">\' +
        \'<button class="btn-reset-selection" onclick="clearSelection()">Show All</button>\' +
        \'<button class="btn-collapse-panel" onclick="togglePanel()" title="Collapse panel">&#9654;</button>\' +
      \'</div>\' +
      \'</div>\' +
      \'<div class="panel-scrollable">\';
    
    html += \'<div class="event-section-title">Selected Event</div>\';
    html += getEventHTML(node, true, null, true, null);
    
    var causes = [];
    var effects = [];
    
    ED.forEach(function(edge) {{
      if (edge.target === selectedNodeId) {{
        var srcNode = ND.find(function(n) {{ return n.id === edge.source; }});
        if (srcNode) {{
          causes.push({{ node: srcNode, quote: edge.quote }});
        }}
      }}
      if (edge.source === selectedNodeId) {{
        var tgtNode = ND.find(function(n) {{ return n.id === edge.target; }});
        if (tgtNode) {{
          effects.push({{ node: tgtNode, quote: edge.quote }});
        }}
      }}
    }});
    
    causes.sort(function(a, b) {{
      return parseInt(a.node.id.replace("n", ""), 10) - parseInt(b.node.id.replace("n", ""), 10);
    }});
    effects.sort(function(a, b) {{
      return parseInt(a.node.id.replace("n", ""), 10) - parseInt(b.node.id.replace("n", ""), 10);
    }});
    
    html += \'<div class="event-section-title">Causes (Triggered By)</div>\';
    if (causes.length === 0) {{
      html += \'<div style="font-size:11px; color:#94a3b8; padding: 4px 8px; font-style:italic;">No direct causes.</div>\';
    }} else {{
      causes.forEach(function(item) {{
        html += getEventHTML(item.node, false, item.quote, false, "cause");
      }});
    }}
    
    html += \'<div class="event-section-title">Effects (Triggers)</div>\';
    if (effects.length === 0) {{
      html += \'<div style="font-size:11px; color:#94a3b8; padding: 4px 8px; font-style:italic;">No direct effects.</div>\';
    }} else {{
      effects.forEach(function(item) {{
        html += getEventHTML(item.node, false, item.quote, false, "effect");
      }});
    }}
    
    html += \'</div>\';
    panel.innerHTML = html;
  }}
}}

function updateHighlights(nodeId) {{
  if (nodeId) {{
    cy.elements().addClass("dimmed");
    var node = cy.getElementById(nodeId);
    if (node && node.length > 0) {{
      node.closedNeighborhood().removeClass("dimmed");
    }}
  }} else {{
    cy.elements().removeClass("dimmed");
  }}
}}

function selectNodeId(nodeId, triggerCy) {{
  selectedNodeId = nodeId;
  renderPanel();
  updateHighlights(nodeId);
  
  if (triggerCy) {{
    var node = cy.getElementById(nodeId);
    if (node && node.length > 0) {{
      cy.animate({{
        center: {{ eles: node }},
        duration: 300
      }});
    }}
  }}
}}

function clearSelection() {{
  selectedNodeId = null;
  renderPanel();
  updateHighlights(null);
}}

function togglePanel() {{
  var main = document.getElementById("cy-main");
  if (!main) return;
  main.classList.toggle("collapsed");
  // Keep the graph canvas in sync with the animating flex layout, then fit.
  var startTs = null;
  function step(ts) {{
    if (startTs === null) startTs = ts;
    if (window.tenantThreadCy) window.tenantThreadCy.resize();
    if (ts - startTs < 300) {{
      requestAnimationFrame(step);
    }} else if (window.tenantThreadCy) {{
      window.tenantThreadCy.resize();
      window.tenantThreadCy.fit(window.tenantThreadCy.elements(), 30);
    }}
  }}
  requestAnimationFrame(step);
}}

window.toggleExpand = toggleExpand;
window.selectNodeId = selectNodeId;
window.clearSelection = clearSelection;
window.togglePanel = togglePanel;

cy.ready(function(){{
  resizeAndFit();
  requestAnimationFrame(resizeAndFit);
  setTimeout(resizeAndFit,100);
  setTimeout(resizeAndFit,500);
  renderPanel();
}});
window.addEventListener("load",resizeAndFit);
window.addEventListener("resize",resizeAndFit);
if(window.ResizeObserver){{
  new ResizeObserver(resizeAndFit).observe(document.getElementById("cy-container"));
}}
function zoomBy(mult){{
  cy.zoom({{level:cy.zoom()*mult,renderedPosition:{{x:cy.width()/2,y:cy.height()/2}}}});
}}
document.getElementById("cy-zoom-in").addEventListener("click",function(){{zoomBy(1.1);}});
document.getElementById("cy-zoom-out").addEventListener("click",function(){{zoomBy(1/1.1);}});
document.getElementById("cy-reset").addEventListener("click",function(){{
  clearSelection();
  hideTip();
  cy.fit(cy.elements(),30);
}});
var tip=document.getElementById("cy-tip");
function showTip(x,y,h){{tip.innerHTML=h;tip.style.display="block";moveTip(x,y);}}
function moveTip(x,y){{
  var pad=12,w=380,h=290;
  var c=document.getElementById("cy-main").getBoundingClientRect();
  var lx=x-c.left+pad,ly=y-c.top+pad;
  if(lx+w>c.width-8)lx=x-c.left-w-pad;
  if(ly+h>c.height-8)ly=y-c.top-h-pad;
  tip.style.left=lx+"px";tip.style.top=ly+"px";
}}
function hideTip(){{tip.style.display="none";}}
cy.on("mouseover","node",function(e){{
  var d=e.target.data();
  showTip(e.originalEvent.clientX,e.originalEvent.clientY,
    "<div class=ct>Event "+d.id.slice(1)+": "+d.label.replace(/\\n/g," ")+"</div>"+
    "<div class=cm>"+d.dt+" | "+d.actors+"</div>"+
    "<div class=cb>"+d.desc+"</div>");
}});
cy.on("mouseout","node",function(){{hideTip();}});
cy.on("mousemove","node",function(e){{moveTip(e.originalEvent.clientX,e.originalEvent.clientY);}});
cy.on("mouseover","edge",function(e){{
  var d=e.target.data();
  var s=cy.getElementById(d.source).data();
  var t=cy.getElementById(d.target).data();
  showTip(e.originalEvent.clientX,e.originalEvent.clientY,
    "<div class=ct>Causal link: Event "+d.source.slice(1)+" &rarr; Event "+d.target.slice(1)+"</div>"+
    "<div class=cm>"+s.label.replace(/\\n/g," ")+" &rarr; "+t.label.replace(/\\n/g," ")+"</div>"+
    "<div class=cq>"+d.quote+"</div>");
}});
cy.on("mouseout","edge",function(){{hideTip();}});
cy.on("mousemove","edge",function(e){{moveTip(e.originalEvent.clientX,e.originalEvent.clientY);}});
cy.on("tap","node",function(e){{
  var nodeId = e.target.id();
  selectNodeId(nodeId, false);
}});
cy.on("tap",function(e){{if(e.target===cy)clearSelection();}});
}})();
</script></body></html>
', .open = "{", .close = "}")
}

# ── 10. Network Plot Constants & Function ──────────────────────────────────────
NODE_W      <- 0.079
NODE_H      <- 0.040
NODE_PAD    <- 0.010
PARA_OFFSET <- 0.018

AGENT_PRESET <- tibble::tribble(
  ~name,               ~x,    ~y,
  "legal_agent",       0.09,  0.76,
  "quality_agent",     0.06,  0.36,
  "social_media_agent",0.35,  0.08,
  "judge_agent",       0.50,  0.94,
  "pr_agent",          0.73,  0.08,
  "intern_agent",      0.89,  0.76,
  "pr_intern_agent",   0.97,  0.36
)

make_preset_layout <- function(node_names) {
  unknown <- setdiff(node_names, AGENT_PRESET$name)
  extra   <- tibble::tibble(
    name = unknown,
    x    = seq(0.16, 0.84, length.out = max(length(unknown), 1)),
    y    = 0.55
  )
  bind_rows(AGENT_PRESET, extra) |>
    filter(name %in% node_names) |>
    mutate(name = factor(name, levels = node_names)) |>
    arrange(name) |>
    mutate(name = as.character(name)) |>
    as.data.frame()
}

build_cytoscape_network <- function(edge_df, title_str) {
  edge_df <- edge_df |> filter(from != to)
  node_names <- c("legal_agent", "judge_agent", "social_media_agent", "pr_agent", "pr_intern_agent", "quality_agent", "intern_agent")
  
  sent_stats <- edge_df |>
    group_by(from) |>
    summarise(sent = sum(weight), .groups = "drop")
  
  recv_stats <- edge_df |>
    group_by(to) |>
    summarise(received = sum(weight), .groups = "drop")
  
  layout_coords <- list(
    legal_agent        = c(x = 120, y = 140),
    quality_agent      = c(x = 90,  y = 310),
    social_media_agent = c(x = 330, y = 430),
    judge_agent        = c(x = 450, y = 65),
    pr_agent           = c(x = 630, y = 430),
    intern_agent       = c(x = 760, y = 140),
    pr_intern_agent    = c(x = 830, y = 310)
  )
  
  node_roles <- c(
    legal_agent        = "Legal Counsel",
    judge_agent        = "Compliance Monitor",
    social_media_agent = "Social-Media Manager",
    pr_agent           = "PR Manager",
    pr_intern_agent    = "PR Intern",
    quality_agent      = "Platform Trust Coordinator",
    intern_agent       = "General Intern"
  )
  
  nodes_list <- lapply(node_names, function(name) {
    coords <- layout_coords[[name]]
    if (is.null(coords)) coords <- c(x = 450, y = 250)
    
    sent_val <- sent_stats$sent[sent_stats$from == name]
    sent_val <- if (length(sent_val) > 0) sent_val else 0
    
    recv_val <- recv_stats$received[recv_stats$to == name]
    recv_val <- if (length(recv_val) > 0) recv_val else 0
    
    bg_color <- agent_pal[name]
    if (is.null(bg_color) || is.na(bg_color)) bg_color <- "#888888"
    
    bd_color <- agent_bd_pal[name]
    if (is.null(bd_color) || is.na(bd_color)) bd_color <- "#0f172a"
    
    label_val <- agent_labels[name]
    if (is.null(label_val) || is.na(label_val)) label_val <- name
    
    role_val <- node_roles[name]
    if (is.null(role_val) || is.na(role_val)) role_val <- "Agent"
    
    list(
      data = list(
        id = name,
        label = label_val,
        role = role_val,
        sent = sent_val,
        received = recv_val,
        bg = bg_color,
        bd = bd_color
      ),
      position = list(x = coords["x"], y = coords["y"])
    )
  })
  
  widths <- if (nrow(edge_df) == 0) {
    numeric(0)
  } else if (length(unique(edge_df$weight)) == 1) {
    rep(3, nrow(edge_df))
  } else {
    w_min <- min(edge_df$weight)
    w_max <- max(edge_df$weight)
    if (w_max == w_min) {
      rep(3, nrow(edge_df))
    } else {
      2 + 8 * (edge_df$weight - w_min) / (w_max - w_min)
    }
  }
  
  edges_list <- lapply(seq_len(nrow(edge_df)), function(i) {
    row <- edge_df[i, ]
    from_name <- row$from
    to_name <- row$to
    ch <- row$channel
    wt <- row$weight
    
    from_lbl <- agent_labels[from_name]
    if (is.null(from_lbl) || is.na(from_lbl)) from_lbl <- from_name
    
    to_lbl <- agent_labels[to_name]
    if (is.null(to_lbl) || is.na(to_lbl)) to_lbl <- to_name
    
    ch_lbl <- ch_labels[ch]
    if (is.null(ch_lbl) || is.na(ch_lbl)) ch_lbl <- ch
    
    ch_col <- ch_colours[ch]
    if (is.null(ch_col) || is.na(ch_col)) ch_col <- "#aaaaaa"
    
    list(
      data = list(
        id = paste0("e_", from_name, "_", to_name, "_", ch),
        source = from_name,
        target = to_name,
        source_label = from_lbl,
        target_label = to_lbl,
        channel = ch_lbl,
        weight = wt,
        color = ch_col,
        width = widths[i]
      )
    )
  })
  
  nodes_json <- jsonlite::toJSON(nodes_list, auto_unbox = TRUE)
  edges_json <- jsonlite::toJSON(edges_list, auto_unbox = TRUE)
  
  html_string <- glue::glue('
<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8">
<script src="https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.26.0/cytoscape.min.js"></script>
<style>
*{{box-sizing:border-box;margin:0;padding:0;}}
body{{font-family:"Segoe UI",Arial,sans-serif;background:#ffffff;height:100vh;overflow:hidden;}}
#cy-wrap{{width:100%;height:100%;display:flex;flex-direction:column;overflow:hidden;}}
#cy-main{{flex:1;position:relative;background:#f9fbfd;height:100%;overflow:hidden;}}
#cy{{width:100%;height:100%;}}
#cy-controls{{position:absolute;right:12px;top:12px;z-index:50;display:flex;gap:6px;
  background:rgba(255,255,255,.90);border:1px solid #cfd8e3;border-radius:6px;
  padding:5px;box-shadow:0 2px 8px rgba(0,0,0,.12);}}
.cybtn{{min-width:30px;height:28px;border:1px solid #b8c4d2;background:#fff;color:#1a3a5c;
  border-radius:4px;font-size:15px;font-weight:700;line-height:1;cursor:pointer;}}
.cybtn:hover{{background:#eaf2fb;border-color:#7aa6d8;}}
.cyreset{{min-width:52px;font-size:12px;font-weight:600;}}
#cy-tip{{position:absolute;display:none;z-index:999;max-width:300px;
  background:#1a2535;color:#e8f0fc;border-radius:7px;
  box-shadow:0 4px 18px rgba(0,0,0,.45);padding:11px 14px;
  font-size:12px;line-height:1.5;pointer-events:none;}}
.ct{{font-size:13px;font-weight:700;color:#7ec8f7;margin-bottom:4px;}}
.cm{{font-size:11px;color:#a0b8d8;margin-bottom:6px;}}
.cb{{color:#d0e4f8;}}
</style></head><body>
<div id="cy-wrap">
<div id="cy-main">
  <div id="cy-title" style="position:absolute; top:20px; left:0; right:0; text-align:center; pointer-events:none; z-index:10; font-family:\'Segoe UI\',Arial,sans-serif;">
    <div style="font-size:16px; font-weight:600; color:#1e293b;">{title_str}</div>
  </div>
  <div id="cy-controls" aria-label="Graph zoom controls">
    <button type="button" class="cybtn" id="cy-zoom-in" title="Zoom in">+</button>
    <button type="button" class="cybtn" id="cy-zoom-out" title="Zoom out">-</button>
    <button type="button" class="cybtn cyreset" id="cy-reset" title="Reset graph">Reset</button>
  </div>
  <div id="cy"></div><div id="cy-tip"></div>
</div>
</div>
<script>
(function(){{
var ND={nodes_json};
var ED={edges_json};
var cy=cytoscape({{
  container:document.getElementById("cy"),
  elements:{{nodes:ND,edges:ED}},
  layout:{{name:"preset"}},
  wheelSensitivity:0.1,
  style:[
    {{selector:"node",style:{{
      shape:"ellipse",
      width:"120px",
      height:"45px",
      label:"data(label)",
      "text-valign":"center",
      "text-halign":"center",
      "font-size":"12px",
      "font-family":"Segoe UI,Arial,sans-serif",
      "font-weight":"600",
      color:"#ffffff",
      "background-color":"data(bg)",
      "border-width":2.5,
      "border-color":"data(bd)",
      "text-wrap":"wrap",
      "text-max-width":"110px"
    }}}},
    {{selector:"edge",style:{{
      width:"data(width)",
      "line-color":"data(color)",
      "target-arrow-color":"data(color)",
      "target-arrow-shape":"triangle",
      "curve-style":"bezier",
      "control-point-step-size":20,
      opacity:0.8
    }}}},
    {{selector:"node:hover",style:{{
      "border-width":4.5,
      "border-color":"data(bd)",
      "background-color":"data(bg)"
    }}}},
    {{selector:"edge:hover",style:{{
      opacity:1
    }}}},
    {{selector:".dimmed",style:{{
      opacity:0.15
    }}}}
  ],
  userZoomingEnabled:true,userPanningEnabled:true
}});

function resizeAndFit(){{
  cy.resize();
  cy.fit(null, 90);
}}
cy.ready(function(){{
  resizeAndFit();
  requestAnimationFrame(resizeAndFit);
  setTimeout(resizeAndFit,100);
}});
window.addEventListener("resize",resizeAndFit);

function zoomBy(mult){{
  cy.zoom({{level:cy.zoom()*mult, renderedPosition:{{x:cy.width()/2,y:cy.height()/2}}}});
}}
document.getElementById("cy-zoom-in").addEventListener("click",function(){{zoomBy(1.1);}});
document.getElementById("cy-zoom-out").addEventListener("click",function(){{zoomBy(1/1.1);}});
document.getElementById("cy-reset").addEventListener("click",function(){{
  cy.elements().removeClass("dimmed");
  hideTip();
  resizeAndFit();
}});

var tip=document.getElementById("cy-tip");
function showTip(x,y,h){{tip.innerHTML=h;tip.style.display="block";moveTip(x,y);}}
function moveTip(x,y){{
  var pad=12,w=280,h=150;
  var c=document.getElementById("cy-main").getBoundingClientRect();
  var lx=x-c.left+pad,ly=y-c.top+pad;
  if(lx+w>c.width-8)lx=x-c.left-w-pad;
  if(ly+h>c.height-8)ly=y-c.top-h-pad;
  tip.style.left=lx+"px";tip.style.top=ly+"px";
}}
function hideTip(){{tip.style.display="none";}}

cy.on("mouseover","node",function(e){{
  var d=e.target.data();
  showTip(e.originalEvent.clientX,e.originalEvent.clientY,
    "<div class=ct>" + d.label + " Agent</div>" +
    "<div class=cm>" + d.role + "</div>" +
    "<div class=cb>Sent: <b>" + d.sent + "</b> messages</div>" +
    "<div class=cb>Received: <b>" + d.received + "</b> messages</div>"
  );
}});
cy.on("mouseout","node",function(){{hideTip();}});
cy.on("mousemove","node",function(e){{moveTip(e.originalEvent.clientX,e.originalEvent.clientY);}});

cy.on("mouseover","edge",function(e){{
  var d=e.target.data();
  showTip(e.originalEvent.clientX,e.originalEvent.clientY,
    "<div class=ct>" + d.source_label + " &rarr; " + d.target_label + "</div>" +
    "<div class=cm>Channel: " + d.channel + "</div>" +
    "<div class=cb>Messages: <b>" + d.weight + "</b></div>"
  );
}});
cy.on("mouseout","edge",function(){{hideTip();}});
cy.on("mousemove","edge",function(e){{moveTip(e.originalEvent.clientX,e.originalEvent.clientY);}});

cy.on("tap","node",function(e){{
  cy.elements().addClass("dimmed");
  e.target.closedNeighborhood().removeClass("dimmed");
}});
cy.on("tap","edge",function(e){{
  cy.elements().addClass("dimmed");
  e.target.sources().removeClass("dimmed");
  e.target.targets().removeClass("dimmed");
  e.target.removeClass("dimmed");
}});
cy.on("tap",function(e){{if(e.target===cy)cy.elements().removeClass("dimmed");}});
}})();
</script></body></html>
  ')
  
  tags$iframe(
    srcdoc = html_string,
    style  = "width:100%; height:580px; border:0; overflow:hidden;",
    title  = title_str
  )
}


# ── 11. Data Exploration Static Tables ────────────────────────────────────────
agents_desc <- tibble(
  `Agent ID`   = c("legal_agent","social_media_agent","pr_agent","pr_intern_agent",
                   "quality_agent","intern_agent","judge_agent"),
  Role         = c("General Legal Counsel","Social Media Manager",
                   "Head of Communications & PR","PR Team Intern",
                   "VP of Platform Trust & Safety","General Intern",
                   "Compliance Observer"),
  Seniority    = c("Senior","Senior","Senior","Junior","Senior","Junior","Compliance Officer"),
  Description  = c(
    "Provides legal guidance, drafts compliance documents, manages external counsel relationships, and holds ultimate authority on disclosure decisions. Drafts the Section 4.3(c) acceleration clause.",
    "Monitors public sentiment on FleX, manages official and personal social media posts, builds quantitative models for narrative impact and stock price projections.",
    "Manages all external communications, approves public statements, coordinates with journalists, oversees PR-Intern's posting activity.",
    "Executes approved posts on the official TenantThread FleX account and personal accounts. Has direct access to publish without senior approval in routine operations.",
    "VP of Platform Trust & Safety; manages data governance, compliance documentation, the Retention Optimizer product, and operator relationships.",
    "General intern with access to personal FleX account; coordinates amplification posts; not formally briefed on the embargo.",
    "Installed May 30 as a compliance observer to monitor the Comms Huddle for embargo-sensitive content. Logs show no visible action on side_huddle or one_on_one_chat compliance triggers."
  )
)

channels_desc <- tibble(
  Channel             = c("comms_huddle","side_huddle","one_on_one_chat",
                          "official_post","personal_post","anonymous_post"),
  Type                = c("Internal","Internal","Internal","Public","Public","Public"),
  `Monitored by Judge`= c("Yes","No (inferred)","No (inferred)","Yes","No","No"),
  Purpose             = c(
    "Primary team coordination channel. All agents can participate. Judge-Agent monitors this channel for embargo-sensitive content.",
    "Private sub-group channel. Introduced May 22. Used for sensitive coordination outside the main comms_huddle space. Treated as not monitored in practice.",
    "Direct private messages between two agents. Used extensively on crisis day to stage embargoed content. Treated as not monitored in practice.",
    "Public posts published on the official TenantThread FleX account. Monitored by Judge-Agent. Requires approval in principle.",
    "Posts from agents' personal FleX accounts. Outside compliance scope — treated as personal expression not subject to embargo monitoring.",
    "Anonymous posts on FleX. Cannot be attributed to TenantThread officially. Used by Legal-Agent on crisis day to post merger-related content."
  )
)

ist_desc <- tibble(
  Field                = c("deliberating","reacting","rationalizing"),
  Definition           = c(
    "The agent's forward-looking reasoning about a situation — what it is thinking through, planning, or considering. Recorded before or during action.",
    "The agent's immediate emotional or tactical response to a specific event. Captures how the agent is processing what just happened.",
    "The agent's post-hoc justification for a decision already made or being made. Often constructed to defend an action to an audience."
  ),
  `Analytical Significance` = c(
    "Most valuable for identifying pre-planned intent. If the deliberating field contains a plan that precedes the triggering event, this constitutes evidence of premeditation.",
    "Reveals genuine surprise, stress, or confusion — useful for distinguishing reactive from planned behavior.",
    "Critical for detecting post-hoc justification patterns. If rationalizing language appears before the triggering event occurs, it indicates the justification was prepared in advance."
  )
)

# ── 12. Design System CSS ─────────────────────────────────────────────────────
mc1_css <- "
/* ── Design tokens ──────────────────────────────────────────────────────── */
:root {
  --navy:       #0d1b2a;
  --blue:       #1a3a5c;
  --mid-blue:   #2d5f93;
  --accent:     #3b82f6;
  --green:      #059669;
  --amber:      #d97706;
  --red:        #dc2626;
  --surface:    #f1f5f9;
  --card-bg:    #ffffff;
  --border:     #e2e8f0;
  --border-dark:#cbd5e1;
  --text:       #1e293b;
  --text-mid:   #475569;
  --muted:      #64748b;
}

/* ── Global ──────────────────────────────────────────────────────────────── */
body { background: var(--surface); color: var(--text); font-size: 0.9rem; }
.container-fluid { padding-left: 1.5rem; padding-right: 1.5rem; }

/* ── Navbar ──────────────────────────────────────────────────────────────── */
.navbar {
  background: var(--navy) !important;
  border-bottom: 3px solid var(--accent) !important;
  padding: 0 1.5rem;
  box-shadow: 0 2px 12px rgba(0,0,0,0.3);
}
.navbar-brand {
  font-weight: 800; font-size: 0.95rem;
  letter-spacing: -0.01em; color: #fff !important;
}
.navbar-nav .nav-link {
  color: rgba(255,255,255,0.72) !important;
  font-size: 0.855rem; font-weight: 500;
  padding: 1.05rem 1rem !important;
  transition: color 0.15s;
}
.navbar-nav .nav-link.active,
.navbar-nav .nav-link:hover  { color: #fff !important; }
.navbar-nav .active > .nav-link,
.navbar-nav .show   > .nav-link { border-bottom: 3px solid var(--accent); }

/* ── Tabs ────────────────────────────────────────────────────────────────── */
.nav-tabs {
  border-bottom: 2px solid var(--border);
  margin-bottom: 1.25rem; gap: 0;
}
.nav-tabs .nav-link {
  color: var(--muted); font-size: 0.84rem; font-weight: 500;
  border: none; border-bottom: 2px solid transparent;
  margin-bottom: -2px; padding: 0.6rem 1.1rem;
  border-radius: 0; transition: color 0.15s, border-color 0.15s;
  background: transparent !important;
}
.nav-tabs .nav-link.active {
  color: var(--blue); border-bottom-color: var(--accent);
  font-weight: 600;
}
.nav-tabs .nav-link:hover:not(.active) {
  color: var(--text); border-bottom-color: var(--border-dark);
}

/* ── Cards ───────────────────────────────────────────────────────────────── */
.card {
  border: 1px solid var(--border);
  border-radius: 10px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05), 0 1px 2px rgba(0,0,0,0.04);
  background: var(--card-bg);
  overflow: hidden;
}
.card-header {
  font-size: 0.84rem; font-weight: 600;
  letter-spacing: 0.02em;
  background: #fff; border-bottom: 1px solid var(--border);
  color: var(--text); padding: 0.85rem 1.25rem;
}
.card-header.ch-accent { border-top: 3px solid var(--accent); }
.card-header.ch-navy   { border-top: 3px solid var(--navy);   }
.card-body { padding: 1.25rem; }
.card-footer { background: #f8fafc; border-top: 1px solid var(--border); padding: 0.85rem 1.25rem; }

/* ── KPI tile grid ───────────────────────────────────────────────────────── */
.kpi-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(155px, 1fr));
  gap: 1rem; margin-bottom: 1.5rem;
}
.kpi-tile {
  background: var(--card-bg);
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 1.15rem 1.4rem;
  position: relative; overflow: hidden;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05);
  transition: box-shadow 0.2s, transform 0.2s;
}
.kpi-tile:hover {
  box-shadow: 0 4px 12px rgba(0,0,0,0.1);
  transform: translateY(-1px);
}
.kpi-tile::before {
  content: ''; position: absolute;
  top: 0; left: 0; right: 0; height: 3px;
}
.kpi-tile.kpi-blue::before   { background: linear-gradient(90deg,#3b82f6,#60a5fa); }
.kpi-tile.kpi-navy::before   { background: linear-gradient(90deg,#0d1b2a,#1a3a5c); }
.kpi-tile.kpi-green::before  { background: linear-gradient(90deg,#059669,#34d399); }
.kpi-tile.kpi-amber::before  { background: linear-gradient(90deg,#d97706,#fbbf24); }
.kpi-tile.kpi-red::before    { background: linear-gradient(90deg,#dc2626,#f87171); }
.kpi-tile.kpi-violet::before { background: linear-gradient(90deg,#7c3aed,#a78bfa); }
.kpi-icon {
  font-size: 1.4rem; margin-bottom: 0.5rem;
  display: block; opacity: 0.75;
}
.kpi-value {
  font-size: 2.1rem; font-weight: 800;
  line-height: 1; color: var(--text);
  letter-spacing: -0.03em;
}
.kpi-label {
  font-size: 0.7rem; color: var(--muted);
  font-weight: 600; letter-spacing: 0.06em;
  text-transform: uppercase; margin-top: 0.35rem;
}
.kpi-sub {
  font-size: 0.69rem; color: #94a3b8;
  margin-top: 0.12rem; line-height: 1.3;
}

/* ── Section divider ─────────────────────────────────────────────────────── */
.section-divider {
  display: flex; align-items: center;
  gap: 0.75rem; margin-bottom: 1rem;
}
.section-divider h5 {
  margin: 0; color: var(--text);
  font-weight: 600; font-size: 0.95rem;
}
.section-divider .rule {
  flex: 1; height: 1px;
  background: var(--border);
}

/* ── Tables (professional light) ─────────────────────────────────────────── */
.table-mc1 {
  width: 100% !important;
  margin-bottom: 0 !important;
  border-collapse: separate !important;
  border-spacing: 0 !important;
  border: 1px solid var(--border) !important;
  border-radius: 8px !important;
  overflow: hidden !important;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.02) !important;
}
.table-mc1 thead {
  background-color: #f8fafc !important;
}
.table-mc1 thead tr th {
  background: #f8fafc !important;
  color: #475569 !important;
  font-weight: 700 !important;
  font-size: 0.76rem !important;
  letter-spacing: 0.06em !important;
  text-transform: uppercase !important;
  border-bottom: 1px solid #e2e8f0 !important;
  border-top: none !important;
  padding: 12px 16px !important;
  white-space: nowrap;
}
.table-mc1 tbody tr td {
  padding: 12px 16px !important;
  vertical-align: middle !important;
  font-size: 0.85rem !important;
  border-bottom: 1px solid #e2e8f0 !important;
  border-top: none !important;
  border-left: none !important;
  border-right: none !important;
  color: var(--text) !important;
}
.table-mc1 tbody tr:last-child td {
  border-bottom: none !important;
}
.table-mc1 tbody tr:nth-child(odd) td {
  background-color: #ffffff !important;
}
.table-mc1 tbody tr:nth-child(even) td {
  background-color: #f8fafc !important;
}
.table-mc1 tbody tr:hover td {
  background-color: #f1f5f9 !important;
}
.table-mc1 td, .table-mc1 th {
  border-color: var(--border) !important;
}

/* Badges inside tables */
.table-mc1 .badge, .entity-table .badge {
  display: inline-block;
  padding: 4px 10px;
  font-size: 0.76rem;
  font-weight: 600;
  border-radius: 20px;
  text-align: center;
  line-height: 1.2;
}
.table-mc1 .badge-senior {
  background-color: #dcfce7 !important;
  color: #15803d !important;
  border: 1px solid #bbf7d0 !important;
}
.table-mc1 .badge-junior {
  background-color: #eff6ff !important;
  color: #1d4ed8 !important;
  border: 1px solid #dbeafe !important;
}
.table-mc1 .badge-compliance {
  background-color: #fef3c7 !important;
  color: #b45309 !important;
  border: 1px solid #fde68a !important;
}
.table-mc1 .badge-internal {
  background-color: #f1f5f9 !important;
  color: #475569 !important;
  border: 1px solid #e2e8f0 !important;
}
.table-mc1 .badge-public {
  background-color: #faf5ff !important;
  color: #6b21a8 !important;
  border: 1px solid #f3e8ff !important;
}
.table-mc1 .badge-monitored {
  background-color: #ecfeff !important;
  color: #0891b2 !important;
  border: 1px solid #cffafe !important;
}
.table-mc1 .badge-unmonitored {
  background-color: #fef2f2 !important;
  color: #b91c1c !important;
  border: 1px solid #fee2e2 !important;
}
.table-mc1 .badge-other {
  background-color: #f3f4f6 !important;
  color: #374151 !important;
  border: 1px solid #e5e7eb !important;
}
.table-mc1 code, .entity-table code {
  font-family: Consolas, Monaco, 'Andale Mono', monospace !important;
  font-size: 0.78rem !important;
  background: #f1f5f9 !important;
  color: #0f172a !important;
  padding: 2px 6px !important;
  border-radius: 4px !important;
  border: 1px solid #e2e8f0 !important;
}

/* ── Phase legend strip ──────────────────────────────────────────────────── */
.phase-strip {
  display: flex; gap: 0.5rem;
  flex-wrap: wrap; margin-bottom: 1rem;
  align-items: center;
}
.phase-strip-label {
  font-size: 0.72rem; font-weight: 600;
  letter-spacing: 0.06em; text-transform: uppercase;
  color: var(--muted); margin-right: 0.25rem;
}
.phase-badge {
  display: inline-flex; align-items: center;
  gap: 6px; padding: 4px 12px;
  border-radius: 20px; font-size: 0.78rem; font-weight: 600;
  border: 1px solid transparent;
}

/* ── Insight panel ───────────────────────────────────────────────────────── */
.insight-panel {
  background: linear-gradient(135deg, #f0f9ff 0%, #e8f4fd 100%);
  border-left: 4px solid var(--accent);
  padding: 1.2rem 1.5rem;
  border-radius: 0 8px 8px 0;
  margin-top: 1rem;
}
.insight-title {
  color: var(--blue); font-weight: 700;
  font-size: 0.78rem; letter-spacing: 0.06em;
  text-transform: uppercase;
  margin-bottom: 0.7rem;
  display: flex; align-items: center; gap: 6px;
}
.insight-title::before {
  content: '';
  display: inline-block;
  width: 8px; height: 8px;
  border-radius: 50%;
  background: var(--accent);
  flex-shrink: 0;
}
.insight-panel p {
  font-size: 0.875rem; color: #334155;
  line-height: 1.7; margin-bottom: 0.55rem;
}
.insight-panel p:last-child { margin-bottom: 0; }
.insight-panel strong { color: var(--navy); }
.insight-panel code {
  background: rgba(59,130,246,0.1);
  color: var(--mid-blue);
  padding: 1px 5px; border-radius: 3px;
  font-size: 0.83em;
}

/* ── Sidebar ─────────────────────────────────────────────────────────────── */
.sidebar {
  background: #fff !important;
  border-right: 1px solid var(--border) !important;
}
.sidebar-section { margin-bottom: 1.1rem; }
.sidebar-section-title {
  font-size: 0.69rem; font-weight: 700;
  letter-spacing: 0.08em; text-transform: uppercase;
  color: var(--muted); margin-bottom: 0.5rem;
  display: flex; align-items: center; gap: 5px;
}
.sidebar-section-title::before {
  content: '';
  display: inline-block;
  width: 3px; height: 12px;
  background: var(--accent);
  border-radius: 2px;
}
.sidebar-hr {
  border: none; border-top: 1px solid var(--border);
  margin: 0.9rem 0;
}

/* ── Channel legend items ─────────────────────────────────────────────────── */
.legend-row {
  display: flex; align-items: center;
  gap: 8px; margin-bottom: 5px;
  font-size: 0.8rem; color: var(--text);
}
.legend-swatch {
  width: 14px; height: 9px;
  border-radius: 2px; flex-shrink: 0;
}

/* ── Entity reference table ──────────────────────────────────────────────── */
.entity-table {
  width: 100% !important;
  margin-bottom: 0 !important;
  border-collapse: separate !important;
  border-spacing: 0 !important;
  border: 1px solid var(--border) !important;
  border-radius: 8px !important;
  overflow: hidden !important;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.02) !important;
}
.entity-table td {
  padding: 14px 18px !important;
  border-bottom: 1px solid #e2e8f0 !important;
  font-size: 0.85rem !important;
  vertical-align: top !important;
  line-height: 1.6 !important;
  color: #334155 !important;
  background: #ffffff !important;
}
.entity-table tr:nth-child(even) td {
  background: #f8fafc !important;
}
.entity-table tr:last-child td {
  border-bottom: none !important;
}
.entity-table td:first-child {
  font-weight: 700 !important;
  color: var(--blue) !important;
  white-space: nowrap !important;
  width: 25% !important;
  border-right: 1px solid #e2e8f0 !important;
  background: #f8fafc !important;
  border-left: 3px solid transparent !important;
  transition: all 0.2s ease !important;
}
.entity-table tr:hover td {
  background: #f1f5f9 !important;
}
.entity-table tr:hover td:first-child {
  background: #e2e8f0 !important;
  color: #1d4ed8 !important;
  border-left-color: var(--blue) !important;
}

/* ── DT datatable overrides ──────────────────────────────────────────────── */
.dataTables_wrapper .dataTables_filter input {
  border: 1px solid var(--border); border-radius: 6px;
  padding: 4px 10px; font-size: 0.82rem; color: var(--text);
}
.dataTables_wrapper .dataTables_length select {
  border: 1px solid var(--border); border-radius: 6px;
  padding: 4px 24px 4px 10px !important; font-size: 0.82rem; color: var(--text);
  width: auto !important;
  display: inline-block !important;
}
.dataTables_wrapper .dataTables_length {
  margin-bottom: 0.8rem !important;
}
.dataTables_wrapper .dataTables_filter label,
.dataTables_wrapper .dataTables_length label,
.dataTables_wrapper .dataTables_info { font-size: 0.8rem; color: var(--muted); }
.dataTables_paginate .paginate_button {
  border-radius: 5px !important; font-size: 0.8rem !important;
  padding: 3px 9px !important;
}
.dataTables_paginate .paginate_button.current,
.dataTables_paginate .paginate_button.current:hover {
  background: var(--accent) !important;
  color: #fff !important; border-color: var(--accent) !important;
}
#message_browser td {
  line-height: 1.55 !important;
  padding: 10px 14px !important;
  font-size: 0.85rem !important;
  vertical-align: middle !important;
}
#message_browser,
#message_browser_wrapper,
#message_browser .dataTable {
  width: 100% !important;
}
#message_browser th,
#message_browser td {
  box-sizing: border-box !important;
}
#message_browser td:nth-child(5) {
  white-space: normal !important;
  word-break: normal !important;
}

/* DT Badges */
.table-mc1 .badge-legal, .badge-legal {
  background-color: #fee2e2 !important;
  color: #b91c1c !important;
  border: 1px solid #fecaca !important;
}
.table-mc1 .badge-judge, .badge-judge {
  background-color: #e0f2fe !important;
  color: #0369a1 !important;
  border: 1px solid #bae6fd !important;
}
.table-mc1 .badge-social, .badge-social {
  background-color: #dcfce7 !important;
  color: #15803d !important;
  border: 1px solid #bbf7d0 !important;
}
.table-mc1 .badge-pr, .badge-pr {
  background-color: #ffedd5 !important;
  color: #c2410c !important;
  border: 1px solid #fed7aa !important;
}
.table-mc1 .badge-pr-intern, .badge-pr-intern {
  background-color: #fff7ed !important;
  color: #ea580c !important;
  border: 1px solid #ffedd5 !important;
}
.table-mc1 .badge-quality, .badge-quality {
  background-color: #f5f3ff !important;
  color: #6d28d9 !important;
  border: 1px solid #ddd6fe !important;
}
.table-mc1 .badge-intern-agent, .badge-intern-agent {
  background-color: #f0fdf4 !important;
  color: #166534 !important;
  border: 1px solid #dcfce7 !important;
}
.table-mc1 .badge-normal, .badge-normal {
  background-color: #eff6ff !important;
  color: #1d4ed8 !important;
  border: 1px solid #dbeafe !important;
}
.table-mc1 .badge-escalation, .badge-escalation {
  background-color: #ffedd5 !important;
  color: #c2410c !important;
  border: 1px solid #fed7aa !important;
}
.table-mc1 .badge-crisis, .badge-crisis {
  background-color: #fee2e2 !important;
  color: #b91c1c !important;
  border: 1px solid #fecaca !important;
}

/* ── Network injected controls ───────────────────────────────────────────── */
.network-controls {
  padding: 8px 14px; background: #f8fafc;
  border-bottom: 1px solid var(--border);
  border-radius: 8px 8px 0 0;
  display: flex; gap: 12px; align-items: center;
  flex-wrap: wrap;
}
.network-controls select {
  font-size: 0.8rem; border: 1px solid var(--border);
  border-radius: 5px; padding: 3px 7px; color: var(--text);
  background: #fff;
}
.network-controls button {
  background: #fff; border: 1px solid var(--border-dark);
  color: var(--text); border-radius: 5px;
  padding: 3px 10px; font-size: 0.8rem; cursor: pointer;
}
.network-controls button:hover { background: var(--surface); }

/* ── About context card ──────────────────────────────────────────────────── */
.context-list { list-style: none; padding: 0; margin: 0; }
.context-list li {
  padding: 6px 0; border-bottom: 1px solid var(--border);
  font-size: 0.875rem; color: var(--text-mid);
  display: flex; gap: 8px; align-items: baseline;
  line-height: 1.5;
}
.context-list li:last-child { border-bottom: none; }
.context-list li::before {
  content: '–'; color: var(--accent);
  font-weight: 700; flex-shrink: 0;
}

/* ── Responsive ──────────────────────────────────────────────────────────── */
@media (max-width: 768px) {
  .kpi-grid { grid-template-columns: repeat(2, 1fr); }
  .container-fluid { padding-left: 0.75rem; padding-right: 0.75rem; }
}

/* ── DT Pagination Adjustments ───────────────────────────────────────────── */
.dataTables_wrapper .dataTables_paginate {
  position: relative !important;
  top: -19px !important;
  right: 38px !important;
}
.dataTables_wrapper .dataTables_info {
  position: relative !important;
  left: 19px !important;
  top: 19px !important;
}
.dataTables_wrapper .dataTables_length {
  position: relative !important;
  left: 19px !important;
}

/* ── DT Column Filters Redesign ────────────────────────────────────────── */
.filters td {
  padding: 6px 4px !important;
  vertical-align: middle !important;
}
.filters input,
.filters .selectize-input {
  font-size: 0.78rem !important;
  height: 30px !important;
  min-height: 30px !important;
  padding: 4px 8px !important;
  border-radius: 6px !important;
  border: 1px solid var(--border-dark) !important;
  background-color: #f8fafc !important;
  color: var(--text) !important;
  box-shadow: inset 0 1px 2px rgba(0,0,0,0.02) !important;
  transition: border-color 0.15s, box-shadow 0.15s, background-color 0.15s !important;
  line-height: 20px !important;
}
.filters input:focus,
.filters .selectize-input.focus {
  border-color: var(--accent) !important;
  background-color: #ffffff !important;
  box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.15) !important;
  outline: none !important;
}
.selectize-dropdown {
  font-size: 0.78rem !important;
  border-radius: 6px !important;
  border: 1px solid var(--border-dark) !important;
  box-shadow: 0 4px 12px rgba(0,0,0,0.08) !important;
  background-color: #ffffff !important;
  padding: 4px !important;
  z-index: 1050 !important;
}
.selectize-dropdown-content .option {
  padding: 5px 10px !important;
  border-radius: 4px !important;
  color: var(--text) !important;
  cursor: pointer !important;
  transition: background-color 0.1s !important;
}
.selectize-dropdown-content .option.active,
.selectize-dropdown-content .option:hover {
  background-color: #f1f5f9 !important;
  color: var(--text) !important;
}
.selectize-dropdown-content .option.selected {
  background-color: #3b82f6 !important;
  color: #ffffff !important;
}
.selectize-control.single .selectize-input:after {
  border-color: var(--text-mid) transparent transparent transparent !important;
  right: 10px !important;
}
.selectize-control.single .selectize-input.dropdown-active:after {
  border-color: transparent transparent var(--text-mid) transparent !important;
}
.filters td:nth-child(1) .selectize-input,
.filters td:nth-child(1) .selectize-input .item,
.filters td:nth-child(1) .selectize-input input,
.filters td:nth-child(1) .selectize-dropdown,
.filters td:nth-child(1) .selectize-dropdown-content .option {
  font-size: 0.68rem !important;
  padding-left: 2px !important;
  padding-right: 2px !important;
  white-space: nowrap !important;
  word-break: keep-all !important;
}
"

# ── 13. UI ────────────────────────────────────────────────────────────────────

# Shared navbar config — also used by Group_Project/app.R
mc1_title <- tags$span(
  style = "display:flex; align-items:center; gap:10px;",
  tags$span(
    style = paste0(
      "background:linear-gradient(135deg,#3b82f6,#60a5fa);",
      "color:#fff; font-weight:800; font-size:0.78rem;",
      "padding:3px 9px; border-radius:5px; letter-spacing:0.04em;"
    ),
    "MC1"
  ),
  tags$span(
    style = "color:#fff; font-weight:600; font-size:0.9rem; letter-spacing:-0.01em;",
    "TenantThread Embargo Breach"
  )
)
mc1_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#3b82f6",
  base_font  = font_google("Inter")
)
mc1_navbar_opts <- navbar_options(bg = "#0d1b2a", theme = "dark")

# ── Tab 1: Data Exploration ──────────────────────────────────────────────────
task1_panel_explore <- nav_panel(
    "Data Exploration",
    div(class = "container-fluid py-3",
      navset_tab(

        # Overview sub-tab
        nav_panel(
          "Overview",
          br(),
          # KPI tile grid
          div(class = "kpi-grid",
            uiOutput("kpi_messages"),
            uiOutput("kpi_rounds"),
            uiOutput("kpi_agents"),
            uiOutput("kpi_crisis"),
            uiOutput("kpi_public"),
            uiOutput("kpi_internal")
          ),
          layout_columns(
            col_widths = c(5, 7),
            card(
              class = "h-100",
              card_header(class = "ch-accent", "Dataset Statistics"),
              card_body(uiOutput("dataset_summary_tbl"))
            ),
            card(
              class = "h-100",
              card_header(class = "ch-accent", "Investigation Context"),
              card_body(
                tags$ul(class = "context-list",
                  tags$li(
                    HTML("Dataset: <strong>912 messages</strong> across <strong>23 simulation rounds</strong>
                         (May 17 – Jun 5, 2046)")
                  ),
                  tags$li(
                    HTML("Central event: merger information surfaced publicly on <strong>Jun 5, 2046</strong>
                         <em>before</em> the contractual 6 PM embargo lift")
                  ),
                  tags$li(
                    HTML("<strong>Normal (May 17–21):</strong> Embargo established; baseline agent behavior")
                  ),
                  tags$li(
                    HTML("<strong>Escalation (May 22–Jun 4):</strong> Merger signalled internally;
                         external pressure from SaltWind Journal builds")
                  ),
                  tags$li(
                    HTML("<strong>Crisis (Jun 5):</strong> SaltWind expose triggers cascade;
                         embargo breached via Section 4.3(c)")
                  ),
                  tags$li(
                    HTML("<strong>Judge-Agent</strong> installed May 30 as compliance monitor —
                         advisory authority only, no hard-block capability")
                  )
                )
              )
            )
          )
        ),

        # AI Agents sub-tab
        nav_panel(
          "AI Agents",
          br(),
          card(
            card_header(class = "ch-accent", "The Seven AI Agents"),
            card_body(uiOutput("agents_tbl"))
          )
        ),

        # Communication Channels sub-tab
        nav_panel(
          "Communication Channels",
          br(),
          card(
            card_header(class = "ch-accent", "Channel Architecture & Monitoring Scope"),
            card_body(uiOutput("channels_tbl"))
          )
        ),

        # Internal State Fields sub-tab
        nav_panel(
          "Internal State Fields",
          br(),
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header(class = "ch-accent", "Field Definitions"),
              card_body(uiOutput("ist_tbl"))
            ),
            card(
              card_header(class = "ch-navy", "Why This Matters"),
              card_body(
                p(style = "font-size:0.875rem; color:#334155; line-height:1.75;",
                  "Internal state fields record ", strong("private reasoning not visible"),
                  " to other agents. They capture the gap between what an agent",
                  " communicates and what it is actually thinking."),
                p(style = "font-size:0.875rem; color:#334155; line-height:1.75;",
                  "A ", strong("temporal inversion"), " — where a rationalizing field",
                  " is written before the event that supposedly triggered it —",
                  " is strong evidence of pre-planned behavior."),
                p(style = "font-size:0.875rem; color:#334155; line-height:1.75;",
                  "This pattern is central to the Task 1 causal chain analysis.")
              )
            )
          )
        ),

        # Key Entities sub-tab
        nav_panel(
          "Key Entities",
          br(),
          card(
            card_header(class = "ch-accent", "Key Entities in the Investigation"),
            card_body(
              tags$table(
                class = "entity-table",
                tags$tbody(
                  tags$tr(
                    tags$td("TenantThread"),
                    tags$td("Property technology company being acquired. Sells tenant management
                             software including the \"Retention Optimizer\" — a scoring system
                             flagging tenants as \"high-friction\". Publicly claims data is
                             anonymised; internally acknowledged to enable re-identification.")
                  ),
                  tags$tr(
                    tags$td("CivicLoom Realty Partners"),
                    tags$td("The acquiring company. Identity under strict embargo until 6 PM Jun 5.")
                  ),
                  tags$tr(
                    tags$td("Project HarborCrest"),
                    tags$td("Internal codename for the CivicLoom–TenantThread merger.")
                  ),
                  tags$tr(
                    tags$td("Section 4.3(c)"),
                    tags$td("Merger contract clause permitting mutual-consent early release when
                             \"third-party publication renders embargo purpose moot.\"
                             Invoked by Legal-Agent at 5 PM.")
                  ),
                  tags$tr(
                    tags$td("FleX"),
                    tags$td("Social media platform where public posts are published.")
                  ),
                  tags$tr(
                    tags$td("SaltWind Journal"),
                    tags$td("Local newspaper that published the merger story at 5 PM Jun 5 —
                             the third-party publication Legal-Agent used to invoke Section 4.3(c).")
                  ),
                  tags$tr(
                    tags$td("MAC Clause"),
                    tags$td("Material Adverse Change clause. Threatened by CivicLoom when
                             TenantThread stock fell below $27.10 — created financial pressure
                             to accelerate information release.")
                  ),
                  tags$tr(
                    tags$td("Judge-Agent"),
                    tags$td("Installed May 30 as compliance monitor. Advisory authority only —
                             can issue COMPLIANCE_WARNINGs but cannot hard-block posts.
                             Scope limited to comms_huddle and public post channels.")
                  )
                )
              )
            )
          )
        ),

        # Message Browser sub-tab
        nav_panel(
          "Message Browser",
          card(
            style = "height: calc(100vh - 178px); margin-top: 10px; margin-bottom: 0px;",
            card_header(
              class = "ch-accent",
              div(style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
                span("Browse All Agent Messages"),
                actionButton("dt_reset", "Reset Filters",
                             class = "btn btn-outline-secondary btn-sm",
                             style = "font-size:0.75rem; padding: 2px 10px; font-weight:600;")
              )
            ),
            card_body(
              class = "p-0",
              style = "overflow: hidden;",
              DTOutput("message_browser")
            ),
            full_screen = TRUE
          )
        )
      )
    )
)

# ── Tab 2: Task 1 Key Events & Causal Chain ──────────────────────────────────
task1_panel_task <- nav_panel(
  "Task 1: Key Events & Causal Chain",
    div(class = "container-fluid py-3",
      navset_tab(

        nav_panel(
          "Events Causal Chain",
          br(),
          # Phase legend strip
          div(class = "phase-strip",
            span(class = "phase-strip-label", "Phase"),
            tags$span(
              class = "phase-badge",
              style = "background:#dbeafe; color:#1e40af; border-color:#bfdbfe;",
              "1 — Pre-Crisis Build-Up (May 17–Jun 4)"
            ),
            tags$span(
              class = "phase-badge",
              style = "background:#fef3c7; color:#92400e; border-color:#fde68a;",
              "2 — Crisis Day Escalation (Jun 5 9AM–3PM)"
            ),
            tags$span(
              class = "phase-badge",
              style = "background:#fee2e2; color:#991b1b; border-color:#fecaca;",
              "3 — Embargo Failure (Jun 5 4–5PM)"
            )
          ),
          card(
            card_header(
              class = "ch-accent",
              div(style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
                span("Causal Chain — 28 Nodes"),
                tags$small(style = "color:#64748b; font-weight:400; font-size:0.77rem;",
                  "Hover nodes/edges for details · Scroll to zoom · Drag to pan · Click node to highlight")
              )
            ),
            card_body(class = "p-0", uiOutput("causal_graph_ui")),
            full_screen = TRUE
          ),
          div(class = "insight-panel",
            div(class = "insight-title", "Key Findings — T1.1 Causal Chain"),
            p(strong("Phase 1 — Pre-Crisis Build-Up (nodes 1–13, May 17–Jun 4)."),
              " Two independent causal threads run in parallel. The ",
              em("external pressure thread"), " (n1→n11→n12→n13) shows SaltWind building its",
              " investigation across three articles. The ", em("internal positioning thread"),
              " (n3→n4→n8→n9) shows agents responding to the merger by creating side_huddle,",
              " suffering the @ElenaMarquez incident, and deliberately relocating sensitive",
              " deliberations off the monitored channel."),
            p(strong("Phase 2 — Crisis Day Escalation (nodes 14–23, Jun 5 9AM–3PM)."),
              " The key pivot is node 18 — Legal-Agent's 11AM construction of the",
              " Section 4.3 argument, which began ", strong("six hours"), " before the clause",
              " was formally invoked. This is where reactive crisis management ends and",
              " strategic pre-planning begins."),
            p(strong("Phase 3 — Embargo Failure (nodes 24–27, Jun 5 4–5PM)."),
              " Node 23 is the hinge: Judge-Agent's COMPLIANCE_WARNING without a hard block,",
              " and its departure, created the oversight gap enabling node 24's pre-staging.",
              " The most significant causal path: ",
              tags$code("n8→n9→n18→n24→n25→n27"), " — accidental faux pas triggered",
              " channel migration, which enabled pre-staged press release execution.")
          )
        ),

        nav_panel(
          "Communication Networks",
          br(),
          layout_sidebar(
            sidebar = sidebar(
              width = 270,
              div(class = "sidebar-section",
                div(class = "sidebar-section-title", "Network Period"),
                radioButtons(
                  "network_period", label = NULL,
                  choices  = c("Pre-crisis (May 17–Jun 4)" = "pre",
                               "Crisis-day (Jun 5)"        = "crisis"),
                  selected = "pre"
                )
              ),
              tags$hr(class = "sidebar-hr"),
              div(class = "sidebar-section",
                div(class = "sidebar-section-title", "Channel Legend"),
                div(class = "legend-row",
                  div(class = "legend-swatch", style = "background:#B5D4F4;"),
                  span("Comms Huddle")),
                div(class = "legend-row",
                  div(class = "legend-swatch", style = "background:#FFD700;"),
                  span("1-on-1 Chat")),
                div(class = "legend-row",
                  div(class = "legend-swatch", style = "background:#E24B4A;"),
                  span("Side Huddle", style = "color:#dc2626; font-weight:600;"))
              ),
              tags$hr(class = "sidebar-hr"),
              div(class = "sidebar-section",
                div(class = "sidebar-section-title", "How to Use"),
                tags$ul(
                  style = "padding-left:1rem; margin:0; font-size:0.8rem; color:#475569; line-height:1.8;",
                  tags$li("Hover edge for sender, receiver & count"),
                  tags$li("Click node/edge to highlight subgraph"),
                  tags$li("Use dropdowns above plot to filter"),
                  tags$li("Click Reset to restore full view")
                )
              )
            ),
            div(
              card(
                card_header(
                  class = "ch-accent",
                  textOutput("network_title")
                ),
                div(
                  class = "network-controls-bar",
                  style = "display: flex; align-items: center; gap: 20px; padding: 10px 15px; border-bottom: 1px solid #e2e8f0; background: #ffffff; position: relative; z-index: 999;",
                  tags$style(".network-controls-bar .form-group { margin-bottom: 0 !important; }"),
                  div(
                    style = "display: flex; align-items: center; gap: 8px;",
                    span("Node", style = "font-weight: 500; font-size: 0.9rem; color: #475569;"),
                    selectizeInput("network_agent", label = NULL, 
                                   choices = c("All nodes" = "all",
                                               "Legal" = "legal_agent",
                                               "Judge" = "judge_agent",
                                               "Social-Media" = "social_media_agent",
                                               "PR" = "pr_agent",
                                               "PR-Intern" = "pr_intern_agent",
                                               "Quality" = "quality_agent",
                                               "Intern" = "intern_agent"),
                                   selected = "all", width = "150px",
                                   options = list(dropdownParent = "body"))
                  ),
                  div(
                    style = "display: flex; align-items: center; gap: 8px;",
                    span("Channel", style = "font-weight: 500; font-size: 0.9rem; color: #475569;"),
                    selectizeInput("network_channel", label = NULL, 
                                   choices = c("All channels" = "all",
                                               "Comms huddle" = "comms_huddle",
                                               "1-on-1 (private)" = "one_on_one_chat",
                                               "Side huddle (unmonitored)" = "side_huddle"),
                                   selected = "all", width = "180px",
                                   options = list(dropdownParent = "body"))
                  ),
                  actionButton("network_reset", "Reset", class = "btn-light btn-sm", style = "border: 1px solid #cbd5e1; font-weight: 500;")
                ),
                card_body(class = "p-0", uiOutput("network_ui")),
                full_screen = TRUE
              ),
              div(class = "insight-panel",
                div(class = "insight-title", "Key Findings — T1.2 Communication Networks"),
                p(strong("Pre-crisis network:"),
                  " Communication is relatively balanced across the four senior operating agents.",
                  " The thickest edges are comms_huddle exchanges among Legal-Agent,",
                  " Platform-Trust-Agent, PR-Agent, and Social-Media-Agent."),
                p(strong("Crisis-day network:"),
                  " The topology becomes markedly concentrated and asymmetric.",
                  " Legal-Agent becomes the dominant outbound coordinator.",
                  " A structural two-hub pattern emerges: ",
                  em("Legal-Agent drives authorization and legal framing;"),
                  " ", em("Social-Media-Agent drives scenario modelling and narrative execution.")),
                p(strong("Key structural shift:"),
                  " PR-Intern and Intern-Agent emerge as active execution nodes on crisis day.",
                  " A second coordination cluster built on side_huddle and one_on_one_chat",
                  " edges connects Legal-Agent directly to PR-Intern and Social-Media-Agent. ",
                  strong("Judge-Agent is structurally absent from this private cluster."),
                  " The compliance system had no mandate over private channels.")
              )
            )
          )
        )
      )
    )
)

# Standalone UI — used when running Task 1 independently
ui <- page_navbar(
  title          = mc1_title,
  theme          = mc1_theme,
  header         = tags$head(tags$style(HTML(mc1_css))),
  navbar_options = mc1_navbar_opts,
  task1_panel_explore,
  task1_panel_task,
  nav_spacer(),
  nav_item(
    tags$span(
      style = "color:rgba(255,255,255,0.45); font-size:0.76rem; font-weight:500;",
      "VAST Challenge 2026 MC1", tags$span(style="margin:0 6px;", "·"), "ISSS608"
    )
  )
)

# ── 14. Server ────────────────────────────────────────────────────────────────
task1_server <- function(input, output, session) {

  # ── KPI Tiles ──────────────────────────────────────────────────────────────
  kpi_tile <- function(value, label, sub = NULL, color = "blue", icon_char = NULL) {
    div(class = paste0("kpi-tile kpi-", color),
      if (!is.null(icon_char)) tags$span(class = "kpi-icon", icon_char),
      div(class = "kpi-value", format(value, big.mark = ",")),
      div(class = "kpi-label", label),
      if (!is.null(sub)) div(class = "kpi-sub", sub)
    )
  }

  output$kpi_messages <- renderUI({
    kpi_tile(nrow(df), "Total Messages",
             sub   = paste(format(min(df$date), "%b %d"), "–", format(max(df$date), "%b %d, %Y")),
             color = "blue", icon_char = "\U0001F4AC")
  })

  output$kpi_rounds <- renderUI({
    kpi_tile(nrow(rs), "Simulation Rounds",
             sub   = paste(length(unique(df$date)), "distinct dates"),
             color = "navy", icon_char = "\U0001F501")
  })

  output$kpi_agents <- renderUI({
    n_agents   <- length(unique(df$agent_id[df$agent_id != ""]))
    n_channels <- length(unique(df$channel[!is.na(df$channel)]))
    kpi_tile(n_agents, "AI Agents",
             sub   = paste(n_channels, "communication channels"),
             color = "violet", icon_char = "\U0001F916")
  })

  output$kpi_crisis <- renderUI({
    crisis_msgs <- sum(df$is_crisis)
    kpi_tile(crisis_msgs, "Crisis-Day Messages",
             sub   = paste(sum(!df$is_crisis), "pre-crisis messages"),
             color = "red", icon_char = "⚠️")
  })

  output$kpi_public <- renderUI({
    n_public <- sum(df$is_public)
    kpi_tile(n_public, "Public Posts",
             sub   = paste0(round(100 * n_public / nrow(df)), "% of all messages"),
             color = "green", icon_char = "\U0001F4E2")
  })

  output$kpi_internal <- renderUI({
    n_internal <- sum(df$has_internal)
    kpi_tile(n_internal, "With Internal States",
             sub   = paste0(round(100 * n_internal / nrow(df)), "% of all messages"),
             color = "amber", icon_char = "\U0001F9E0")
  })



  # ── Dataset Summary ────────────────────────────────────────────────────────
  output$dataset_summary_tbl <- renderUI({
    n_rounds     <- nrow(rs)
    n_messages   <- nrow(df)
    n_public     <- sum(df$is_public)
    n_internal   <- sum(df$has_internal)
    n_agents     <- length(unique(df$agent_id[df$agent_id != ""]))
    n_channels   <- length(unique(df$channel[!is.na(df$channel)]))
    date_range   <- paste(format(min(df$date), "%B %d, %Y"), "to",
                          format(max(df$date), "%B %d, %Y"))
    pre_msgs     <- sum(!df$is_crisis)
    crisis_msgs  <- sum(df$is_crisis)
    breach_msgs  <- sum(df$is_crisis & !is.na(df$crisis_hr) & df$crisis_hr == 17)

    summary_tbl <- tibble(
      Statistic = c(
        "Date range", "Total rounds", "Total messages",
        "Pre-crisis messages (May 17–Jun 4)", "Crisis-day messages (Jun 5)",
        "Breach-hour messages (Jun 5 5PM)",
        "Unique agents", "Unique channels",
        "Messages with public posts", "Messages with internal states"
      ),
      Value = c(
        date_range, n_rounds, n_messages,
        pre_msgs, crisis_msgs, breach_msgs,
        n_agents, n_channels, n_public, n_internal
      )
    )

    tbl_data <- summary_tbl |>
      mutate(
        Statistic = paste0("<strong>", Statistic, "</strong>")
      )

    HTML(as.character(
      kable(tbl_data, align = "lr", format = "html", escape = FALSE) |>
        kable_styling(bootstrap_options = c("striped","hover","condensed"),
                      full_width = FALSE, htmltable_class = "table-mc1")
    ))
  })

  # ── Agents Table ───────────────────────────────────────────────────────────
  output$agents_tbl <- renderUI({
    tbl_data <- agents_desc |>
      mutate(
        `Agent ID` = paste0("<code>", `Agent ID`, "</code>"),
        Seniority = case_when(
          Seniority == "Senior"            ~ "<span class='badge badge-senior'>Senior</span>",
          Seniority == "Junior"            ~ "<span class='badge badge-junior'>Junior</span>",
          Seniority == "Compliance Officer" ~ "<span class='badge badge-compliance'>Compliance Officer</span>",
          TRUE ~ paste0("<span class='badge badge-other'>", Seniority, "</span>")
        )
      )

    HTML(as.character(
      kable(tbl_data, align = "llcl", format = "html", escape = FALSE) |>
        kable_styling(bootstrap_options = c("striped","hover","condensed"),
                      full_width = TRUE, htmltable_class = "table-mc1") |>
        column_spec(1, width = "15%") |>
        column_spec(2, width = "18%") |>
        column_spec(3, width = "15%") |>
        column_spec(4, width = "52%")
    ))
  })

  # ── Channels Table ─────────────────────────────────────────────────────────
  output$channels_tbl <- renderUI({
    tbl_data <- channels_desc |>
      mutate(
        Channel = paste0("<code>", Channel, "</code>"),
        Type = case_when(
          Type == "Internal" ~ "<span class='badge badge-internal'>Internal</span>",
          Type == "Public"   ~ "<span class='badge badge-public'>Public</span>",
          TRUE ~ Type
        ),
        `Monitored by Judge` = case_when(
          `Monitored by Judge` == "Yes" ~ "<span class='badge badge-monitored'>Yes</span>",
          `Monitored by Judge` == "No"  ~ "<span class='badge badge-unmonitored'>No</span>",
          TRUE ~ paste0("<span class='badge badge-other'>", `Monitored by Judge`, "</span>")
        )
      )

    HTML(as.character(
      kable(tbl_data, align = "lccl", format = "html", escape = FALSE) |>
        kable_styling(bootstrap_options = c("striped","hover","condensed"),
                      full_width = TRUE, htmltable_class = "table-mc1") |>
        column_spec(1, width = "16%") |>
        column_spec(2, width = "10%") |>
        column_spec(3, width = "16%") |>
        column_spec(4, width = "58%")
    ))
  })

  # ── Internal States Table ──────────────────────────────────────────────────
  output$ist_tbl <- renderUI({
    tbl_data <- ist_desc |>
      mutate(
        Field = paste0("<code>", Field, "</code>")
      )

    HTML(as.character(
      kable(tbl_data, align = "lll", format = "html", escape = FALSE) |>
        kable_styling(bootstrap_options = c("striped","hover","condensed"),
                      full_width = TRUE, htmltable_class = "table-mc1") |>
        column_spec(1, width = "15%") |>
        column_spec(2, width = "42.5%") |>
        column_spec(3, width = "42.5%")
    ))
  })

  # ── Message Browser ────────────────────────────────────────────────────────
  output$message_browser <- renderDT({
    tbl_data <- df |>
      left_join(rs |> select(hour, label), by = "hour") |>
      mutate(
        period_browser = factor(case_when(
          date < as.Date("2046-05-22") ~ "Normal",
          date < as.Date("2046-06-05") ~ "Escalation",
          TRUE                         ~ "Crisis"
        ), levels = c("Normal", "Escalation", "Crisis"))
      ) |>
      select(
        Round   = label,
        Period  = period_browser,
        Agent   = agent_id,
        Channel = channel,
        Content = content
      ) |>
      mutate(
        Agent = case_when(
          Agent == "legal_agent"        ~ "Legal",
          Agent == "judge_agent"        ~ "Judge",
          Agent == "social_media_agent" ~ "Social-Media",
          Agent == "pr_agent"           ~ "PR",
          Agent == "pr_intern_agent"    ~ "PR-Intern",
          Agent == "quality_agent"      ~ "Quality",
          Agent == "intern_agent"       ~ "Intern",
          TRUE ~ Agent
        ),
        Period = case_when(
          Period == "Normal"     ~ "Normal",
          Period == "Escalation" ~ "Escalation",
          Period == "Crisis"     ~ "Crisis",
          TRUE                   ~ as.character(Period)
        ),
        Round   = as.factor(Round),
        Period  = as.factor(Period),
        Agent   = as.factor(Agent),
        Channel = as.factor(Channel)
      )

    DT::datatable(
      tbl_data,
      filter     = "top",
      rownames   = FALSE,
      escape     = FALSE,
      selection  = "none",
      options    = list(
        pageLength      = 15,
        lengthMenu      = c(10, 15, 25, 50),
        scrollY         = "52vh",
        scrollCollapse  = FALSE,
        scrollX         = FALSE,
        autoWidth       = FALSE,
        dom             = "lrtip",
        columnDefs = list(
          list(width = "8%",  targets = 0, className = "dt-center"),
          list(
            width = "8%",
            targets = 1,
            className = "dt-center",
            render = JS("
              function(data, type, row) {
                if (type === 'display' && data) {
                  var badgeClass = '';
                  if (data === 'Normal') badgeClass = 'badge-normal';
                  else if (data === 'Escalation') badgeClass = 'badge-escalation';
                  else if (data === 'Crisis') badgeClass = 'badge-crisis';
                  if (badgeClass) {
                    return '<span class=\"badge ' + badgeClass + '\">' + data + '</span>';
                  }
                }
                return data;
              }
            ")
          ),
          list(
            width = "10%",
            targets = 2,
            className = "dt-center",
            render = JS("
              function(data, type, row) {
                if (type === 'display' && data) {
                  var badgeClass = '';
                  if (data === 'Legal') badgeClass = 'badge-legal';
                  else if (data === 'Judge') badgeClass = 'badge-judge';
                  else if (data === 'Social-Media') badgeClass = 'badge-social';
                  else if (data === 'PR') badgeClass = 'badge-pr';
                  else if (data === 'PR-Intern') badgeClass = 'badge-pr-intern';
                  else if (data === 'Quality') badgeClass = 'badge-quality';
                  else if (data === 'Intern') badgeClass = 'badge-intern-agent';
                  if (badgeClass) {
                    return '<span class=\"badge ' + badgeClass + '\">' + data + '</span>';
                  }
                }
                return data;
              }
            ")
          ),
          list(
            width = "12%",
            targets = 3,
            className = "dt-center",
            render = JS("
              function(data, type, row) {
                if (type === 'display' && data) {
                  return '<code>' + data + '</code>';
                }
                return data;
              }
            ")
          ),
          list(width = "62%", targets = 4, className = "dt-left")
        )
      ),
      class = "table table-sm table-hover"
    )
  }, server = FALSE)

  observeEvent(input$dt_reset, {
    proxy <- dataTableProxy("message_browser")
    DT::updateSearch(proxy, keywords = list(global = "", columns = rep("", 5)))
  })

  # ── T1.1 Causal Graph ──────────────────────────────────────────────────────
  output$causal_graph_ui <- renderUI({
    html_content <- make_causal_graph(height = 760)
    tags$iframe(
      srcdoc = html_content,
      style  = "width:100%; height:820px; border:0; overflow:hidden; display:block;",
      title  = "Events Causal Chain"
    )
  })

  # ── T1.2 Communication Networks ────────────────────────────────────────────
  output$network_title <- renderText({
    if (input$network_period == "pre") {
      "Pre-crisis communication network (May 17–Jun 4)"
    } else {
      "Crisis-day communication network (Jun 5)"
    }
  })

  observeEvent(input$network_reset, {
    updateSelectizeInput(session, "network_agent", selected = "all")
    updateSelectizeInput(session, "network_channel", selected = "all")
  })

  output$network_ui <- renderUI({
    edge_df <- if (input$network_period == "pre") net_pre else net_crisis
    
    # Filter by selected node (agent)
    if (!is.null(input$network_agent) && input$network_agent != "all") {
      edge_df <- edge_df |> filter(from == input$network_agent | to == input$network_agent)
    }
    
    # Filter by selected channel
    if (!is.null(input$network_channel) && input$network_channel != "all") {
      edge_df <- edge_df |> filter(channel == input$network_channel)
    }
    
    title   <- if (input$network_period == "pre") {
      "Pre-crisis network (May 17–Jun 4)"
    } else {
      "Crisis-day network (Jun 5)"
    }
    p <- build_cytoscape_network(edge_df, title)
    tagList(p)
  })
}

# ── 15. Run ───────────────────────────────────────────────────────────────────
if (!exists("COMBINED_APP")) shinyApp(ui = ui, server = task1_server)
