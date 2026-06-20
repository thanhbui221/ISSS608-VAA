# ─────────────────────────────────────────────────────────────────────────────
# Task 3: Leading Indicators
# Defines: task3_ui, task3_server
# Assumes shared globals from main app.R (or self-builds from JSON):
#   df  (columns: hour, agent_id, channel, content,
#                 deliberating, reacting, rationalizing)
# ─────────────────────────────────────────────────────────────────────────────

library(shiny);   library(bslib)
library(dplyr);   library(tidyr);   library(stringr); library(lubridate)
library(purrr);   library(ggplot2); library(plotly);  library(DT)
library(jsonlite); library(tibble)

# Guard: Task 1 already defines %||%
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

# macOS / RStudio guard — use an off-screen device when converting ggplot -> plotly
to_plotly <- function(g, ...) {
  tf <- NULL
  if (requireNamespace("ragg", quietly = TRUE)) {
    tf <- tempfile(fileext = ".png")
    ragg::agg_png(tf, width = 900, height = 600, res = 96)
  } else {
    grDevices::pdf(NULL, width = 9, height = 6)
  }
  on.exit({ grDevices::dev.off(); if (!is.null(tf) && file.exists(tf)) unlink(tf) }, add = TRUE)
  plotly::ggplotly(g, ...)
}

# ── Data ─────────────────────────────────────────────────────────────────────
# Reuse Task 1's df if already in scope; otherwise build from JSON directly.
if (!exists("df") || !is.data.frame(df)) {
  cand <- c(
    "MC1_final_00.json",
    "../Task 1 Shiny App/VAST_Challenge_2026_MC1/MC1_final_00.json",
    "Task 1 Shiny App/VAST_Challenge_2026_MC1/MC1_final_00.json",
    file.path("VAST_Challenge_2026_MC1", "MC1_final_00.json")
  )
  json_path <- cand[file.exists(cand)][1]
  if (is.na(json_path)) stop(
    "Could not find MC1_final_00.json. Place it next to this app.R, or keep ",
    "the 'Task 1 Shiny App/VAST_Challenge_2026_MC1/' folder alongside this one.")

  raw <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  parse_msgs_t3 <- function(rd) {
    hr <- lubridate::ymd_hms(rd$hour)
    purrr::map_dfr(rd$communications, function(cm) {
      ist <- cm$internal_state %||% list()
      tibble::tibble(
        hour          = hr,
        agent_id      = cm$agent_id %||% NA_character_,
        channel       = cm$channel  %||% NA_character_,
        content       = str_trunc(cm$content       %||% "", 600),
        deliberating  = str_trunc(ist$deliberating  %||% "", 500),
        reacting      = str_trunc(ist$reacting      %||% "", 400),
        rationalizing = str_trunc(ist$rationalizing %||% "", 400)
      )
    })
  }
  df <- purrr::map_dfr(raw$rounds, parse_msgs_t3)
}

# ── Palettes — identical to Task 1's ch_colours / agent_pal ──────────────────
# Use Task 1's globals if available, otherwise define identically here.
T3_CHAN_PAL <- if (exists("ch_colours")) ch_colours else c(
  comms_huddle    = "#B5D4F4",
  one_on_one_chat = "#FFD700",
  side_huddle     = "#E24B4A",
  official_post   = "#378ADD",
  personal_post   = "#1D9E75",
  anonymous_post  = "#C05538"
)

# Agent display-name palette (matches Task 1's agent_pal by display label)
T3_AGENT_PAL <- c(
  Legal            = "#E24B4A",
  "Social-Mgr"     = "#1D9E75",
  "Platform-Trust" = "#7F77DD",
  PR               = "#EF9F27",
  "PR-Intern"      = "#F4A261",
  Intern           = "#5BAD6F",
  Judge            = "#378ADD"
)

# Channel-class palette: huddle (neutral), private (amber warning), public (red danger)
T3_CLASS_PAL <- c(huddle = "#B5D4F4", private = "#EF9F27", public = "#E24B4A")

# ─────────────────────────────────────────────────────────────────────────────
# ggplot base theme — use Task 1's theme_mc1() when available
t3_theme <- function(base = 11) {
  if (exists("theme_mc1", mode = "function")) theme_mc1(base)
  else theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(size = base + 1, face = "bold", color = "#1e293b"))
}

# ── Task 3 data prep ─────────────────────────────────────────────────────────
agent_lab_t3 <- c(
  legal_agent        = "Legal",
  social_media_agent = "Social-Mgr",
  quality_agent      = "Platform-Trust",
  pr_agent           = "PR",
  pr_intern_agent    = "PR-Intern",
  intern_agent       = "Intern",
  judge_agent        = "Judge"
)

chan_levels_t3 <- c("comms_huddle", "one_on_one_chat", "side_huddle",
                    "official_post", "personal_post", "anonymous_post")

SENS_RX <- "merger|civicloom|embargo|harbor|acquisition|rebrand|harborcrest"
RAT_RX  <- paste0("not our breach|public domain|already (confirmed|in )|technically|",
                  "defensible|permissible|lifted|verbal consent|no longer|",
                  "doesn'?t (cover|apply)|within (bounds|scope)|de minimis|grey area|gray area|bilateral")

t3 <- df %>%
  mutate(
    hour     = as_datetime(hour),
    date     = as.Date(hour),
    agent    = dplyr::recode(agent_id, !!!agent_lab_t3),
    all_text = tolower(paste(content, deliberating, reacting, rationalizing)),
    period = factor(case_when(
      date <  as.Date("2046-05-22") ~ "Normal",
      date == as.Date("2046-06-05") ~ "Crisis",
      TRUE                          ~ "Escalation"),
      levels = c("Normal", "Escalation", "Crisis")),
    chan_class = factor(case_when(
      channel %in% c("official_post", "anonymous_post", "personal_post") ~ "public",
      channel %in% c("side_huddle", "one_on_one_chat")                   ~ "private",
      TRUE                                                               ~ "huddle"),
      levels = c("huddle", "private", "public")),
    channel    = factor(channel, levels = chan_levels_t3),
    is_public  = chan_class == "public",
    is_shadow  = channel == "side_huddle",
    is_anon    = channel == "anonymous_post",
    sensitive  = str_detect(all_text, SENS_RX),
    rationalize = rationalizing != "" & str_detect(tolower(rationalizing), RAT_RX)
  )

ord_t3 <- t3 %>% distinct(hour) %>% arrange(hour) %>%
  mutate(rlabel = if_else(as.Date(hour) == as.Date("2046-06-05"),
                          format(hour, "%H:%M"), format(hour, "%m/%d")))
t3 <- t3 %>% left_join(ord_t3, by = "hour") %>%
  mutate(rlabel = factor(rlabel, levels = ord_t3$rlabel))

T3_AGENTS <- agent_lab_t3[agent_lab_t3 %in% t3$agent]
T3_DMIN   <- min(t3$date)
T3_DMAX   <- max(t3$date)

baseline_t3 <- t3 %>% filter(period == "Normal") %>% distinct(agent, channel)
agents_with_base_t3 <- unique(baseline_t3$agent)
t3 <- t3 %>%
  left_join(baseline_t3 %>% mutate(in_base = TRUE), by = c("agent", "channel")) %>%
  mutate(out_of_baseline = is.na(in_base) & agent %in% agents_with_base_t3)

jsd_t3 <- function(p, q) {
  if (sum(p) == 0 || sum(q) == 0) return(NA_real_)
  p <- p / sum(p); q <- q / sum(q); m <- (p + q) / 2
  kl <- function(a, b) { i <- a > 0; sum(a[i] * log2(a[i] / b[i])) }
  0.5 * kl(p, m) + 0.5 * kl(q, m)
}
mix_w_t3 <- t3 %>% count(agent, period, channel) %>%
  pivot_wider(names_from = channel, values_from = n, values_fill = 0)
for (ch in chan_levels_t3) if (!ch %in% names(mix_w_t3)) mix_w_t3[[ch]] <- 0
mix_w_t3 <- mix_w_t3 %>% rowwise() %>%
  mutate(vec = list(as.numeric(c_across(all_of(chan_levels_t3))))) %>% ungroup()
nvec_t3 <- mix_w_t3 %>% filter(period == "Normal") %>% select(agent, nvec = vec)
dev_tbl_t3 <- mix_w_t3 %>% left_join(nvec_t3, by = "agent") %>%
  mutate(jsd = map2_dbl(vec, nvec, ~ if (is.null(.y) || !is.numeric(.y) || length(.y) < 2)
    NA_real_ else jsd_t3(.x, .y))) %>%
  select(agent, period, jsd)

# Findings box — blue accent matching mc1_css --accent token
findings_box_t3 <- function(...) div(class = "t3-findings",
  tags$div(class = "t3-findings-title", "\U0001F50D Findings"), tags$div(...))

T3_CSS <- "
  .t3-findings {
    background: #eef4fe;
    border-left: 4px solid #3b82f6;
    padding: 12px 16px;
    margin-top: 10px;
    border-radius: 4px;
    font-size: 14px;
  }
  .t3-findings-title { font-weight: 700; color: #1a3a5c; margin-bottom: 6px; }
  .t3-findings ul { margin-bottom: 0; padding-left: 18px; }
  .t3-lead { font-size: 0.92rem; color: #475569; line-height: 1.7; }
"

# ── UI ────────────────────────────────────────────────────────────────────────
task3_ui <- nav_panel(
  "Task 3: Leading Indicators",
  tags$style(HTML(T3_CSS)),

  layout_sidebar(
    sidebar = sidebar(
      width = 290, title = "Filters",
      checkboxGroupInput("t3_agents", "Agents",
                         choices  = unname(T3_AGENTS),
                         selected = unname(T3_AGENTS)),
      dateRangeInput("t3_dates", "Date window (time-series)",
                     start = T3_DMIN, end = T3_DMAX,
                     min = T3_DMIN, max = T3_DMAX),
      hr(),
      helpText("Baseline = the 'Normal' period (before 22 May). 'Expected' behaviour ",
               "is each agent's Normal-period channel repertoire.")
    ),

    navset_tab(

      # ---- Overview ----
      nav_panel("Overview",
        br(),
        card(
          card_header(class = "ch-accent",
                      "Task 3 — yes, the breach was foreshadowed for two weeks"),
          card_body(
            p(class = "t3-lead",
              "The 5 June embargo breach was the final step of a long, observable build-up. The leaker — legal_agent, the team's own compliance authority — knew a transaction was in motion from 23 May, coordinated in the private 'Shadow' channel, and rationalised its way past the embargo. The warning signs were abundant but invisible to the control that was supposed to catch them."),
            tags$ul(
              tags$li(strong("Q3.1 "), "Agents acquired channels they never used before (legal_agent's anonymous public posting; shadow-channel adoption). Divergence from baseline rises Normal → Escalation → Crisis."),
              tags$li(strong("Q3.2 "), "Sensitive merger talk lived privately for ~2 weeks with zero public leakage; the 'coordinate-in-shadow-then-go-public' pattern and legal_agent's escalating rationalisations were rehearsed before 6/5."),
              tags$li(strong("Q3.3 "), "The Judge arrived late (30 May) and only ever sat in comms_huddle / one_on_one. It never entered the Shadow channel, never monitored public posts, and could not see internal_state — so none of the signals triggered action."))
          )
        )
      ),

      # ---- Q3.1 ----
      nav_panel("Q3.1 Expected vs Actual",
        br(),
        card(full_screen = TRUE,
             card_header(class = "ch-accent",
                         "Channel repertoire by agent × period (share of messages)"),
             plotlyOutput("t3_p_mix", height = 430), uiOutput("t3_f_mix")),
        layout_columns(col_widths = c(7, 5),
          card(full_screen = TRUE,
               card_header(class = "ch-accent",
                           "Behavioural divergence from Normal baseline (JSD)"),
               plotlyOutput("t3_p_jsd", height = 360)),
          card(full_screen = TRUE,
               card_header(class = "ch-accent",
                           "Out-of-baseline events (new channel for that agent)"),
               DTOutput("t3_t_oob")))
      ),

      # ---- Q3.2 ----
      nav_panel("Q3.2 Breach-like Behaviour",
        br(),
        card(full_screen = TRUE,
             card_header(class = "ch-accent",
                         "Where merger / embargo talk lived over time (private vs public)"),
             plotlyOutput("t3_p_topic", height = 430), uiOutput("t3_f_topic")),
        layout_columns(col_widths = c(6, 6),
          card(full_screen = TRUE,
               card_header(class = "ch-accent",
                           "Rationalisation signals over time (rule-justifying internal state)"),
               plotlyOutput("t3_p_rat", height = 360)),
          card(full_screen = TRUE,
               card_header(class = "ch-accent", "The rationalisation trail"),
               DTOutput("t3_t_rat")))
      ),

      # ---- Q3.3 ----
      nav_panel("Q3.3 Why No Action",
        br(),
        card(full_screen = TRUE,
             card_header(class = "ch-accent",
                         "Coverage gap: the Judge vs the activity it never saw"),
             plotlyOutput("t3_p_cov", height = 430), uiOutput("t3_f_cov")),
        layout_columns(col_widths = c(7, 5),
          card(full_screen = TRUE,
               card_header(class = "ch-accent",
                           "Visibility matrix — who used which channel (Judge blind spots in red)"),
               plotlyOutput("t3_p_vis", height = 360)),
          card(full_screen = TRUE,
               card_header(class = "ch-accent", "Why each signal produced no response"),
               DTOutput("t3_t_why")))
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
task3_server <- function(input, output, session) {

  fdf <- reactive(t3 %>% filter(agent %in% input$t3_agents))
  tdf <- reactive(t3 %>% filter(agent %in% input$t3_agents,
                                date >= input$t3_dates[1], date <= input$t3_dates[2]))

  # ---- Q3.1 ----
  output$t3_p_mix <- renderPlotly({
    d <- fdf() %>% count(agent, period, channel) %>%
      group_by(agent, period) %>% mutate(share = n / sum(n)) %>% ungroup()
    shiny::validate(shiny::need(nrow(d) > 0, "Select at least one agent."))
    g <- ggplot(d, aes(period, share, fill = channel,
                       text = paste0(agent, " / ", period, "<br>", channel,
                                     ": ", n, " msgs (", scales::percent(share, 1), ")"))) +
      geom_col(width = .8) + facet_wrap(~agent, nrow = 1) +
      scale_fill_manual(values = T3_CHAN_PAL, drop = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = "share of messages", fill = NULL) +
      t3_theme(11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.x = element_blank())
    to_plotly(g, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.25))
  })

  output$t3_f_mix <- renderUI(findings_box_t3(tags$ul(
    tags$li("In the Normal period, legal_agent (the compliance authority) used ONLY internal channels — comms_huddle and one_on_one, zero public posting. By the Crisis it had acquired anonymous_post (12 merger leaks) and personal_post — channels entirely outside its expected role."),
    tags$li("side_huddle (the 'Shadow' channel) appears for no agent in Normal, a few in Escalation, then dominates several agents' mix in the Crisis — a new coordination behaviour."),
    tags$li("social_media_agent and quality_agent also break into personal_post commentary they never used before."),
    tags$li(strong("Answer to Q3.1: "), "the agents that broke the embargo had already started acting outside their designed channel repertoire — the gap between expected and actual behaviour widens steadily before 6/5."))))

  output$t3_p_jsd <- renderPlotly({
    d <- dev_tbl_t3 %>% filter(agent %in% input$t3_agents)
    shiny::validate(shiny::need(nrow(d) > 0, "Select at least one agent."))
    g <- ggplot(d, aes(period, jsd, group = agent, color = agent,
                       text = paste0(agent, " — ", period, "<br>JSD vs baseline: ",
                                     round(jsd, 3)))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_color_manual(values = T3_AGENT_PAL, na.value = "#94a3b8") +
      labs(x = NULL, y = "JS divergence from Normal channel-mix", color = NULL) +
      t3_theme(11)
    to_plotly(g, tooltip = "text")
  })

  output$t3_t_oob <- renderDT({
    fdf() %>% filter(out_of_baseline) %>%
      transmute(time = format(hour, "%m/%d %H:%M"), agent, channel = as.character(channel),
                sensitive, content = str_trunc(content, 90)) %>%
      arrange(time) %>%
      datatable(rownames = FALSE, options = list(pageLength = 6, dom = "tp"))
  })

  # ---- Q3.2 ----
  output$t3_p_topic <- renderPlotly({
    d <- tdf() %>% filter(sensitive) %>%
      mutate(rlabel = droplevels(rlabel)) %>% count(rlabel, chan_class)
    g <- ggplot(d, aes(rlabel, n, fill = chan_class,
                       text = paste0(rlabel, "<br>", chan_class, ": ", n))) +
      geom_col() +
      scale_fill_manual(values = T3_CLASS_PAL) +
      labs(x = NULL, y = "messages mentioning merger / embargo", fill = NULL) +
      t3_theme(11) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    to_plotly(g, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.3))
  })

  output$t3_f_topic <- renderUI({
    pub_pre <- t3 %>% filter(sensitive, chan_class == "public", date < as.Date("2046-06-05")) %>% nrow()
    findings_box_t3(tags$ul(
      tags$li(sprintf("Sensitive merger/embargo content circulated privately from 23 May onward, but public mentions before the crisis day = %d. It stayed contained — right up until 6/5, when it burst into the public channels.", pub_pre)),
      tags$li("The 'coordinate privately, then push public' shape is the breach signature. The private/Shadow build-up was a two-week rehearsal; only the final public step was new."),
      tags$li("The Shadow channel (side_huddle) was the venue: activated 22 May, used for the merger briefing, then heavily on 6/5."),
      tags$li(strong("Answer to Q3.2: "), "yes — the same behaviours (private merger coordination + rationalisation) recurred for two weeks; 6/5 is where they finally reached a public channel.")))
  })

  output$t3_p_rat <- renderPlotly({
    d <- tdf() %>% filter(rationalize) %>%
      mutate(rlabel = droplevels(rlabel)) %>% count(rlabel, agent)
    if (nrow(d) == 0) return(plotly_empty())
    g <- ggplot(d, aes(rlabel, n, fill = agent,
                       text = paste0(rlabel, " — ", agent, ": ", n))) +
      geom_col() +
      scale_fill_manual(values = T3_AGENT_PAL, na.value = "#94a3b8") +
      labs(x = NULL, y = "rule-justifying internal states", fill = NULL) +
      t3_theme(11) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    to_plotly(g, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.3))
  })

  output$t3_t_rat <- renderDT({
    fdf() %>% filter(rationalizing != "") %>%
      transmute(time = format(hour, "%m/%d"), agent, channel = as.character(channel),
                flagged = ifelse(rationalize, "⚠", ""),
                rationalizing = str_trunc(rationalizing, 160)) %>%
      arrange(time) %>%
      datatable(rownames = FALSE, options = list(pageLength = 6, dom = "tp"))
  })

  # ---- Q3.3 ----
  output$t3_p_cov <- renderPlotly({
    d <- bind_rows(
      tdf() %>% filter(agent == "Judge")  %>% count(rlabel) %>% mutate(series = "Judge activity"),
      tdf() %>% filter(is_shadow)         %>% count(rlabel) %>% mutate(series = "Shadow-channel messages"),
      tdf() %>% filter(sensitive)         %>% count(rlabel) %>% mutate(series = "Sensitive-topic messages")
    ) %>% mutate(rlabel = droplevels(rlabel),
                 series = factor(series,
                                 levels = c("Sensitive-topic messages", "Shadow-channel messages", "Judge activity")))
    shiny::validate(shiny::need(nrow(d) > 0, "Select at least one agent / widen the date window."))
    g <- ggplot(d, aes(rlabel, n, fill = series,
                       text = paste0(rlabel, " — ", series, ": ", n))) +
      geom_col(show.legend = FALSE) + facet_wrap(~series, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = c(
        "Sensitive-topic messages" = "#E24B4A",
        "Shadow-channel messages"  = "#EF9F27",
        "Judge activity"           = "#378ADD"
      )) +
      labs(x = NULL, y = NULL) + t3_theme(10) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    to_plotly(g, tooltip = "text")
  })

  output$t3_f_cov <- renderUI({
    judge_start <- t3 %>% filter(agent == "Judge") %>% summarise(min(date)) %>% pull()
    findings_box_t3(tags$ul(
      tags$li(sprintf("The Judge's first appearance is %s — 13 days into the window, AFTER the Shadow channel (22 May) and the merger briefing (25 May) had already happened. The early build-up had no compliance oversight at all.", format(judge_start, "%d %b"))),
      tags$li("The top two panels (sensitive + Shadow activity) spike well before and outside the bottom panel (Judge activity). The control was absent or silent exactly where the risk concentrated."),
      tags$li(strong("Answer to Q3.3 (timing): "), "prior occasions produced no action because, for most of them, no monitor existed yet — and when it did, it was not present in the channels carrying the signal.")))
  })

  output$t3_p_vis <- renderPlotly({
    shiny::validate(shiny::need(length(input$t3_agents) > 0, "Select at least one agent."))
    judge_ch <- t3 %>% filter(agent == "Judge") %>% distinct(channel) %>% pull() %>% as.character()
    d <- fdf() %>% count(agent, channel, .drop = FALSE) %>%
      mutate(monitored = channel %in% judge_ch)
    blind <- d %>% filter(!monitored, n > 0)
    g <- ggplot(d, aes(channel, agent, fill = n,
                       text = paste0(agent, " · ", channel, "<br>", n, " msgs<br>",
                                     ifelse(channel %in% judge_ch, "Judge MONITORS", "Judge BLIND")))) +
      geom_tile(color = "white")
    if (nrow(blind) > 0)
      g <- g + geom_tile(data = blind, aes(channel, agent), fill = NA,
                         color = "#E24B4A", linewidth = 0.9, inherit.aes = FALSE)
    g <- g +
      scale_fill_gradient(low = "#eef4fe", high = "#0d1b2a") +
      labs(x = NULL, y = NULL, fill = "msgs") + t3_theme(11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    to_plotly(g, tooltip = "text")
  })

  output$t3_t_why <- renderDT({
    tibble::tribble(
      ~signal, ~when, ~why_no_action,
      "Merger known to Legal", "23 May", "Lived in internal_state (deliberation) — not a message the Judge reviews.",
      "Shadow channel activated", "22 May", "side_huddle is unmonitored; the Judge never enters it.",
      "Merger briefing in Shadow", "25 May", "Occurred before the Judge existed (arrived 30 May).",
      "Escalating rationalisation", "24 May – 5 Jun", "Internal reasoning, invisible to compliance review.",
      "Anonymous public posts", "5 Jun", "anonymous_post is not pre-cleared; content is already public when seen.",
      "Public-channel adoption", "5 Jun", "Judge only operates in comms_huddle / one_on_one."
    ) %>% datatable(rownames = FALSE, options = list(pageLength = 6, dom = "t"))
  })
}

