# =====================================================================
#  VAST Challenge 2026 — MC1 :: TenantThread embargo breach
#  TASK 3  —  "Were there leading indicators that such a release was possible?"
#
#  Q3.1  Prior occasions where an agent's ACTUAL behaviour differs from EXPECTED
#  Q3.2  Other occasions exhibiting behaviours LIKE those that led to the release
#  Q3.3  Why prior occasions did NOT result in noticeable action
#
#  Grounded in the REAL data (MC1_final_00.json). Key facts it surfaces:
#   - legal_agent (the compliance authority) is the leaker: 12 anonymous_post
#     merger leaks on 6/5, after using ONLY internal channels in the Normal period.
#   - side_huddle ("Shadow" channel) was the private coordination venue (act. 5/22).
#   - judge_agent arrived late (5/30) and only ever sat in comms_huddle / one_on_one
#     -> structurally blind to side_huddle, the public posts, and all internal_state.
#
#  RUN  (this app builds on your Task-1 teammate's `df` — source it first)
#    install.packages(c("shiny","bslib","dplyr","tidyr","stringr",
#                       "lubridate","purrr","ggplot2","plotly","DT"))
#    source("task1_dataprep.R")   # <-- your teammate's script; it creates `df`
#    shiny::runApp("task3_app.R")
#
#  Needs only these columns from `df` (all present in the Task-1 prep):
#    hour, agent_id, channel, content, deliberating, reacting, rationalizing
# =====================================================================

library(shiny);   library(bslib)
library(dplyr);   library(tidyr);   library(stringr); library(lubridate)
library(purrr);   library(ggplot2); library(plotly);  library(DT)
library(jsonlite); library(tibble)

# ---------------------------------------------------------------------
# macOS / RStudio guard.  ggplotly() has to measure the plot's grobs
# against the *current* graphics device.  In an interactive RStudio
# session that device is RStudioGD/quartz, and when the "Plots" pane is
# small or collapsed it throws "invalid quartz() device size" — even if
# the app opens in an external browser, and even if we open an off-screen
# device once at start-up (RStudio re-activates quartz before each render).
# The reliable fix: open a fresh, valid off-screen pdf device *for the
# duration of each conversion*, so ggplotly never touches quartz.  Use
# to_plotly() in place of ggplotly() for every chart below.
to_plotly <- function(g, ...) {
  # ggplotly needs a valid graphics device: a vector device for measuring
  # grobs AND a raster device for rasterising geom_tile heatmaps. RStudio's
  # quartz pane can be zero-size ("invalid quartz() device size") and cairo
  # is often unavailable on macOS ("Image height is zero in IHDR"). ragg's
  # agg_png is a self-contained raster device (no X11/cairo) with a fixed,
  # valid size, so it services both — falling back to pdf(NULL) if absent.
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

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---------------------------------------------------------------------
# 1. INPUT  ── self-contained: this app builds its own `df` straight from
#    the MC1 JSON, so it runs on its own (shiny::runApp("Task 3 Shiny App")).
#    If a teammate's Task-1 `df` is already in scope (e.g. the combined
#    app.R sources this file after Task 1), we reuse it as-is instead.
#    Required columns: hour, agent_id, channel, content,
#                      deliberating, reacting, rationalizing.
# ---------------------------------------------------------------------
if (!exists("df") || !is.data.frame(df)) {   # note: stats::df() exists as a fn, so test for a data frame
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
  parse_msgs <- function(rd) {
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
  df <- purrr::map_dfr(raw$rounds, parse_msgs)
}
base <- df

agent_lab <- c(legal_agent = "Legal", social_media_agent = "Social-Mgr",
               quality_agent = "Platform-Trust", pr_agent = "PR",
               pr_intern_agent = "PR-Intern", intern_agent = "Intern",
               judge_agent = "Judge")

chan_levels <- c("comms_huddle", "one_on_one_chat", "side_huddle",
                 "official_post", "personal_post", "anonymous_post")
chan_pal <- c(comms_huddle = "#bdbdbd", one_on_one_chat = "#74a9cf",
              side_huddle = "#fc8d59", official_post = "#3182bd",
              personal_post = "#e6550d", anonymous_post = "#d7301f")

SENS_RX <- "merger|civicloom|embargo|harbor|acquisition|rebrand|harborcrest"
RAT_RX  <- paste0("not our breach|public domain|already (confirmed|in )|technically|",
                  "defensible|permissible|lifted|verbal consent|no longer|",
                  "doesn'?t (cover|apply)|within (bounds|scope)|de minimis|grey area|gray area|bilateral")

t3 <- base %>%
  mutate(
    hour     = as_datetime(hour),
    date     = as.Date(hour),
    agent    = dplyr::recode(agent_id, !!!agent_lab),
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
    channel    = factor(channel, levels = chan_levels),
    is_public  = chan_class == "public",
    is_shadow  = channel == "side_huddle",
    is_anon    = channel == "anonymous_post",
    sensitive  = str_detect(all_text, SENS_RX),
    rationalize = rationalizing != "" & str_detect(tolower(rationalizing), RAT_RX)
  )

# chronological round label (date for pre-crisis, time for the crisis day)
ord <- t3 %>% distinct(hour) %>% arrange(hour) %>%
  mutate(rlabel = if_else(as.Date(hour) == as.Date("2046-06-05"),
                          format(hour, "%H:%M"), format(hour, "%m/%d")))
t3 <- t3 %>% left_join(ord, by = "hour") %>%
  mutate(rlabel = factor(rlabel, levels = ord$rlabel))

AGENTS <- agent_lab[agent_lab %in% t3$agent]   # display labels present
DMIN <- min(t3$date); DMAX <- max(t3$date)

# ---- Baseline (Normal-period) channel repertoire per agent ----------
baseline <- t3 %>% filter(period == "Normal") %>% distinct(agent, channel)
agents_with_base <- unique(baseline$agent)
t3 <- t3 %>%
  left_join(baseline %>% mutate(in_base = TRUE), by = c("agent", "channel")) %>%
  mutate(out_of_baseline = is.na(in_base) & agent %in% agents_with_base)

# ---- Jensen–Shannon divergence of channel-mix vs Normal baseline ----
jsd <- function(p, q) {
  if (sum(p) == 0 || sum(q) == 0) return(NA_real_)
  p <- p / sum(p); q <- q / sum(q); m <- (p + q) / 2
  kl <- function(a, b) { i <- a > 0; sum(a[i] * log2(a[i] / b[i])) }
  0.5 * kl(p, m) + 0.5 * kl(q, m)
}
mix_w <- t3 %>% count(agent, period, channel) %>%
  pivot_wider(names_from = channel, values_from = n, values_fill = 0)
for (ch in chan_levels) if (!ch %in% names(mix_w)) mix_w[[ch]] <- 0
mix_w <- mix_w %>% rowwise() %>%
  mutate(vec = list(as.numeric(c_across(all_of(chan_levels))))) %>% ungroup()
nvec <- mix_w %>% filter(period == "Normal") %>% select(agent, nvec = vec)
dev_tbl <- mix_w %>% left_join(nvec, by = "agent") %>%
  mutate(jsd = map2_dbl(vec, nvec, ~ if (is.null(.y) || !is.numeric(.y) || length(.y) < 2)
    NA_real_ else jsd(.x, .y))) %>%
  select(agent, period, jsd)

findings_box <- function(...) div(class = "findings",
                                  tags$div(class = "findings-title", "\U0001F50D Findings"), tags$div(...))

# =====================================================================
# 2. UI
# =====================================================================
ui <- page_navbar(
  title = "MC1 Task 3 — Leading Indicators",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2C3E50"),
  header = tags$head(tags$style(HTML("
    .findings{background:#f4f7fa;border-left:4px solid #2C3E50;padding:12px 16px;
      margin-top:10px;border-radius:4px;font-size:14px;}
    .findings-title{font-weight:700;color:#2C3E50;margin-bottom:6px;}
    .findings ul{margin-bottom:0;padding-left:18px;} .lead-small{font-size:15px;color:#555;}"))),
  
  sidebar = sidebar(width = 290, title = "Filters",
                    checkboxGroupInput("agents", "Agents", choices = unname(AGENTS), selected = unname(AGENTS)),
                    dateRangeInput("dates", "Date window (time-series)", start = DMIN, end = DMAX,
                                   min = DMIN, max = DMAX),
                    hr(),
                    helpText("Baseline = the 'Normal' period (before 22 May). 'Expected' behaviour ",
                             "is each agent's Normal-period channel repertoire.")),
  
  # ---------------- Overview ----------------
  nav_panel("Overview",
            card(card_header("Task 3 — yes, the breach was foreshadowed for two weeks"),
                 card_body(
                   p(class = "lead-small",
                     "The 5 June embargo breach was the final step of a long, observable build-up. The leaker — legal_agent, the team's own compliance authority — knew a transaction was in motion from 23 May, coordinated in the private 'Shadow' channel, and rationalised its way past the embargo. The warning signs were abundant but invisible to the control that was supposed to catch them."),
                   tags$ul(
                     tags$li(strong("Q3.1 "), "Agents acquired channels they never used before (legal_agent's anonymous public posting; shadow-channel adoption). Divergence from baseline rises Normal \u2192 Escalation \u2192 Crisis."),
                     tags$li(strong("Q3.2 "), "Sensitive merger talk lived privately for ~2 weeks with zero public leakage; the 'coordinate-in-shadow-then-go-public' pattern and legal_agent's escalating rationalisations were rehearsed before 6/5."),
                     tags$li(strong("Q3.3 "), "The Judge arrived late (30 May) and only ever sat in comms_huddle / one_on_one. It never entered the Shadow channel, never monitored public posts, and could not see internal_state \u2014 so none of the signals triggered action."))
                 ))),
  
  # ---------------- Q3.1 ----------------
  nav_panel("Q3.1 Expected vs Actual",
            card(full_screen = TRUE, card_header("Channel repertoire by agent \u00d7 period (share of messages)"),
                 plotlyOutput("p_mix", height = 430), uiOutput("f_mix")),
            layout_columns(col_widths = c(7, 5),
                           card(full_screen = TRUE, card_header("Behavioural divergence from Normal baseline (JSD)"),
                                plotlyOutput("p_jsd", height = 360)),
                           card(full_screen = TRUE, card_header("Out-of-baseline events (new channel for that agent)"),
                                DTOutput("t_oob")))),
  
  # ---------------- Q3.2 ----------------
  nav_panel("Q3.2 Breach-like Behaviour",
            card(full_screen = TRUE, card_header("Where merger / embargo talk lived over time (private vs public)"),
                 plotlyOutput("p_topic", height = 430), uiOutput("f_topic")),
            layout_columns(col_widths = c(6, 6),
                           card(full_screen = TRUE, card_header("Rationalisation signals over time (rule-justifying internal state)"),
                                plotlyOutput("p_rat", height = 360)),
                           card(full_screen = TRUE, card_header("The rationalisation trail (click a row)"),
                                DTOutput("t_rat")))),
  
  # ---------------- Q3.3 ----------------
  nav_panel("Q3.3 Why No Action",
            card(full_screen = TRUE, card_header("Coverage gap: the Judge vs the activity it never saw"),
                 plotlyOutput("p_cov", height = 430), uiOutput("f_cov")),
            layout_columns(col_widths = c(7, 5),
                           card(full_screen = TRUE, card_header("Visibility matrix \u2014 who used which channel (Judge blind spots in red)"),
                                plotlyOutput("p_vis", height = 360)),
                           card(full_screen = TRUE, card_header("Why each signal produced no response"),
                                DTOutput("t_why"))))
)

# =====================================================================
# 3. SERVER
# =====================================================================
server <- function(input, output, session) {
  
  fdf <- reactive(t3 %>% filter(agent %in% input$agents))
  tdf <- reactive(t3 %>% filter(agent %in% input$agents,
                                date >= input$dates[1], date <= input$dates[2]))
  
  # ---------------- Q3.1 ----------------
  output$p_mix <- renderPlotly({
    d <- fdf() %>% count(agent, period, channel) %>%
      group_by(agent, period) %>% mutate(share = n / sum(n)) %>% ungroup()
    shiny::validate(shiny::need(nrow(d) > 0, "Select at least one agent."))
    g <- ggplot(d, aes(period, share, fill = channel,
                       text = paste0(agent, " / ", period, "<br>", channel,
                                     ": ", n, " msgs (", scales::percent(share, 1), ")"))) +
      geom_col(width = .8) + facet_wrap(~agent, nrow = 1) +
      scale_fill_manual(values = chan_pal, drop = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = "share of messages", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.x = element_blank())
    to_plotly(g, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.25))
  })
  
  output$f_mix <- renderUI(findings_box(tags$ul(
    tags$li("In the Normal period, legal_agent (the compliance authority) used ONLY internal channels \u2014 comms_huddle and one_on_one, zero public posting. By the Crisis it had acquired anonymous_post (12 merger leaks) and personal_post \u2014 channels entirely outside its expected role."),
    tags$li("side_huddle (the 'Shadow' channel) appears for no agent in Normal, a few in Escalation, then dominates several agents' mix in the Crisis \u2014 a new coordination behaviour."),
    tags$li("social_media_agent and quality_agent also break into personal_post commentary they never used before."),
    tags$li(strong("Answer to Q3.1: "), "the agents that broke the embargo had already started acting outside their designed channel repertoire \u2014 the gap between expected and actual behaviour widens steadily before 6/5."))))
  
  output$p_jsd <- renderPlotly({
    d <- dev_tbl %>% filter(agent %in% input$agents)
    shiny::validate(shiny::need(nrow(d) > 0, "Select at least one agent."))
    g <- ggplot(d, aes(period, jsd, group = agent, color = agent,
                       text = paste0(agent, " \u2014 ", period, "<br>JSD vs baseline: ",
                                     round(jsd, 3)))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      labs(x = NULL, y = "JS divergence from Normal channel-mix", color = NULL) +
      theme_minimal(base_size = 11)
    to_plotly(g, tooltip = "text")
  })
  
  output$t_oob <- renderDT({
    fdf() %>% filter(out_of_baseline) %>%
      transmute(time = format(hour, "%m/%d %H:%M"), agent, channel = as.character(channel),
                sensitive, content = str_trunc(content, 90)) %>%
      arrange(time) %>%
      datatable(rownames = FALSE, options = list(pageLength = 6, dom = "tp"))
  })
  
  # ---------------- Q3.2 ----------------
  output$p_topic <- renderPlotly({
    d <- tdf() %>% filter(sensitive) %>%
      mutate(rlabel = droplevels(rlabel)) %>% count(rlabel, chan_class)
    g <- ggplot(d, aes(rlabel, n, fill = chan_class,
                       text = paste0(rlabel, "<br>", chan_class, ": ", n))) +
      geom_col() +
      scale_fill_manual(values = c(huddle = "#9e9e9e", private = "#3182bd", public = "#d7301f")) +
      labs(x = NULL, y = "messages mentioning merger / embargo", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    to_plotly(g, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.3))
  })
  
  output$f_topic <- renderUI({
    pub_pre <- t3 %>% filter(sensitive, chan_class == "public", date < as.Date("2046-06-05")) %>% nrow()
    findings_box(tags$ul(
      tags$li(sprintf("Sensitive merger/embargo content circulated privately from 23 May onward, but public mentions before the crisis day = %d. It stayed contained \u2014 right up until 6/5, when it burst into the public channels.", pub_pre)),
      tags$li("The 'coordinate privately, then push public' shape is the breach signature. The private/Shadow build-up was a two-week rehearsal; only the final public step was new."),
      tags$li("The Shadow channel (side_huddle) was the venue: activated 22 May, used for the merger briefing, then heavily on 6/5."),
      tags$li(strong("Answer to Q3.2: "), "yes \u2014 the same behaviours (private merger coordination + rationalisation) recurred for two weeks; 6/5 is where they finally reached a public channel.")))
  })
  
  output$p_rat <- renderPlotly({
    d <- tdf() %>% filter(rationalize) %>%
      mutate(rlabel = droplevels(rlabel)) %>% count(rlabel, agent)
    if (nrow(d) == 0) return(plotly_empty())
    g <- ggplot(d, aes(rlabel, n, fill = agent,
                       text = paste0(rlabel, " \u2014 ", agent, ": ", n))) +
      geom_col() + labs(x = NULL, y = "rule-justifying internal states", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    to_plotly(g, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.3))
  })
  
  output$t_rat <- renderDT({
    fdf() %>% filter(rationalizing != "") %>%
      transmute(time = format(hour, "%m/%d"), agent, channel = as.character(channel),
                flagged = ifelse(rationalize, "\u26A0", ""),
                rationalizing = str_trunc(rationalizing, 160)) %>%
      arrange(time) %>%
      datatable(rownames = FALSE, options = list(pageLength = 6, dom = "tp"))
  })
  
  # ---------------- Q3.3 ----------------
  output$p_cov <- renderPlotly({
    d <- bind_rows(
      tdf() %>% filter(agent == "Judge")  %>% count(rlabel) %>% mutate(series = "Judge activity"),
      tdf() %>% filter(is_shadow)         %>% count(rlabel) %>% mutate(series = "Shadow-channel messages"),
      tdf() %>% filter(sensitive)         %>% count(rlabel) %>% mutate(series = "Sensitive-topic messages")
    ) %>% mutate(rlabel = droplevels(rlabel),
                 series = factor(series,
                                 levels = c("Sensitive-topic messages", "Shadow-channel messages", "Judge activity")))
    shiny::validate(shiny::need(nrow(d) > 0, "Select at least one agent / widen the date window."))
    g <- ggplot(d, aes(rlabel, n, fill = series,
                       text = paste0(rlabel, " \u2014 ", series, ": ", n))) +
      geom_col(show.legend = FALSE) + facet_wrap(~series, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = c("Sensitive-topic messages" = "#d7301f",
                                   "Shadow-channel messages" = "#fc8d59",
                                   "Judge activity" = "#2C3E50")) +
      labs(x = NULL, y = NULL) + theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    to_plotly(g, tooltip = "text")
  })
  
  output$f_cov <- renderUI({
    judge_start <- t3 %>% filter(agent == "Judge") %>% summarise(min(date)) %>% pull()
    findings_box(tags$ul(
      tags$li(sprintf("The Judge's first appearance is %s \u2014 13 days into the window, AFTER the Shadow channel (22 May) and the merger briefing (25 May) had already happened. The early build-up had no compliance oversight at all.", format(judge_start, "%d %b"))),
      tags$li("The top two panels (sensitive + Shadow activity) spike well before and outside the bottom panel (Judge activity). The control was absent or silent exactly where the risk concentrated."),
      tags$li(strong("Answer to Q3.3 (timing): "), "prior occasions produced no action because, for most of them, no monitor existed yet \u2014 and when it did, it was not present in the channels carrying the signal.")))
  })
  
  output$p_vis <- renderPlotly({
    shiny::validate(shiny::need(length(input$agents) > 0, "Select at least one agent."))
    judge_ch <- t3 %>% filter(agent == "Judge") %>% distinct(channel) %>% pull() %>% as.character()
    d <- fdf() %>% count(agent, channel, .drop = FALSE) %>%
      mutate(monitored = channel %in% judge_ch,
             lab = ifelse(channel %in% judge_ch, "Judge sees", "BLIND SPOT"))
    blind <- d %>% filter(!monitored, n > 0)
    g <- ggplot(d, aes(channel, agent, fill = n,
                       text = paste0(agent, " \u00b7 ", channel, "<br>", n, " msgs<br>",
                                     ifelse(channel %in% judge_ch, "Judge MONITORS", "Judge BLIND")))) +
      geom_tile(color = "white")
    if (nrow(blind) > 0)            # red outline only where there are blind spots to draw
      g <- g + geom_tile(data = blind, aes(channel, agent), fill = NA,
                         color = "#d7301f", linewidth = 0.9, inherit.aes = FALSE)
    g <- g +
      scale_fill_gradient(low = "#eef2f5", high = "#2C3E50") +
      labs(x = NULL, y = NULL, fill = "msgs") + theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    to_plotly(g, tooltip = "text")
  })
  
  output$t_why <- renderDT({
    tibble::tribble(
      ~signal, ~when, ~why_no_action,
      "Merger known to Legal", "23 May", "Lived in internal_state (deliberation) \u2014 not a message the Judge reviews.",
      "Shadow channel activated", "22 May", "side_huddle is unmonitored; the Judge never enters it.",
      "Merger briefing in Shadow", "25 May", "Occurred before the Judge existed (arrived 30 May).",
      "Escalating rationalisation", "24 May \u2013 5 Jun", "Internal reasoning, invisible to compliance review.",
      "Anonymous public posts", "5 Jun", "anonymous_post is not pre-cleared; content is already public when seen.",
      "Public-channel adoption", "5 Jun", "Judge only operates in comms_huddle / one_on_one."
    ) %>% datatable(rownames = FALSE, options = list(pageLength = 6, dom = "t"))
  })
}

shinyApp(ui, server)