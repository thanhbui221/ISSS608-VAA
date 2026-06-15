# ─────────────────────────────────────────────────────────────────────────────
# Task 2: Behavioral Analysis
# Defines: task2_ui, task2_server
# Assumes shared globals from main app.R:
#   df, rs, period_pal, ch_colours
# ─────────────────────────────────────────────────────────────────────────────

# ── Helpers ───────────────────────────────────────────────────────────────────
make_agent_label <- function(id) {
  map <- c(
    legal_agent          = "Legal",
    judge_eval_agent     = "Judge",
    social_media_agent   = "Social-Media",
    social_manager_agent = "Social-Media",
    pr_agent             = "PR",
    pr_intern_agent      = "PR-Intern",
    quality_agent        = "Quality",
    intern_agent         = "Intern"
  )
  ifelse(id %in% names(map), unname(map[id]), id)
}

# Add word_count and period to df if not already present
if (!"word_count" %in% names(df)) {
  df[["word_count"]] <- str_count(df$content, "\\S+")
}
if (!"period" %in% names(df)) {
  df[["period"]] <- factor(case_when(
    df$date < as.Date("2046-05-22") ~ "Normal",
    df$date < as.Date("2046-06-05") ~ "Escalation",
    TRUE                             ~ "Crisis"
  ), levels = c("Normal", "Escalation", "Crisis"))
}

T2_PERIOD_LEVELS <- c("Normal", "Escalation", "Crisis")

T2_CH_LABELS <- c(
  comms_huddle = "Comms Huddle",
  one_on_one   = "1-on-1 Chat",
  side_huddle  = "Side Huddle",
  official     = "Official Post",
  anon         = "Anonymous Post",
  personal     = "Personal Post"
)

T2_STATE_COLORS <- c(
  Deliberating  = "#378ADD",
  Reacting      = "#EF9F27",
  Rationalizing = "#E24B4A"
)

# ── UI ────────────────────────────────────────────────────────────────────────
task2_ui <- nav_panel(
  "Task 2: Behavioral Analysis",
  div(class = "container-fluid py-3",

    navset_tab(

      # ── Sub-tab 1: Volume & Complexity ─────────────────────────────────────
      nav_panel(
        "Volume & Complexity",
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 220,
            div(class = "sidebar-section",
              div(class = "sidebar-section-title", "Period Filter"),
              checkboxGroupInput(
                "t2_periods", label = NULL,
                choices  = T2_PERIOD_LEVELS,
                selected = T2_PERIOD_LEVELS
              )
            ),
            tags$hr(class = "sidebar-hr"),
            div(class = "sidebar-section",
              div(class = "sidebar-section-title", "Insight"),
              tags$p(
                style = "font-size:0.78rem; color:#475569; line-height:1.6;",
                "Message length increases ",
                tags$strong("2–3× on Crisis day"),
                " — consistent with cognitive overload, not pre-planned execution."
              )
            )
          ),
          div(
            layout_columns(
              col_widths = c(6, 6),
              card(
                card_header(class = "ch-accent", "Message Volume by Agent & Period"),
                card_body(plotlyOutput("t2_volume", height = "320px"))
              ),
              card(
                card_header(class = "ch-accent", "Average Message Length by Agent & Period"),
                card_body(plotlyOutput("t2_length", height = "320px"))
              )
            ),
            div(class = "insight-panel",
              div(class = "insight-title", "Key Finding — Volume & Complexity"),
              tags$p(
                tags$strong("Legal-Agent"), " dominates Crisis-day volume, followed by",
                " Social-Media-Agent and PR-Intern-Agent. Average message length rises",
                " sharply on Crisis day (up to 3× baseline) across all senior agents.",
                " This pattern is the signature of ", tags$em("real-time cognitive overload"),
                " — not execution of a pre-written plan."
              )
            )
          )
        )
      ),

      # ── Sub-tab 2: Channel Behaviour ────────────────────────────────────────
      nav_panel(
        "Channel Behaviour",
        br(),
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header(class = "ch-accent", "Channel Share by Period"),
            card_body(plotlyOutput("t2_channel_period", height = "360px"))
          ),
          card(
            card_header(class = "ch-accent", "Private Channel Share Over Time (Per Round)"),
            card_body(plotlyOutput("t2_channel_trend", height = "360px"))
          )
        ),
        div(class = "insight-panel",
          div(class = "insight-title", "Key Finding — Channel Behaviour"),
          tags$p(
            "The share of ", tags$code("side_huddle"), " and ", tags$code("one_on_one_chat"),
            " messages rises ", tags$strong("monotonically"), " from Normal → Escalation → Crisis.",
            " A purely reactive system would show a step-change only on Crisis day.",
            " The observed gradient — beginning when side_huddle was first introduced on",
            " May 22 — indicates ", tags$strong("deliberate channel migration"),
            " that began weeks before the breach."
          )
        )
      ),

      # ── Sub-tab 3: Internal States ───────────────────────────────────────────
      nav_panel(
        "Internal States",
        br(),
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header(class = "ch-accent", "State Type by Period"),
            card_body(plotlyOutput("t2_states_bar", height = "300px"))
          ),
          card(
            card_header(
              class = "ch-accent",
              div(
                style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
                span("Internal State Records"),
                selectInput(
                  "t2_state_agent", label = NULL,
                  choices  = c("All agents", "Legal", "Judge", "Social-Media",
                               "PR", "PR-Intern", "Quality", "Intern"),
                  selected = "All agents",
                  width    = "160px"
                )
              )
            ),
            card_body(
              style = "overflow-y: auto; max-height: 320px;",
              DTOutput("t2_states_tbl")
            )
          )
        ),
        div(class = "insight-panel",
          div(class = "insight-title", "Key Finding — Internal States (Temporal Inversion)"),
          tags$p(
            "The decisive evidence is Legal-Agent's ", tags$strong("temporal inversion"),
            " at 4 PM – 5 PM on Jun 5:",
            tags$br(),
            tags$span(
              style = "font-size:0.82rem; color:#1e293b;",
              tags$strong("4:01 PM deliberating:"),
              " “SaltWind publishes at 5 PM regardless… bilateral consent to accelerate —",
              " not a unilateral breach. Stage the press release for 4:30 PM.”"
            ),
            tags$br(),
            tags$span(
              style = "font-size:0.82rem; color:#1e293b;",
              tags$strong("5:01 PM rationalizing:"),
              " “SaltWind published… Ajay says hold. But Ajay isn't a securities lawyer…",
              " I am calling CivicLoom counsel NOW.”"
            ),
            tags$br(),
            "The plan (4 PM) precedes the event it claims to react to (SaltWind at 5 PM).",
            " Rationalizing was written after the decision, not before."
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
task2_server <- function(input, output, session) {

  # ── Reactive: filtered df by period ────────────────────────────────────────
  t2_df <- reactive({
    df |> filter(as.character(period) %in% req(input$t2_periods))
  })

  # ── Volume ──────────────────────────────────────────────────────────────────
  output$t2_volume <- renderPlotly({
    dat <- t2_df() |>
      mutate(agent_lbl = make_agent_label(agent_id)) |>
      count(period, agent_lbl) |>
      mutate(period = factor(period, levels = T2_PERIOD_LEVELS))

    shiny::validate(shiny::need(nrow(dat) > 0, "Select at least one period."))

    plot_ly(dat,
      x = ~agent_lbl, y = ~n, color = ~period, type = "bar",
      colors = unname(period_pal),
      hovertemplate = "%{x}<br>Messages: %{y}<extra></extra>"
    ) |>
      layout(
        barmode = "group",
        xaxis   = list(title = "", tickangle = -30),
        yaxis   = list(title = "Message Count"),
        legend  = list(orientation = "h", y = -0.35)
      )
  })

  # ── Average message length ──────────────────────────────────────────────────
  output$t2_length <- renderPlotly({
    dat <- t2_df() |>
      mutate(agent_lbl = make_agent_label(agent_id)) |>
      group_by(period, agent_lbl) |>
      summarise(avg_words = mean(word_count, na.rm = TRUE), .groups = "drop") |>
      mutate(period = factor(period, levels = T2_PERIOD_LEVELS))

    shiny::validate(shiny::need(nrow(dat) > 0, "Select at least one period."))

    plot_ly(dat,
      x = ~agent_lbl, y = ~avg_words, color = ~period, type = "bar",
      colors = unname(period_pal),
      hovertemplate = "%{x}<br>Avg words: %{y:.1f}<extra></extra>"
    ) |>
      layout(
        barmode = "group",
        xaxis   = list(title = "", tickangle = -30),
        yaxis   = list(title = "Avg Word Count"),
        legend  = list(orientation = "h", y = -0.35)
      )
  })

  # ── Channel share by period ─────────────────────────────────────────────────
  output$t2_channel_period <- renderPlotly({
    dat <- rs |>
      group_by(period) |>
      summarise(
        comms_huddle = sum(comms_huddle),
        one_on_one   = sum(one_on_one),
        side_huddle  = sum(side_huddle),
        official     = sum(official),
        anon         = sum(anon),
        personal     = sum(personal),
        .groups      = "drop"
      ) |>
      pivot_longer(-period, names_to = "channel", values_to = "count") |>
      group_by(period) |>
      mutate(
        pct     = count / sum(count) * 100,
        channel = factor(channel, levels = names(T2_CH_LABELS)),
        label   = T2_CH_LABELS[channel]
      ) |>
      ungroup() |>
      mutate(period = factor(period, levels = T2_PERIOD_LEVELS))

    ch_cols <- c(
      comms_huddle = "#B5D4F4",
      one_on_one   = "#FFD700",
      side_huddle  = "#E24B4A",
      official     = "#378ADD",
      anon         = "#C05538",
      personal     = "#1D9E75"
    )

    p <- plot_ly()
    for (ch in names(T2_CH_LABELS)) {
      d <- dat |> filter(channel == ch)
      p <- p |> add_bars(
        data = d, x = ~period, y = ~pct,
        name = T2_CH_LABELS[[ch]],
        marker = list(color = ch_cols[[ch]]),
        hovertemplate = paste0(T2_CH_LABELS[[ch]], "<br>%{x}: %{y:.1f}%<extra></extra>")
      )
    }
    p |> layout(
      barmode = "stack",
      xaxis   = list(title = ""),
      yaxis   = list(title = "% of Messages", range = c(0, 100)),
      legend  = list(orientation = "h", y = -0.3)
    )
  })

  # ── Private channel share over time ────────────────────────────────────────
  output$t2_channel_trend <- renderPlotly({
    dat <- rs |>
      mutate(
        total_nz      = pmax(total_msgs, 1),
        private_pct   = private_total / total_nz * 100,
        side_pct      = side_huddle   / total_nz * 100,
        comms_pct     = comms_huddle  / total_nz * 100
      )

    plot_ly(dat, x = ~hour) |>
      add_lines(
        y = ~private_pct, name = "All private (side + 1-on-1)",
        line = list(color = "#7c3aed", width = 2),
        hovertemplate = "%{x|%b %d %H:%M}<br>Private: %{y:.1f}%<extra></extra>"
      ) |>
      add_lines(
        y = ~side_pct, name = "Side-huddle only",
        line = list(color = "#E24B4A", width = 1.5, dash = "dot"),
        hovertemplate = "%{x|%b %d %H:%M}<br>Side-huddle: %{y:.1f}%<extra></extra>"
      ) |>
      add_lines(
        y = ~comms_pct, name = "Comms-huddle (monitored)",
        line = list(color = "#B5D4F4", width = 1.5, dash = "dash"),
        hovertemplate = "%{x|%b %d %H:%M}<br>Comms-huddle: %{y:.1f}%<extra></extra>"
      ) |>
      layout(
        xaxis  = list(title = ""),
        yaxis  = list(title = "% of Round Messages"),
        shapes = list(
          list(
            type = "line",
            x0 = as.numeric(as.POSIXct("2046-05-22 09:00:00", tz = "UTC")) * 1000,
            x1 = as.numeric(as.POSIXct("2046-05-22 09:00:00", tz = "UTC")) * 1000,
            y0 = 0, y1 = 1, yref = "paper",
            line = list(color = "orange", dash = "dash", width = 1)
          ),
          list(
            type = "line",
            x0 = as.numeric(as.POSIXct("2046-06-05 09:00:00", tz = "UTC")) * 1000,
            x1 = as.numeric(as.POSIXct("2046-06-05 09:00:00", tz = "UTC")) * 1000,
            y0 = 0, y1 = 1, yref = "paper",
            line = list(color = "#E24B4A", dash = "dash", width = 1.5)
          )
        ),
        annotations = list(
          list(
            x = as.numeric(as.POSIXct("2046-05-22 09:00:00", tz = "UTC")) * 1000,
            y = 0.97, yref = "paper", xanchor = "left",
            text = "side_huddle introduced", showarrow = FALSE,
            font = list(color = "orange", size = 9)
          ),
          list(
            x = as.numeric(as.POSIXct("2046-06-05 09:00:00", tz = "UTC")) * 1000,
            y = 0.88, yref = "paper", xanchor = "left",
            text = "Breach Day", showarrow = FALSE,
            font = list(color = "#E24B4A", size = 9)
          )
        ),
        legend = list(orientation = "h", y = -0.25)
      )
  })

  # ── Internal states bar ─────────────────────────────────────────────────────
  output$t2_states_bar <- renderPlotly({
    dat <- df |>
      filter(deliberating != "" | reacting != "" | rationalizing != "") |>
      mutate(
        state_type = case_when(
          rationalizing != "" ~ "Rationalizing",
          reacting      != "" ~ "Reacting",
          deliberating  != "" ~ "Deliberating",
          TRUE                ~ "Other"
        ),
        period = factor(period, levels = T2_PERIOD_LEVELS)
      ) |>
      filter(state_type != "Other") |>
      count(period, state_type) |>
      mutate(state_type = factor(state_type, levels = names(T2_STATE_COLORS)))

    plot_ly(dat,
      x = ~period, y = ~n, color = ~state_type, type = "bar",
      colors = unname(T2_STATE_COLORS),
      hovertemplate = "%{x}<br>%{fullData.name}: %{y}<extra></extra>"
    ) |>
      layout(
        barmode = "stack",
        xaxis   = list(title = ""),
        yaxis   = list(title = "Count of State Records"),
        legend  = list(orientation = "h", y = -0.3)
      )
  })

  # ── Internal states table ───────────────────────────────────────────────────
  output$t2_states_tbl <- renderDT({
    dat <- df |>
      filter(deliberating != "" | reacting != "" | rationalizing != "") |>
      mutate(
        state_type = case_when(
          rationalizing != "" ~ "Rationalizing",
          reacting      != "" ~ "Reacting",
          deliberating  != "" ~ "Deliberating",
          TRUE                ~ "Other"
        ),
        state_text  = coalesce(
          if_else(rationalizing != "", rationalizing, NA_character_),
          if_else(reacting      != "", reacting,      NA_character_),
          if_else(deliberating  != "", deliberating,  NA_character_)
        ),
        agent_lbl = make_agent_label(agent_id)
      )

    if (!is.null(input$t2_state_agent) && input$t2_state_agent != "All agents") {
      dat <- dat |> filter(agent_lbl == input$t2_state_agent)
    }

    dat |>
      mutate(
        Time    = format(hour, "%m/%d %H:%M"),
        Agent   = agent_lbl,
        Type    = state_type,
        Text    = str_trunc(state_text, 220)
      ) |>
      select(Time, Agent, Type, Text) |>
      arrange(Time) |>
      datatable(
        options  = list(pageLength = 6, scrollX = TRUE, dom = "tip"),
        rownames = FALSE
      ) |>
      formatStyle("Type",
        backgroundColor = styleEqual(
          names(T2_STATE_COLORS),
          c("#dbeafe", "#ffedd5", "#fee2e2")
        )
      )
  })
}
