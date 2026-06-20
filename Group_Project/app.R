# ─────────────────────────────────────────────────────────────────────────────
# Group Project — VAST Challenge 2026 MC1
# Combined Shiny App: sources Task 1 + Task 2 (+ Task 3 when ready)
# ─────────────────────────────────────────────────────────────────────────────

# Prevent Task 1 from auto-launching when sourced
COMBINED_APP <- TRUE

# ── Source task files ─────────────────────────────────────────────────────────
# chdir = TRUE so Task 1's relative data path resolves from its own folder
source("Task 1 Shiny App/app.R", chdir = TRUE)
# Exports: mc1_title, mc1_theme, mc1_navbar_opts, mc1_css,
#          task1_panel_explore, task1_panel_task, task1_server
# Globals: df, rs, edges_raw, net_pre, net_crisis,
#          period_pal, ch_colours, agent_pal

source("Task 2 Shiny App/app.R", chdir = TRUE)
# Exports: task2_ui, task2_server

source("Task 3 Shiny App/app.R", chdir = TRUE)
# Exports: task3_ui, task3_server

# ── Combined UI ───────────────────────────────────────────────────────────────
ui <- page_navbar(
  title          = mc1_title,
  theme          = mc1_theme,
  header         = tags$head(tags$style(HTML(mc1_css))),
  navbar_options = mc1_navbar_opts,

  # Task 1: Data Exploration + Event Sequence / Causal Chain
  task1_panel_explore,
  task1_panel_task,

  # Task 2: Behavioral Analysis
  task2_ui,

  # Task 3: Leading Indicators
  task3_ui,

  nav_spacer(),
  nav_item(
    tags$span(
      style = "color:rgba(255,255,255,0.45); font-size:0.76rem; font-weight:500;",
      "VAST Challenge 2026 MC1", tags$span(style = "margin:0 6px;", "·"), "ISSS608"
    )
  )
)

# ── Combined Server ───────────────────────────────────────────────────────────
server <- function(input, output, session) {
  task1_server(input, output, session)
  task2_server(input, output, session)
  task3_server(input, output, session)
}

shinyApp(ui = ui, server = server)
