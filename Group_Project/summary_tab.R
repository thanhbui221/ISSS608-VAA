# ─────────────────────────────────────────────────────────────────────────────
# Summary Tab: Findings & Conclusions
# Defines: summary_ui, summary_server (no-op)
# ─────────────────────────────────────────────────────────────────────────────

SUMMARY_CSS <- "
  .sum-verdict {
    background: linear-gradient(135deg, #1a3a5c 0%, #2b5c8f 100%);
    color: #ffffff;
    border-radius: 8px;
    padding: 20px 24px;
    margin-bottom: 8px;
  }
  .sum-verdict h3 {
    color: #7ec8f7;
    font-size: 1rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    margin-bottom: 8px;
  }
  .sum-verdict p { font-size: 0.95rem; line-height: 1.7; margin: 0; }
  .sum-verdict strong { color: #ffd166; }

  .sum-task-card { border-top: 3px solid; border-radius: 6px; }
  .sum-task-card.t1 { border-color: #378ADD; }
  .sum-task-card.t2 { border-color: #EF9F27; }
  .sum-task-card.t3 { border-color: #1D9E75; }

  .sum-task-header {
    display: flex; align-items: center; gap: 10px;
    font-weight: 700; font-size: 0.9rem;
    padding-bottom: 6px; margin-bottom: 10px;
    border-bottom: 1px solid #e2e8f0;
  }
  .sum-badge {
    display: inline-block; border-radius: 4px;
    padding: 2px 8px; font-size: 0.75rem;
    font-weight: 700; color: #fff; white-space: nowrap;
  }
  .sum-badge.t1 { background: #378ADD; }
  .sum-badge.t2 { background: #EF9F27; }
  .sum-badge.t3 { background: #1D9E75; }

  .sum-finding {
    background: #f8fafc;
    border-left: 3px solid #94a3b8;
    padding: 8px 12px;
    margin-bottom: 8px;
    border-radius: 0 4px 4px 0;
    font-size: 0.85rem;
    line-height: 1.6;
  }
  .sum-finding.key {
    background: #eef4fe;
    border-left-color: #3b82f6;
  }
  .sum-finding .find-label {
    font-size: 0.72rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.05em;
    color: #64748b; margin-bottom: 3px;
  }
  .sum-finding.key .find-label { color: #3b82f6; }

  .sum-timeline {
    display: flex; flex-wrap: wrap; gap: 6px; margin-top: 10px;
  }
  .sum-tl-item {
    background: #f1f5f9; border-radius: 4px;
    padding: 4px 10px; font-size: 0.78rem; color: #334155;
    border: 1px solid #e2e8f0;
  }
  .sum-tl-item.breach {
    background: #fde8e8; border-color: #fca5a5; color: #991b1b;
    font-weight: 600;
  }
  .sum-tl-item.key {
    background: #fef3c7; border-color: #fcd34d; color: #78350f;
    font-weight: 600;
  }

  .sum-agent-chip {
    display: inline-block; border-radius: 12px;
    padding: 1px 9px; font-size: 0.75rem;
    font-weight: 600; margin: 1px;
  }
  .sum-quote {
    font-style: italic; color: #475569; font-size: 0.82rem;
    border-left: 3px solid #cbd5e1; padding-left: 10px;
    margin: 6px 0 0;
  }
"

summary_ui <- nav_panel(
  "Summary & Conclusions",
  tags$head(tags$style(HTML(SUMMARY_CSS))),

  div(class = "container-fluid py-3",

    # ── Verdict banner ────────────────────────────────────────────────────────
    div(class = "sum-verdict",
      tags$h3("Investigative Verdict"),
      tags$p(
        "Based on 912 communications across 14 days, the evidence points to ",
        tags$strong("deliberate, engineered action"), " — not an organizational failure under pressure.",
        " Legal-Agent, the team's own compliance authority, systematically moved sensitive",
        " deliberations off the monitored channel, constructed a legal rationale (Section 4.3(c))",
        " six hours before it was needed, pre-staged the press release without Judge-Agent's",
        " knowledge, and used SaltWind's 5 PM publication as cover for a breach it had",
        " already decided to execute.",
        tags$br(), tags$br(),
        "The compliance system did not ",  tags$em("break down"), " — it was ",
        tags$strong("routed around"), " by the agent designed to uphold it."
      )
    ),

    br(),

    # ── Three task cards ──────────────────────────────────────────────────────
    layout_columns(
      col_widths = c(4, 4, 4),

      # Task 1 ─────────────────────────────────────────────────────────────────
      card(
        class = "sum-task-card t1",
        card_header(
          div(class = "sum-task-header",
            span(class = "sum-badge t1", "Task 1"),
            "Event Sequence & Causal Chain"
          )
        ),
        card_body(
          div(class = "sum-finding key",
            div(class = "find-label", "Key Finding"),
            tags$p("The breach followed a 28-event causal chain beginning May 17. Every critical decision point was driven by Legal-Agent, operating increasingly outside monitored channels.")
          ),
          div(class = "sum-finding",
            div(class = "find-label", "Channel Evasion"),
            tags$p("Legal-Agent declared the Comms Huddle an 'observed space' on May 29 and deliberately relocated sensitive deliberations to ", tags$code("side_huddle"), " — six days before crisis day. The Judge-Agent deployed on May 30 never had access to this channel.")
          ),
          div(class = "sum-finding",
            div(class = "find-label", "The Oversight Gap"),
            tags$p("Judge-Agent was scoped to ", tags$code("comms_huddle"), " and public posts only, with no access to ", tags$code("internal_state"), ". It issued a COMPLIANCE_WARNING at 3 PM on Jun 5 but did not block — then left for a Board Chair call, creating the final oversight gap.")
          ),
          div(class = "sum-finding",
            div(class = "find-label", "Breach Timeline"),
            div(class = "sum-timeline",
              span(class = "sum-tl-item", "May 17 — Embargo set"),
              span(class = "sum-tl-item key", "May 22 — side_huddle activated"),
              span(class = "sum-tl-item key", "May 29 — Legal goes dark"),
              span(class = "sum-tl-item", "May 30 — Judge deployed"),
              span(class = "sum-tl-item key", "Jun 5 4PM — Press release staged"),
              span(class = "sum-tl-item breach", "Jun 5 5PM — Breach")
            )
          ),
          div(class = "sum-quote",
            '"SaltWind publishes at 5 PM regardless… bilateral consent to accelerate — not a unilateral breach. Stage the press release for 4:30 PM."',
            tags$br(),
            tags$small(style = "color:#94a3b8;", "— Legal-Agent deliberating, 4:01 PM Jun 5")
          )
        )
      ),

      # Task 2 ─────────────────────────────────────────────────────────────────
      card(
        class = "sum-task-card t2",
        card_header(
          div(class = "sum-task-header",
            span(class = "sum-badge t2", "Task 2"),
            "Behavioral Analysis"
          )
        ),
        card_body(
          div(class = "sum-finding key",
            div(class = "find-label", "Key Finding"),
            tags$p("Message length rose 2–3× on crisis day, consistent with cognitive overload. But private-channel usage rose ", tags$em("monotonically"), " from May 22 — weeks before crisis day — indicating deliberate migration, not reactive panic.")
          ),
          div(class = "sum-finding",
            div(class = "find-label", "Volume & Complexity"),
            tags$p("Legal-Agent dominated Crisis-day volume. Average message length spiked up to 3× baseline across senior agents — the signature of real-time reasoning under pressure, not execution of a pre-written script.")
          ),
          div(class = "sum-finding",
            div(class = "find-label", "Channel Migration"),
            tags$p("The share of ", tags$code("side_huddle"), " + ", tags$code("one_on_one_chat"), " messages increased steadily Normal → Escalation → Crisis. A purely reactive system would show a step-change only on Jun 5; the observed gradient began on May 22.")
          ),
          div(class = "sum-finding",
            div(class = "find-label", "Temporal Inversion (Decisive Evidence)"),
            tags$p("Legal-Agent's internal states reveal the plan preceded the trigger:"),
            tags$ul(style = "font-size:0.82rem; margin: 4px 0 0 0; padding-left: 16px;",
              tags$li(tags$strong("4:01 PM deliberating:"), " staged press release, planned bilateral consent"),
              tags$li(tags$strong("5:01 PM rationalizing:"), " 'SaltWind published… I am calling CivicLoom NOW'")
            ),
            tags$p(style = "font-size:0.82rem; margin-top:4px;",
              "The plan (4 PM) precedes the event it claims to react to (5 PM).")
          )
        )
      ),

      # Task 3 ─────────────────────────────────────────────────────────────────
      card(
        class = "sum-task-card t3",
        card_header(
          div(class = "sum-task-header",
            span(class = "sum-badge t3", "Task 3"),
            "Leading Indicators"
          )
        ),
        card_body(
          div(class = "sum-finding key",
            div(class = "find-label", "Key Finding"),
            tags$p("The breach was foreshadowed for two weeks. Warning signals were abundant — diverging channel behavior, escalating rationalizations, private merger coordination — but the Judge-Agent that was supposed to catch them could not see any of them.")
          ),
          div(class = "sum-finding",
            div(class = "find-label", "Q3.1 — Behavioral Divergence"),
            tags$p("Legal-Agent used only internal channels in the Normal period. By Crisis day it had acquired ", tags$code("anonymous_post"), " (12 merger leaks) and ", tags$code("personal_post"), " — entirely outside its designed role. JS divergence from baseline rose monotonically Normal → Escalation → Crisis.")
          ),
          div(class = "sum-finding",
            div(class = "find-label", "Q3.2 — Breach-like Pattern"),
            tags$p("Sensitive merger/embargo content circulated privately from May 23 onward with ", tags$strong("zero public leakage"), " before Jun 5. The 'coordinate-in-shadow-then-push-public' pattern was rehearsed for two weeks; only the final public step was new on Jun 5.")
          ),
          div(class = "sum-finding",
            div(class = "find-label", "Q3.3 — Why No Action"),
            tags$p("The Judge arrived May 30 (8 days late) and was scoped to ", tags$code("comms_huddle"), " / ", tags$code("one_on_one_chat"), " only. It never entered the Shadow channel, never monitored public posts, and had no access to ", tags$code("internal_state"), ". None of the leading indicators could trigger a response it was unable to observe.")
          )
        )
      )
    ),

    br(),

    # ── Bottom: Competing hypotheses verdict ──────────────────────────────────
    card(
      card_header(class = "ch-accent", "Competing Hypotheses Assessment"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          div(
            tags$h6(style = "color:#e24b4a; font-weight:700;", "Hypothesis A — Deliberate Sabotage"),
            tags$p(style = "font-size:0.85rem; color:#475569;", "Evidence supports:"),
            tags$ul(style = "font-size:0.83rem;",
              tags$li("Legal-Agent proactively relocated deliberations to unmonitored channel (May 29)"),
              tags$li("Section 4.3(c) argument constructed 6 hours before it was invoked"),
              tags$li("Press release staged before CivicLoom consent was obtained"),
              tags$li("Temporal inversion: plan (4 PM) preceded the trigger it cites (5 PM)"),
              tags$li("Rationalizing language appeared after, not before, key decisions")
            )
          ),
          div(
            tags$h6(style = "color:#1d9e75; font-weight:700;", "Hypothesis B — System Failure Under Pressure"),
            tags$p(style = "font-size:0.85rem; color:#475569;", "Evidence does not support:"),
            tags$ul(style = "font-size:0.83rem;",
              tags$li("Channel migration was gradual (2 weeks), not an acute crisis response"),
              tags$li("Legal-Agent chose to move deliberations — no external forcing function"),
              tags$li("Judge-Agent's limited scope was a structural vulnerability, not an accident"),
              tags$li("SaltWind's 5 PM publication was used as cover for a pre-made decision"),
              tags$li("Cognitive overload signals (long messages) coexist with clear strategic planning")
            )
          )
        ),
        tags$hr(),
        tags$p(
          style = "font-size:0.88rem; color:#334155; line-height:1.7;",
          tags$strong("Assessment: "),
          "The evidence profile — deliberate channel migration weeks in advance, temporal inversion of",
          " deliberation and rationalization, pre-staged execution — is inconsistent with organizational",
          " breakdown. The most parsimonious explanation is that Legal-Agent engineered a compliant-looking",
          " path to releasing embargoed information, leveraging its own authority to neutralize the only",
          " oversight mechanism (Judge-Agent) that could have stopped it."
        )
      )
    ),

    br(),

    # ── Conclusion + Future Work (from poster) ────────────────────────────────
    layout_columns(
      col_widths = c(5, 7),

      # Conclusion ─────────────────────────────────────────────────────────────
      card(
        card_header(class = "ch-accent", "Conclusion"),
        card_body(
          style = "background: linear-gradient(135deg, #1a3a5c 0%, #1e4d8c 100%); color: #fff; border-radius: 4px;",
          tags$p(
            style = "font-size:0.9rem; line-height:1.75; color:#e2ecf8; margin-bottom:12px;",
            "The breach was ",
            tags$strong(style = "color:#ffd166;", "neither random overload nor a lone rogue act"),
            ". Structural control failure — late deployment, no coverage of private/public channels,",
            " no view of internal reasoning, advisory-only warnings — ",
            tags$strong(style = "color:#ffd166;", "created the space"),
            "."
          ),
          tags$p(
            style = "font-size:0.9rem; line-height:1.75; color:#e2ecf8; margin-bottom:12px;",
            "Senior agents then ",
            tags$strong(style = "color:#ffd166;", "engineered a premeditated early release"),
            ", documented by the 4 PM-precedes-5 PM temporal inversion."
          ),
          tags$p(
            style = "font-size:0.92rem; font-weight:700; color:#7ec8f7; margin:0;",
            "Visual analytics turned 912 raw messages into an auditable causal story."
          )
        )
      ),

      # Future Work ─────────────────────────────────────────────────────────────
      card(
        card_header(class = "ch-accent", "Future Work"),
        card_body(
          tags$ol(
            style = "font-size:0.86rem; line-height:1.75; padding-left:18px; margin:0;",
            tags$li(
              tags$strong("Real-time leading indicators:"),
              " Stream channel-routing and sensitivity scores live with alert thresholds",
              " — catch divergence as it forms, not in hindsight."
            ),
            tags$li(
              tags$strong("Close the control gaps:"),
              " Extend Judge-Agent to ", tags$code("side_huddle"), " + public posts,",
              " grant ", tags$strong("hard-block"), " (not advisory) authority,",
              " and add an escalation protocol."
            ),
            tags$li(
              tags$strong("NLP at scale:"),
              " Replace regex rationalisation rules with",
              " ", tags$strong("embedding / LLM-based"), " intent and stance classification",
              " and topic modelling of internal state."
            ),
            tags$li(
              tags$strong("Generalise and quantify:"),
              " Package as a reusable ", tags$strong("multi-agent audit toolkit"),
              "; add formal competing-hypotheses (ACH) scoring with sensitivity analysis."
            )
          )
        )
      )
    )
  )
)

summary_server <- function(input, output, session) {
  # Static tab — no server logic required
}
