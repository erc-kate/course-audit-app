# ============================================================
# ui.R — User Interface
# Defines layout, tabs, filters, and table structure.
# Nothing here touches data or logic — that's server.R.
# ============================================================

ui <- page_fluid(

  theme = bs_theme(
    bootswatch = NULL,
    bg         = "white",
    fg         = "#222222"
  ),

  tags$style(HTML("
    .card { border: none !important; box-shadow: none !important; }

    /* ── Pill tab buttons ── */
    .pill-tabs { display: flex; gap: 8px; margin: 12px 0 0 0; flex-wrap: wrap; }
    .pill-tab {
      padding: 6px 18px;
      border-radius: 999px;
      border: 1.5px solid #d1d1d1;
      background: white;
      color: #500000;
      font-size: 0.88em;
      font-weight: 600;
      cursor: pointer;
      transition: background 0.15s;
    }
    .pill-tab:hover { background: #f0f0f0; color: #500000; }
    .pill-tab.active { background: #f6f6f6; color: #500000; border-color: #d1d1d1; }

    /* ── Tab panels ── */
    .tab-panel { display: none; }
    .tab-panel.active { display: block; }

    /* ── Page description — plain white, normal text ── */
    .page-description {
      background: white;
      color: #444;
      font-weight: 400;
      font-size: 0.95em;
      line-height: 1.6;
      margin-bottom: 16px;
      text-align: left;
    }

    /* ── Table wrapper ── */
    .dataTables_wrapper {
      border: 1px solid #e0e0e0;
      border-radius: 6px;
      padding: 0 0 8px 0;
      overflow: hidden;
    }
    table.dataTable {
      font-size: 0.92em;
      width: 100% !important;
      border: none !important;
      margin: 0 !important;
    }

    /* ── Table column headers: #f5f1f0 bg, black bold, left-aligned ── */
    table.dataTable thead th {
      font-weight: 700;
      font-size: 0.9em;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      color: #111;
      border-bottom: 2px solid #e0e0e0 !important;
      padding: 12px 10px !important;
      background-color: #f5f1f0 !important;
      text-align: left !important;
    }

    /* ── Sorted column header: maroon bg, white bold text, white arrow ── */
    table.dataTable thead th.sorting_asc,
    table.dataTable thead th.sorting_desc {
      background-color: #500000 !important;
      color: white !important;
      font-weight: 700 !important;
    }
    table.dataTable thead th.sorting_asc::after,
    table.dataTable thead th.sorting_asc::before,
    table.dataTable thead th.sorting_desc::after,
    table.dataTable thead th.sorting_desc::before {
      filter: brightness(0) invert(1) !important;
    }

    /* ── Alternating row colors ── */
    table.dataTable tbody tr:nth-child(odd) td  { background-color: white   !important; }
    table.dataTable tbody tr:nth-child(even) td { background-color: #f6f6f6 !important; }

    /* ── Table data cells: left-aligned ── */
    table.dataTable tbody td {
      padding: 9px 10px !important;
      vertical-align: middle;
      border-top: 1px solid #f0f0f0 !important;
      text-align: left !important;
    }

    /* ── Row hover (overrides alternating) ── */
    table.dataTable tbody tr:hover td {
      background-color: #EBEBEB !important;
      cursor: pointer;
    }

    /* ── Open (expanded) row: maroon bold text, grey bg (overrides alternating) ── */
    table.dataTable tbody tr.row-open td {
      background-color: #EBEBEB !important;
      font-weight: 700 !important;
      color: #500000 !important;
    }

    /* ── Child (detail) row background ── */
    table.dataTable tbody tr.child td,
    table.dataTable tbody tr.child {
      background-color: #fcf8f7 !important;
    }

    /* ── Expand toggle arrow: aggie maroon ── */
    .toggle-col {
      color: #500000;
      font-size: 0.8em;
      transition: transform 0.2s ease;
      display: inline-block;
      user-select: none;
    }
    .row-open .toggle-col { transform: rotate(90deg); }

    .dataTables_paginate { padding: 8px 12px 0 12px !important; }
    .dataTables_info { display: none !important; }

    /* ── Filter sidebar labels ── */
    .filter-label {
      font-size: 0.82em;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      color: #666;
      margin-bottom: 3px;
    }
    .course-total {
      font-size: 0.82em;
      color: #999;
      margin-top: 8px;
      text-align: right;
    }
    .filter-hint {
      font-size: 0.78em;
      color: #999;
      margin-top: 16px;
      line-height: 1.4;
    }

    @media (max-width: 768px) {
      .layout-col-filters { width: 100% !important; margin-bottom: 20px; }
      .layout-col-table   { width: 100% !important; }
    }
  ")),

  # Page header — white background, aggie maroon title
  div(style = "padding: 24px 8px 0 8px; background: white;",
      h2("Graduate Course Audit",
         style = "margin-bottom: 8px; color: #500000;"),
      div(class = "pill-tabs",
          tags$button(
            id = "tab-btn-courses", class = "pill-tab active",
            onclick = "switchTab('courses')", "Course Browser"
          ),
          tags$button(
            id = "tab-btn-themes", class = "pill-tab",
            onclick = "switchTab('themes')", "Themes & Definitions"
          )
      )
  ),

  hr(style = "margin: 16px 0 20px 0;"),

  # ── TAB 1: Course Browser ──────────────────────────────
  div(id = "panel-courses", class = "tab-panel active",

      div(style = "padding: 0 8px 20px 8px;",
          p(class = "page-description",
            "Browse and filter graduate courses relevant to education policy
         research and careers. Click any row to expand full course details
         including description and prerequisites.")
      ),

      layout_columns(
        col_widths = c(3, 9),

        # LEFT: Filters
        div(class = "layout-col-filters",
            div(class = "filter-label", "Search"),
            textInput("search", label = NULL, value = ""),

            div(class = "filter-label", style = "margin-top: 14px;", "Department"),
            selectInput("dept_filter", label = NULL,
                        choices  = c("All", sort(unique(courses$department))),
                        selected = "All"),

            div(class = "filter-label", "Theme"),
            selectInput("theme_filter", label = NULL,
                        choices  = c("All", sort(unique(courses$theme))),
                        selected = "All"),

            div(class = "filter-label", "Subtheme"),
            selectInput("subtheme_filter", label = NULL,
                        choices  = c("All", sort(unique(courses$subtheme))),
                        selected = "All"),

            actionButton("reset", "Reset Filters",
                         class = "btn btn-outline-secondary btn-sm w-100",
                         style = "margin-top: 16px;"),

            div(class = "filter-hint",
                "💡 Click any course row to expand its full details.")
        ),

        # RIGHT: Table
        div(class = "layout-col-table",
            DTOutput("course_table"),
            div(class = "course-total", textOutput("total_count"))
        )
      )
  ),

  # ── TAB 2: Themes & Definitions ───────────────────────
  # max-width + margin auto = centered block, text still left-aligned inside
  div(id = "panel-themes", class = "tab-panel",
      div(style = "max-width: 860px; padding: 0 8px; margin: 0 auto;",
          p(class = "page-description",
            style = "margin-bottom: 28px;",
            "Each course in the audit is aligned to a theme and subtheme
         indicating the primary competencies it supports. Use these
         definitions to better understand how courses were categorized."
          ),
          build_themes_tab(themes, subthemes)
      )
  ),

  # JavaScript for tab switching
  tags$script(HTML("
    function switchTab(tab) {
      document.getElementById('panel-courses').classList.remove('active');
      document.getElementById('panel-themes').classList.remove('active');
      document.getElementById('tab-btn-courses').classList.remove('active');
      document.getElementById('tab-btn-themes').classList.remove('active');
      document.getElementById('panel-' + tab).classList.add('active');
      document.getElementById('tab-btn-' + tab).classList.add('active');
    }
  "))
)
