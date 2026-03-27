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

    .pill-tabs { display: flex; gap: 8px; margin: 12px 0 0 0; flex-wrap: wrap; }
    .pill-tab {
      padding: 6px 18px;
      border-radius: 999px;
      border: 1.5px solid #0054a4;
      background: white;
      color: #0054a4;
      font-size: 0.88em;
      font-weight: 600;
      cursor: pointer;
      transition: background 0.15s, color 0.15s;
    }
    .pill-tab:hover { background: #e8f0fe; color: #0054a4; }
    .pill-tab.active { background: #0054a4; color: white; }

    .tab-panel { display: none; }
    .tab-panel.active { display: block; }

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
    table.dataTable thead th {
      font-weight: 600;
      font-size: 0.85em;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      color: #555;
      border-bottom: 2px solid #e0e0e0 !important;
      padding: 12px 10px !important;
      background-color: #fafafa !important;
    }
    table.dataTable tbody td {
      padding: 9px 10px !important;
      vertical-align: middle;
      border-top: 1px solid #f0f0f0 !important;
    }
    table.dataTable tbody tr:hover {
      background-color: #f5f8ff !important;
      cursor: pointer;
    }
    .dataTables_paginate { padding: 8px 12px 0 12px !important; }
    .dataTables_info { display: none !important; }

    .toggle-col {
      color: #0054a4;
      font-size: 0.8em;
      transition: transform 0.2s ease;
      display: inline-block;
      user-select: none;
    }
    .row-open .toggle-col { transform: rotate(90deg); }

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
    .page-description {
      color: #555;
      font-size: 0.95em;
      line-height: 1.6;
      margin-bottom: 0;
    }
    @media (max-width: 768px) {
      .layout-col-filters { width: 100% !important; margin-bottom: 20px; }
      .layout-col-table   { width: 100% !important; }
    }
  ")),
  
  # Page header
  div(style = "padding: 24px 8px 0 8px;",
      h2("Graduate Course Audit", style = "margin-bottom: 8px;"),
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
  div(id = "panel-themes", class = "tab-panel",
      div(style = "max-width: 860px; padding: 0 8px;",
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