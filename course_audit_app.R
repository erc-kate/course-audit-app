# ============================================================
# COURSE AUDIT SHINY APP
# Education Policy — Texas A&M Education Research Center
#
# PURPOSE:
# This app displays the graduate course audit as an interactive,
# searchable, filterable table for embedding on the ERC website.
#
# CREATED: 2025
# MAINTAINED BY: [Your name here]
#
# HOW TO UPDATE THE COURSE LIST:
# 1. Edit the course_audit.csv file in this project folder
# 2. Re-run this app — changes appear automatically
#
# HOW TO UPDATE THEMES & DEFINITIONS:
# 1. Edit theme.csv or subtheme.csv in this project folder
# 2. Re-run this app — changes appear automatically
#
# HOW TO RUN THIS APP:
# Click the "Run App" button at the top of this window in RStudio
#
# FILES NEEDED IN THIS FOLDER:
#   course_audit.csv  — the full course list
#   theme.csv         — the 4 themes and their definitions
#   subtheme.csv      — the 14 subthemes and their definitions
# ============================================================


# ------------------------------------------------------------
# STEP 1: Load packages
# ------------------------------------------------------------

library(shiny)   # builds the web app
library(readr)   # reads our CSV data files
library(dplyr)   # helps us filter and work with data
library(DT)      # makes the interactive table
library(bslib)   # makes things look clean


# ------------------------------------------------------------
# STEP 2: Load the data
# ------------------------------------------------------------

courses   <- read_csv("course_audit.csv")
themes    <- read_csv("theme.csv")
subthemes <- read_csv("subtheme.csv")


# ------------------------------------------------------------
# STEP 3: Quick data check (console only, not visible to users)
# ------------------------------------------------------------

message("Courses loaded: ",   nrow(courses),   " rows")
message("Themes loaded: ",    nrow(themes),    " rows")
message("Subthemes loaded: ", nrow(subthemes), " rows")


# ------------------------------------------------------------
# STEP 4: Build the expandable row detail HTML
# ------------------------------------------------------------

make_detail_html <- function(row) {
  prereq_text <- if (is.na(row$prereq) || row$prereq == "") {
    "None listed"
  } else {
    row$prereq
  }
  
  paste0('
    <div style="
      border-left: 4px solid #0054a4;
      background: #f8f9fa;
      padding: 18px 24px;
      margin: 4px 24px 8px 48px;
      border-radius: 0 4px 4px 0;
      font-size: 0.93em;
    ">
      <div style="font-weight:700; font-size:1.05em; margin-bottom:4px;">',
         row$title,
         '</div>
      <div style="color:#666; margin-bottom:16px;">',
         row$department, ' &nbsp;·&nbsp; ', row$credit_hours, ' credit hours
      </div>

      <div style="
        font-weight:700; font-size:0.75em;
        text-transform:uppercase; letter-spacing:0.06em;
        color:#888; margin-bottom:2px;">Theme</div>
      <div style="margin-bottom:12px;">', row$theme, '</div>

      <div style="
        font-weight:700; font-size:0.75em;
        text-transform:uppercase; letter-spacing:0.06em;
        color:#888; margin-bottom:2px;">Subtheme</div>
      <div style="margin-bottom:12px;">', row$subtheme, '</div>

      <div style="
        font-weight:700; font-size:0.75em;
        text-transform:uppercase; letter-spacing:0.06em;
        color:#888; margin-bottom:2px;">Description</div>
      <div style="margin-bottom:12px; line-height:1.6;">',
         row$course_description,
         '</div>

      <div style="
        font-weight:700; font-size:0.75em;
        text-transform:uppercase; letter-spacing:0.06em;
        color:#888; margin-bottom:2px;">Prerequisites</div>
      <div style="line-height:1.6;">', prereq_text, '</div>
    </div>
  ')
}


# ------------------------------------------------------------
# STEP 5: Build Themes & Definitions tab
#
# LAYOUT per theme:
#   One grey callout box with blue left border containing:
#     - Theme name (bold) — no quote bar on subthemes
#     - Theme definition (normal weight)
#     - Light divider line
#     - Subthemes listed below, indented, bold name,
#       normal definition — NO box, NO border on subthemes
# ------------------------------------------------------------

build_themes_tab <- function(themes_df, subthemes_df) {
  
  theme_blocks <- lapply(1:nrow(themes_df), function(i) {
    
    this_theme     <- themes_df[i, ]
    this_subthemes <- subthemes_df |>
      filter(theme == this_theme$theme)
    
    # Subthemes — indented, bold name, plain definition, no box
    subtheme_entries <- lapply(1:nrow(this_subthemes), function(j) {
      sub <- this_subthemes[j, ]
      div(
        style = "
          padding-left: 16px;
          margin-top: 14px;
        ",
        # Subtheme name — bold
        div(
          style = "
            font-weight: 700;
            font-size: 0.92em;
            color: #222;
            margin-bottom: 3px;
          ",
          sub$subtheme
        ),
        # Subtheme definition — normal, slightly grey
        div(
          style = "
            font-size: 0.87em;
            color: #555;
            line-height: 1.55;
          ",
          sub$subtheme_definition
        )
      )
    })
    
    # One callout box per theme — theme is bold with blue bar,
    # subthemes live inside the same box, just indented below
    div(
      style = "margin-bottom: 20px;",
      div(
        style = "
          background: #f8f9fa;
          border-left: 4px solid #0054a4;
          border-radius: 0 4px 4px 0;
          padding: 18px 22px 20px 22px;
        ",
        
        # Theme name — bold, larger
        div(
          style = "
            font-weight: 700;
            font-size: 1.05em;
            color: #111;
            margin-bottom: 4px;
          ",
          this_theme$theme
        ),
        
        # Theme definition — normal weight, slightly muted
        div(
          style = "
            font-size: 0.9em;
            color: #555;
            line-height: 1.5;
          ",
          this_theme$theme_definition
        ),
        
        # Divider between theme info and subthemes
        hr(style = "margin: 12px 0 4px 0; border-color: #ddd;"),
        
        # Subthemes — all indented inside the same box
        tagList(subtheme_entries)
      )
    )
  })
  
  tagList(theme_blocks)
}


# ------------------------------------------------------------
# STEP 6: The App
# ------------------------------------------------------------

ui <- page_fluid(
  
  theme = bs_theme(
    bootswatch = NULL,
    bg         = "white",
    fg         = "#222222"
  ),
  
  tags$style(HTML("

    .card {
      border: none !important;
      box-shadow: none !important;
    }

    /* Pill tab buttons */
    .pill-tabs {
      display: flex;
      gap: 8px;
      margin: 12px 0 0 0;
      flex-wrap: wrap;
    }
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
    .pill-tab:hover {
      background: #e8f0fe;
      color: #0054a4;
    }
    .pill-tab.active {
      background: #0054a4;
      color: white;
    }

    /* Tab panels */
    .tab-panel { display: none; }
    .tab-panel.active { display: block; }

    /* FIX 2: Table border — clean frame, no shadow */
    .dataTables_wrapper {
      border: 1px solid #e0e0e0;
      border-radius: 6px;
      padding: 0 0 8px 0;
      overflow: hidden;
    }

    /* Table */
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

    /* Pagination sits inside the border */
    .dataTables_paginate {
      padding: 8px 12px 0 12px !important;
    }

    /* Toggle arrow */
    .toggle-col {
      color: #0054a4;
      font-size: 0.8em;
      transition: transform 0.2s ease;
      display: inline-block;
      user-select: none;
    }
    .row-open .toggle-col {
      transform: rotate(90deg);
    }

    /* Filter labels */
    .filter-label {
      font-size: 0.82em;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      color: #666;
      margin-bottom: 3px;
    }

    /* Total count below table */
    .course-total {
      font-size: 0.82em;
      color: #999;
      margin-top: 8px;
      text-align: right;
      padding-right: 2px;
    }

    .dataTables_info { display: none !important; }

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
                        choices = c("All", sort(unique(courses$department))),
                        selected = "All"),
            
            div(class = "filter-label", "Theme"),
            selectInput("theme_filter", label = NULL,
                        choices = c("All", sort(unique(courses$theme))),
                        selected = "All"),
            
            div(class = "filter-label", "Subtheme"),
            selectInput("subtheme_filter", label = NULL,
                        choices = c("All", sort(unique(courses$subtheme))),
                        selected = "All"),
            
            actionButton("reset", "Reset Filters",
                         class = "btn btn-outline-secondary btn-sm w-100",
                         style = "margin-top: 16px;"),
            
            div(class = "filter-hint",
                "💡 Click any course row to expand its full details.")
        ),
        
        # RIGHT: Table + total count
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
  
  # JavaScript tab switching
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


# ------------------------------------------------------------
# STEP 7: Server logic
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  filtered_courses <- reactive({
    df <- courses
    
    if (input$search != "") {
      search_term <- tolower(input$search)
      df <- df |> filter(
        grepl(search_term, tolower(title)) |
          grepl(search_term, tolower(course_description)) |
          grepl(search_term, tolower(paste(course_name, course_num)))
      )
    }
    
    if (input$dept_filter != "All") df <- df |> filter(department == input$dept_filter)
    if (input$theme_filter != "All") df <- df |> filter(theme == input$theme_filter)
    if (input$subtheme_filter != "All") df <- df |> filter(subtheme == input$subtheme_filter)
    
    df
  })
  
  observeEvent(input$reset, {
    updateTextInput(session, "search", value = "")
    updateSelectInput(session, "dept_filter", selected = "All")
    updateSelectInput(session, "theme_filter", selected = "All")
    updateSelectInput(session, "subtheme_filter", selected = "All")
  })
  
  output$total_count <- renderText({
    paste0(nrow(courses), " total courses in this audit")
  })
  
  output$course_table <- renderDT({
    
    df <- filtered_courses()
    
    detail_html <- sapply(1:nrow(df), function(i) make_detail_html(df[i, ]))
    
    display_df <- df |>
      mutate(
        course_number = paste(course_name, course_num),
        toggle        = '<span class="toggle-col">&#9654;</span>',
        detail        = detail_html
      ) |>
      select(
        detail,
        "  "         = toggle,
        "Course"     = course_number,
        "Title"      = title,
        "Department" = department,
        "Theme"      = theme
      )
    
    datatable(
      display_df,
      rownames   = FALSE,
      escape     = FALSE,
      options    = list(
        pageLength = 25,
        dom        = "tip",
        scrollX    = FALSE,
        autoWidth  = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = 0),
          list(width = "30px",  targets = 1, orderable = FALSE),
          list(width = "80px",  targets = 2),
          list(width = "36%",   targets = 3),
          list(width = "26%",   targets = 4),
          list(width = "15%",   targets = 5)
        ),
        drawCallback = JS("function(settings) {
          var api = this.api();
          $('#course_table tbody').off('click', 'tr');
          $('#course_table tbody').on('click', 'tr', function() {
            var tr = $(this);
            if (tr.hasClass('child')) return;
            var row = api.row(tr);
            if (row.child.isShown()) {
              row.child.hide();
              tr.removeClass('row-open');
            } else {
              var detailHtml = row.data()[0];
              row.child(detailHtml).show();
              tr.addClass('row-open');
            }
          });
        }")
      )
    )
  })
}

shinyApp(ui = ui, server = server) 