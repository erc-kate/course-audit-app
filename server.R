# ============================================================
# server.R — Server Logic
# Defines what the app DOES: filtering, rendering, reacting.
# No layout here. No data loading here. Just logic.
# ============================================================

server <- function(input, output, session) {
  
  # -- Filtered data --
  # reactive() means this code re-runs automatically whenever
  # any input$ value it depends on changes. It's the core
  # idea of Shiny — outputs react to inputs.
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
    
    if (input$dept_filter     != "All") df <- df |> filter(department == input$dept_filter)
    if (input$theme_filter    != "All") df <- df |> filter(theme      == input$theme_filter)
    if (input$subtheme_filter != "All") df <- df |> filter(subtheme   == input$subtheme_filter)
    
    df
  })
  
  # -- Reset button --
  # observeEvent() watches for one specific thing to happen
  # (button click) then runs its code block once in response.
  observeEvent(input$reset, {
    updateTextInput(session,  "search",           value    = "")
    updateSelectInput(session,"dept_filter",      selected = "All")
    updateSelectInput(session,"theme_filter",     selected = "All")
    updateSelectInput(session,"subtheme_filter",  selected = "All")
  })
  
  # -- Total course count --
  # renderText() produces a text string for a textOutput()
  # in ui.R. The names must match: "total_count" here
  # corresponds to textOutput("total_count") in ui.R.
  output$total_count <- renderText({
    paste0(nrow(courses), " total courses in this audit")
  })
  
  # -- Main table --
  # renderDT() produces an interactive table for DTOutput()
  # in ui.R. Called every time filtered_courses() changes.
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
      rownames  = FALSE,
      escape    = FALSE,
      options   = list(
        pageLength = 25,
        dom        = "tip",
        scrollX    = FALSE,
        autoWidth  = FALSE,
        columnDefs = list(
          list(visible = FALSE,  targets = 0),
          list(width = "30px",   targets = 1, orderable = FALSE),
          list(width = "80px",   targets = 2),
          list(width = "36%",    targets = 3),
          list(width = "26%",    targets = 4),
          list(width = "15%",    targets = 5)
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