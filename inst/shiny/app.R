#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(gt)
library(tidyverse)
library(readr)
library(rlang)

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Edit decision table output"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV"),

      tabsetPanel(
        id = "sidebar_tabs",
        br(),
        tabPanel("Overwrite",
                 textInput("filter", "Filter condition (e.g., variable == 'PM10')", ""),
                 textInput("variable", "The variable to overwrite", ""),
                 textInput("new_value", "The value modified to", ""),
                 actionButton("apply", "Apply changes"),
                 actionButton("confirm", "Confirm")
                 ),

        tabPanel("Delete",
                 textInput("highlight_filter", "Row to delete (e.g. id == 1)", ""),
                 actionButton("delete", "Delete rows")
                 ),

        tabPanel("Add",
                 textInput("new_paper", "paper"),
                 numericInput("new_id", "id", value = 0),
                 textInput("new_model", "model"),
                 textInput("new_variable", "variable"),
                 textInput("new_method", "method"),
                 textInput("new_parameter", "parameter"),
                 textInput("new_type", "type"),
                 textInput("new_reason", "reason"),
                 textInput("new_decision", "decision"),
                 actionButton("add_row", "Add Row"),
                 actionButton("confirm_add", "Confirm"),
                 ),
      ),

      downloadButton("download", "Download CSV"),
      hr(),
      h4("Generated tidyverse code"),
      verbatimTextOutput("code")
    ),

    mainPanel(
      gt_output("table")
    )
  )
)


server <- function(input, output, session) {
  data_orig <- reactiveVal()
  modified_data <- reactiveVal(NULL)
  confirmed <- reactiveVal(FALSE)
  new_rows <- reactiveVal(tibble())
  code_temp <- reactiveVal("df %>% ")
  code_final <- reactiveVal("")

  # Reactive: always apply filter to current modified data (or original)
  current_data <- reactive({
    modified_data() %||% data_orig()
  })

  filtered_data <- reactive({
    req(current_data())
    df <- current_data()

    if (confirmed()) return(df)
    if (input$filter == "") return(df)

    df_filtered <- tryCatch({
      df %>% filter(!!rlang::parse_expr(input$filter))
    }, error = function(e) df)

    tryCatch({
      filtered <- df %>% filter(!!rlang::parse_expr(input$filter))
      bind_rows(filtered, df %>% filter(just_added == TRUE)) %>% distinct()
    }, error = function(e) df)

    df_filtered
  })

  delete_filtered_data <- reactive({
    req(current_data())  # Ensure we have data

    df <- current_data()  # Get the current data

    # If there's no filter, return the original data
    if (input$delete_filter == "") return(df)

    tryCatch({
      # Apply the filter condition entered in the delete_filter
      df %>%
        filter(!!rlang::parse_expr(input$delete_filter))
    }, error = function(e) {
      showNotification(paste("Error in delete filter expression:", e$message), type = "error")
      return(df)  # Return original data if error occurs
    })
  })

  ########################################################################
  ########################################################################
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    data_orig(df)
  })

  observeEvent(input$apply, {
    req(current_data(), input$variable, input$new_value, input$filter)

    new_df <- tryCatch({
      current_data() %>%
        mutate(
          !!rlang::sym(input$variable) := ifelse(
            eval(rlang::parse_expr(input$filter), current_data()),
            input$new_value,
            !!rlang::sym(input$variable)
          )
        )
    }, error = function(e) {current_data()})

    modified_data(new_df)
  })

  observeEvent(input$confirm, {
    req(data_orig(), input$variable, input$new_value, input$filter)

    new_df <- tryCatch({
      current_data() %>%
        mutate(
          !!rlang::sym(input$variable) := ifelse(
            eval(rlang::parse_expr(input$filter), current_data()),
            input$new_value,
            !!rlang::sym(input$variable)
          )
        )
    }, error = function(e) {data_orig()})

    code <- glue::glue('mutate({input$variable} = ifelse({input$filter},
                       "{input$new_value}", {input$variable}))')
    code_temp(paste0(code_temp(), code, sep = " %>% "))
    data_orig(new_df)
    confirmed(TRUE)
    showNotification("Changes confirmed and applied to full data.", type = "default")

    updateTextInput(session, "filter", value = "")
    updateTextInput(session, "new_value", value = "")
    updateSelectInput(session, "variable", selected = "")
    confirmed(FALSE)

  })


  observeEvent(input$delete, {
    df <- current_data()

    # Determine which rows to delete
    rows_to_delete <- tryCatch({
      with(df, eval(parse(text = input$highlight_filter)))
    }, error = function(e) rep(FALSE, nrow(df)))  # delete none if error

    # Remove rows and update master
    updated_df <- df[!rows_to_delete, ]
    data_orig(updated_df)

    code <- glue::glue('filter(!({input$filter} & {input$highlight_filter}))')
    code_temp(paste0(code_temp(), code, sep = " %>% "))

    updateTextInput(session, "highlight_filter", value = "")
    confirmed(FALSE)
  })

  observeEvent(input$add_row, {
    new_row <- sapply(names(current_data()), function(col) {
      input[[paste0("new_", col)]]
    })
    new_row <- as_tibble_row(new_row) |> mutate(id = as.numeric(id))
    all_rows <- bind_rows(new_rows(), new_row)
    new_rows(all_rows)

    modified_data(bind_rows(data_orig(), all_rows))
    lapply(names(current_data()), function(col) {
      updateTextInput(session, paste0("new_", col), value = "")
    })

    code <- glue::glue("add_row(tibble(", map2(names({new_row}), new_row, ~glue::glue({.x}, '="', {.y}, '"' )) |> paste0(collapse = ", "), "))")
    code_temp(paste0(code_temp(), code, sep = " %>% "))
  })

  observeEvent(input$confirm_add, {
    new_row <- sapply(names(current_data()), function(col) {
      input[[paste0("new_", col)]]
    })
    new_row <- as_tibble_row(new_row) |> mutate(id = as.numeric(id))

    # Add the new row to the existing data
    new_df <- tryCatch({
      current_data() |> bind_rows(new_row_df)
    }, error = function(e){current_data()})
    data_orig(new_df)
    confirmed(TRUE)

    lapply(names(current_data()), function(col) {
      updateTextInput(session, paste0("new_", col), value = "")
    })

    code <- glue::glue("add_row(tibble(", map2(names({new_row}), new_row, ~glue::glue({.x}, '="', {.y}, '"' )) |> paste0(collapse = ", "), "))")
    code_temp(paste0(code_temp(), code, sep = " %>% "))
    confirmed(FALSE)
  })

  ########################################################################
  ########################################################################
  output$table <- render_gt({
    df_to_show <- filtered_data()  # Use filtered_data reactive
    req(df_to_show)

    rows_to_highlight <- tryCatch({
      which(with(df_to_show, eval(parse(text = input$highlight_filter))))
    }, error = function(e) integer(0))

    gt::gt(df_to_show) %>%
      gt::tab_style(
        style = gt::cell_fill(color = "#fff3cd"),
        locations = gt::cells_body(rows = rows_to_highlight)
      )

  })


  ########################################################################
  ########################################################################
  format_code <- function(code_string) {
    code_lines <- unlist(strsplit(code_string, "%>%"))
    code_lines <- trimws(code_lines)
    paste(code_lines, collapse = " %>%\n ")
  }


  output$code <- renderText({
    format_code(paste0(code_final(), code_temp()))
  })

  output$code_display <- renderUI({
    req(code_final(), code_temp())
    code_formatted <- format_code(paste0(code_final(), code_temp()))
    tags$pre(id = "code-box",
             style = "background:#f8f8f8; padding:20px; border-radius:5px;
                    white-space:pre-wrap; font-family: monospace;",
             paste0("data_orig() %>%\n ", code_formatted))
  })


  output$download <- downloadHandler(
    filename = function() {
      "modified_data.csv"
    },
    content = function(file) {
      write.csv(current_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
