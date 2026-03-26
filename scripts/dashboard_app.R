#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(ggplot2)
})

data_path <- "data/clean/combined_clean_strict.csv"
if (!file.exists(data_path)) {
  data_path <- "data/clean/combined_clean.csv"
}
df <- read_csv(data_path, show_col_types = FALSE)

df <- df %>%
  mutate(
    year = as.integer(year),
    citations = as.numeric(citations),
    keywords = ifelse(is.na(keywords), "", keywords),
    journal = ifelse(is.na(journal), "unknown source", journal)
  )

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Bibliometric Dashboard: Financial Inclusion & Agricultural Productivity"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Year range", min = min(df$year, na.rm = TRUE), max = max(df$year, na.rm = TRUE),
                  value = c(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE)), sep = ""),
      sliderInput("min_citations", "Minimum citations", min = 0, max = max(df$citations, na.rm = TRUE), value = 0),
      checkboxInput("journal_only", "Journal-like sources only", value = TRUE),
      actionButton("apply", "Apply Filters"),
      br(), br(),
      downloadButton("download_filtered", "Download Filtered CSV")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", plotlyOutput("annual_plot"), DTOutput("overview_table")),
        tabPanel("Authors", plotlyOutput("authors_plot"), DTOutput("authors_table")),
        tabPanel("Journals", plotlyOutput("journals_plot"), DTOutput("journals_table")),
        tabPanel("Keywords", plotlyOutput("keywords_plot"), DTOutput("keywords_table")),
        tabPanel("Documents", DTOutput("docs_table")),
        tabPanel(
          "Generated Outputs",
          h4("Generated Plot Preview"),
          selectInput("plot_file", "Choose plot", choices = c()),
          imageOutput("selected_plot", height = "700px"),
          br(),
          h4("Exported Tables"),
          DTOutput("generated_tables")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  plot_dir <- "results/plots"
  table_dir <- "results/tables"

  observe({
    plot_files <- character()
    if (dir.exists(plot_dir)) {
      plot_files <- list.files(plot_dir, pattern = "\\.png$", full.names = FALSE)
      plot_files <- sort(plot_files)
    }
    updateSelectInput(session, "plot_file", choices = plot_files, selected = ifelse(length(plot_files) > 0, plot_files[1], ""))
  })

  filtered <- eventReactive(input$apply, {
    x <- df %>%
      filter(!is.na(year), year >= input$year_range[1], year <= input$year_range[2]) %>%
      filter(citations >= input$min_citations)

    if (isTRUE(input$journal_only)) {
      x <- x %>%
        filter(
          !str_detect(tolower(journal), "unknown source|zenodo|ssrn|ebook|repository")
        )
    }
    x
  }, ignoreNULL = FALSE)

  output$annual_plot <- renderPlotly({
    ann <- filtered() %>% count(year, name = "articles")
    p <- ggplot(ann, aes(year, articles)) +
      geom_line(color = "#2C7FB8", linewidth = 1.2) +
      geom_point(color = "#2C7FB8", size = 2) +
      labs(title = "Annual Scientific Production", x = "Year", y = "Articles") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })

  output$overview_table <- renderDT({
    x <- filtered()
    meta <- tibble::tibble(
      metric = c("Documents", "Average citations", "Unique journals", "Unique authors"),
      value = c(
        nrow(x),
        round(mean(x$citations, na.rm = TRUE), 2),
        n_distinct(x$journal),
        n_distinct(unlist(str_split(paste(x$authors, collapse = ";"), ";")))
      )
    )
    datatable(meta, options = list(pageLength = 10))
  })

  output$authors_plot <- renderPlotly({
    a <- filtered() %>%
      mutate(authors = ifelse(is.na(authors), "", authors)) %>%
      separate_rows(authors, sep = ";") %>%
      mutate(authors = str_trim(authors)) %>%
      filter(authors != "", authors != "unknown") %>%
      count(authors, sort = TRUE) %>%
      slice_head(n = 20)
    p <- ggplot(a, aes(x = reorder(authors, n), y = n)) +
      geom_col(fill = "#2C7FB8") +
      coord_flip() +
      labs(title = "Top 20 Authors", x = "Author", y = "Documents") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })

  output$authors_table <- renderDT({
    a <- filtered() %>%
      mutate(authors = ifelse(is.na(authors), "", authors)) %>%
      separate_rows(authors, sep = ";") %>%
      mutate(authors = str_trim(authors)) %>%
      filter(authors != "", authors != "unknown") %>%
      count(authors, sort = TRUE) %>%
      slice_head(n = 50)
    datatable(a, options = list(pageLength = 10))
  })

  output$journals_plot <- renderPlotly({
    j <- filtered() %>% count(journal, sort = TRUE) %>% slice_head(n = 20)
    p <- ggplot(j, aes(x = reorder(journal, n), y = n)) +
      geom_col(fill = "#1B9E77") +
      coord_flip() +
      labs(title = "Top 20 Journals", x = "Journal", y = "Documents") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })

  output$journals_table <- renderDT({
    j <- filtered() %>% count(journal, sort = TRUE)
    datatable(j, options = list(pageLength = 10))
  })

  output$keywords_plot <- renderPlotly({
    k <- filtered() %>%
      separate_rows(keywords, sep = ";") %>%
      mutate(keywords = str_trim(tolower(keywords))) %>%
      filter(keywords != "") %>%
      count(keywords, sort = TRUE) %>%
      slice_head(n = 20)
    p <- ggplot(k, aes(x = reorder(keywords, n), y = n)) +
      geom_col(fill = "#D95F02") +
      coord_flip() +
      labs(title = "Top 20 Keywords", x = "Keyword", y = "Frequency") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })

  output$keywords_table <- renderDT({
    k <- filtered() %>%
      separate_rows(keywords, sep = ";") %>%
      mutate(keywords = str_trim(tolower(keywords))) %>%
      filter(keywords != "") %>%
      count(keywords, sort = TRUE)
    datatable(k, options = list(pageLength = 10))
  })

  output$docs_table <- renderDT({
    docs <- filtered() %>%
      select(title, authors, year, journal, doi, citations) %>%
      arrange(desc(citations))
    datatable(docs, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("filtered_bibliometric_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered(), file)
    }
  )

  output$selected_plot <- renderImage({
    req(input$plot_file)
    path <- file.path(plot_dir, input$plot_file)
    validate(need(file.exists(path), "Selected plot file not found."))
    list(src = path, contentType = "image/png", alt = input$plot_file)
  }, deleteFile = FALSE)

  output$generated_tables <- renderDT({
    if (!dir.exists(table_dir)) {
      return(datatable(data.frame(message = "No tables found yet.")))
    }
    tbl_files <- list.files(table_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(tbl_files) == 0) {
      return(datatable(data.frame(message = "No CSV tables generated yet.")))
    }
    info <- data.frame(
      file = basename(tbl_files),
      size_kb = round(file.info(tbl_files)$size / 1024, 2),
      modified = as.character(file.info(tbl_files)$mtime),
      stringsAsFactors = FALSE
    )
    datatable(info, options = list(pageLength = 10))
  })
}

shinyApp(ui, server)

