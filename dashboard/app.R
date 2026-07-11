library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(bslib)

catalog <- read_csv("../data/models_catalog.csv", show_col_types = FALSE)
benchmarks <- read_csv("../data/benchmark_scores.csv", show_col_types = FALSE)

catalog <- catalog %>%
  mutate(
    release_date = ymd(release_date),
    open_source = if_else(access_type %in% c("open_weights", "open_source"), "Open", "Closed"),
    is_multimodal = if_else(str_detect(tolower(modality), "multimodal|image|audio|video"), TRUE, FALSE)
  )

org_colors <- c("OpenAI" = "#10a37f", "Google" = "#4285f4", "Meta" = "#1877f2",
                "Anthropic" = "#d97706", "DeepSeek" = "#4f46e5", "Mistral" = "#ea580c",
                "Microsoft" = "#00a4ef", "Alibaba" = "#ff6a00", "xAI" = "#1a1a1a",
                "AI21" = "#7c3aed", "TII" = "#dc2626", "Inflection" = "#0891b2",
                "01.AI" = "#ca8a04", "BAAI" = "#059669", "BigScience" = "#c026d3")

ui <- page_navbar(
  title = "AI Models Analysis (2020\u20132026)",
  theme = bs_theme(bootswatch = "flatly", primary = "#1a1a2e"),
  
  nav_panel("Executive Summary",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Notable Models", value = nrow(catalog),
                theme = "primary"),
      value_box(title = "Organizations", value = n_distinct(catalog$organization),
                theme = "success"),
      value_box(title = "Benchmarks Tracked", value = n_distinct(benchmarks$benchmark),
                theme = "info"),
      value_box(title = "Years Covered", value = "2020\u20132026",
                theme = "warning")
    ),
    card(
      card_header("Timeline of AI Models"),
      plotlyOutput("timeline_plot", height = "500px")
    ),
    layout_columns(
      card(card_header("Models Per Year"), plotlyOutput("year_plot", height = "300px")),
      card(card_header("Open vs Closed"), plotlyOutput("open_plot", height = "300px"))
    )
  ),
  
  nav_panel("Company Analysis",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput("companies", "Organizations:",
                           choices = sort(unique(catalog$organization)),
                           selected = c("OpenAI", "Google", "Meta", "Anthropic", "DeepSeek", "Mistral"))
      ),
      card(card_header("Company Releases Over Time"), plotlyOutput("company_time", height = "400px")),
      layout_columns(
        card(card_header("Top Companies"), plotlyOutput("top_companies", height = "350px")),
        card(card_header("Market Share"), plotlyOutput("market_share", height = "350px"))
      )
    )
  ),
  
  nav_panel("Benchmark Analysis",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("benchmark", "Benchmark:", choices = sort(unique(benchmarks$benchmark)),
                    selected = "MMLU", multiple = FALSE),
        sliderInput("year_range", "Year Range:",
                    min = 2020, max = 2026, value = c(2020, 2026), step = 1)
      ),
      card(card_header("Benchmark Score Progression"), plotlyOutput("benchmark_trend", height = "500px")),
      card(card_header("Benchmark Data Table"), DTOutput("bench_table"))
    )
  ),
  
  nav_panel("Open Source Trends",
    card(card_header("Open vs Closed by Year"), plotlyOutput("open_trend", height = "400px")),
    layout_columns(
      card(card_header("Access Type Distribution"), plotlyOutput("access_pie", height = "350px")),
      card(card_header("Modality Evolution"), plotlyOutput("modality_plot", height = "350px"))
    )
  ),
  
  nav_panel("Capability Comparison",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("comp_models", "Select Models:",
                    choices = catalog$model_name, selected = tail(catalog$model_name, 6),
                    multiple = TRUE, selectize = TRUE)
      ),
      card(card_header("Benchmark Radar (Selected Models)"), plotlyOutput("radar_plot", height = "550px")),
      card(card_header("Model Details"), DTOutput("model_table"))
    )
  ),
  
  nav_panel("Data Explorer",
    card(
      card_header("Full Model Catalog"),
      DTOutput("full_table", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  output$timeline_plot <- renderPlotly({
    orgs <- unique(catalog$organization)
    plot_ly(catalog, x = ~release_date, y = ~params_billions,
            type = "scatter", mode = "markers",
            color = ~organization, colors = org_colors,
            text = ~paste0("<b>", model_name, "</b><br>",
                          organization, "<br>Params: ", params_billions, "B<br>",
                          "Context: ", context_window_k_tokens, "K<br>",
                          "Type: ", access_type),
            hoverinfo = "text",
            marker = list(size = ~sqrt(context_window_k_tokens + 1) * 2.5,
                         line = list(color = "#333", width = 0.5))) %>%
      layout(yaxis = list(title = "Parameters (Billions)", type = "log"),
             xaxis = list(title = "Release Date"),
             plot_bgcolor = "white", paper_bgcolor = "white",
             hovermode = "closest") %>%
      config(displayModeBar = FALSE)
  })
  
  output$year_plot <- renderPlotly({
    catalog %>%
      count(release_year) %>%
      plot_ly(x = ~release_year, y = ~n, type = "bar",
              marker = list(color = ~n, colorscale = list(c(0, "#a8d8ea"), c(1, "#1a1a2e"))),
              text = ~n, textposition = "outside",
              hovertemplate = "Year: %{x}<br>Models: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Year", dtick = 1),
             yaxis = list(title = "Models Released"),
             plot_bgcolor = "white", paper_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })
  
  output$open_plot <- renderPlotly({
    catalog %>%
      count(open_source) %>%
      plot_ly(labels = ~open_source, values = ~n, type = "pie",
              marker = list(colors = c("#e63946", "#10a37f")),
              textinfo = "label+percent",
              hovertemplate = "%{label}: %{value} models (%{percent})<extra></extra>") %>%
      layout(title = NULL, plot_bgcolor = "white", paper_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })
  
  company_data <- reactive({
    catalog %>%
      filter(organization %in% input$companies)
  })
  
  output$company_time <- renderPlotly({
    df <- company_data() %>%
      count(release_year, organization) %>%
      complete(release_year = 2020:2026, organization, fill = list(n = 0))
    
    plot_ly(df, x = ~release_year, y = ~n, color = ~organization,
            type = "scatter", mode = "lines+markers",
            colors = org_colors,
            hovertemplate = "%{y} models<extra></extra>") %>%
      layout(xaxis = list(title = "Year", dtick = 1),
             yaxis = list(title = "Models Released"),
             plot_bgcolor = "white", paper_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })
  
  output$top_companies <- renderPlotly({
    catalog %>%
      count(organization, sort = TRUE) %>%
      slice_max(n, n = 12) %>%
      plot_ly(x = ~n, y = ~fct_reorder(organization, n),
              type = "bar", orientation = "h",
              marker = list(color = ~n, colorscale = list(c(0, "#a8d8ea"), c(1, "#1a1a2e"))),
              text = ~n, textposition = "outside",
              hovertemplate = "%{y}: %{x} models<extra></extra>") %>%
      layout(xaxis = list(title = "Models"), yaxis = list(title = NULL),
             plot_bgcolor = "white", paper_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })
  
  output$market_share <- renderPlotly({
    catalog %>%
      count(organization, sort = TRUE) %>%
      slice_max(n, n = 8) %>%
      plot_ly(labels = ~organization, values = ~n, type = "pie",
              textinfo = "label+percent",
              hovertemplate = "%{label}: %{value} models (%{percent})<extra></extra>") %>%
      layout(plot_bgcolor = "white", paper_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })
  
  output$benchmark_trend <- renderPlotly({
    bm <- benchmarks %>%
      filter(benchmark == input$benchmark) %>%
      mutate(year = year(ymd(release_date))) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])
    
    plot_ly(bm, x = ~as.Date(release_date), y = ~score,
            type = "scatter", mode = "markers",
            color = ~organization, colors = org_colors,
            text = ~paste0("<b>", model_name, "</b><br>",
                          organization, "<br>Score: ", round(score, 1), "%"),
            hoverinfo = "text",
            marker = list(size = 8, opacity = 0.7,
                         line = list(color = "#333", width = 0.5))) %>%
      layout(yaxis = list(title = "Score (%)", range = c(0, 100)),
             xaxis = list(title = "Release Date"),
             plot_bgcolor = "white", paper_bgcolor = "white",
             hovermode = "closest") %>%
      config(displayModeBar = FALSE)
  })
  
  output$bench_table <- renderDT({
    benchmarks %>%
      filter(benchmark == input$benchmark) %>%
      select(model_name, organization, release_date, score) %>%
      arrange(desc(score)) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE) %>%
      formatRound("score", 1)
  })
  
  output$open_trend <- renderPlotly({
    df <- catalog %>%
      count(release_year, open_source) %>%
      complete(release_year = 2020:2026, open_source, fill = list(n = 0))
    
    plot_ly(df, x = ~release_year, y = ~n, color = ~open_source,
            type = "bar", colors = c("#e63946", "#10a37f"),
            text = ~n, textposition = "outside",
            hovertemplate = "%{x}: %{y} %{color}<extra></extra>") %>%
      layout(xaxis = list(title = "Year", dtick = 1),
             yaxis = list(title = "Models"),
             barmode = "group", plot_bgcolor = "white", paper_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })
  
  output$access_pie <- renderPlotly({
    catalog %>%
      count(access_type) %>%
      plot_ly(labels = ~access_type, values = ~n, type = "pie",
              textinfo = "label+percent",
              hovertemplate = "%{label}: %{value} models<extra></extra>") %>%
      layout(plot_bgcolor = "white", paper_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })
  
  output$modality_plot <- renderPlotly({
    df <- catalog %>%
      mutate(modality_group = case_when(
        str_detect(tolower(modality), "multimodal") ~ "Multimodal",
        str_detect(tolower(modality), "image|audio|video") ~ "Multimodal",
        modality == "text+code" ~ "Text+Code",
        modality == "text" ~ "Text",
        TRUE ~ "Other"
      )) %>%
      count(release_year, modality_group) %>%
      complete(release_year = 2020:2026, modality_group, fill = list(n = 0))
    
    plot_ly(df, x = ~release_year, y = ~n, color = ~modality_group,
            type = "scatter", mode = "lines+markers",
            colors = c("#e63946", "#457b9d", "#2a9d8f"),
            hovertemplate = "%{y} models<extra></extra>") %>%
      layout(xaxis = list(title = "Year", dtick = 1),
             yaxis = list(title = "Models"),
             plot_bgcolor = "white", paper_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })
  
  output$radar_plot <- renderPlotly({
    req(input$comp_models)
    
    selected <- catalog %>%
      filter(model_name %in% input$comp_models) %>%
      pull(model_id)
    
    radar_data <- benchmarks %>%
      filter(model_id %in% selected,
             benchmark %in% c("MMLU", "HumanEval", "GSM8K", "MATH", "GPQA Diamond")) %>%
      group_by(model_id, model_name, benchmark) %>%
      slice_max(score, n = 1) %>%
      ungroup()
    
    if (nrow(radar_data) == 0) return(NULL)
    
    # Create radar using plotly scatterpolar
    p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
    
    for (m in unique(radar_data$model_name)) {
      m_data <- radar_data %>% filter(model_name == m)
      p <- p %>% add_trace(
        r = m_data$score,
        theta = m_data$benchmark,
        name = m,
        hovertemplate = "%{theta}: %{r:.1f}%<extra>%{name}</extra>"
      )
    }
    
    p %>% layout(
      polar = list(radialaxis = list(range = c(0, 100), visible = TRUE)),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>% config(displayModeBar = FALSE)
  })
  
  output$model_table <- renderDT({
    catalog %>%
      filter(model_name %in% input$comp_models) %>%
      select(model_name, organization, release_date, params_billions,
             context_window_k_tokens, access_type, modality) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE) %>%
      formatRound("params_billions", 1)
  })
  
  output$full_table <- renderDT({
    catalog %>%
      select(model_name, organization, release_date, params_billions,
             context_window_k_tokens, access_type, modality, model_type) %>%
      arrange(desc(release_date)) %>%
      datatable(options = list(pageLength = 25, scrollX = TRUE),
                rownames = FALSE,
                filter = "top") %>%
      formatRound("params_billions", 1)
  })
}

shinyApp(ui, server)
