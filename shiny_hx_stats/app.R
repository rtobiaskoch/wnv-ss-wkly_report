library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("Mosquito Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Make sidebar narrower
      fileInput("file", "Upload CSV File", accept = ".csv"),
      conditionalPanel(
        condition = "output.fileUploaded",
        hr(),
        h4("Data Filters"),
        sliderInput("year_range", "Year Range:",
                    min = 2010, max = 2023,
                    value = c(2018, 2022),
                    step = 1, sep = ""),
        pickerInput("spp_filter", "Species:",
                    choices = NULL,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        materialSwitch("show_current", "Show Current Year", 
                       value = TRUE, status = "primary"),
        actionButton("reset", "Reset Filters", 
                     icon = icon("redo"), class = "btn-sm")
      )
    ),
    mainPanel(
      width = 9,  # Make main panel wider
      plotlyOutput("mosquito_plot", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value for storing uploaded data
  uploaded_data <- reactiveVal()
  
  # Observe file upload
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      df <- read_csv(input$file$datapath) %>%
        mutate(
          year = as.numeric(year),
          week = as.numeric(week),
          zone = factor(zone),
          spp = factor(spp),
          type = factor(type),
          est = factor(est)
        )
      
      uploaded_data(df)
      
      # Update controls based on data
      year_range <- range(df$year, na.rm = TRUE)
      updateSliderInput(session, "year_range",
                        min = year_range[1],
                        max = year_range[2],
                        value = year_range)
      
      updatePickerInput(session, "spp_filter",
                        choices = levels(df$spp),
                        selected = levels(df$spp))
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$fileUploaded <- reactive(!is.null(uploaded_data()))
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Filter data based on controls
  filtered_data <- reactive({
    req(uploaded_data())
    df <- uploaded_data()
    
    df %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2],
        spp %in% input$spp_filter,
        if (!input$show_current) type != "current" else TRUE
      )
  })
  
  # Reset all filters
  observeEvent(input$reset, {
    df <- uploaded_data()
    updateSliderInput(session, "year_range", 
                      value = range(df$year, na.rm = TRUE))
    updatePickerInput(session, "spp_filter",
                      selected = levels(df$spp))
    updateMaterialSwitch(session, "show_current", value = TRUE)
  })
  
  # Create plot with improved legend
  output$mosquito_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = week, y = value, 
                        color = if ("type" %in% names(df)) year else NULL,
                        fill = if ("type" %in% names(df)) NULL else spp,
                        group = interaction(year, type, spp),
                        text = paste("Year:", year, "<br>Zone:", zone, 
                                     "<br>Species:", spp))) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      facet_grid(est ~ zone, scales = "free_y") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.box = "vertical") +
      ggtitle("Mosquito Data Analysis") +
      labs(x = "Week", y = "Value")
    
    # Add historical data if exists
    if ("type" %in% names(df) && any(df$type == "hx")) {
      p <- p + geom_line(
        data = ~ filter(.x, type == "hx"),
        alpha = 0.7,
        linewidth = 0.8
      ) +
        scale_color_gradient(low = "grey40", high = "grey80", 
                             name = "Historical Years")
    }
    
    # Add current data if exists and selected
    if (input$show_current && "type" %in% names(df) && any(df$type == "current")) {
      p <- p + geom_area(
        data = ~ filter(.x, type == "current"),
        aes(fill = spp),
        alpha = 0.7,
        position = "identity"
      ) +
        scale_fill_viridis_d(name = "Current Year Species", 
                             option = "plasma")
    }
    
    # If no type column, just show colored by species
    if (!"type" %in% names(df)) {
      p <- p + geom_line(size = 0.8) +
        scale_color_viridis_d(name = "Species", option = "viridis")
    }
    
    ggplotly(p, tooltip = "text", height = 800) %>%
      layout(legend = list(orientation = "h", 
                           x = 0.5, 
                           xanchor = "center",
                           y = -0.2,
                           yanchor = "top"))
  })
}

shinyApp(ui, server)
