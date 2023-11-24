library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  titlePanel("CPI Comparison"),
  sidebarLayout(
    sidebarPanel(
      selectInput("from_year", "From:", choices = unique(1993:2022)),
      selectInput("to_year", "To:", choices = unique(1993:2022)),
      checkboxGroupInput("lines", "Select Lines to view:",
                         choices = c(
                           "General.Population",
                           "Lowest.20..income",
                           "Middle.60..income",
                           "Highest.20..income"
                         ),
                         selected = c(
                           "General.Population",
                           "Lowest.20..income",
                           "Middle.60..income",
                           "Highest.20..income"
                         ))
    ),
    mainPanel(
      plotlyOutput("cpi_plot")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$from_year, {
    updateSelectInput(session, "to_year", choices = input$from_year:2022)
  })
  
  combined_data <- read.csv("combined_data.csv")
  
  output$cpi_plot <- renderPlotly({  
    filtered_data <- combined_data
    
    filtered_data <- filtered_data[filtered_data$Year >= input$from_year & filtered_data$Year <= input$to_year, ]
    
    selected_lines <- input$lines
    
    color_palette <- c(
      "General.Population" = "blue",
      "Lowest.20..income" = "red",
      "Middle.60..income" = "green",
      "Highest.20..income" = "purple"
    )
    
    plot <- ggplot(data = filtered_data, aes(x = Year)) +
      labs(title = "CPI Comparison",
           x = "Year",
           y = "CPI") +
      theme_minimal()
    
    line_data <- data.frame()
    
    if ("General.Population" %in% selected_lines) {
      line_data <- rbind(line_data, 
                         data.frame(x = filtered_data$Year, 
                                    y = filtered_data$`General.Population`,
                                    line_type = "General.Population"))
    }
    
    if ("Lowest.20..income" %in% selected_lines) {
      line_data <- rbind(line_data, 
                         data.frame(x = filtered_data$Year, 
                                    y = filtered_data$`Lowest.20..income`,
                                    line_type = "Lowest.20..income"))
    }
    
    if ("Middle.60..income" %in% selected_lines) {
      line_data <- rbind(line_data, 
                         data.frame(x = filtered_data$Year, 
                                    y = filtered_data$`Middle.60..income`,
                                    line_type = "Middle.60..income"))
    }
    
    if ("Highest.20..income" %in% selected_lines) {
      line_data <- rbind(line_data, 
                         data.frame(x = filtered_data$Year, 
                                    y = filtered_data$`Highest.20..income`,
                                    line_type = "Highest.20..income"))
    }
    
    plotly::plot_ly(line_data, x = ~x, y = ~y, color = ~line_type, text = ~paste("Year:", x, "<br>", line_type, ":", y),
                    type = 'scatter', mode = 'lines') %>%
      layout(title = "CPI Comparison",
             xaxis = list(title = "Year"),
             yaxis = list(title = "CPI"),
             legend = list(title = "Lines"),
             showlegend = TRUE,
             colorway = color_palette)
  })
}

shinyApp(ui = ui, server = server)