library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr) 
library(plotly)


data <- read.csv("enrollment.csv")

ui <- fluidPage(
  titlePanel("Tertiary Education Enrollment"),
  mainPanel(
    plotlyOutput("bar_chart")
  )
)

server <- function(input, output) {
  output$bar_chart <- renderPlotly({
    data <- data %>%
      mutate(NotInTertiary = 100 - Percentage) %>%
      pivot_longer(
        cols = c(Percentage, NotInTertiary),
        names_to = "EnrollmentStatus",
        values_to = "Percentage"
      ) %>%
      mutate(
        EnrollmentStatus = recode(EnrollmentStatus, `Percentage` = "In Tertiary", `NotInTertiary` = "Not In Tertiary")
      ) %>%
      arrange(Year, EnrollmentStatus) %>%
      group_by(Year) %>%
      mutate(HoverText = paste("Year:", Year,
                               "<br>In Tertiary:", Percentage[EnrollmentStatus == "In Tertiary"], "%",
                               "<br>Not In Tertiary:", Percentage[EnrollmentStatus == "Not In Tertiary"], "%")) %>%
      ungroup()
    
    # Create the ggplot object
    p <- ggplot(data, aes(x = Year, y = Percentage, fill = EnrollmentStatus, text = HoverText)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = c("In Tertiary" = "#a6bf6b", "Not In Tertiary" = "#e8633a")) +
      scale_y_continuous(labels = scales::label_percent()) +
      labs(title = "Tertiary Education Enrollment Over The Years",
           x = "Year",
           y = "Percentage",
           fill = "Enrollment Status",
           caption = "Hover over the chart to view specific percentages for that year"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(hjust = 0))
    
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)


shinyApp(ui = ui, server = server)
