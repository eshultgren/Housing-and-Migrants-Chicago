library(shiny)
library(ggplot2)
library(plotly)

setwd("C:/Users/steph/Documents/GitHub/R-DATA-2/Housing-and-Migrants-Chicago/")

texas_feelings <- read.csv("texas_feelings.csv")
texas_graph<- read.csv("texas_graph.csv")

glimpse(texas_graph)

#UI
ui <- fluidPage(
  # Application title
  titlePanel("Texas Sentiment and Count Analysis"),
  
  # Create tabs
  tabsetPanel(
    tabPanel("Sentiment Analysis", 
             selectInput("statType", "Choose Statistic:", 
                         choices = c("Mean" = "mean_afinn", 
                                     "Median" = "median_affin", 
                                     "Max" = "max_afinn",
                                     "Min" = "min_afinn",
                                     "SD"= "sd_afinn")),
             plotlyOutput("sentimentPlot")),
    tabPanel("Count Over Time", 
             selectInput("citySelect", "Choose a city:", 
                         choices = unique(texas_graph$City), 
                         selected = unique(texas_graph$City)[1], multiple = TRUE),
             plotlyOutput("countPlot"))
  )
)

# SERVER
server <- function(input, output) {
  
  # For Sentiment Analysis Plot
  output$sentimentPlot <- renderPlotly({
    req(input$statType)  # Ensure that statType is not NULL or missing
    
    overall_texas_feelings <- texas_feelings %>%
      mutate(article_id = as.numeric(article_id)) %>% 
      group_by(article_id) %>%
      summarise(mean_afinn = mean(afinn, na.rm=TRUE),
                median_affin = median(afinn, na.rm=TRUE),
                sd_afinn = sd(afinn, na.rm = TRUE),
                min_afinn = min(afinn, na.rm = TRUE),
                max_afinn = max(afinn, na.rm = TRUE)) 
    
    # Dynamically select the statistic based on user input
    sentiment_stat <- overall_texas_feelings %>% 
      select(article_id, !!sym(input$statType)) %>%
      arrange(desc(article_id))  
    
    # Convert ggplot to plotly for interactivity
    p <- ggplot(sentiment_stat, aes(x = article_id, y = !!sym(input$statType))) +
      geom_line(color = "#00BFC4", size = 1) +  
      geom_point(color = "#F8766D", size = 2, alpha = 0.8) +  
      theme_minimal() +  
      labs(
        title = "Overall Sentiment  by  Article across time",
        subtitle = "Sentiment scores with linear regression line",
        x = "Article ID",
        y = "Mean Sentiment Score"
      ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
      )  
    
    ggplotly(p)
  })
      

  # For Count Over Time Plot
  output$countPlot <- renderPlotly({
    req(input$citySelect)  # Ensure that citySelect is not NULL or missing
    
    # Assuming texas_graph is available in your global environment
    filtered_data <- texas_graph %>%
      filter(City %in% input$citySelect) %>%
      mutate(month_year = as.Date(month_year, format = "%Y-%m-%d"))
    
    # Ensure there are no zero or negative counts
    filtered_data <- filtered_data %>% filter(Count > 0)
      
  
    
    p <- ggplot(filtered_data, aes(x = month_year, y = log(Count), color = City)) +
      geom_line() + 
      geom_point() + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      labs(x = "Time", y = "Count", title = "Count Over Time by City") 
    
    ggplotly(p)
   
  })
}

# Run the application
shinyApp(ui = ui, server = server)