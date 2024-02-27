

library(tidyverse)
library(shiny)
library(sf)
library(rnaturalearth)
library(viridis)
library(scales)
library(ggmap)
library(plotly)
library(shinyFeedback)
library(rsconnect)
library(shinythemes)
library(jsonlite)
library(viridis)
library(cowplot)
library(ggplot2)
library(readr)
library(purrr)
library(dplyr)


##Data Prep
# unhoused locations per PITS report 
path <- ("C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago\\outside_data\\")

unhoused_locations_df <- read_csv(file.path(path, "unhoused_locations.csv"))

# Convert locations dataset into shapefile format
unhoused_locations <- st_as_sf(unhoused_locations_df,
                               coords = c("Longitude", "Latitude"),  crs=4326, remove = FALSE)

#merged ACS and IDPH data prior and write.csv to github repository, read in that file for shiny ease
opioid_data <- read_csv(file.path(path, "opioid_data.csv"))

#download Chicago neighborhood shapefiles
zip_chi_shape <- st_read("https://data.cityofchicago.org/resource/unjd-c2ca.geojson")

#create opioid shape file
opioid_shape <- merge(zip_chi_shape, opioid_data, by = "zip")


opioid_shape_long <- opioid_shape %>% 
  select(zip, geometry, overdose_rate_2022, overdose_rate_2021, overdose_rate_2020) %>% 
  rename(`2022` = overdose_rate_2022,
         `2021` = overdose_rate_2021,
         `2020` = overdose_rate_2020) %>% 
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Overdose_Rate")


##APP


ui <- fluidPage(theme = shinytheme("flatly"),
                useShinyFeedback(),
                titlePanel("Opioid Overdose Rate and Unhoused Populations"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "year",
                                label = "Choose a Year of Interest.",
                                choices = c("2020", "2021", "2022"),
                                selected = NULL),
                    h5("Data: IL Dept of Public Health, ACS, Chicago Point-in-Time Survey")
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel(title = "Opioid Rates", plotOutput("plot"))
                    )
                  )))


##shiny
#manipulate year in drop down to select which column in opioid dataframe

server <- function(input, output) {


##Plot 2022 RATE
output$plot <- 
  
  renderPlot({
    
    if (input$year == "2022") {
      
      # Make the plot
      p <-   ggplot() +
                  geom_sf(data = zip_chi_shape) +
                  geom_sf(data = filter(opioid_shape_long, Year == "2022"), aes(fill = Overdose_Rate), color = NA) +
                  labs(title = "Opioid Overdose Rate (Nonfatal and Fatal Per 10,000 Ppl)",
                        subtitle = paste("By Zip in 2022. Dot Size Correlated W/ High-Density Unhoused Pop.")) +
                  scale_fill_viridis(name="Opioid Overdose Rate", option = "magma", trans = "reverse", 
                                    breaks = pretty_breaks(n = 5)) +
                  geom_point(data = filter(unhoused_locations, Year == 2023), aes(x = Longitude, y = Latitude,
                                size = Responses,),
                                color = "#8c62aa",alpha = 0.7) +
                  guides(size = FALSE) +
                  theme_map() +
                  theme(
                  plot.title = element_text(hjust = 0, size= 13),
                  plot.subtitle = element_text(size = 10, hjust = 0), #subtitle not showing
                  legend.title = element_text(size = 12),
                  legend.title.align = 0.5)
                  
      
      
    }
    
    if (input$year == "2021") {

      
      p <-  ggplot() +
        geom_sf(data = zip_chi_shape) +
        geom_sf(data = filter(opioid_shape_long, Year == "2021"), aes(fill = Overdose_Rate), color = NA) +
        labs(title = "Opioid Overdose Rate (Nonfatal and Fatal Per 10,000 Ppl)",
              subtitle = paste("By Zip in 2021. Dot Size Correlated W/ High-Density Unhoused Pop.")) +
        scale_fill_viridis(name="Opioid Overdose Rate", option = "magma", trans = "reverse", 
                           breaks = pretty_breaks(n = 5)) +
        geom_point(data = filter(unhoused_locations, Year == 2022), aes(x = Longitude, y = Latitude,
                                                                        size = Responses,),
                   color = "#8c62aa",alpha = 0.7) +
        guides(size = FALSE) +
        theme_map() +
        theme(
          plot.title = element_text(hjust = 0, size= 13),
          plot.subtitle = element_text(size = 10, hjust = 0), #subtitle not showing
          legend.title = element_text(size = 12),
          legend.title.align = 0.5)
      
 
    }
    
    if (input$year == "2020") {
      
      
      p <-  ggplot() +
        geom_sf(data = zip_chi_shape) +
        geom_sf(data = filter(opioid_shape_long, Year == "2020"), aes(fill = Overdose_Rate), color = NA) +
        labs(title = "Opioid Overdose Rate (Nonfatal and Fatal Per 10,000 Ppl)",
             subtitle = paste("By Zip in 2020. Dot Size Correlated W/ High-Density Unhoused Pop.")) +
        scale_fill_viridis(name="Opioid Overdose Rate", option = "magma", trans = "reverse", 
                           breaks = pretty_breaks(n = 5)) +
        geom_point(data = filter(unhoused_locations, Year == 2022), aes(x = Longitude, y = Latitude,
                                                                        size = Responses,),
                   color = "#8c62aa",alpha = 0.7) +
        guides(size = FALSE) +
        theme_map() +
        theme(
          plot.title = element_text(hjust = 0, size= 13),
          plot.subtitle = element_text(size = 10, hjust = 0), #subtitle not showing
          legend.title = element_text(size = 12),
          legend.title.align = 0.5)
      
    }
    
    p
    
  
})
  
}
  
  
  
shinyApp(ui = ui, server = server)



       