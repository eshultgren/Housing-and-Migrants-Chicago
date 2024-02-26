

library(tidyverse)
library(shiny)
library(sf)
library(rnaturalearth)
library(viridis)
library(scales)
library(ggmap)
#library(shiny)
library(plotly)
library(shinyFeedback)
library(rsconnect)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("flatly"),
                useShinyFeedback(),
                titlePanel("Opioid Overdose Rate and Unhoused Populations"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "year",
                                label = "Choose a Year of Interest.",
                                choices = c("2020", "2021", "P2022"))
                  ),


##shiny
#manipulate year in drop down to select which column in opioid dataframe






##Plot 2022 RATE
opioid_rate_2022 <- ggplot() +
  geom_sf(data = zip_chi_shape) +
  geom_sf(data = opioid_shape, aes(fill = overdose_rate_22), color = NA) +
  labs(title = "Opioid Overdose Rate (Nonfatal and Fatal Per 10,000 Ppl) by Zip in 2022",
       subtitle = "Dot Size Correlated W/ High-Density Unhoused Pop.",
       caption = "Data: IL Dept of Public Health, ACS, PIT Survey") +
  scale_fill_viridis(name="Opioid Overdose Rate", option = "magma", trans = "reverse", 
                     breaks = pretty_breaks(n = 5)) +
  geom_point(data = unhoused_locations, aes(x = Longitude, y = Latitude,
                                            size = Responses,),
             color = "#8c62aa",alpha = 0.7) +
  guides(size = FALSE) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0, size= 13),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5
  )