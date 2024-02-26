# Clear workspace and set working directory 
rm(list = ls())
setwd("C:/Users/higgi/Documents/GitHub/Housing-and-Migrants-Chicago/outside_data")

# Load libraries 
library(tidyverse)
library(tidyr)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)
library(ggmap)
library(shiny)

# Import equity zone data 
equity_zones <- read.csv("Healthy_Chicago_Equity_Zones_20240222.csv")

# Import grocery store data
grocery_stores <- read.csv("Map_of_Grocery_Stores_-_2013.csv")

# Import mental health clinic location data 
mh_clinics <- read.csv("CDPH_Mental_Health_Resources_20240222.csv")

# Import community area shapefiles 

# Specify zipfile path 
zip_path <- "Boundaries - Community Areas (current).zip"

# Create a temporary directory to extract the contents + unzip files 
temp_dir <- tempdir()
unzip(zip_path, exdir = temp_dir)

# List the files in the temporary directory
list.files(temp_dir)

# Read shapefiles into R
chi_comm_area <- st_read(temp_dir)

# Extract latitude and longitude
coordinates <- st_coordinates(chi_comm_area$geometry)

# Create new columnswith lat and long
chi_comm_area$Latitude <- head(coordinates[, "Y"], n = nrow(chi_comm_area))
chi_comm_area$Longitude <- head(coordinates[, "X"], n = nrow(chi_comm_area))

# Convert all three dfs to shapefile format to support spatial join 
equity_zones_sf <- st_as_sf(equity_zones, wkt = "Geometry")
grocery_stores_sf <- st_as_sf(grocery_stores, coords = c("LONGITUDE", "LATITUDE"))

# Extract lat and long from mh_clinics sf as intermediate step
mh_clinics$Longitude <- as.numeric(gsub("POINT \\((.*?) .*", "\\1", mh_clinics$Location))
mh_clinics$Latitude <- as.numeric(gsub("POINT \\(.*? (.*?)\\)", "\\1", mh_clinics$Location))
mh_clinics_sf <- st_as_sf(mh_clinics, coords = c("Longitude", "Latitude"))

# Standardize coordinate reference systems across sfs before joining 
st_crs(chi_comm_area)
st_crs(equity_zones_sf)

chi_comm_area <- st_set_crs(chi_comm_area, st_crs(equity_zones_sf))

# Perform spatial join to create master dataset for mapping 
joined_data <- st_join(chi_comm_area, equity_zones_sf, join = st_intersects)
joined_data <- st_join(joined_data, grocery_stores_sf, join = st_intersects)
joined_data <- st_join(joined_data, mh_clinics_sf, join = st_intersects)

# FIRST MAP: EQUITY ZONES
# Simple map that shows equity zone distribution 
equity_zones_plot <- ggplot(data = unique(joined_data)) +
  geom_sf(aes(fill = Equity.Zone), alpha = 0.7)  

equity_zones_plot

# SECOND MAP: GROCERY STORES

# Aggregate number of grocery stores by Community Area
grocery_count <- grocery_stores_sf %>%
  group_by(COMMUNITY.AREA.NAME) %>%
  summarise(Num_Grocery_Stores = n())

# Join the aggregated counts with the spatial data
plot_data <- st_join(joined_data, grocery_count, by = "COMMUNITY.AREA.NAME")

grocery_plot_int <- ggplot(data = unique(plot_data)) +
  geom_sf(aes(fill = Num_Grocery_Stores), alpha = 0.7) +
  scale_fill_gradient(name = "Number of Grocery Stores", low = "#feca8d", high = "#cd4071")

grocery_plot <- grocery_plot_int +
  geom_sf(data = equity_zones_sf, aes(color = Equity.Zone), fill = NA, size = 1.5, lwd = 1.2) +
  scale_color_manual(values = c("red", "green", "blue", "purple", "orange", "pink"))+
  #ggtitle("Equity Zones + Grocery Stores")+
  theme(plot.title = element_text(hjust = 0.5))

grocery_plot

# THIRD MAP: MH CLINICS
# Add a new column with the count of clinics per ZIP code
mh_clinics_sf <- mh_clinics_sf %>%
  group_by(ZIP) %>%
  mutate(Num_Clinics = n())

# Exclude the top 2 outliers with the highest latitude values?

mh_clinic_plot <- grocery_plot +
  geom_sf(data = mh_clinics_sf, aes(fill = NULL), color = '#721f81', size = 1, alpha = 0.7) +
  geom_point(data = mh_clinics_sf, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], size = Num_Clinics), color = '#721f81', alpha = 0.7, guide = "legend") +
  #ggtitle('Equity Zones + Grocery Stores + Mental Health Clinics') +
  theme_minimal() +
  guides(size = guide_legend(title = "Number of MH Clinics")) +
  theme(axis.text = element_blank(), axis.title = element_blank())

mh_clinic_plot

# FOURTH MAP: UNHOUSED COUNT 
# Compile unhoused locations dataframe 
unhoused_locations_df <- data.frame(
  Location = c("The Loop, River North", "CTA - Red Line (95th/Dan Ryan)", "CTA - Blue Line (Forest Park)",
               "Near West Side/Medical District", "CTA - Blue Line (Cumberland/Rosemont)", "North Side",
               "CTA - Red Line", "South Side (East of State)", "O'Hare Airport", "South Side (West of State)",
               "CTA - Red Line (Howard)", "West Town, Kennedy Expressway", "Midway Airport Terminal", "Northwest Side"),
  Responses = c(170, 125, 118, 112, 71, 58, 54, 38, 33, 29, 13, 13, 12, 2),
  Latitude = c(41.89248568824814, 41.72258849002998, 41.87370675013457, 41.86883204433447, 41.983883465320844, 41.90389519059428,
               41.889900182393696, 41.75042676688248, 41.98037675551297, 41.75042676688248, 42.01889620273381, 41.95930482218085,
               41.78852626097361, 41.88530874443534),
  Longitude = c(-87.634044880516, -87.62443385767142, -87.81697954232854, -87.67398565436856, -87.83862460424264, -87.63211612952345,
                -87.62808780459785, -87.63411546128873, -87.90900606226334, -87.63411546128873, -87.67255656226084, -87.74088846041468,
                -87.74167007391868, -87.79271259245185)
)

# Convert locations dataset into shapefile format
unhoused_locations_sf <- st_as_sf(unhoused_locations_df, coords = c("Longitude", "Latitude"))

unhoused_pop_plot <- mh_clinic_plot +
  geom_sf(data = unhoused_locations_sf, aes(size = Responses, alpha = Responses), shape = 17, color = "#150e38", fill = "#150e38") +  scale_alpha_continuous(range = c(0.1, 1), name = "Number of Unhoused Individuals") +
  #labs(title = "Equity Zones, MH Clinics, Grocery Stores + Unhoused Population") +
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank())

unhoused_pop_plot

# remove triangles from MH clinic legend?

# Configure Shiny 

# Define UI
ui <- fluidPage(
  titlePanel("Determinants of Homelessness + Distribution of Unhoused Population in Chicago"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_selector", "Select Plot", 
                  choices = c("Equity Zones", "Grocery Stores", "Mental Health Clinics", "Unhoused Population"),
                  selected = "Equity Zones")
    ),
    mainPanel(
      plotOutput("selected_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive function to select the plot based on user input
  selected_plot <- reactive({
    switch(input$plot_selector,
           "Equity Zones" = equity_zones_plot,
           "Grocery Stores" = grocery_plot,
           "Mental Health Clinics" = mh_clinic_plot,
           "Unhoused Population" = unhoused_pop_plot)
  })
  
  # Render the selected plot with an adjustable size
  output$selected_plot <- renderPlot({
    selected_plot() +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 20))
  })
}

# Run the Shiny app
shinyApp(ui, server)

# remove X and Y axis labels - why are they re-appearing?
# omit titles?
# resizeplots based on size of window?