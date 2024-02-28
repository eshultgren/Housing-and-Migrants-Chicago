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

# Add a new column with the count of clinics per ZIP code
mh_clinics_sf <- mh_clinics_sf %>%
  group_by(ZIP) %>%
  mutate(Num_Clinics = n())

ggplot() +
  geom_sf(data = mh_clinics_sf, aes(fill = NULL), color = 'red', size = 1, alpha = 0.7) +
  geom_point(data = mh_clinics_sf, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], size = Num_Clinics), color = 'red', alpha = 0.7, guide = "legend") +
  ggtitle('Mental Health Clinics in Chicago by Community Area') +
  theme_minimal() +
  guides(size = guide_legend(title = "Number of MH Clinics"))

# Standardize coordinate reference systems across sfs before joining 
st_crs(chi_comm_area)
st_crs(equity_zones_sf)

chi_comm_area <- st_set_crs(chi_comm_area, st_crs(equity_zones_sf))

# Perform spatial join to create master dataset for mapping 
joined_data <- st_join(chi_comm_area, equity_zones_sf, join = st_intersects)
joined_data <- st_join(joined_data, grocery_stores_sf, join = st_intersects)
joined_data <- st_join(joined_data, mh_clinics_sf, join = st_intersects)

# Plot the base map using equity zones
equity_zone_base_map <- ggplot(data = unique(joined_data)) +
  geom_sf(aes(fill = Equity.Zone), alpha = 0.7)

# Layer in additional features
# Simple map that shows equity zone distribution 
gg <- ggplot(data = unique(joined_data)) +
  geom_sf(aes(fill = Equity.Zone), alpha = 0.7)  

gg1 <- gg +
  geom_sf(data = mh_clinics_sf, aes(fill = NULL), color = 'red', size = 1, alpha = 0.7) +
  geom_point(data = mh_clinics_sf, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], size = Num_Clinics), color = 'red', alpha = 0.7, guide = "legend") +
  ggtitle('Mental Health Clinics in Chicago by Community Area') +
  theme_minimal() +
  guides(size = guide_legend(title = "Number of MH Clinics"))

gg1

# Aggregate number of grocery stores by Community Area
grocery_count <- grocery_stores_sf %>%
  group_by(COMMUNITY.AREA.NAME) %>%
  summarise(Num_Grocery_Stores = n())

# Join the aggregated counts with the spatial data
plot_data <- st_join(joined_data, grocery_count, by = "COMMUNITY.AREA.NAME")

# Aggregate number of MH clinics by Zip Code
mh_clinic_count <- mh_clinics_sf %>%
  group_by(ZIP) %>%
  rename(ZIP.CODE = ZIP) %>%
  summarise(Num_MH_clinics = n())

# Join the aggregated counts with the spatial data
plot_data <- st_join(plot_data, mh_clinic_count, by = "ZIP.CODE", left = TRUE)

# Remove superfluous columns before continuing plotting 
plot_data <- plot_data %>%
  select(Equity.Zone, Num_Grocery_Stores, Longitude, Latitude, Num_MH_clinics)

gg1 <- ggplot(data = unique(plot_data)) +
  geom_sf(aes(fill = Num_Grocery_Stores), alpha = 0.7) +
  geom_point(aes(x = Longitude, y = Latitude, size = Num_MH_clinics), color = "blue", alpha = 0.7) +
  scale_fill_gradient(name = "Number of Grocery Stores", low = "lightblue", high = "darkblue") +
  scale_size_continuous(name = "Number of MH Clinics", range = c(2, 10)) +
  theme_minimal() +
  ggtitle("Distribution of Grocery Stores and Mental Health Clinics in Chicago") +
  labs(source = "Chicago Data Portal, 2023")

gg1

# Attempting 2-way tile plot 
gg_plot_data <- ggplot(data = unique(plot_data)) +
  geom_sf(aes(fill = Num_Grocery_Stores), alpha = 0.7) +
  geom_sf(aes(fill = Num_MH_clinics), alpha = 0.7) + 
  scale_fill_gradient(name = "Number of Grocery Stores", low = "lightblue", high = "darkblue") +
  theme_minimal() +
  ggtitle("Distribution of Grocery Stores in Chicago") +
  labs(source = "Chicago Data Portal, 2023")

# Create a custom legend
legend <- ggdraw() +
  draw_plot(
    ggplot() +
      geom_sf(data = unique(plot_data), aes(fill = Num_MH_clinics), alpha = 0.7) +
      scale_fill_gradient(name = "Number of MH Clinics", low = "lightgreen", high = "darkgreen") +
      theme_minimal(),
    0, 0, 1, 1
  ) +
  draw_plot(
    get_legend(
      ggplot() +
        geom_sf(data = unique(plot_data), aes(fill = Num_MH_clinics), alpha = 0.7) +
        scale_fill_gradient(name = "Number of MH Clinics", low = "lightgreen", high = "darkgreen") +
        theme_minimal()
    ),
    x = 0.5, y = 0.4, width = 0.4, height = 0.4
  )

# Combine the main plot and legend
combined_plot <- ggdraw() +
  draw_plot(gg_plot_data, 0, 0, 1, 1) +
  draw_plot(legend, x = 0.5, y = 0.4, width = 0.4, height = 0.4)

print(combined_plot)





# Add equity zones with distinct border colors
gg2 <- gg1 +
  geom_sf(data = equity_zones_sf, aes(color = Equity.Zone), fill = NA, size = 1.5) +
  scale_color_manual(values = c("red", "green", "blue", "purple", "orange", "pink"))

gg2

# Layer in unhoused locations per PITS report 
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
unhoused_locations <- st_as_sf(unhoused_locations_df, coords = c("Longitude", "Latitude"))

# Check and standardize CRS
if (!identical(st_crs(unhoused_locations), st_crs(plot_data))) {
  unhoused_locations <- st_transform(unhoused_locations, st_crs(plot_data))
}

# Perform spatial join
plot_data_with_PITC <- st_join(plot_data, unhoused_locations, join = st_intersects)

# don;t need to merge in, can just layer on

gg3 <- gg2 +
  geom_sf(data = plot_data_with_PITC, aes(color = Equity.Zone), fill = NA, size = 1.5) +
  scale_color_manual(values = c("red", "green", "blue", "purple", "orange", "pink")) +
  geom_point(data = plot_data_with_PITC, aes(x = Longitude, y = Latitude, size = Responses), color = "red", alpha = 0.7) +
  scale_size_continuous(name = "Number of Unhoused People", range = c(1, 16)) +
  theme_minimal()

gg3

# tile plot for gg2 
# make it a chloropleth within the equity zone!
# check with Andre tomorrow - check Calendly 

# Edit to correct individual points and re-incorporate MH clinic legend - brainstorm way to combine legends and use different shapes?

# Experimenting with other map forms - tile plot?

library(ggplot2)
library(cowplot)
library(reshape2)

# Define color palette
bvColors <- c("#be64ac", "#8c62aa", "#3b4994", "#dfb0d6", "#a5add3", "#5698b9", "#e8e8e8", "#ace4e4", "#5ac8c8")

# Create a legend data frame
legendGoal <- melt(matrix(1:9, nrow = 3))

# Create a legend plot
legendPlot <- ggplot(legendGoal, aes(x = Num_MH_clinics, y = Num_Grocery_Stores, fill = as.factor(value))) +
  geom_tile() +
  scale_fill_manual(name = "", values = bvColors) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = rel(1), color = bvColors[3])) +
  xlab("More MH Clinics -->") +
  theme(axis.title.y = element_text(size = rel(1), color = bvColors[3])) +
  ylab("More Grocery Stores -->") +
  theme(axis.text = element_blank()) +
  theme(line = element_blank())

legendPlot

# Main map plot
mainMapPlot <- ggplot(plot_data_with_PITC, aes(x = Longitude, y = Latitude, fill = Num_Grocery_Stores)) +
  geom_sf() +
  scale_fill_gradientn(name = "Number of Grocery Stores", colors = bvColors) +
  theme_minimal()

# Combine both plots using cowplot
finalPlot <- ggdraw() +
  draw_plot(legendPlot, 0.1, 0.7, width = 0.2, height = 0.2) +
  draw_plot(mainMapPlot, 0.3, 0, width = 0.7, height = 0.7)

# Display or save the final plot
print(finalPlot)
finalPlot


# Shiny app conversion
# Ultimate vision = deployed app with a drop down menu to select layers to bring into the map.
# Start with equity zone, and then build in grocery stores --> MH clinics --> unhoused population 

ui <- fluidPage(
  titlePanel("Map with Layers"),
  selectInput("layer", "Select Layer", choices = c("Equity Zones", "Grocery Stores", "MH Clinics", "Unhoused Population")),
  leafletOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addLayersControl(overlayGroups = c("Equity Zones", "Grocery Stores", "MH Clinics", "Unhoused Population"))
  })
  
  observe({
    map <- leafletProxy("map")
    
    if (input$layer == "Equity Zones") {
      map %>% clearMarkers() %>%
        addMarkers(data = equity_zones, ~lon, ~lat, group = "Equity Zones", popup = ~name)
    } else if (input$layer == "Grocery Stores") {
      map %>% clearMarkers() %>%
        addMarkers(data = grocery_stores, ~lon, ~lat, group = "Grocery Stores", popup = ~name)
    } else if (input$layer == "MH Clinics") {
      map %>% clearMarkers() %>%
        addMarkers(data = mh_clinics, ~lon, ~lat, group = "MH Clinics", popup = ~name)
    } else if (input$layer == "Unhoused Population") {
      map %>% clearMarkers() %>%
        addMarkers(data = unhoused_population, ~lon, ~lat, group = "Unhoused Population", popup = ~name)
    }
  })
}

shinyApp(ui, server)
