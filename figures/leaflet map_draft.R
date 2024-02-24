
# Clear workspace and set working directory 
rm(list = ls())
setwd("C:/Users/higgi/Documents/GitHub/Housing-and-Migrants-Chicago/outside_data")

library(ggplot2)
library(leaflet)
library(RColorBrewer)  # For color scales
library(leaflet.extras)
library(tmap)
library(tmaptools)
library(sf)
library(ggmap)
library(osmdata)

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
unhoused_locations <- st_as_sf(unhoused_locations_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Import shapefiles for spatial data: 

# Specify zipfile path 
zip_path <- "Boundaries - Community Areas (current).zip"

# Create a temporary directory to extract the contents + unzip files 
temp_dir <- tempdir()
unzip(zip_path, exdir = temp_dir)

# List the files in the temporary directory
list.files(temp_dir)

# Read shapefiles into R
chi_comm_area <- st_read(temp_dir)

# Check and transform coordinate reference system  
if (!identical(st_crs(unhoused_locations), st_crs(chi_comm_area))) {
  # Transform the unhoused_locations dataset to match the CRS of chi_comm_area
  unhoused_locations <- st_transform(unhoused_locations, st_crs(chi_comm_area))
}

# Perform spatial join 
joined_data <- st_join(unhoused_locations, chi_comm_area)

# Plot distribution of unhoused population 
ggplot() +
  geom_sf(data = chi_comm_area, fill = "white", color = "black", size = 0.5) +
  geom_sf(data = joined_data, aes(size = Responses, color = Responses), alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Unhoused Population, per 2023 PITS Report") +
  theme(legend.position = "bottom") + 
  scale_color_viridis_c(option = "magma")

# Create a dataframe for reasons for being unhoused 
unhoused_reason <- data.frame(
  Cause = c("Family Disputes", "Multiple", "Loss of employment/underemployment",
            "Eviction, Foreclosure, Unable to Renew", "Disasters (i.e. Fire, Flood)",
            "Death of Parent/Guardian/Spouse", "Release from Jail/Prison", "Total"),
  Percent = c(46.90, 18.50, 14.80, 9.90, 6.20, 1.20, 2.50, 100.00)
)

# Remove total row before plotting 
unhoused_reason <- unhoused_reason[unhoused_reason$Cause != "Total", ]

# Plot options 
ggplot(unhoused_reason, aes(y = reorder(Cause, -Percent), x = Percent, fill = Cause)) +
  geom_bar(stat = "identity", color = "white", alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Causes of Loss of Housing",
       y = "Causes",
       x = "Percentage",
       caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_viridis_d(option = "magma") 

ggplot(unhoused_reason, aes(x = reorder(Cause, -Percent), y = Percent, fill = Cause)) +
  geom_bar(stat = "identity", color = "white", alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove X-axis labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Causes of Loss of Housing",
       x = NULL,
       y = "Percentage",
       caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_viridis_d(option = "magma") +
  guides(fill = guide_legend(title = "Causes")) 


