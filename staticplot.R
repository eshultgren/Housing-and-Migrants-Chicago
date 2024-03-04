##Libraries##
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(scales)
library(viridis)
library(ggthemes)
library(tmap)
library(tmaptools)
library(sf)
library(ggmap)
library(osmdata)
library(patchwork)
library(grid)

##Update Path Here##
path <- "/Users/josemacias/Documents/GitHub/Housing-and-Migrants-Chicago"
setwd(path)
############## Google Map Layer
google_api_key <- "AIzaSyAmWiRl8MXntcSV-YrTDc7DrgmjyPz7gzg"
register_google(key = google_api_key)
############### Google Map Layer End
############## Import shape and csvs
unhoused_locations <- 
  st_read("shape_files/unhoused/unhoused_locations.shp")
chi_comm_area  <- 
  st_read("shape_files/chicago_commarea/chicago_shape.shp") |>
  select(community, geometry)
# shape unhoused data data
joined_data <- 
  st_read("shape_files/unhoused/unhoused_locations_chi.shp")
sf_clean_mental_health <- 
  st_read("shape_files/mental_health/comm_area_locations_mental_health_.shp")
filtered_census_tracts <- st_read( "shape_files/chicago_tracts/chicago_tracts.shp")
chi_town <- st_read("shape_files/chicago_commarea/chicago_shape.shp")
# Check and transform coordinate reference system
if (!identical(st_crs(unhoused_locations), st_crs(chi_comm_area))) {
  # Transform the unhoused_locations dataset to match the CRS of chi_comm_area
  unhoused_locations <- st_transform(unhoused_locations, st_crs(chi_comm_area))
}
# Read in unhoused reasons
unhoused_reason <- 
 read_csv("outside_data/unhoused_reason.csv") |>
  filter(Cause != "Total")
# Veteran visuals
unhoused_veterans_nm <- 
  read_csv("outside_data/unhoused_veterans.csv") |>
  drop_na()

zip_chi_shape <-st_read("shape_files/chicago_zipcodes/chicago_zipcodes.shp")
opioid_shape <- st_read("shape_files/opioid_zip/opioid_shape.gpkg")
unhoused_locations_multiyr <- st_read('shape_files/unhoused/unhoused_locations_multiyr.shp')


# Isolate dates that could have an effect on unhoused veteran population
important_dates <- read_csv("outside_data/important_dates.csv")
# Demographics: race
# Sheltered data
sheltered_race_data <- 
  read_csv("outside_data/sheltered_race_data.csv")
# Unsheltered data
unsheltered_race_data <- 
  read_csv("outside_data/unsheltered_race_data.csv")
# Total data
total_race_data <-
  read_csv("outside_data/total_race_data.csv") |>
  mutate(Year = as.numeric(gsub("\\*$", "", as.character(Year))))
# Demographics: Gender
# Sheltered data
sheltered_gender_data <- 
  read_csv("outside_data/sheltered_gender_data.csv")
# Unsheltered data
unsheltered_gender_data <- read_csv("outside_data/unsheltered_gender_data.csv")
# Total data
total_gender_data_tidy <- 
  read_csv("outside_data/total_gender_data.csv") |>
  pivot_longer(cols = -Gender, names_to = "Year", values_to = "Percent") |>
  mutate(Gender = as_factor(Gender),
         Year = as.numeric(str_replace(Year, "^X", "")),
         Percent = as.numeric(str_replace(Percent, "%", ""))) |>
  ungroup()
# Demographics: Age
# Sheltered Data
sheltered_age_data <- read_csv("outside_data/sheltered_age_data.csv")
# Unsheltered Data
unsheltered_age_data <- read_csv("outside_data/unsheltered_age_data.csv")
# Total Data
total_age_data_tidy <- read_csv("outside_data/total_age_data.csv") |>
  pivot_longer(cols = -Age.Category, names_to = "Year", values_to = "Percent") |>
  mutate(Year = as.numeric(str_replace(Year, "^X", "")),
         Percent = as.numeric(str_replace(Percent, "%", "")))
############## Plotting Begin
# Plot distribution of unhoused population
unhoused_location_map <- 
  ggplot() +
  geom_sf(data = chi_comm_area, fill = "white", color = "black", size = 0.5) +
  geom_sf(data = joined_data, aes(size = Responses, color = Responses), alpha = 0.7) +
  scale_color_viridis_c(option = "magma") +  
  theme_minimal() +
  labs(title = "Distribution of Chicago Unhoused Population",
       caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
  guides(size = FALSE)
unhoused_location_map
# Plot unhoused reasons
unhoused_reason_plot <- 
  unhoused_reason |>
  ggplot(aes(y = reorder(Cause, -Percent), x = Percent, fill = Cause)) +
  geom_bar(stat = "identity", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_viridis_d(option = "magma") +
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
       caption = "Source: Chicago Point-in-Time County Survey Report, 2023")
# Plot unhoused veteran population over time
magma_colors <- c("#7201A8", "#F26911", "#1E2022")
veteran_pop_plot <- 
  ggplot(unhoused_veterans_nm, aes(x = Year)) +
  geom_smooth(aes(y = Sheltered_Count, color = "Sheltered"), size = 1.1, alpha = 0.8, method = "auto", se = FALSE, span = 0.5) +
  geom_smooth(aes(y = Unsheltered_Count, color = "Unsheltered"), size = 1.1, alpha = 0.8, method = "auto", se = FALSE, span = 0.5) +
  geom_smooth(aes(y = Total_Count, color = "Total"), size = 1.2, alpha = 0.1, method = "auto", se = FALSE, span = 0.5) +
  scale_color_manual(values = setNames(magma_colors, c("Sheltered", "Unsheltered", "Total"))) +
  labs(title = "Veteran Homelessness Over Time",
       y = "Count",
       color = "Population Type",
       caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "gray80", color="transparent"))
# Plot change in racial distribution of unhoused population
race_change_plot <- 
  total_race_data |>
  na.omit() |>
  arrange(Year) |>
  ggplot(aes(x = Year, y = Percent, color = Race)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = magma(length(unique(total_race_data$Race)))) +
  labs(title = "Change in Racial Distribution of Unhoused Population",
       x = "Year",
       y = "Percentage",
       color = "Race",
       caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "gray80", color="transparent"))
# Plot change in gender distribution of unhoused population
gender_change_plot <- 
  total_gender_data_tidy |>
  ggplot(aes(x = Year, y = Percent, color = as.factor(Gender)) ) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = magma(length(unique(total_gender_data_tidy$Gender)))) +
  labs(
    title = "Change in Gender Distribution of Unhoused Population",
    x = "Year",
    y = "Percentage",
    color = "Gender",
    caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "gray80", color="transparent"))
# Plot change in gender distribution of unhoused population
age_change_plot <- 
  total_age_data_tidy |>
  na.omit() |>
  ggplot(aes(x = Year, y = Percent, color = Age.Category)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = magma(length(unique(total_age_data_tidy$Age.Category)))) +
  labs(
    title = "Change in Age Distribution of Unhoused Population",
    x = "Year",
    y = "Percentage",
    color = "Age Category",
    caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "gray80", color="transparent"))
#### Opioid Static Plot ####
##This plot represented overdose rate by zip code with high concentrations of unhoused folks by year. In order to get 2021 and 2022, update plot name, fill variable, and title
### 3 potential plots from this code: 2020 - 2022
opioid_rate_2020 <- 
  ggplot() +
  geom_sf(data = zip_chi_shape) +
  geom_sf(data = opioid_shape, aes(fill = overdose_rate_20), color = NA) +
  labs(title = "Opioid Overdose Rate (Nonfatal and Fatal Per 10,000 Ppl) by Zip in 2020",
       subtitle = "Dot Size Correlated W/ High-Density Unhoused Pop.",
       caption = "Data: IL Dept of Public Health, ACS, PIT Survey") +
  scale_fill_viridis(name="Opioid Overdose Rate", option = "magma", trans = "reverse", 
                     breaks = pretty_breaks(n = 5)) +
  geom_point(data = filter(unhoused_locations_multiyr, Year == 2022), 
             aes(x = Longitude, y = Latitude, size = Responses,),
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
############### Chi Mental Health Map Begin
# point color 
colourCount = length(unique(sf_clean_mental_health$other_languages))
# Maps
bbox <- st_bbox(filtered_census_tracts)

# These are  bounding box coordinates
(xmin <- bbox["xmin"])
(xmax <- bbox["xmax"])
(ymin <- bbox["ymin"])
(ymax <- bbox["ymax"])
expanded_bbox <- c(xmin - 0.01, xmax + 0.01, ymin - 0.01, ymax + 0.01)
# Maps

# Calculate the center of  bounding box
center_lon <- (xmin + xmax) / 2
center_lat <- (ymin + ymax) / 2

zoom_level <- 11  # Start with 11, then adjust as needed

map <- get_googlemap(center = c(lon = center_lon, lat = center_lat), zoom = zoom_level, scale = 2, 
                     maptype = "terrain", size = c(640, 640), 
                     boundingbox = expanded_bbox)
raster_map <- rasterGrob(as.raster(map))

mental_health_lng <- 
  chi_town |>
  ggplot() +
  annotation_custom(raster_map, 
                    xmin = xmin, 
                    xmax = xmax, 
                    ymin = ymin,
                    ymax = ymax) +
  geom_sf(data = filtered_census_tracts, aes(fill = frgn_br), alpha = .75) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma_format()) +
  geom_sf(color = "gold", fill = NA, linewidth = .2) +
  geom_point(data = sf_clean_mental_health, 
             aes(x =bckp_lt, 
                 y = bckp_ln, color =othr_ln),
             shape = 6,
             alpha = .8,
             size = .7) +
  # scale_color_manual(values = colorRampPalette(brewer.pal(8, "Paired"))(colourCount)) + 
  geom_sf_label(data = chi_town, aes(label = community),
                # face = "bold",
                size = 1, color = "gold", fill = "black", 
                label.size = 0.1,
                label.padding = unit(0.15, "lines"),
                alpha = .3) +
  labs(title = "Diversity of Language Services in Chicago's Mental Health Resources",
       subtitle = "Spatial Distribution and Language Diversity of Mental Health Facilities",
       color = "Languages Served",
       fill = "Foreign Population",
       caption = "Sources: Chicago Open Data Portal, 2022 U.S. American Comunity Census (ACS) 5-Year Estimates") +
  theme_map()+
  guides(
    color = guide_legend(
      title.position = "top", 
      title.theme = element_text(size = 10,face = "bold"), 
      legend.text = element_text(size = 9),
      ncol = 2, byrow = TRUE, keywidth = 1, 
      keyheight = 1)) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 5, face = "bold"),
    # plot.caption.position = "panel"
  ) 
# Compile demographic visuals into a single static dashboard
combined_dashboard <- age_change_plot + gender_change_plot + race_change_plot + unhoused_location_map + unhoused_reason_plot +veteran_pop_plot
# Export individual png files for each plot
# Export Age Change Plot
ggsave("age_change_plot.png", path = paste0(path, "/figures"),
       plot = age_change_plot, width = 10, height = 6, units = "in", dpi = 300)
# Export Gender Change Plot
ggsave("gender_change_plot.png", 
       path = paste0(path, "/figures"), plot = gender_change_plot, width = 10, height = 6, units = "in", dpi = 500)
# Export Race Change Plot
ggsave("race_change_plot.png",
       path = paste0(path, "/figures"), plot = race_change_plot, width = 10, height = 6, units = "in", dpi = 500)
# Export Unhoused Location Map
ggsave("unhoused_location_map.png", 
       path = paste0(path, "/figures"), plot = unhoused_location_map, width = 10, height = 6, units = "in", dpi = 500)
# Export Unhoused Reason Plot
ggsave("unhoused_reason_plot.png", 
       path = paste0(path, "/figures"), plot = unhoused_reason_plot, width = 10, height = 6, units = "in", dpi = 500)
# Export Veteran Population Plot
ggsave("veteran_pop_plot.png", 
       path = paste0(path, "/figures"), plot = veteran_pop_plot, width = 10, height = 6, units = "in", dpi = 500)
# Export Combined Dashboard
ggsave("combined_dashboard.png", 
       path = paste0(path, "/figures"), plot = combined_dashboard, width = 20, height = 12, units = "in", dpi = 500)
# Export Overdose
ggsave("Overdose_highdense_2022.png",
       path = paste0(path, "/figures"), plot = opioid_rate_2020, width = 20, height = 12, units = "in", dpi = 500)
# Export mental health
ggsave("mental_health_lng.png",
       path = paste0(path, "/figures"), plot = mental_health_lng, width = 20, height = 12, units = "in", dpi = 500)
