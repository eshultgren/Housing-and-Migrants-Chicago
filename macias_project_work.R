# Project 
library(tidyverse)
library(jsonlite)
library(sf)
library(tidycensus)
library(ggthemes)
library(RColorBrewer)
library(ggmap)
library(grid)

############## Google Map Layer
google_api_key <- "AIzaSyAmWiRl8MXntcSV-YrTDc7DrgmjyPz7gzg"
register_google(key = google_api_key)
############### Google Map Layer End

path <- "/Users/josemacias/Desktop/Harris Work/Winter 2024/R Programming 2"
setwd(path)

############### API datasets
me_health_res_link <- "https://data.cityofchicago.org/resource/wust-ytyg.json"
# Mental health resources
mental_health <- 
  fromJSON(me_health_res_link, simplifyVector = TRUE)
# pullling lat and long from nested data frame
latitude <- # unpacking a datafram within the pull
  sapply(mental_health$location$coordinates, function(coords) coords[1])
longitude <- # unpacking a datafram within the pull
  sapply(mental_health$location$coordinates, function(coords) coords[2])
mental_health$longitude <- longitude # saving lat from former data frame
mental_health$latitude  <- latitude # saving lat from former data frame
# Final Mental Health
clean_mental_health <- 
  mental_health |>
  select(-location) |>
  mutate(backup_lat = latitude,  # saving in case we turn into a simple frame
         backup_long = longitude, # saving incase we turn into a simple frame
         other_languages = case_when(
           other_languages == "Spanish;" ~ "Spanish",
           other_languages == "Spanish;Korean;" ~ "Spanish; Korean",
           other_languages == "Polish; Spanish" ~ "Spanish; Polish",
           other_languages == "Amharic; Arabic; Bengali; Bosnian; Dari; Farsi; French; Gujarati; Kurdish; Malayalam; Nepali; Pashto; Spanish; Tagalog; Urdu" ~ "Amharic; Arabic; Bengali; Bosnian; Dari;\nFarsi; French; Gujarati; Kurdish;\nMalayalam; Nepali; Pashto; Spanish; Tagalog; Urdu",
           is.na(other_languages) ~ "Information Not Available",
           TRUE ~ other_languages  # Default case
         )) 
# Affordable housing deve
af_house_link <- "https://data.cityofchicago.org/resource/s6ha-ppgi.json"

af_house_dev <- 
  fromJSON(af_house_link, simplifyVector = TRUE) |>
  select(1:14, -c(location,x_coordinate,y_coordinate)) |>
  mutate(
    units = as.numeric(units),
    proper_latitude = longitude,
         proper_longitude = latitude,
         backup_lat = proper_latitude, # saving incase we turn into a simple frame
         backup_long = proper_longitude) |> # saving incase we turn into a simple frame
  select(community_area,property_type,units,proper_latitude,proper_longitude,backup_lat,backup_long) |>
  rename(latitude=proper_latitude,longitude= proper_longitude)
  
# Chi Town shape

chi_town <- st_read("https://data.cityofchicago.org/resource/igwz-8jzy.geojson") %>%
  select(community, geometry)
############### API Datasets End
############### Spatial Work Begin
# Spatial Merges with Chi Shape
sf_clean_mental_health <- 
  clean_mental_health |>
  st_as_sf(coords = c("latitude","longitude"),
                      crs = st_crs(chi_town)) |>
  st_join(chi_town) |>
  filter(!is.na(community))
############### NOT FINISHED FEB 3 2024
sf_af_house_dev <- 
  af_house_dev |>
  st_as_sf(coords = c("latitude","longitude"),
           crs = 4326) 

chi_town |>
ggplot() +
  geom_sf() +
  geom_sf(data = sf_af_house_dev, aes(size = units), alpha = .3) 
############### NOT FINISHED FEB 3 2024
# Census Data
options(timeout = 999)

vars <- c(
  "B01003_001", # Total Pop
  "B01001B_001", # Black Pop
  "B01001I_001", # Hispanic Pop
  "B07011_001" # Median Income
  )
acs_vars_table <- load_variables(2022,dataset = "acs5")
census_data <- 
  get_acs(geography = "tract", 
                       variables = vars, 
                       state = "IL", 
                       county = "Cook", 
                       geometry = T,
                       output = "wide",
                       year = 2022) |>
  rename(black_pop = B01001B_001E,latino_pop = B01001I_001E, total_pop = B01003_001E, median_income = B07011_001E)

# change crs to chicago in census data
chi_town_crs <- st_crs(chi_town)
census_data <- st_transform(census_data, crs = chi_town_crs)
# chicago track info
chi_tract_link <- "https://data.cityofchicago.org/resource/74p9-q2aq.json"
chi_track_sf <-
  fromJSON(chi_tract_link)
# filter for chicago tracts
intersections <- st_intersects(census_data, chi_town, sparse = FALSE)
filtered_census_tracts <- census_data[rowSums(intersections) > 0, ]
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
############### Spatial Work End

############### Chi Mentla Health Map Begin

chi_town |>
  ggplot() +
  annotation_custom(raster_map, 
                    xmin = xmin, 
                    xmax = xmax, 
                    ymin = ymin,
                    ymax = ymax) +
  geom_sf(data = filtered_census_tracts, aes(fill = black_pop), alpha = .75) +
  scale_fill_viridis_c(labels = scales::comma_format()) +
  geom_sf(color = "gold", fill = NA, linewidth = .2) +
  geom_point(data = sf_clean_mental_health, 
             aes(x =backup_lat, 
                 y = backup_long, color =other_languages),
             shape = 6,
             alpha = .8,
             size = .7) +
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Paired"))(colourCount)) + 
  geom_sf_label(data = chi_town, aes(label = community),
                # face = "bold",
                size = 1, color = "gold", fill = "black", 
                label.size = 0.1,
                label.padding = unit(0.15, "lines"),
                alpha = .3) +
  labs(title = "Diversity of Language Services in Chicago's Mental Health Resources",
       subtitle = "Spatial Distribution and Language Diversity of Mental Health Facilities",
       color = "Languages Served",
       fill = "Black Population",
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

ggsave("chi_town_health.png",plot = last_plot(), width = 10, height = 6, bg = "white", dpi = 500)


############### Chi Mental Health Map End
############### Chi Affordable Housing Development Map Begin
chi_town |>
  ggplot() +
  annotation_custom(raster_map, 
                    xmin = xmin, 
                    xmax = xmax, 
                    ymin = ymin,
                    ymax = ymax) +
  geom_sf(data = filtered_census_tracts, aes(fill = black_pop), alpha = .75) +
  scale_fill_viridis_c(labels = scales::comma_format()) +
  geom_sf(color = "gold", fill = NA, linewidth = .2) +
  geom_point(data = sf_clean_mental_health, 
             aes(x =backup_lat, 
                 y = backup_long, color =other_languages),
             shape = 6,
             alpha = .8,
             size = .7) +
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Paired"))(colourCount)) + 
  geom_sf_label(data = chi_town, aes(label = community),
                # face = "bold",
                size = 1, color = "gold", fill = "black", 
                label.size = 0.1,
                label.padding = unit(0.15, "lines"),
                alpha = .3) +
  labs(title = "Diversity of Language Services in Chicago's Mental Health Resources",
       subtitle = "Spatial Distribution and Language Diversity of Mental Health Facilities",
       color = "Languages Served",
       fill = "Black Population",
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




