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

path <- "/Users/josemacias/Documents/GitHub/Housing-and-Migrants-Chicago"
setwd(path)

############### API datasets
me_health_res_link <- "https://data.cityofchicago.org/resource/wust-ytyg.json"
# 1 Mental health resources
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
# 2 Affordable housing deve
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
  select(community_area,property_name,property_type,units,proper_latitude,proper_longitude,backup_lat,backup_long) |>
  rename(latitude=proper_latitude,longitude= proper_longitude)
  
# 3 Chi Town shape

chi_town <- st_read("https://data.cityofchicago.org/resource/igwz-8jzy.geojson") |>
  select(community, geometry)

# 4 Chicago zipcode shapefiles
zip_chi_shape <- st_read("https://data.cityofchicago.org/resource/unjd-c2ca.geojson")

# 5 Census Data
options(timeout = 999)

vars <- c(
  "B01003_001", # Total Pop
  "B01001B_001", # Black Pop
  "B01001I_001", # Hispanic Pop
  "B07011_001" # Median Income
)
census_data <- 
  get_acs(geography = "tract", 
          variables = vars, 
          state = "IL", 
          county = "Cook", 
          geometry = T,
          output = "wide",
          year = 2022) |>
  rename(black_pop = B01001B_001E,latino_pop = B01001I_001E, total_pop = B01003_001E, median_income = B07011_001E) |>
  select(-c(B01003_001M,B01001I_001M,B01001B_001M,B07011_001M))
############### API Datasets End
############### Spatial Work Begin
# Census Tract: change crs to chicago in census data
chi_town_crs <- st_crs(chi_town)
census_data <- st_transform(census_data, crs = chi_town_crs)
intersections <- st_intersects(census_data, chi_town, sparse = FALSE)
filtered_census_tracts <- census_data[rowSums(intersections) > 0, ] # filter for chicago tracts

# Spatial Merges with Chi Shape
# 1 Mental Health shape
# The lat and long of resources according to Comm Area
sf_clean_mental_health <- 
  clean_mental_health |>
  st_as_sf(coords = c("latitude","longitude"),
                      crs = st_crs(chi_town)) |>
  st_join(chi_town) |>
  filter(!is.na(community))

# The lat and long of resources according to census tract
sf_clean_mental_health_census_tracts <- 
  clean_mental_health |>
  st_as_sf(coords = c("latitude","longitude"),
           crs = st_crs(chi_town)) |>
  st_join(filtered_census_tracts) |>
  select(14:10, 1:13)

# Counts of Resources by Comm Area
counts_of_mental_resources <-
  sf_clean_mental_health |>
  st_drop_geometry() |>
  summarise(.by = community,
            total_mental_health_resources = n())

sf_mental_health_census_aggregate_comm_area <-
  left_join(chi_town,counts_of_mental_resources, join_by(community))

# Counts of Resources by tract
counts_of_mental_resources_tract <-
  sf_clean_mental_health_census_tracts |>
  st_drop_geometry() |>
  summarise(.by = GEOID,
            total_mental_health_resources = n())

sf_mental_health_census_aggregate_census_tracts <-
  left_join(filtered_census_tracts,counts_of_mental_resources_tract, join_by(GEOID))

# 2 Affordable housing shape
# The lat and long of Affordable housing according to Comm Area
sf_af_house_dev <- 
  af_house_dev |>
  st_as_sf(coords = c("latitude","longitude"),
           crs = st_crs(chi_town))|>
  st_join(chi_town) |>
  filter(!is.na(community)) |>
  select(-community_area) |>
  relocate(community, .before = 1)
# The lat and long of Affordable housing according to tract
sf_af_house_dev_tract <- 
  af_house_dev |>
  st_as_sf(coords = c("latitude","longitude"),
           crs = st_crs(chi_town)) |>
  st_join(filtered_census_tracts) |>
  select(7:13,1:6)
# Counts of AF by Comm Area
counts_of_affordable_housing <-
  sf_af_house_dev |>
  st_drop_geometry() |>
  summarise(.by = community,
            total_affordable_housing = sum(units))

sf_counts_of_affordable_housing_aggregate_comm_area <-
  left_join(chi_town,counts_of_affordable_housing, join_by(community))

# Counts of AF by tract
counts_of_affordable_housing_tract <-
  sf_af_house_dev_tract |>
  st_drop_geometry() |>
  summarise(.by = GEOID,
            total_affordable_housing = sum(units))
sf_counts_of_affordable_housing_aggregate_tract <-
  left_join(filtered_census_tracts,counts_of_affordable_housing_tract, join_by(GEOID))

# 3 Opoid Merge by Emma
data20 <- 
  read_csv(file.path(path, "outside_data/overdose counts 2020.csv"))
data21 <- 
  read_csv(file.path(path, "outside_data/overdose counts 2021.csv"))
data22 <- 
  read_csv(file.path(path, "outside_data/overdose counts 2022.csv"))
opioid_data <- 
  reduce(list(data20, data21, data22), full_join) |>
  mutate(zip = as.character(Patient_Zip))
opioid_shape <- 
  left_join(zip_chi_shape,
                          opioid_data, join_by(zip)) |>
  select(-c(shape_area,shape_len, objectid,Patient_Zip))
############### Spatial Work End
############### Writing out all SF Data Frames
# commarea shape
st_write(chi_town, 
         "shape_files/chicago_commarea/chicago_shape.shp", append=FALSE)
# census shape
st_write(filtered_census_tracts, 
         "shape_files/chicago_tracts/chicago_tracts.shp", append=FALSE)
# zip shape
st_write(zip_chi_shape, 
         "shape_files/chicago_zipcodes/chicago_zipcodes.shp", append=FALSE)
# Opioid data as a GeoPackage ############### 
st_write(opioid_shape, 
         "shape_files/opioid_zip/opioid_shape.gpkg", driver = "GPKG", append=FALSE)
# Mental Health data as a Shapes ############### 
st_write(sf_clean_mental_health, # locations in community area
         "shape_files/mental_health/comm_area_locations_mental_health_.shp", append=FALSE)
st_write(sf_clean_mental_health_census_tracts, # locations in census tracts
         "shape_files/mental_health/censustract_locations_mental_health_.shp", append=FALSE)
st_write(sf_mental_health_census_aggregate_comm_area, # total by community area
         "shape_files/mental_health/counts_of_mental_health_resources_by_comm_area.shp", append=FALSE)
st_write(sf_mental_health_census_aggregate_census_tracts, # total by census tracts
         "shape_files/mental_health/counts_of_mental_health_resources_by_census_tract.shp", append=FALSE)
# Affordable Housing data as a Shapes ############### 
st_write(sf_af_house_dev, # Affordable housing locations in community areas
         "shape_files/af_dev/comm_area_locations_af_housing_.shp", append=FALSE)
st_write(sf_af_house_dev_tract, # Affordable housing locations by tract
         "shape_files/af_dev/censustract_locations_af_housing_.shp", append=FALSE)
st_write(sf_counts_of_affordable_housing_aggregate_comm_area, # total units by community area
         "shape_files/af_dev/counts_of_af_housing_by_comm_area.shp", append=FALSE)
st_write(sf_counts_of_affordable_housing_aggregate_tract,  # total units by census tracts
         "shape_files/af_dev/counts_of_affordable_housing_by_census_tract.shp", append=FALSE)

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
