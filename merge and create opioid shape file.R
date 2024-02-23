
library(tidyverse)
library(shiny)
library(sf)
library(rnaturalearth)
library(viridis)
library(scales)
library(ggmap)
library(biscale)
library(cowplot)
library(ggplot2)
library(readr)
library(purrr)
library(dplyr)
library(jsonlite)

##Path##
path <- "C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago\\outside_data"
#####

unhoused_dense_22 <- read_csv(file.path(path, "high density unhoused_22.csv"))
unhoused_dense_21 <- read_csv(file.path(path, "high density unhoused_21.csv"))

data20 <- read_csv(file.path(path, "overdose counts 2020.csv"))
data21 <- read_csv(file.path(path, "overdose counts 2021.csv"))
data22 <- read_csv(file.path(path, "overdose counts 2022.csv"))

opioid_data <- reduce(list(data20, data21, data22), full_join)

#rename column
opioid_data <- opioid_data %>% 
  rename(
    zip = Patient_Zip)

#download Chicago neighborhood shapefiles
zip_chi_shape <- st_read("https://data.cityofchicago.org/resource/unjd-c2ca.geojson")

opioid_shape <- merge(zip_chi_shape, opioid_data, by = "zip")

##would be great if we could save this mereged file
st_write(opioid_shape,
         "opioid_shape.shp",
         driver="ESRI Shapefile")


###mapping opioids by year
##2022
##select corresponding column

opioid_2022 <- ggplot() +
  geom_sf(data = zip_chi_shape) +
  geom_sf(data = opioid_shape, aes(fill = Overdose_Count_by_Zip_2022), color = NA) +
  labs(title = "Overdose Count by Zip in 2022",
       subtitle = "Counts combine both nonfatal and fatal overdose data.",
       caption = "Data: IL Dept of Public Health") +
  scale_fill_distiller(name="Opioid Overdose Count", palette = "BuPu", trans = "reverse", 
                       breaks = pretty_breaks(n = 5)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5
  )

print(opioid_2022)


##add element

path <- "C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago\\shape_files\\chicago_tracts\\"

tract_shape <- st_read(file.path(path, 
                                 "chicago_tracts.shp"))

high_dense_shape_22 <- merge(tract_shape, unhoused_dense_22, by = "GEOID")
high_dense_shape_21 <- merge(tract_shape, unhoused_dense_21, by = "GEOID")

##Plot 2022
opioid_unhoused_2022 <- ggplot() +
  geom_sf(data = zip_chi_shape) +
  geom_sf(data = opioid_shape, aes(fill = Overdose_Count_by_Zip_2022), color = NA) +
  geom_sf(data = high_dense_shape_22, fill = "darkgoldenrod2", fill = NA) +
  labs(title = "Overdose Count (Nonfatal and Fatal) by Zip in 2022",
       subtitle = "Yellow Indicates High-Density Populations of Unhoused People",
       caption = "Data: IL Dept of Public Health") +
  scale_fill_distiller(name="Opioid Overdose Count", palette = "BuPu", trans = "reverse", 
                       breaks = pretty_breaks(n = 5)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0, size= 13),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5
  )

print(opioid_unhoused_2022)

##Plot 2021
opioid_unhoused_2021 <- ggplot() +
  geom_sf(data = zip_chi_shape) +
  geom_sf(data = opioid_shape, aes(fill = Overdose_Count_by_Zip_2021), color = NA) +
  geom_sf(data = high_dense_shape_21, fill = "darkgoldenrod2", fill = NA) +
  labs(title = "Overdose Count (Nonfatal and Fatal) by Zip in 2021",
       subtitle = "Yellow Indicates High-Density Populations of Unhoused People",
       caption = "Data: IL Dept of Public Health") +
  scale_fill_distiller(name="Opioid Overdose Count", palette = "BuPu", trans = "reverse", 
                       breaks = pretty_breaks(n = 5)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0, size= 13),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5
  )

print(opioid_unhoused_2021)


##shiny
#manipulate year in drop down to select which column in opioid dataframe


