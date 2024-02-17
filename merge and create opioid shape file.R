
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

##Path##
path <- "C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago"
#####

data20 <- read_csv(file.path(path, "overdose counts 2020.csv"))
data21 <- read_csv(file.path(path, "overdose counts 2021.csv"))
data22 <- read_csv(file.path(path, "overdose counts 2022.csv"))

opioid_data <- reduce(list(data20, data21, data22), full_join)

#rename column
opioid_data <- opioid_data %>% 
  rename(
    zip = Patient_Zip)

##Shape Path##
path <- "C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago\\Boundaries - ZIP Codes"
####

#download Chicago neighborhood shapefiles
zip_chi_shape <- st_read(file.path(path, 
                                  "geo_export_32855347-20ff-48e3-9078-cead470bbf31.shp"))

opioid_shape <- merge(zip_chi_shape, opioid_data, by = "zip")

st_write(opioid_shape,
         "opioid_shape.shp",
         driver="ESRI Shapefile")
