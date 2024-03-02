
library(tidyverse)
library(shiny)
library(sf)
library(rnaturalearth)
library(scales)
library(ggmap)
library(biscale)
library(cowplot)
library(ggplot2)
library(readr)
library(purrr)
library(dplyr)
library(jsonlite)
library(viridis)


##Data Procesing##


##Update Path Here##
 path <- "C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago\\outside_data"
#####


#####OPIOID AND HOUSING DATA PREP#####
 unhoused_dense_22 <- read_csv(file.path(path, "high density unhoused_22.csv"))
 unhoused_dense_21 <- read_csv(file.path(path, "high density unhoused_21.csv"))
 
 age_zip <- read_csv(file.path(path, "acs_med_age_zip.csv"))
 
 edu_zip <- read_csv(file.path(path, "acs_education_zip.csv"))
 
 pop_22 <- read_csv(file.path(path, "population zip_2022.csv"))
 pop_21 <- read_csv(file.path(path, "population_zip_2021.csv"))
 pop_20 <- read_csv(file.path(path, "population_zip_2020.csv"))
 
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
 
 ##add age
 opioid_shape <- merge(opioid_shape, age_zip, by = "zip")
 ##add education
 opioid_shape <- merge(opioid_shape, edu_zip, by = "zip")
 ##add population
 opioid_shape <- merge(opioid_shape, pop_22, by = "zip")
 opioid_shape <- merge(opioid_shape, pop_21, by = "zip")
 opioid_shape <- merge(opioid_shape, pop_20, by = "zip")
 
 
 opioid_shape <- opioid_shape %>% 
   mutate(overdose_rate_22 = Overdose_Count_by_Zip_2022/pop_10000_2022,
          overdose_rate_21 = Overdose_Count_by_Zip_2021/pop_10000_2021,
          overdose_rate_20 = Overdose_Count_by_Zip_2020/pop_10000_2020)
 
 ###This is the opioid file with all the info in it!###
 ##save datafile to folder as csv
 #don't need to run as file already saved to repo
 #write.csv(opioid_data, "opioid_data.csv")
 
 #encampment data into shape
 encampment <- read_csv(file.path(path, "encampment data.csv"))
 
 encampment <- as.data.frame(encampment) %>% 
   st_as_sf(coords=c("Longitude","Latitude"), crs=4326, remove=FALSE)  
 ####
 
 
 # unhoused locations per PITS report 
 # Compile unhoused locations dataframe 
 unhoused_locations_df <- data.frame(
   Location = c("The Loop, River North", "CTA - Red Line (95th/Dan Ryan)", "CTA - Blue Line (Forest Park)",
                "Near West Side/Medical District", "CTA - Blue Line (Cumberland/Rosemont)", "North Side",
                "CTA - Red Line", "South Side (East of State)", "O'Hare Airport", "South Side (West of State)",
                "CTA - Red Line (Howard)", "West Town, Kennedy Expressway", "Midway Airport Terminal", "Northwest Side",
                "CTA - Blue Line (Forest Park)", "CTA - Red Line (95th/Dan Ryan)", "CTA - Red Line (Howard)",
                "Near West Side/Medical District", "The Loop, River North", "North Side", "West Town, Kennedy Expressway", "CTA - Blue Line (Cumberland/Rosemont)",  "CTA - Red Line", "Stevenson Expressway (Archer/Canalport)", "O'Hare Airport"),
   Responses = c(170, 125, 118, 112, 71, 58, 54, 38, 33, 29, 13, 13, 12, 2, 175, 174, 141, 82, 65, 49, 35, 32, 32, 13, 10),
   Year = c(2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023,
            2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022),
   Latitude = c(41.89248568824814, 41.72258849002998, 41.87370675013457, 41.86883204433447, 41.983883465320844, 41.90389519059428,
                41.889900182393696, 41.75042676688248, 41.98037675551297, 41.75042676688248, 42.01889620273381, 41.95930482218085,
                41.78852626097361, 41.88530874443534, 41.87370675013457, 41.72258849002998, 42.01889620273381, 41.86883204433447,
                41.89248568824814, 41.90389519059428, 41.95930482218085, 41.983883465320844, 41.889900182393696, 41.848050, 41.98037675551297),
   Longitude = c(-87.634044880516, -87.62443385767142, -87.81697954232854, -87.67398565436856, -87.83862460424264, -87.63211612952345,
                 -87.62808780459785, -87.63411546128873, -87.90900606226334, -87.63411546128873, -87.67255656226084, -87.74088846041468,
                 -87.74167007391868, -87.79271259245185, -87.81697954232854, -87.62443385767142, -87.67255656226084, -87.67398565436856,
                 -87.634044880516, -87.63211612952345, -87.74088846041468, -87.83862460424264, -87.62808780459785, -87.63485, -87.90900606226334)
 )
 
 # Convert locations dataset into shapefile format
 unhoused_locations <- st_as_sf(unhoused_locations_df,
                                coords = c("Longitude", "Latitude"),  crs=4326, remove = FALSE)
 
 #Don't need to run as file already saved to repo
 #writeOGR(unhoused_locations_shp, dsn = '.', layer = 'poly', driver = "ESRI Shapefile")
 
 ################ End of Opioid Data Prep ######################
 