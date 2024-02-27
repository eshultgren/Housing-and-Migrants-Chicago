
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
library(viridis)

##Path##
path <- "C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago\\outside_data"
#####

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


##would be great if we could save this mereged file
st_write(opioid_shape,
         "opioid_shape.shp",
         driver="ESRI Shapefile")
####

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

writeOGR(unhoused_locations, dsn = '.', layer = 'poly', driver = "ESRI Shapefile")


##
# Define color palette
bvColors <- c("#be64ac", "#8c62aa", "#3b4994", "#dfb0d6", "#a5add3", "#5698b9", "#e8e8e8", "#ace4e4", "#5ac8c8")

magma <- magma(n, alpha = 1, begin = 0, end = 1, direction = 1)

###mapping opioids by year
##2022
##select corresponding column

opioid_2022 <- ggplot() +
  geom_sf(data = zip_chi_shape) +
  geom_sf(data = opioid_shape, aes(fill = Overdose_Count_by_Zip_2022), color = NA) +
  labs(title = "Overdose Count by Zip in 2022",
       subtitle = "Counts combine both nonfatal and fatal overdose data.",
       caption = "Data: IL Dept of Public Health") +
  scale_fill_distiller(name="Opioid Overdose Count", palette = , trans = "reverse", 
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

##Plot 2022 Count
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


##Plot 2022 RATE
opioid_rate_2022 <- ggplot() +
  geom_sf(data = zip_chi_shape) +
  geom_sf(data = opioid_shape, aes(fill = overdose_rate_22), color = NA) +
  geom_sf(data = high_dense_shape_22, fill = "darkgoldenrod2") +
  labs(title = "Overdose Rate (Nonfatal and Fatal Per 10,000) by Zip in 2022",
       subtitle = "Yellow are areas w/ high-density unhoused pop. Black dots are encampments.",
       caption = "Data: IL Dept of Public Health, ACS") +
  scale_fill_distiller(name="Opioid Overdose Rate", palette = "BuPu", trans = "reverse", 
                       breaks = pretty_breaks(n = 5)) +
  geom_point(data=encampment, aes(x=`Longitude`, y=`Latitude`, size=Num_Ppl)) +
  labs(size = "Size of Encampment") +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0, size= 13),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5
  )

print(opioid_rate_2022)


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

##education and opioids
opioid_shape_22 <- opioid_shape %>% 
  filter(!is.na(Overdose_Count_by_Zip_2022))

data <- bi_class(opioid_shape_22, x = Overdose_Count_by_Zip_2022, y = perc_college_or_more, style = "quantile", dim = 3)

# plot
edu_opi_plot <- ggplot() +
  geom_sf(data = opioid_shape_22) +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Overdose Count and Percent College Educated",
    subtitle = "By Zip 2022",
    caption = "Data: IDPH, ACS"
  ) +
  bi_theme() +
  theme(
    plot.title = element_text(hjust = 0.1, size= 15),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 12)
  )

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "+ Overdose Count",
                    ylab = "+ % College Educated",
                    size = 12)


edu_opi_plot <- ggdraw() +
  draw_plot(edu_opi_plot, 0, 0, 1, 1) +
  draw_plot(legend, x= 0.5, y=0.4, 0.4, 0.4)

print(edu_opi_plot)

##BI-CLASS ##
##age and opioids

##define variables on legend
data <- bi_class(opioid_shape_22, x = Overdose_Count_by_Zip_2022, y = Median_age, style = "quantile", dim = 3)

# plot
age_opi_plot <- ggplot() +
  geom_sf(data = opioid_shape_22) +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) + #show legend= FALSE
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Overdose Count and Median Age",
    subtitle = "By Zip 2022",
    caption = "Data: IDPH, ACS"
  ) +
  bi_theme() + ##Bi-Theme
  theme(
    plot.title = element_text(hjust = 0.1, size= 15),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 12)
  )

legend <- bi_legend(pal = "GrPink", #build legend
                    dim = 3,
                    xlab = "+ Overdose Count",
                    ylab = "+ % College Educated",
                    size = 12)


age_opi_plot <- ggdraw() + #draw legend next to plot
  draw_plot(age_opi_plot, 0, 0, 1, 1) +
  draw_plot(legend, x= 0.5, y=0.4, 0.4, 0.4)

print(age_opi_plot)

################Final Plots############
##Opioid Overdose Rate
##Areas of Concentrated Unhoused
##By Year (2020-2021)

##Plot RATE
opioid_rate_2020 <- ggplot() +
  geom_sf(data = zip_chi_shape) +
  geom_sf(data = opioid_shape, aes(fill = overdose_rate_20), color = NA) +
  labs(title = "Opioid Overdose Rate (Nonfatal and Fatal Per 10,000 Ppl) by Zip in 2020",
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

print(opioid_rate_2020)

print(opioid_rate_2021)

print(opioid_rate_2022)

