
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

age_zip <- read_csv(file.path(path, "acs_med_age_zip.csv"))

edu_zip <- read_csv(file.path(path, "acs_education_zip.csv"))

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

##
##age and opioids
data <- bi_class(opioid_shape_22, x = Overdose_Count_by_Zip_2022, y = Median_age, style = "quantile", dim = 3)

# plot
age_opi_plot <- ggplot() +
  geom_sf(data = opioid_shape_22) +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Overdose Count and Median Age",
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


age_opi_plot <- ggdraw() +
  draw_plot(age_opi_plot, 0, 0, 1, 1) +
  draw_plot(legend, x= 0.5, y=0.4, 0.4, 0.4)

print(age_opi_plot)










##shiny
#manipulate year in drop down to select which column in opioid dataframe


