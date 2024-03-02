
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


##Static Plot##


##### OPIOID STATIC PLOT #####

#Overdose Rate for 2020
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

#plot for 2021 is similar but fill = overdose_rate_21 and for 2022 fill = overdose_rate_22


