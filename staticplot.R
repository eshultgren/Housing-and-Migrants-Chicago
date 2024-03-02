##Static Plot##

# The first part of this script uses data extracted from the 2023 Point in Time
# Survey Report to generate a set of static plots to visualize the demographic
# changes in Chicago's unhoused population over time (2005-2023).

# Clear workspace and set working directory
rm(list = ls())
setwd("C:/Users/higgi/Documents/GitHub/Housing-and-Migrants-Chicago/outside_data")

library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(leaflet.extras)
library(tmap)
library(tmaptools)
library(sf)
library(ggmap)
library(osmdata)
library(patchwork)
library(dplyr)
library(tidyr)
library(viridis)
library(stringr)

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

chi_comm_area  <- st_read("https://data.cityofchicago.org/resource/igwz-8jzy.geojson") |>
  select(community, geometry)

# Specify zipfile path
zip_path <- "Boundaries - Community Areas (current).zip"

# Check and transform coordinate reference system
if (!identical(st_crs(unhoused_locations), st_crs(chi_comm_area))) {
  # Transform the unhoused_locations dataset to match the CRS of chi_comm_area
  unhoused_locations <- st_transform(unhoused_locations, st_crs(chi_comm_area))
}

# Perform spatial join
joined_data <- st_join(unhoused_locations, chi_comm_area)

# Plot distribution of unhoused population

unhoused_location_map <- ggplot() +
  geom_sf(data = chi_comm_area, fill = "white", color = "black", size = 0.5) +
  geom_sf(data = joined_data, aes(size = Responses, color = Responses), alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Chicago Unhoused Population") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +  # Center the title
  scale_color_viridis_c(option = "magma") +  # Apply magma color scale
  labs(caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  guides(size = FALSE)  # Remove size legend

unhoused_location_map

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
unhoused_reason_plot <- ggplot(unhoused_reason, aes(y = reorder(Cause, -Percent), x = Percent, fill = Cause)) +
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

# Veteran visuals

unhoused_veterans <- data.frame(
  Year = c(2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2009, 2007, 2005),
  Sheltered_Count = c(202, 203, 171, 286, 279, 296, 304, 399, 346, 465, 406, 270, 250, 242, NA, NA),
  Sheltered_PercentChange = c(-0.50, 18.70, -40.20, 2.50, -5.70, -2.60, -23.80, 15.30, -25.60, 14.50, 50.40, 8.00, 3.30, NA, NA, NA),
  Unsheltered_Count = c(115, 119, 77, 201, 120, 198, 245, 202, 406, 256, 306, NA, 250, 115, NA, NA),
  Unsheltered_PercentChange = c(-3.30, 9.40, 5.30, 13.30, 11.90, 14.60, 15.70, 16.40, 19.20, 26.80, 25.50, NA, 15.10, 14.10, 18.40, 16),
  Total_Count = c(317, 322, 248, 487, 399, 494, 549, 601, 752, 721, 712, NA, 500, 357, NA, NA),
  Total_PercentChange = c(-1.60, 8.30, 5.50, 11.40, 10.60, 11.70, 12.20, 13.20, 14.90, 16.00, 15.50, NA, 10.10, 9.20, 15.40, 11)
)

# Define colors within magma palette to be used for longitudinal veteran plot
magma_colors <- c("#7201A8", "#F26911", "#1E2022")

# Drop missing values from dataset prior to plotting
unhoused_veterans_nm <- unhoused_veterans %>%
  drop_na()

# Plot unhoused veteran population over time

veteran_pop_plot <- ggplot(unhoused_veterans_nm, aes(x = Year)) +
  geom_smooth(aes(y = Sheltered_Count, color = "Sheltered"), size = 1.1, alpha = 0.8, method = "auto", se = FALSE, span = 0.5) +
  geom_smooth(aes(y = Unsheltered_Count, color = "Unsheltered"), size = 1.1, alpha = 0.8, method = "auto", se = FALSE, span = 0.5) +
  geom_smooth(aes(y = Total_Count, color = "Total"), size = 1.2, alpha = 0.1, method = "auto", se = FALSE, span = 0.5) +
  labs(title = "Veteran Homelessness Over Time",
       y = "Count",
       color = "Population Type",
       caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  scale_color_manual(values = setNames(magma_colors, c("Sheltered", "Unsheltered", "Total"))) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "gray80", color="transparent"))

# Isolate dates that could have an effect on unhoused veteran population
important_dates <- data.frame(date = as.Date(c("2011-12-15", "2021-08-30")), label = c("End of Iraq War", "US withdrawal from Afghanistan"))

# Demographics: race

# Sheltered data
sheltered_race_data <- data.frame(
  Race = rep(c("NonLatino/a/x Black", "NonLatino/a/x White", "NonLatino/a/x Asian", "NonLatino/a/x Multiple", "NonLatino/a/x Other", "Latino/a/x"), each = 16),
  Year = rep(c("2023**", "2022*", "2021*", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2009", "2007", "2005"), times = 6),
  Percent = c(41.20, 73.00, 77.50, 78.60, 78.80, 69.10, 74.60, 76.30, 76.00, 75.60, 76.30, 78.00, 79.00, 80.00, 76.00, 73.00,
              6.50, 20.40, 18.50, 18.10, 18.30, 25.40, 21.40, 19.40, 20.50, 22.10, 21.60, 20.00, 20.00, 17.10, 15.00, 12.00,
              0.80, 1.10, 0.90, 1.30, 0.90, 0.90, 0.70, 1.00, 0.90, 1.30, 1.40, 1.20, 1.00, 1.30, 0.40, 1.00,
              1.00, 3.50, 1.30, 0.80, 0.60, 2.90, 1.10, 2.30, 2.30, NA, NA, NA, NA, NA, NA, NA,
              0.00, 2.00, 1.80, 1.20, 1.30, 1.70, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
              50.50, 13.20, 11.50, 12.30, 10.80, 11.00, 9.90, 9.50, 10.10, 9.30, 10.00, 11.00, 10.00, 12.00, 6.00, 8.00)
)

# Unsheltered data
unsheltered_race_data <- data.frame(
  Race = rep(c("NonLatino/a/x Black", "NonLatino/a/x White", "NonLatino/a/x Asian", "NonLatino/a/x Multiple", "NonLatino/a/x Other", "Latino/a/x"), each = 16),
  Year = rep(c("2023**", "2022*", "2021*", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2009", "2007", "2005"), times = 6),
  Percent = c(65.60, 81.80, 64.20, 72.80, 73.60, 73.50, 75.90, 71.80, 72.10, 73.80, 73.30, NA, 77.00, 76.00, 74.00, 73.00,
              21.40, 17.70, 27.60, 25.40, 23.40, 22.50, 23.10, 26.00, 23.10, 23.90, 24.90, NA, 22.00, 23.00, 18.00, 17.00,
              0.70, 0.30, 1.60, 0.70, 0.90, 0.70, 0.60, 0.20, 0.40, 0.50, 0.30, NA, 0.50, 1.00, 1.00, 0.00,
              0.20, 0.20, 4.80, 0.40, 0.60, 1.80, 0.40, 1.30, 3.30, NA, NA, NA, NA, NA, NA, NA,
              0.00, 0.00, 1.80, 0.70, 1.40, 1.50, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
              12.10, 7.60, 14.00, 9.20, 10.20, 11.00, 6.10, 12.80, 10.90, 9.20, 6.10, NA, 7.20, 9.00, 5.00, 7.00)
)

# Total data
total_race_data <- data.frame(
  Race = rep(c("NonLatino/a/x Black", "NonLatino/a/x White", "NonLatino/a/x Asian", "NonLatino/a/x Multiple", "NonLatino/a/x Other", "Latino/a/x"), each = 16),
  Year = rep(c("2023**", "2022*", "2021*", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2009", "2007", "2005"), times = 6),
  Percent = c(45.10, 75.90, 75.00, 77.00, 77.40, 70.50, 75.00, 75.00, 74.50, 75.20, 75.60, NA, 78.00, 79.20, 75.00, 73.00,
              8.90, 19.50, 20.20, 20.20, 19.70, 24.50, 22.00, 21.30, 21.30, 22.50, 22.30, NA, 20.00, 18.00, 16.00, 14.00,
              0.80, 0.80, 1.00, 1.10, 0.90, 0.80, 0.70, 0.80, 0.70, 1.10, 1.10, NA, 1.10, 1.20, 0.40, 0.60,
              0.90, 2.50, 2.00, 0.70, 0.60, 2.60, 0.80, 2.00, 2.70, NA, NA, NA, NA, NA, NA, NA,
              0.90, 2.50, 2.00, 0.70, 0.60, 2.60, 0.80, 2.00, 2.70, NA, NA, NA, NA, NA, NA, NA,
              44.30, 11.40, 12.00, 11.40, 10.20, 11.00, 8.60, 10.50, 10.40, 10.00, 9.10, NA, 10.20, 11.00, 6.00, 7.00)
)

# Clean up asterisks included in Year column (run both lines to account for multiple *)
total_race_data$Year <- as.numeric(gsub("\\*$", "", as.character(total_race_data$Year)))
total_race_data$Year <- as.numeric(gsub("\\*$", "", as.character(total_race_data$Year)))

# Plot change in racial distribution of unhoused population
race_change_plot <- total_race_data %>%
  na.omit() %>%
  arrange(Year) %>%
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

# Demographics: Gender

# Sheltered data
sheltered_gender_data <- data.frame(
  Gender = c("Female", "Male", "Transgender", "Other Gender"),
  `2023**` = c("32.70%", "66.70%", "0.60%", "0.20%"),
  `2022*` = c("40.40%", "58.60%", "0.70%", "0.30%"),
  `2021*` = c("45.40%", "54.30%", "0.20%", "0.10%"),
  `2020` = c("44.70%", "54.80%", "0.40%", "0.10%"),
  `2019` = c("45.00%", "54.40%", "0.50%", "0.20%"),
  `2018` = c("41.00%", "58.10%", "0.50%", "0.40%"),
  `2017` = c("42.20%", "57.30%", "0.30%", NA),
  `2016` = c("40.00%", "59.50%", "0.50%", NA),
  `2015` = c("43.10%", "56.50%", "0.50%", NA),
  `2014` = c("42.10%", "57.90%", NA, NA),
  `2013` = c("42.70%", "57.30%", NA, NA),
  `2012` = c("43%", "57%", NA, NA),
  `2011` = c("42%", "58%", NA, NA),
  `2009` = c("43%", "57%", NA, NA),
  `2007` = c("35%", "65%", NA, NA),
  `2005` = c("41%", "59%", NA, NA)
)

# Unsheltered data
unsheltered_gender_data <- data.frame(
  Gender = c("Female", "Male", "Transgender", "Other Gender"),
  `2023**` = c("23.10%", "76.80%", "0.00%", "0.10%"),
  `2022*` = c("14.80%", "84.90%", "0.30%", "0.00%"),
  `2021*` = c("18.20%", "81.80%", "0.00%", "0.00%"),
  `2020` = c("21.30%", "78.70%", "0.00%", "0.00%"),
  `2019` = c("19.50%", "79.70%", "0.60%", "0.20%"),
  `2018` = c("15.30%", "84.70%", "0.00%", "0.00%"),
  `2017` = c("16.80%", "83.20%", "0.10%", NA),
  `2016` = c("16.20%", "83.50%", "0.30%", NA),
  `2015` = c("13.20%", "86.70%", "0.10%", NA),
  `2014` = c("18.10%", "81.90%", NA, NA),
  `2013` = c("18.50%", "81.50%", NA, NA),
  `2012` = c(NA, NA, NA, NA),
  `2011` = c("20%", "80%", NA, NA),
  `2009` = c("19%", "81%", NA, NA),
  `2007` = c("18%", "82%", NA, NA),
  `2005` = c("22%", "78%", NA, NA)
)

# Total data
total_gender_data <- data.frame(
  Gender = c("Female", "Male", "Transgender", "Other Gender"),
  `2023**` = c("68.20%", "68.20%", "0.20%", "0.20%"),
  `2022*` = c("67.20%", "67.20%", "0.20%", "0.20%"),
  `2021*` = c("59.50%", "59.50%", "0.10%", "0.10%"),
  `2020` = c("61.60%", "61.60%", "0.10%", "0.10%"),
  `2019` = c("61.10%", "61.10%", "0.20%", "0.20%"),
  `2018` = c("66.60%", "66.60%", "0.30%", "0.30%"),
  `2017` = c("66.30%", "66.30%", NA, NA),
  `2016` = c("66.00%", "66.00%", NA, NA),
  `2015` = c("67.90%", "67.90%", NA, NA),
  `2014` = c("62.80%", "62.80%", NA, NA),
  `2013` = c("63.30%", "63.30%", NA, NA),
  `2012` = c(NA, NA, NA, NA),
  `2011` = c("65%", "65%", NA, NA),
  `2009` = c("61%", "61%", NA, NA),
  `2007` = c("68%", "68%", NA, NA),
  `2005` = c("66%", "66%", NA, NA)
)

# Make total gender df tidy prior to plotting
total_gender_data_tidy <- total_gender_data %>%
  pivot_longer(cols = -Gender, names_to = "Year", values_to = "Percent") %>%
  mutate(Year = as.numeric(str_replace(Year, "^X", ""))) %>%
  mutate(Percent = as.numeric(str_replace(Percent, "%", "")))

# Plot change in gender distribution of unhoused population
gender_change_plot <- total_gender_data_tidy %>%
  ggplot(aes(x = Year, y = Percent, color = Gender)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = magma(length(unique(total_gender_data_tidy$Gender)))) +
  labs(
    title = "Change in Gender Distribution of Unhoused Population",
    x = "Year",
    y = "Percentage",
    color = "Gender",
    caption = "Source: Chicago Point-in-Time County Survey Report, 2023") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "gray80", color="transparent"))

# to do - need to troubleshoot why gender categories are not plotting correctly - underlying data type issue?
str(total_gender_data_tidy)
summary(total_gender_data_tidy$Gender)
unique(total_gender_data_tidy$Gender)

# Demographics: Age

# Sheltered Data
sheltered_age_data <- data.frame(
  "Age Category" = c(
    "Under 18 Years", "18-24 Years", "25-34 Years", "35-44 Years", "45-54 Years", "55-64 Years", "Over 65 Years"
  ),
  "2023**" = c(19.50, 14.80, 26.00, 23.20, 2.60, 9.60, 4.40),
  "2022*" = c(24.00, 11.10, 21.30, 30.20, NA, 13.40, NA),
  "2021*" = c(26.50, 10.90, 21.20, 30.10, NA, 11.30, NA),
  "2020" = c(29.90, 10.40, 22.30, 28.90, NA, 8.50, NA),
  "2019" = c(24.60, 8.90, 19.50, 34.60, NA, 12.40, NA),
  "2018" = c(29.60, 10.10, 19.40, 31.50, NA, 9.50, NA),
  "2017" = c(29.20, 11.20, 20.20, 29.30, NA, 10.00, NA),
  "2016" = c(29.80, 10.90, 19.80, 33.90, NA, 5.60, NA),
  "2015" = c(29.90, 12.10, 19.00, 32.60, NA, 6.40, NA),
  "2014" = c(31.10, 10.50, 20.00, 31.60, NA, 6.80, NA),
  "2013" = c(29.70, 9.70, 19.50, 35.50, NA, 5.50, NA),
  "2012" = c(31.00, NA, NA, 35.00, NA, 4.00, NA),
  "2011" = c(31.00, NA, NA, 36.00, NA, 4.00, NA),
  "2009" = c(34.00, NA, NA, 30.00, NA, 4.00, NA),
  "2007" = c(33.00, NA, NA, 35.00, NA, 3.00, NA),
  "2005" = c(31.00, NA, NA, 34.00, NA, 3.00, NA)
)

# Unsheltered Data
unsheltered_age_data <- data.frame(
  "Age Category" = c(
    "Under 18 Years", "18-24 Years", "25-34 Years", "35-44 Years", "45-54 Years", "55-64 Years", "Over 65 Years"
  ),
  "2023**" = c(3.40, 17.00, 25.40, 31.40, 19.60, 3.10, 0.10),
  "2022*" = c(0.00, 2.90, 34.00, 55.30, NA, 7.80, NA),
  "2021*" = c(0.00, 3.30, 33.10, 43.50, NA, 20.10, NA),
  "2020" = c(1.20, 6.10, 32.80, 45.60, NA, 14.40, NA),
  "2019" = c(0.10, 2.70, 35.00, 50.10, NA, 12.10, NA),
  "2018" = c(0.10, 5.00, 34.70, 47.40, NA, 12.80, NA),
  "2017" = c(0.30, 4.00, 38.10, 46.70, NA, 10.80, NA),
  "2016" = c(0.20, 2.10, 28.20, 58.30, NA, 11.20, NA),
  "2015" = c(1.10, 5.50, 33.30, 52.10, NA, 8.00, NA),
  "2014" = c(0.20, 8.30, 30.50, 53.70, NA, 7.20, NA),
  "2013" = c(3.10, 12.40, 30.20, 43.40, NA, 10.90, NA),
  "2012" = c(NA, NA, NA, NA, NA, NA, NA),
  "2011" = c(2.00, NA, NA, 44.00, NA, 9.00, NA),
  "2009" = c(2.00, NA, NA, 53.00, NA, 4.00, NA),
  "2007" = c(0.90, NA, NA, 49.00, NA, 6.00, NA),
  "2005" = c(2.30, NA, NA, 47.00, NA, 4.00, NA)
)

# Total Data
total_age_data <- data.frame(
  "Age Category" = c(
    "Under 18 Years", "18-24 Years", "25-34 Years", "35-44 Years", "45-54 Years", "55-64 Years", "Over 65 Years"
  ),
  "2023**" = c(16.30, 12.90, 24.50, 23.60, 7.30, 11.20, 4.20),
  "2022*" = c(16.20, 8.50, 25.40, 38.40, 11.60, 11.60, NA),
  "2021*" = c(21.50, 9.50, 23.50, 32.60, 12.90, 12.90, NA),
  "2020" = c(21.00, 9.20, 25.60, 34.00, 10.20, 10.20, NA),
  "2019" = c(20.10, 7.70, 22.40, 37.40, 12.30, 12.30, NA),
  "2018" = c(22.30, 8.80, 23.20, 35.50, 10.30, 10.30, NA),
  "2017" = c(21.30, 9.20, 25.10, 34.10, 10.30, 10.30, NA),
  "2016" = c(23.60, 9.00, 21.60, 39.00, 6.80, 6.80, NA),
  "2015" = c(21.20, 10.10, 23.40, 38.50, 6.80, 6.80, NA),
  "2014" = c(26.80, 10.20, 21.50, 34.70, 6.80, 6.80, NA),
  "2013" = c(3.10, 12.40, 30.20, 43.40, 10.90, 10.90, NA),
  "2012" = c(NA, NA, NA, NA, NA, NA, NA),
  "2011" = c(28.00, NA, NA, 35.00, 4.00, 4.00, NA),
  "2009" = c(28.00, NA, NA, 35.00, 4.00, 4.00, NA),
  "2007" = c(25.00, NA, NA, 38.00, 4.00, 4.00, NA),
  "2005" = c(26.00, NA, NA, 37.00, 3.00, 3.00, NA)
)

# Make total age df tidy prior to plotting
total_age_data_tidy <- total_age_data %>%
  pivot_longer(cols = -Age.Category, names_to = "Year", values_to = "Percent") %>%
  mutate(Year = as.numeric(str_replace(Year, "^X", ""))) %>%
  mutate(Percent = as.numeric(str_replace(Percent, "%", "")))

# Plot change in gender distribution of unhoused population
age_change_plot <- total_age_data_tidy %>%
  na.omit() %>%
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


# Compile demographic visuals into a single static dashboard
combined_dashboard <- age_change_plot + gender_change_plot + race_change_plot + unhoused_location_map + unhoused_reason_plot +veteran_pop_plot

combined_dashboard

# Export individual png files for each plot

# Export Age Change Plot
ggsave("age_change_plot.png", plot = age_change_plot, width = 10, height = 6, units = "in", dpi = 300)

# Export Gender Change Plot
ggsave("gender_change_plot.png", plot = gender_change_plot, width = 10, height = 6, units = "in", dpi = 300)

# Export Race Change Plot
ggsave("race_change_plot.png", plot = race_change_plot, width = 10, height = 6, units = "in", dpi = 300)

# Export Unhoused Location Map
ggsave("unhoused_location_map.png", plot = unhoused_location_map, width = 10, height = 6, units = "in", dpi = 300)

# Export Unhoused Reason Plot
ggsave("unhoused_reason_plot.png", plot = unhoused_reason_plot, width = 10, height = 6, units = "in", dpi = 300)

# Export Veteran Population Plot
ggsave("veteran_pop_plot.png", plot = veteran_pop_plot, width = 10, height = 6, units = "in", dpi = 300)

# Export Combined Dashboard
ggsave("combined_dashboard.png", plot = combined_dashboard, width = 20, height = 12, units = "in", dpi = 300)
