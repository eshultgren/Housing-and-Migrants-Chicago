##Libraries##
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(ggmap)
library(sf)
library(rnaturalearth)
library(shiny)
library(biscale)
library(cowplot)
library(viridis)
library(scales)
##Update Path Here##
path <- "/Users/josemacias/Documents/GitHub/Housing-and-Migrants-Chicago"
setwd(path)
##Data Processing##
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
           other_languages == "ASL; Spanish" ~ "Spanish; ASL",
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
var <- load_variables(2022, "acs1")
vars <- c(
  "B01003_001", # Total Pop
  "B01001B_001", # Black Pop
  "B01001I_001", # Hispanic Pop
  "B07011_001", # Median Income
  "B05002_013"   # foriegn born
)

census_data <- 
  get_acs(geography = "tract", 
          variables = vars, 
          state = "IL", 
          county = "Cook", 
          geometry = T,
          output = "wide",
          year = 2022) |>
  rename(black_pop = B01001B_001E,latino_pop = B01001I_001E, total_pop = B01003_001E, median_income = B07011_001E, foreign_born = B05002_013E) |>
  select(-c(B01003_001M,B01001I_001M,B01001B_001M,B07011_001M))
############### API Datasets End
############### Outside CSV Data Load
# Opioid data
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
  mutate(zip = as.character(zip)) |>
  select(-c(shape_area,shape_len, objectid,Patient_Zip))
unhoused_dense_22 <- 
  read_csv(file.path(path, "outside_data/high density unhoused_22.csv"))
unhoused_dense_21 <- 
  read_csv(file.path(path, "outside_data/high density unhoused_21.csv"))
age_zip <- read_csv(file.path(path, "outside_data/acs_med_age_zip.csv"))|>
  mutate(zip = as.character(zip))
edu_zip <- read_csv(file.path(path, "outside_data/acs_education_zip.csv"))|>
  mutate(zip = as.character(zip))
pop_22 <- 
  read_csv(file.path(path, "outside_data/population zip_2022.csv"))|>
  mutate(zip = as.character(zip))
pop_21 <- 
  read_csv(file.path(path, "outside_data/population_zip_2021.csv"))|>
  mutate(zip = as.character(zip))
pop_20 <- 
  read_csv(file.path(path, "outside_data/population_zip_2020.csv"))|>
  mutate(zip = as.character(zip))
opioid_shape <- 
  opioid_shape |>
  left_join(age_zip, by = "zip") |>
  left_join(edu_zip, by = "zip") |>
  left_join(pop_22, by = "zip") |>
  left_join(pop_21, by = "zip") |>
  left_join(pop_20, by = "zip") |> 
  mutate(overdose_rate_22 = Overdose_Count_by_Zip_2022/pop_10000_2022,
         overdose_rate_21 = Overdose_Count_by_Zip_2021/pop_10000_2021,
         overdose_rate_20 = Overdose_Count_by_Zip_2020/pop_10000_2020)
encampment <- read_csv(file.path(path, "outside_data/encampment data.csv"))
# Unhoused locations per PITS report 
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

unhoused_reason <- 
  data.frame(
    Cause = c("Family Disputes", "Multiple", "Loss of employment/underemployment",
              "Eviction, Foreclosure, Unable to Renew", "Disasters (i.e. Fire, Flood)",
              "Death of Parent/Guardian/Spouse", "Release from Jail/Prison", "Total"),
    Percent = c(46.90, 18.50, 14.80, 9.90, 6.20, 1.20, 2.50, 100.00))

unhoused_veterans <- 
  data.frame(
    Year = c(2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2009, 2007, 2005),
    Sheltered_Count = c(202, 203, 171, 286, 279, 296, 304, 399, 346, 465, 406, 270, 250, 242, NA, NA),
    Sheltered_PercentChange = c(-0.50, 18.70, -40.20, 2.50, -5.70, -2.60, -23.80, 15.30, -25.60, 14.50, 50.40, 8.00, 3.30, NA, NA, NA),
    Unsheltered_Count = c(115, 119, 77, 201, 120, 198, 245, 202, 406, 256, 306, NA, 250, 115, NA, NA),
    Unsheltered_PercentChange = c(-3.30, 9.40, 5.30, 13.30, 11.90, 14.60, 15.70, 16.40, 19.20, 26.80, 25.50, NA, 15.10, 14.10, 18.40, 16),
    Total_Count = c(317, 322, 248, 487, 399, 494, 549, 601, 752, 721, 712, NA, 500, 357, NA, NA),
    Total_PercentChange = c(-1.60, 8.30, 5.50, 11.40, 10.60, 11.70, 12.20, 13.20, 14.90, 16.00, 15.50, NA, 10.10, 9.20, 15.40, 11))

# Important dates for vets
important_dates <- 
  data.frame(date = as.Date(c("2011-12-15", "2021-08-30")), label = c("End of Iraq War", "US withdrawal from Afghanistan"))


# Sheltered data
sheltered_race_data <- 
  data.frame(
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
unsheltered_race_data <- 
  data.frame(
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

# encampment data into shape
encampment <- 
  as.data.frame(encampment) |> 
  st_as_sf(coords=c("Longitude","Latitude"), crs=4326, remove=FALSE)  

# Convert unhoused locations dataset into shapefile format
unhoused_locations_multiyr <- 
  st_as_sf(unhoused_locations_df,
           coords = c("Longitude", "Latitude"),  crs=4326, remove = FALSE)
unhoused_locations <- 
  st_as_sf(unhoused_locations_df,
           coords = c("Longitude", "Latitude"),  crs=4326, remove = FALSE)

# unhoused join
unhoused_locations_chi <- 
  st_join(unhoused_locations, chi_town)

############### Spatial Work End
############### Writing out all SF Data Frames and CSV
# commarea shape ############### 
st_write(chi_town, 
         "shape_files/chicago_commarea/chicago_shape.shp", append=FALSE)
# census shape ############### 
st_write(filtered_census_tracts, 
         "shape_files/chicago_tracts/chicago_tracts.shp", append=FALSE)
# zip shape ############### 
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

# Unhoused as shapes ############### 
st_write(unhoused_locations, 
         dsn = 'shape_files/unhoused/unhoused_locations.shp', driver = "ESRI Shapefile", append=FALSE)
st_write(unhoused_locations_multiyr, 
         dsn = 'shape_files/unhoused/unhoused_locations_multiyr.shp', driver = "ESRI Shapefile", append=FALSE)

st_write(unhoused_locations_chi,
         dsn = 'shape_files/unhoused/unhoused_locations_chi.shp', driver = "ESRI Shapefile", append=FALSE)
# Writing out csv ############### 
write_csv(opioid_data, "outside_data/opioid_data.csv")
write_csv(unhoused_reason, "outside_data/unhoused_reason.csv")
write_csv(unhoused_veterans, "outside_data/unhoused_veterans.csv" )
write_csv(important_dates, "outside_data/important_dates.csv")
write_csv(sheltered_race_data, "outside_data/sheltered_race_data.csv")
write_csv(unsheltered_race_data, "outside_data/unsheltered_race_data.csv")
write_csv(total_race_data, "outside_data/total_race_data.csv")
write_csv(sheltered_gender_data,"outside_data/sheltered_gender_data.csv" )
write_csv(unsheltered_gender_data, "outside_data/unsheltered_gender_data.csv")
write_csv(total_gender_data, "outside_data/total_gender_data.csv")
write_csv(sheltered_age_data, "outside_data/sheltered_age_data.csv")
write_csv(unsheltered_age_data, "outside_data/unsheltered_age_data.csv")
write_csv(total_age_data, "outside_data/total_age_data.csv")
write_csv(unhoused_locations_df,
          "outside_data/unhoused_locations_df.csv")
############### Data Work End
 