
DP II Final -- Housing-and-Migrants-Chicago

Order of Operations:


#1. Run data.R #

--Update path on Line 14

This code calls in the necessary data and shapefiles for the analysis. Shapefiles are called in via API from the Chicago Data Portal. After this code is run, you will have shape files for community area, zip code, and census tract. Additionally, you will have data on mental health resources, affordable housing, census demographics, data on high-density regions of people experiencing homelessness, data on veterans experiencing homelessness, data on housing insecurity by race and ethnicity, gender, and age. All data is cleaned and ready for plotting. Data is cleaned and saved to file path.

#2. Run staticplot.R #

--Updated path on Line 18

This code reads in the necessary data to prepare static plots demonstrating demographics of people experiencing homelessness over time i.e. veteran status, age, gender, race, geographic distribution of mental health resources and the languages serviced, and opioid overdase rates as they relate to concentrations of unhoused populations.

#3. textprocess.R #

--Update path on line 12 to the folder with the Texas announcements in it

This code calls in all of the announcements from Governor Abbott from March 2021 to late February 2024. Data was gathered via web crawler. Next, announcements are parsed by words and analyzed by their afinn sentiment. Summary statistics are produced to assess sentiment of announcements over time. The mean sentiment of announcements are plotted ver time.



#4. Run model.R #


--Update path on line 6

Lone Star Announcements are grouped by month and year. Next, the code creates plots presenting the announced number of new arrivals sent to key cities of interest each month.

The regression starting on line 80 assesses if sentiment is correlated with time of announcment. 



#4. Run shiny.R #

--Update path on Line 27

This code produces a shiny app with interactive features divided across four tabs.
Data for opioiod analysis is called in and cleaned. Due to difficulties saving newly created shape files, data and shape files are merged here for shiny purposes. Next equity map, unhoused populations, grocery store, and mental health data is prepped and maps created to be called into the shiny app later. Data for the text anaalysis is called in in likes 166 and 167. The UI for the shiny begins on line 178 and server starts on line 259.



