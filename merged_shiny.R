library(tidyverse)
library(shiny)
library(sf)
library(rnaturalearth)
library(viridis)
library(scales)
library(ggmap)
library(plotly)
library(shinyFeedback)
library(rsconnect)
library(shinythemes)
library(jsonlite)
library(viridis)
library(cowplot)
library(ggplot2)
library(readr)
library(purrr)
library(dplyr)
library(reshape2)
library(leaflet)
install.packages("leaflet")
#install.packages("reshape2")

path <- ("C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago\\outside_data\\")

path_texas <- ("C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago\\text files texas\\full data\\")

#####opioid data prep####
# unhoused locations per PITS report 

unhoused_locations_yrs_df <- read_csv(file.path(path, "unhoused_locations.csv"))

# Convert locations dataset into shapefile format
unhoused_locations_yrs <- st_as_sf(unhoused_locations_yrs_df,
                               coords = c("Longitude", "Latitude"),  crs=4326, remove = FALSE)

#merged ACS and IDPH data prior and write.csv to github repository, read in that file for shiny ease
opioid_data <- read_csv(file.path(path, "opioid_data.csv"))

#download Chicago neighborhood shapefiles
zip_chi_shape <- st_read("https://data.cityofchicago.org/resource/unjd-c2ca.geojson")

#create opioid shape file
opioid_shape <- merge(zip_chi_shape, opioid_data, by = "zip")


opioid_shape_long <- opioid_shape %>% 
  select(zip, geometry, overdose_rate_2022, overdose_rate_2021, overdose_rate_2020) %>% 
  rename(`2022` = overdose_rate_2022,
         `2021` = overdose_rate_2021,
         `2020` = overdose_rate_2020) %>% 
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Overdose_Rate")

#####################################################

##### equity grocery mental health data prep #####

# Import equity zone data 
equity_zones <- read_csv(file.path(path, "Healthy_Chicago_Equity_Zones_20240222.csv"))

# Import grocery store data
grocery_stores <- read_csv(file.path(path, "Map_of_Grocery_Stores_-_2013.csv"))

# Import mental health clinic location data 
mh_clinics <- read_csv(file.path(path, "CDPH_Mental_Health_Resources_20240222.csv"))

# # Compile unhoused locations dataframe 
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

# Import community area shapefiles 

####UPDTAED THIS TO NOT USE TEMP_DIR BUT TO CALL IN VIA API####

chi_comm_area  <- st_read("https://data.cityofchicago.org/resource/igwz-8jzy.geojson") |>
  select(community, geometry)

# Extract latitude and longitude
coordinates <- st_coordinates(chi_comm_area$geometry)

# Create new columnswith lat and long
chi_comm_area$Latitude <- head(coordinates[, "Y"], n = nrow(chi_comm_area))
chi_comm_area$Longitude <- head(coordinates[, "X"], n = nrow(chi_comm_area))

# Convert all dfs to shapefile format to support spatial join 
equity_zones_sf <- st_as_sf(equity_zones, wkt = "Geometry")
grocery_stores_sf <- st_as_sf(grocery_stores, coords = c("LONGITUDE", "LATITUDE"))
unhoused_locations_sf <- st_as_sf(unhoused_locations_df, coords = c("Longitude", "Latitude"))

# Extract lat and long from mh_clinics sf as intermediate step
mh_clinics$Longitude <- as.numeric(gsub("POINT \\((.*?) .*", "\\1", mh_clinics$Location))
mh_clinics$Latitude <- as.numeric(gsub("POINT \\(.*? (.*?)\\)", "\\1", mh_clinics$Location))
mh_clinics_sf <- st_as_sf(mh_clinics, coords = c("Longitude", "Latitude"))

# Standardize coordinate reference systems across sfs before joining 
st_crs(chi_comm_area)
st_crs(equity_zones_sf)

chi_comm_area <- st_set_crs(chi_comm_area, st_crs(equity_zones_sf))

# Perform spatial join to create master dataset for mapping 
joined_data <- st_join(chi_comm_area, equity_zones_sf, join = st_intersects)
joined_data <- st_join(joined_data, grocery_stores_sf, join = st_intersects)
joined_data <- st_join(joined_data, mh_clinics_sf, join = st_intersects)

# FIRST MAP: EQUITY ZONES
# Simple map that shows equity zone distribution 
equity_zones_plot <- ggplot(data = unique(joined_data)) +
  geom_sf(aes(fill = `Equity Zone`), alpha = 0.7)  

# equity_zones_plot

# SECOND MAP: GROCERY STORES

# Aggregate number of grocery stores by Community Area
grocery_count <- grocery_stores_sf %>%
  group_by(`COMMUNITY AREA NAME`) %>%
  summarise(Num_Grocery_Stores = n())

# Join the aggregated counts with the spatial data
plot_data <- st_join(joined_data, grocery_count, by = "COMMUNITY.AREA.NAME")

grocery_plot <- ggplot(data = unique(plot_data)) +
  geom_sf(aes(fill = Num_Grocery_Stores), alpha = 0.7) +
  scale_fill_gradient(name = "Number of Grocery Stores", low = "#fcfdbf", high = "#842681")

grocery_plot_equity <- grocery_plot +
  geom_sf(data = equity_zones_sf, aes(color = `Equity Zone`), fill = NA, size = 1.5, lwd = 1.2) +
  scale_color_manual(values = c("red", "green", "blue", "purple", "orange", "pink"))+
  #ggtitle("Equity Zones + Grocery Stores")+
  theme(plot.title = element_text(hjust = 0.5))

# Rename responses column in unhoused sf for clarity
unhoused_locations_sf <- unhoused_locations_sf %>%
  rename(`Number of Unhoused Individuals` = Responses)

grocery_plot_unhoused_pop <- grocery_plot_equity +
  geom_sf(data = unhoused_locations_sf, aes(size = `Number of Unhoused Individuals`), color = "#150e38", shape=17, fill = "#150e38") +  scale_alpha_continuous(range = c(0.1, 1), name = "Number of Unhoused Individuals") +
  #labs(title = "Equity Zones, MH Clinics, Grocery Stores + Unhoused Population") +
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank())+
  guides(color = guide_legend(ncol = 2))

# THIRD MAP: MH CLINICS

# Add a new column with the count of clinics per ZIP code
mh_clinic_count <- mh_clinics_sf %>%
  group_by(ZIP) %>%
  summarise(Num_Clinics = n())

# Join the aggregated counts with the spatial data
plot_data <- st_join(joined_data, mh_clinic_count, by = "ZIP")

mh_clinic_plot <- ggplot(data = unique(plot_data)) +
  geom_sf(aes(fill = Num_Clinics), alpha = 0.7) +
  scale_fill_gradient(name = "Number of Mental Health Clinics", low = "#fcfdbf", high = "#eb5760")

mh_clinic_plot_equity <- mh_clinic_plot +
  geom_sf(data = equity_zones_sf, aes(color = `Equity Zone`), fill = NA, size = 1.5, lwd = 1.2) +
  scale_color_manual(values = c("red", "green", "blue", "purple", "orange", "pink"))+
  #ggtitle("Equity Zones + Grocery Stores")+
  theme(plot.title = element_text(hjust = 0.5))

mh_clinic_plot_unhoused_pop <- mh_clinic_plot_equity +
  geom_sf(data = unhoused_locations_sf, aes(size = `Number of Unhoused Individuals`), color = "#150e38", shape=17, fill = "#150e38") +  scale_alpha_continuous(range = c(0.1, 1), name = "Number of Unhoused Individuals") +
  #labs(title = "Equity Zones, MH Clinics, Grocery Stores + Unhoused Population") +
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank())+
  guides(color = guide_legend(ncol = 2))


##### text analysis #####

texas_feelings <- read_csv(file.path(path_texas, "texas_feelings.csv"))
texas_graph<-read_csv(file.path(path_texas, "texas_graph.csv"))


######################################################################################



#################################### SHINY APP ############################################



ui <- fluidPage(theme = shinytheme("flatly"),
                useShinyFeedback(),
                titlePanel("Chicago Unhoused Populations and Migrant Crisis"),
                tabsetPanel( #opioids
                  tabPanel(
                    
                    h4("Opioid Overdose Rate and Unhoused Populations"),
                    
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(inputId = "year",
                                    label = "Choose a Year of Interest.",
                                    choices = c("2020", "2021", "2022"),
                                    selected = NULL),
                        h5("Data: IL Dept of Public Health, ACS, Chicago Point-in-Time Survey")
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel(title = "Opioid Rates", plotOutput("plot"))
                        ))
                    )),
              
                tabPanel( #equity mental health grocery
                  
                  h4("Determinants of Homelessness and Unhoused Population"),
                  
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("plot_selector", "Select variables to plot", 
                                  choices = c("Equity Zones", "Equity Zones + Grocery Stores + Unhoused Population",
                                              "Equity Zones + Mental Health Clinics + Unhoused Population"),
                                  selected = "Equity Zones"),
                    ),
                  
                    mainPanel(
                      tabsetPanel(
                      tabPanel(title = "Equity Maps", plotOutput("selected_plot"))
                    ))
                  )),
                
                tabPanel( #text analysis
                  
                  h4("Texas Sentiment and Count Analysis"),
                  
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("statType", "Choose Statistic:", 
                                  choices = c("Mean" = "mean_afinn", 
                                              "Median" = "median_affin", 
                                              "Max" = "max_afinn",
                                              "Min" = "min_afinn",
                                              "SD"= "sd_afinn"))
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        tabPanel(title = "Texas Migrant Announcements",  plotlyOutput("sentimentPlot")))
                      ))),
                
                tabPanel( #text analysis
                  
                  h4("New Arrival Count Over Time"),
                  
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("citySelect", "Choose a city:", 
                                  choices = unique(texas_graph$City), 
                                  selected = "Chicago", multiple = TRUE)
                    ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel(title = "Sentiment Analysis",  plotlyOutput("countPlot")))
                  ))
                
                
                ))
                
)

server <- function(input, output) {
  
  
  ##OPIOIDS##
  output$plot <- 
    
    renderPlot({
      
      if (input$year == "2022") { #2022 Rate
        
        # Make the plot
        p <-   ggplot() +
          geom_sf(data = zip_chi_shape) +
          geom_sf(data = filter(opioid_shape_long, Year == "2022"), aes(fill = Overdose_Rate), color = NA) +
          labs(title = "Opioid Overdose Rate (Nonfatal and Fatal Per 10,000 Ppl)",
               subtitle = paste("By Zip in 2022. Dot Size Correlated W/ High-Density Unhoused Pop.")) +
          scale_fill_viridis(name="Opioid Overdose Rate", option = "magma", trans = "reverse", 
                             breaks = pretty_breaks(n = 5)) +
          geom_point(data = filter(unhoused_locations_yrs, Year == 2023), aes(x = Longitude, y = Latitude,
                                                                          size = Responses,),
                     color = "#8c62aa",alpha = 0.7) +
          guides(size = FALSE) +
          theme_map() +
          theme(
            plot.title = element_text(hjust = 0, size= 13),
            plot.subtitle = element_text(size = 10, hjust = 0), #subtitle not showing
            legend.title = element_text(size = 12),
            legend.title.align = 0.5)
        
        
        
      }
      
      if (input$year == "2021") { #2021 Rate
        
        
        p <-  ggplot() +
          geom_sf(data = zip_chi_shape) +
          geom_sf(data = filter(opioid_shape_long, Year == "2021"), aes(fill = Overdose_Rate), color = NA) +
          labs(title = "Opioid Overdose Rate (Nonfatal and Fatal Per 10,000 Ppl)",
               subtitle = paste("By Zip in 2021. Dot Size Correlated W/ High-Density Unhoused Pop.")) +
          scale_fill_viridis(name="Opioid Overdose Rate", option = "magma", trans = "reverse", 
                             breaks = pretty_breaks(n = 5)) +
          geom_point(data = filter(unhoused_locations_yrs, Year == 2022), aes(x = Longitude, y = Latitude,
                                                                          size = Responses,),
                     color = "#8c62aa",alpha = 0.7) +
          guides(size = FALSE) +
          theme_map() +
          theme(
            plot.title = element_text(hjust = 0, size= 13),
            plot.subtitle = element_text(size = 10, hjust = 0), #subtitle not showing
            legend.title = element_text(size = 12),
            legend.title.align = 0.5)
        
        
      }
      
      if (input$year == "2020") { #2020 Rate
        
        
        p <-  ggplot() +
          geom_sf(data = zip_chi_shape) +
          geom_sf(data = filter(opioid_shape_long, Year == "2020"), aes(fill = Overdose_Rate), color = NA) +
          labs(title = "Opioid Overdose Rate (Nonfatal and Fatal Per 10,000 Ppl)",
               subtitle = paste("By Zip in 2020. Dot Size Correlated W/ High-Density Unhoused Pop.")) +
          scale_fill_viridis(name="Opioid Overdose Rate", option = "magma", trans = "reverse", 
                             breaks = pretty_breaks(n = 5)) +
          geom_point(data = filter(unhoused_locations_yrs, Year == 2022), aes(x = Longitude, y = Latitude,
                                                                          size = Responses,),
                     color = "#8c62aa",alpha = 0.7) +
          guides(size = FALSE) +
          theme_map() +
          theme(
            plot.title = element_text(hjust = 0, size= 13),
            plot.subtitle = element_text(size = 10, hjust = 0), #subtitle not showing
            legend.title = element_text(size = 12),
            legend.title.align = 0.5)
        
      }
      
      p
      
      
    })
  
  ###Resource Equity
  
  # Reactive function to select the plot based on user input
  selected_plot <- reactive({
    switch(input$plot_selector,
           "Equity Zones" = equity_zones_plot,
           "Equity Zones + Grocery Stores + Unhoused Population" = grocery_plot_unhoused_pop,
           "Equity Zones + Mental Health Clinics + Unhoused Population" = mh_clinic_plot_unhoused_pop) 
  })
  
  # Render the selected plot with an adjustable size
  output$selected_plot <- renderPlot({
    selected_plot() +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16)) +
      theme(axis.text = element_blank(), axis.title = element_blank())
  })
  
  
  ##Sentiment analysis
  
  # For Sentiment Analysis Plot
  output$sentimentPlot <- renderPlotly({
    req(input$statType)  # Ensure that statType is not NULL or missing
    
    overall_texas_feelings <- texas_feelings %>%
      mutate(article_id = as.numeric(article_id)) %>% 
      group_by(article_id) %>%
      summarise(mean_afinn = mean(afinn, na.rm=TRUE),
                median_affin = median(afinn, na.rm=TRUE),
                sd_afinn = sd(afinn, na.rm = TRUE),
                min_afinn = min(afinn, na.rm = TRUE),
                max_afinn = max(afinn, na.rm = TRUE)) 
    
    # Dynamically select the statistic based on user input
    sentiment_stat <- overall_texas_feelings %>% 
      select(article_id, !!sym(input$statType)) %>%
      arrange(desc(article_id))  
    
    # Convert ggplot to plotly for interactivity
    p3 <- ggplot(sentiment_stat, aes(x = article_id, y = !!sym(input$statType))) +
      geom_line(color = "#00BFC4", size = 1) +  
      geom_point(color = "#F8766D", size = 2, alpha = 0.8) +  
      theme_minimal() +  
      labs(
        title = "Overall Sentiment  by  Article across time",
        subtitle = "Sentiment scores with linear regression line",
        x = "Article ID",
        y = "Mean Sentiment Score"
      ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
      )  
    
    ggplotly(p3)
  })
  
  
  # For Count Over Time Plot
  output$countPlot <- renderPlotly({
    
    # Validate input
    validate(
      need(input$citySelect, "Please select at least one city.")
    )
    
    filtered_data <- texas_graph %>%
      filter(City %in% !!sym(input$citySelect))
    
    
    p2 <- ggplot(filtered_data, aes(x = month_year, y = log(Count), color = City)) +
      geom_line() + 
      geom_point() + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      labs(x = "Time", y = "Count", title = "Count Over Time by City") 
    
    ggplotly(p2)
    
  })
  

  
}

                    
                    
                    

shinyApp(ui = ui, server = server)









