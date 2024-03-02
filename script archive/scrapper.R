#Webscrape
#I did 2024 by itself since it only had 2 month and it seemed too much of a hasle to code seperate breaks just for 2024 #sue me

###2024####
##Feb

feb_url <- "https://gov.texas.gov/news/archive/2024/02"
      
response <- read_html(feb_url)

      title <- response %>%
        html_elements("h3.textTransform-none.h5 a") %>%
        html_text(trim = TRUE)
  
      link <- response %>%
        html_elements("a.readMore") %>%
        html_attr("href")
      
      date <- response %>%
        html_elements("div.date.date--horiz") %>%
        html_text() 
      
     
      # Cleaning up and putting into dataframe
      df_02_2024 <- tibble(title, date, link)

##January
      
jan_url <- "https://gov.texas.gov/news/archive/2024/01"

response <- read_html(jan_url)
      
title <- response %>%
  html_elements("h3.textTransform-none.h5 a") %>%
  html_text(trim = TRUE)
      
link <- response %>%
  html_elements("a.readMore") %>%
  html_attr("href")
      
date <- response %>%
  html_elements("div.date.date--horiz") %>%
  html_text() 
      
# Cleaning up and putting into dataframe
df_01_2024 <- tibble(title, date, link)       

df_2024 <- rbind(df_01_2024,df_02_2024)

df_2024 <- df_2024 %>% 
  mutate(year = 2024) %>% 
  select(year,title,date,link)

#### Everything else ####
df_texas <- tibble()

for (i in c(2023,2022,2021)) {
  
base_url <- paste0("https://gov.texas.gov/news/archive/", i, "/")
  
  for (month in 1:12) {
    # Ensure the month is formatted with two digits
    formatted_month <- sprintf("%02d", month)
    
    # Construct the URL with the correctly formatted month
    url <- paste0(base_url, formatted_month)
    
    response <- read_html(url)
    
    title <- response %>%
      html_elements("h3.textTransform-none.h5 a") %>%
      html_text(trim = TRUE)
    
    link <- response %>%
      html_elements("a.readMore") %>%
      html_attr("href")
    
    date <- response %>%
      html_elements("div.date.date--horiz") %>%
      html_text() 
    
    
    # Only create a dataframe if there's data to add
    if (length(title) > 0 && length(date) > 0 && length(link) > 0) {
      temp_df <- tibble(year = rep(i, length(title)), title, date, link)
      df_texas <- bind_rows(df_texas, temp_df)
    }
  }

# Polite scraping: pause between requests
Sys.sleep(time = 1)
}
}

df_texas <- rbind(df_2024,df_texas)

filtered_df <- df_texas %>%
  filter(str_detect(title, "Operation Lone Star"))
