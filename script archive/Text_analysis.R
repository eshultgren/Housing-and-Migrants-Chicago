library(tidytext)
library(textdata)
library(tidyverse)
library(sentimentr)
library(udpipe)
library(SnowballC)
library(rvest)
library(stargazer)

path <- 'C:/Users/steph/Documents/GitHub/R-DATA-2/Housing-and-Migrants-Chicago/text files texas/full data'
setwd(path)
texas_sent <- read.csv("data.csv")
chicago_newcomers <- read.csv("Southern_Border_Arrivals_to_Chicago_-_2022-Present_20240227.csv")

articles <- list()
parsed_articles <- list()

for (i in 1:108) {
  file_name <- paste0("article_", i, ".html")
  
  # Read HTML content and get the text
  article_content <- read_html(file_name) %>%
    html_text(trim = TRUE)
  
  # Store the content in the articles list (if you want to keep it)
  articles[[i]] <- article_content
  
  # Use udpipe to annotate the current article content
  parsed <- udpipe(article_content,"english")
  
  # Convert to data.frame (if needed)
  parsed_df <- as.data.frame(parsed)
  
  # Store the parsed data frame in the list
  parsed_articles[[i]] <- parsed_df
}


# Now combine all the data frames into one
combined_parsed_df <- bind_rows(parsed_articles, .id = "article_id")

# Creating the "data" dataframe
chicago_df <- combined_parsed_df %>% 
  mutate(article_id = as.numeric(article_id)) %>% 
  filter(str_detect(sentence, regex("Chicago", ignore_case = TRUE))) %>%
  filter(upos %in% c("NUM", "PROPN")) %>% 
  select(article_id,token)

##From here I wrote it and had to manualy filter it
##since it had a lot of noise that i was not able to clean easily inside R, 
##since some new report refered to DC as the nations capital or other particularities


## Sentiment Analysis

sentiment_afinn <- get_sentiments("afinn") %>% 
  rename(afinn = value)

combined_parsed_df <- anti_join(combined_parsed_df, stop_words, by = c("lemma" = "word"))

texas_feelings <- left_join(combined_parsed_df, sentiment_afinn, by = c("lemma" = "word"))

texas_feelings <- texas_feelings %>% 
  filter(!is.na(afinn))

overall_texas_feelings <- texas_feelings %>%
  mutate(article_id = as.numeric(article_id)) %>% 
  group_by(article_id) %>%
  summarise(mean_afinn = mean(afinn, na.rm=TRUE),
            median_affin = median(afinn, na.rm=TRUE),
            sd_afinn = sd(afinn, na.rm = TRUE),
            min_afinn = min(afinn, na.rm = TRUE),
            max_afinn = max(afinn, na.rm = TRUE)) 

overall_texas_feelings %>%
  ggplot(aes(x = desc(article_id), y = mean_afinn)) +
  geom_line(color = "#00BFC4", size = 1) +  # Line for sentiment scores
  geom_point(color = "#F8766D", size = 2, alpha = 0.8) +  # Points for individual data
  geom_smooth(method = "lm", color = "blue", se = TRUE, linetype = "dashed") + 
  theme_minimal() +  
  labs(
    title = "Overall Sentiment  by  Article across time",
    subtitle = "Mean sentiment scores with linear regression line",
    x = "Article ID",
    y = "Mean Sentiment Score"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) 


## Regresion##
Operation_lonestar_df <- Operation_lonestar_df %>% 
  mutate(article_id = 1:103)


dates <- Operation_lonestar_df %>% 
  select(article_id,full_date)




chicago_newcomers <-chicago_newcomers %>% 
  mutate(Date = mdy(Date))

chicago_newcomers_reg <-chicago_newcomers %>% 
  mutate(month_year = paste(year(Date), month(Date), sep = "-"))


texas_sent <- left_join(texas_sent,dates, by = "article_id")

texas_sent <- texas_sent %>%
  mutate(Count = as.numeric(gsub(",", "", Count)))

texas_sent <- texas_sent %>% 
  select(full_date,Count, article_id,City,) %>% 
  mutate(Count = as.numeric(Count))

### Graph From abbots news


texas_graph <- texas_sent %>%
  mutate(month_year = paste(year(full_date), month(full_date), sep = "-")) %>%
  arrange(month_year) %>% 
  group_by(month_year,City) %>%
  summarise(Count = sum(Count, na.rm = TRUE))

texas_graph <- texas_graph %>% 
  mutate(per_cent = if_else(row_number() == 1, 0, (Count / lag(Count) - 1) * 100))

##texas graph more digested

texas_graph <- texas_graph %>%
  mutate(month_year = as.Date(paste0(month_year, "-01"), format = "%Y-%m-%d"))

ggplot(texas_graph, aes(x = month_year, y = log(Count), color = City)) +
  geom_line() + # Draw lines
  geom_point() + # Add points to make individual counts more visible
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Improve x-axis label readability
  labs(x = "Time", y = "Count", title = "Count Over Time by City")

chicago_sent <- texas_sent %>%
  filter(City == 'Chicago') %>%
  mutate(month_year = paste(year(full_date), month(full_date), sep = "-"))
 
ggplot(texas_graph, aes(x = month_year, y = log(Count), group = City, color = City)) +
  geom_line(aes(alpha = (City != "Chicago"))) +  
  geom_point(aes(alpha = (City != "Chicago"))) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_color_manual(values = c("Chicago" = "red", 
                                "Other" = "grey")) +  
  scale_alpha_manual(values = c(1, 0.3)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "none") +  # Move legend to bottom
  labs(x = "Time", y = "Count", title = "Count Over Time vs Chicago", color = "City", alpha = NULL)  

regresion_df <- left_join(chicago_sent, overall_texas_feelings,by = "article_id" )

regresion_df <- regresion_df %>%
  arrange(month_year) %>% 
  group_by(month_year) %>%
  summarise(Count = sum(Count, na.rm = TRUE),
            afinn = mean(mean_afinn, na.rm = TRUE))

regresion_df <- regresion_df %>% 
  mutate(per_cent = if_else(row_number() == 1, 0, (Count / lag(Count) - 1) * 100))


regresion_chicago_df <- left_join(chicago_newcomers_reg,regresion_df, by = 'month_year')
regresion_chicago_df <- regresion_chicago_df %>% 
  drop_na()

  
reg_newcomers <- lm(Cumulative.Total ~ Count + afinn + per_cent + Date  , data = regresion_chicago_df )

regresion_chicago_df$lag_Cumulative_Total <- lag(regresion_chicago_df$Cumulative.Total, 1)

reg_newcomers_2 <- lm(Cumulative.Total ~ Count + afinn + per_cent + Date + lag_Cumulative_Total , data = regresion_chicago_df )

summary(reg_newcomers_2)



# Create the stargazer table with both regression models
stargazer(reg_newcomers, reg_newcomers_2, type = "text", 
          title = "Regression Results", no.space = TRUE, 
          single.row = TRUE, digits = 2, 
          intercept.bottom = FALSE, omit.stat = c("LL", "ser", "f", "adj.rsq"))

         