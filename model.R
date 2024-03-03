##Model##

library(tidyverse)
library(stargazer)

path <- 'C:/Users/steph/Documents/GitHub/R-DATA-2/Housing-and-Migrants-Chicago/outside_data'
setwd(path)

##I used a lot of dataframes from the text analysis so i read them in so the model can run by itself##
##I left notes of when a frame was written##

texas_sent <- read.csv("data.csv")
chicago_newcomers <- read.csv("Southern_Border_Arrivals_to_Chicago_-_2022-Present_20240227.csv")
Operation_lonestar_df <- read.csv("Operation_lonestar.csv")
overall_texas_feelings <- read.csv("overall_texas_feelings.csv")

## Regression##
##Added the date associated with every article
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

### Graph from Gov. Abbots announcements

texas_graph <- texas_sent %>%
  mutate(month_year = paste(year(full_date), month(full_date), sep = "-")) %>%
  arrange(month_year) %>% 
  group_by(month_year,City) %>%
  summarise(Count = sum(Count, na.rm = TRUE))

texas_graph <- texas_graph %>% 
  mutate(per_cent = if_else(row_number() == 1, 0, (Count / lag(Count) - 1) * 100))

##texas graph more digested seperating for Chicago

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

## Joining Frames for the Regresion

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

