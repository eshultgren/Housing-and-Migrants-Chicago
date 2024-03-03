library(tidytext)
library(textdata)
library(tidyverse)
library(sentimentr)
library(udpipe)
library(SnowballC)
library(rvest)
library(stargazer)

#The articles were scraped using the script called scrapper, i debated including it here.
#Since it takes a few minutes to read in every article and it takes a few minutes to run the scrapper i left it seperate

path <- 'C:/Users/steph/Documents/GitHub/R-DATA-2/Housing-and-Migrants-Chicago/text files texas/full data'
setwd(path)


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

##I wrote overall_texas_feelings from here##

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


