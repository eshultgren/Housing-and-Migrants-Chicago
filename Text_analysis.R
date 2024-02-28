library(tidytext)
library(textdata)
library(tidyverse)
library(sentimentr)
library(udpipe)
library(SnowballC)
library(rvest)

setwd("M:/p/School/2024/quarter 5/r/final r/text files texas/full data")

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

chicago_df <- combined_parsed_df %>% 
  mutate(article_id = as.numeric(article_id)) %>% 
  filter(str_detect(sentence, regex("Chicago", ignore_case = TRUE))) %>%
  filter(upos %in% c("NUM", "PROPN")) %>% 
  select(article_id,token)