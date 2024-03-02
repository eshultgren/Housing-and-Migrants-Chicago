install.packages("pdftools")
library(pdftools)
library(tidyverse)
install.packages("tabulizer")


##Path##
path <- "C:\\Users\\emmas\\OneDrive\\Documents\\GitHub\\Housing-and-Migrants-Chicago"
#####

home_sales <- pdf_text(file.path(path, "\\1M_SF_Sales_CA_and_Ward.pdf"))
url <- c("https://s6624.pcdn.co/wp-content/uploads/2024/01/1M_SF_Sales_CA_and_Ward.pdf")
home_sales <- map(url, pdf_text)

view(home_sales)

clean_table <- function(raw) {
  
  raw <- map(raw, ~str_split(.x, "\\n") %>%  unlist())
  
  raw <- reduce(raw, c)
  
  table_start <- stringr::str_which(tolower(raw), "Rogers Park")
  table_end <- stringr::str_which(tolower(raw), "Edgewater")
  table_end <- table_end[min(which(table_end > table_start))]
  
  table <- raw[(table_start) : (table_end)]
  table <- str_replace_all(table, "\\s{2, }" , "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
  
  colnames(data_table) <- c("Community Area", "Total Sales", "Less than $1M", "1M to 1.5M", "1.5 to 2M", "2M to 3M", "3M to 5M", "5M or More")
  data_table
}

results <- map_df(home_sales, clean_table)
head(results)


head(home_sales)

home_sales <- trimws(home_sales)

head(home_sales)

####

ext <- el(extract_tables(link, encoding="UTF-8"))