# Scrape attempt: 2023 PITS Report

# Install and load required packages
install.packages(c("pdftools", "readtext"))
library(pdftools)
library(readtext)

pdf_path <- "C:/Users/higgi/Documents/GitHub/Housing-and-Migrants-Chicago/outside_data/2023 PIT Report_FINAL.pdf"

# Extract text from the PDF
pdf_text <- pdf_text(pdf_path)

# Specify the page containing the table 
page_number <- 30

# Extract the text from the specified page
page_text <- pdf_text[[page_number]]

# Extract the table from the text
total_homeless_pop <- "Sheltered Unsheltered Total
Year Count % Change Count % Change Count % Change
2023** 5,149 97% 990 -21% 6,139 53%
2022* 2,612 -14% 1,263 -13% 3,875 -13%
2021* 3,023 -22% 1,454 -5% 4,477 -17%
2020 3,861 -4% 1,529 21% 5,390 2%
2019 4,030 -2% 1,260 -7% 5,290 -3%
2018 4,093 -1% 1,357 -13% 5,450 -4%
2017 4,135 -11% 1,561 26% 5,696 -3%
2016 4,646 -2% 1,243 -40% 5,889 -13%
2015 4,731 -11% 2,055 113% 6,786 8%
2014 5,329 5% 965 -21% 6,294 0%
2013 5,060 1% 1,219 -29% 6,279 -5%
2012 4,988 2% NA NA NA NA
2011 4,873 -9% 1,725 95% 6,598 6%
2009 5,356 23% 884 -44% 6,240 5%
2007 4,346 13% 1,576 9% 5,922 12%"

# Convert the table text to a data frame
total_homeless_pop_df <- read.table(text = total_homeless_pop, header = TRUE)

# Print the final data frame
print(total_homeless_pop_df)