# Load the necessary package
library(httr)

# Specify the URL of the website you want to read
url <- "https://www.bbc.com/news"

# Send a GET request to retrieve the webpage
response <- GET(url)

# Extract the content of the response
content <- content(response, "text")

# Use regular expressions to extract the text of interest
text <- gsub("<.*?>", "", content)

# Print the resulting text
cat(text)


install.packages("rvest")
install.packages("dplyr")

# Load the necessary packages
library(rvest)
library(dplyr)

# Specify the URL of the website containing the table
url <- "https://www.example.com/table.html"

# Use rvest to scrape the HTML content of the page
page <- read_html(url)

# Extract the table using html_table()
table <- page %>% html_node("table") %>% html_table()

# Print the resulting table




