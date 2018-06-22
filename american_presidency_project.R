#------------------------------------------------------------------------------
# Scrape the Public Papers of the Presidents: The American Presidency Project

# This function scrapes and does minimal processing to official presidential
# statements. It relies on UCSB's The American Presidency Project (APP) to 
# create a corpus.

# Packages
library(rvest)     # Scraping
library(tidyverse) # Cleaning data
library(lubridate) # Cleaning dates
library(tidytext)  # Sentiment analysis

# Build function that accepts document identifier
  
pullprez <- function(id) {
  url <- paste('http://www.presidency.ucsb.edu/ws/index.php?pid=', id, sep = '')
  encoding <- guess_encoding(url)
  try( # Don't crash if there is an error
    data <- read_html(url, encoding = encoding$encoding[1]) # Encoding with highest probability
  )
  title <- data %>%        # Page title
    html_node('title') %>%
    html_text()
  date <- data %>%         # Page date
    html_node('.docdate') %>%
    html_text() %>%
    mdy() %>%
    as.character()
  collection <- data %>%   # American presidency collection
    html_nodes('td.ver10') %>%
    html_node('img') %>%
    html_attr('alt') %>%
    paste(collapse = ' ')
    collection <- sub("NA.*", "", collection) # Clean up string
    collection <- sub("<.*", "", collection)  # Further cleaning
  text <- data %>%        # Page text
    html_nodes('span.displaytext') %>%
    html_text() %>%
    paste(collapse = ' ')
  president <- gsub("\\:.*", "", title) # President, VP, FLOTUS, etc.
  title <- str_sub(title, (str_length(president) + 2)) # Remove president's name from title
  #--- Sentiment from document
  tokens <- data_frame(text = text) %>% unnest_tokens(word, text)
  sentiment <- tokens %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment)  # Count negative and postive words
  sentiment <-        # Data to wide, if error, return fake object with 0
    try(
      sentiment %>%
        spread(sentiment, n, fill = 0))
  if (length(sentiment) == 0) {
    # Create fake object with 0 for positive and negative
    sentiment <- NULL
    sentiment$positive <- 0
    sentiment$negative <- 0
    sentiment <- as.data.frame(sentiment)
  }
  return(as_tibble(cbind(id, president, date, collection, title, text, sentiment, words = nrow(tokens))))
}

# Test function
function.test <- pullprez(20832)
