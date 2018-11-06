#------------------------------------------------------------------------------
# Scrape the Public Papers of the Presidents: The American Presidency Project

# This function scrapes and does minimal processing to official presidential
# statements. It relies on UCSB's The American Presidency Project (APP) to 
# create a corpus.

# Build function that accepts document identifier

pullprez <- function(id) {
  # Packages
  require(rvest)     # For scraping
  require(lubridate) # For dates
  require(tidyverse) # Cleaning data
  require(tidytext)  # For basic word count
  url <- paste('https://www.presidency.ucsb.edu/node/', id, sep = '')
  encoding <- guess_encoding(url)
  try( # Don't crash if there is an error
    data <- read_html(url, encoding = encoding$encoding[1]) # Encoding with highest probability
  )
  title <- data %>%        # Page title
    html_node('div.field-ds-doc-title') %>%
    html_text()
  date <- data %>%         # Page date
    html_node('span.date-display-single') %>%
    html_text() %>%
    mdy() %>%
    as.character()
  president <- data %>%   # President
    html_node('div.field-title') %>%
    html_text() %>%
    paste(collapse = ' ')
  text <- data %>%        # Page text
    html_nodes('div.field-docs-content') %>%
    html_text() %>%
    paste(collapse = ' ')
  #--- Word count from document
  tokens <- data_frame(text = text) %>% unnest_tokens(word, text)
  return(as.data.frame(cbind(id, date, president, title, text, words = nrow(tokens))))
}

# Test function
function.test <- pullprez(266619)
