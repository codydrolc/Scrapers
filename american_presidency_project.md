Scrape the Public Papers of the Presidents: The American Presidency Project
================
Cody A. Drolc

This document presents a function for scraping the Public Papers of the Presidents. The scraper relies on UCSB's The [American Presidency Project](https://www.presidency.ucsb.edu/) (APP) to create a data frame with details such as the president, whether the document comes from the Vice President or First Lady, and the whole document text.

The function was recently updated to work with the APP's website overhaul. The information that is pulled is not perfectly clean, but it provides a base for either creating a corpus or doing more complex text analyses. Note that some public papers do not pull perfectly.

Feel free to use and/or edit the `pullprez` function.

pullprez Function
-----------------

``` r
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
  title <- data %>%             # Page title
    html_node('div.field-ds-doc-title') %>%
    html_text()
  date <- data %>%              # Page date
    html_node('span.date-display-single') %>%
    html_text() %>%
    mdy() %>%
    as.character()
  president <- data %>%         # President
    html_node('div.field-title') %>%
    html_text() 
  press_secretary <- data %>%   # Press Secretary indicator
    html_node("div.group-meta") %>%
    html_text() %>%
    str_detect("Press Secretary")
  press_secretary <- if_else(press_secretary == TRUE, 1, 0)
  vice_president <- data %>%    # Vice President
    html_node("div.group-meta") %>%
    html_text() %>%
    str_detect("Vice President")
  vice_president <- if_else(vice_president == TRUE, 1, 0)
  first_lady <- data %>%        # First Lady
    html_node("div.group-meta") %>%
    html_text() %>%
    str_detect("First Lady")
  first_lady <- if_else(first_lady == TRUE, 1, 0)
  text <- data %>%              # Page text
    html_nodes('div.field-docs-content') %>%
    html_text() 
  #--- Word count from document
  tokens <- data_frame(text = text) %>% unnest_tokens(word, text)
  return(as.data.frame(cbind(id, date, president, press_secretary, vice_president, 
    first_lady, title, text, words = nrow(tokens))))
}
```

Now test the function by feeding it a six digit identifier.

``` r
test <- pullprez(210000)
kable(select(test, -text)) # Don't show full text
```

| id     | date       | president             | press\_secretary | vice\_president | first\_lady | title                                                                             | words |
|:-------|:-----------|:----------------------|:-----------------|:----------------|:------------|:----------------------------------------------------------------------------------|:------|
| 210000 | 1942-01-26 | Franklin D. Roosevelt | 0                | 0               | 0           | Statement on Raw Materials, Munition Assignments, and Shipping Adjustment Boards. | 693   |

Compile all Public Papers
-------------------------

What if I want all the public papers? Well, you're in luck! By making a few assumptions about the range of unique identifiers, we can give the function a large sequence and produce a full data frame. Here I demonstrate how this is done in parallel to save a bit of time. Note that I modified the function a little bit so that when an error is encountered, the whole process does not fail.

``` r
# Create data frame of public papers
library(furrr)
plan(multisession) # For Windows
possibly_pullprez <- possibly(pullprez, otherwise = NULL) # Keep mapping if error

app <- bind_rows(future_map(200000:350000, possibly_pullprez))
```

Caution! Running over what *I think* is the universe of public paper identifiers will take some time.
