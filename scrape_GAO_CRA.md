Scrape GAO Congressional Review Act Data
================
Cody Drolc
November 14, 2018

CRA Scraping Function
---------------------

The [Government Accountability Office](https://www.gao.gov/) keeps the bolded information for each published summary listing in a single table on a [unique page](https://www.gao.gov/fedrules/176331). There is only one table on each page. So, parsing the html nodes only requires taking "table" and nothing else. Converting the table from a list to a data frame creates "long" format data that is reshaped by the function. As a result, each time the function is pulled there will only be one row.

``` r
# Scraping function specific to the Government Accountability Office
pull_cra <- function(cn) {
  require(tidyverse); require(rvest); require(lubridate); require(plyr)
  if(nchar(as.integer(cn)) != 6) {
    stop("Control number must be 6 digits")
  } else {
    page <- read_html(paste0("https://www.gao.gov/fedrules/", cn)) # Fill control number
    table <- html_table(page, fill = TRUE) # Find only number on page
    if(length(table) == 0) { # No data if control number is not used
      warning("No data found")
      return(data.frame(control_number = cn,
                        no_data = 1))
    } else {
      table <- table[[1]] %>% # Table to data frame
        spread(X1, X2)
      names <- colnames(table) %>% # Create acceptable names
        str_remove_all(":|\\.|-") %>% 
        str_replace_all(" ", "_") %>%
        tolower()
      names(table) <- names
      table <- table %>% mutate(control_number, as.character) # Treat as character
      return(table)
    }
  }
}
```

Example
-------

Below are a few minimal examples using the function. Note that all "control numbers" must be six digits.

``` r
pull_cra(123)           # Throw an error
```

    ## Error in pull_cra(123): Control number must be 6 digits

``` r
kable(pull_cra(123456)) # No warnings
```

| agency                                     | control\_number | description | effective    | fed\_reg\_number | identifier | priority           | published    | received    | subagency                       | type      |
|:-------------------------------------------|:----------------|:------------|:-------------|:-----------------|:-----------|:-------------------|:-------------|:------------|:--------------------------------|:----------|
| Independent Agencies and Govt Corporations | 123456          | Final Rule  | Feb 12, 2002 | 67 FR 6414       | 2070-AB78  | Routine/Info/Other | Feb 12, 2002 | Feb 7, 2002 | Environmental Protection Agency | Non-Major |

``` r
pull_cra(200000)        # Warning that there is no data
```

    ##   control_number no_data
    ## 1          2e+05       1

Compile all CRA Data
--------------------

What if I want all the rules reported under the Congressional Review Act? By making a few assumptions about the range of control numbers, we can give the function a large sequence and produce a full data frame. Here I demonstrate how this is done in parallel to save a bit of time. Note that I modified the function a little bit so that when an error is encountered, the whole process does not fail. This process should pull around 76,000 rules submitted to GAO, but the number grows every day.

``` r
# Create data frame of rules under CRA
library(furrr); library(purrr)
plan(multisession) # For Windows
possibly_pull_cra <- possibly(pull_cra, otherwise = NULL) # Keep mapping even if error

cra <- rbind.fill(future_map(100000:200000, possibly_pull_cra, .progress = T))
```

Caution! Running over what *I think* is the universe of control numbers will take some time.

``` r
# Clean up a little bit

# Removing rows where the control number returned no data
cra <- cra %>% 
  filter(no_data != 1) %>% 
  select(-no_data) %>%
  # Convert dates
  mutate(received = mdy(received),
         effective = mdy(effective),     # Might be missing
         published = mdy(published)) %>% # Might be missing
  arrange(recieved)
```
