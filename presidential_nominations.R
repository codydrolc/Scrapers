#-------------------------------------------------------------------------------
#---- Scrape Congress.gov nominations data
# Created: 6/21/2018
# Modified: 6/22/2018

# Note: This function is still VERY much in development!

# Major issue: Not all nominee names are contained in a single table. The current
# function does not handle/parse multiple tables well.

# Load packages
library(tidyverse)

# Scraping function
# Congress.gov data run from 1981 (the 97th Congress) to the present
pullnomz <- function(cong, pn) {
  try(   # Should keep running if page does not exist
    page <- read_html(paste('https://www.congress.gov/nomination/', cong, '-congress/', pn, sep = ''))
  )
  title <- html_node(page, 'h1') %>% # Title of page
    html_text()
  description <- html_nodes(page, 'ul.plain.little_margin') %>% # Basic info
    html_text() %>% 
    str_replace_all("[\r\n]" , "")
  if(length(description) == 3) { # Some pages have limited information; committee and final/latest action tend to be missing
    nominee_description <- description[[1]]
    organization <- description[[2]]
    rcvd_president <- description[[3]]
  } else{ 
  nominee_description <- description[[1]]
  organization <- description[[2]]
  latest_action <- description[[3]]
  rcvd_president <- description[[4]]
  committee <- description[[5]]
  }
  nominee_names <- as.data.frame(html_table(page)) # Names of all nominees
  if(colnames(nominee_names)[[1]] != "Nominee") { return(NA)
  } else {
    return(nominee_data <- data.frame(title = title,
                                      nominee_description = nominee_description,
                                      organization = organization,
                                      latest_action = latest_action,
                                      rcvd_president = rcvd_president,
                                      committee = committee, 
                                      nominee_names = nominee_names[, 1]))
  } # close if else
} # close function

# Produce NA
test <- pullnomz(98, 1)

# 111 nominations
test <- pullnomz(97, 1)
