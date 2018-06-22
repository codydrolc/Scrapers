#-------------------------------------------------------------------------------
#---- Scrape Congress.gov nominations data
# Created: 6/22/2018

# Load packages
library(rvest); library(tidyverse)

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
  nominee_description <- description[[1]]
  organization <- description[[2]]
  latest_action <- description[[3]]
  rcvd_president <- description[[4]]
  committee <- description[[5]]
  confirmed <- if_else(str_detect(latest_action, "Confirmed"), 1, 0) # Final action has "Confirmed" in description
  nominee_names <- as.data.frame(html_table(page)) # Names of all nominees
  if(ncol(nominee_names) > 1) { return(NA)
  } else {
    return(nominee_data <- data.frame(title = title,
                                      nominee_description = nominee_description,
                                      organization = organization,
                                      latest_action = latest_action,
                                      rcvd_president = rcvd_president,
                                      committee = committee, 
                                      nominee_names = nominee_names, 
                                      confirmed = confirmed))
  } # close if else
} # close function

# Produce NA
test <- pullnomz(98, 1)

# 111 nominations
test <- pullnomz(98, 1)
