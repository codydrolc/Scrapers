#-------------------------------------------------------------------------------
#---- Scrape GAO Congressional Review Act data
# Created: 1/17/2018
# Modified: 10/18/2018

# Note: GAO stores the bolded rule information in a single table on each page.
# There is only one table on each page. So, parsing the html nodes only requires
# taking "table" and nothing else. Converting the "table" from a list to a data
# frame creates "long" format data that is reshaped by the function. As a result,
# each time the function is pulled there will only be one row. 

# An example loop is provided that takes the universe of rules submitted
# under the Congressional Review Act.

#-------------------------------------------------------------------------------

# Load packages
library(tidyverse); library(plyr)

# Scraping function specific to the Government Accountability Office
pull_cra <- function(cn) {
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
      return(table)
    }
  }
}

pull_cra(123) # Should stop
pull_cra(123456) # No warnings
pull_cra(200000) # Warning that there is no data

# Null object to fill
cra <- NULL

# Loop over control numbers
# The range pulls the "universe" of federal rules (~76,000) submitted to GAO
for (i in 100000:200000) {
  print(i)
  bind <- pull_cra(i)
  cra <- rbind.fill(cra, bind) # Combine iterations
}

# Removing rows where the control number returned no data
cra <- cra %>% filter(no_data != 1) %>% select(-no_data)

# Convert dates
cra$received <- mdy(cra$received)
cra$effective <- mdy(cra$effective) # Might be missing
cra$published <- mdy(cra$published) # Might be missing
cra <- arrange(cra$received)
