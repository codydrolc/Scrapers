#-------------------------------------------------------------------------------
#---- Scrape GAO Congressional Review Act data
# Created: 1/17/2018
# Modified: 9/6/2018

# Note: GAO stores the bolded rule information in a single table on each page.
# There is only one table on each page. So, parsing the html nodes only requires
# taking "table" and nothing else. Converting the "table" from a list to a data
# frame creates "long" format data that is reshaped at each iteration of the
# loop. As a result, one row is added to the data frame at each iteration.
# In addition, some rules have an "identification" variable, making base 
# "rbind" unusable.

# The control number range given in the loop is much wider than it needs to be,
# but the range ensures that every listed rule is captured. The loop will skip
# unused control numbers and continue running, but may "choke" if the connection
# with a page times out.

#-------------------------------------------------------------------------------
rm(list = ls())

# Load packages
library(rvest); library(plyr); library(tidyr); library(dplyr); library(lubridate)

# Null object to fill
cra <- NULL

# Loop over control numbers
# The range pulls the "universe" of federal rules (~76,000) submitted to GAO
for (i in 100000:200000) {
  print(i) # Show iteration
  url <- paste0("https://www.gao.gov/fedrules/", i) # Fill control number
  read_page <- read_html(url) # Import page
  table <- html_table(read_page, fill = TRUE) # Find only table on page
  if (length(table) == 0) next # Skip if empty
  table <- table[[1]] # Table to data frame
  table <- spread(table, X1, X2) # Convert long to wide (agency, sub-agency, etc. to variable names)
  cra <- rbind.fill(cra, table) # Combine iterations
}

# Rename columns
names(cra) <- sub(":", "", names(cra))

# Convert dates
cra$Received <- mdy(cra$Received)
cra$Effective <- mdy(cra$Effective) # Might be missing
cra$Published <- mdy(cra$Published) # Might be missing
cra <- arrange(cra$Received)

# Reorder columns
cra <- select(cra, Agency, `Sub-Agency`, Type, Description, Priority, 
  Effective, Received, Published, `Fed. Reg. Number`)
