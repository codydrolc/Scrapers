Scrape the Public Papers of the Presidents: The American Presidency Project
================
Cody A. Drolc

This document presents a function for scraping the Public Papers of the Presidents. The scraper relies on UCSB's The American Presidency Project (APP) to create a data frame with details such as the president, whether the document comes from the Vice President or First Lady, and the whole document text.

The function was recently updated to work with the APP's website overhaul. The information that is pulled is not perfectly clean, but it provides a base for either creating a corpus or doing more complex text analyses. Note that some public papers do not pull perfectly.

Feel free to use and/or edit the **pullprez** function.

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
```

    ## Loading required package: rvest

    ## Loading required package: xml2

    ## Loading required package: lubridate

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    ## Loading required package: tidyverse

    ## -- Attaching packages ----------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts -------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x lubridate::as.difftime() masks base::as.difftime()
    ## x lubridate::date()        masks base::date()
    ## x dplyr::filter()          masks stats::filter()
    ## x readr::guess_encoding()  masks rvest::guess_encoding()
    ## x lubridate::intersect()   masks base::intersect()
    ## x dplyr::lag()             masks stats::lag()
    ## x purrr::pluck()           masks rvest::pluck()
    ## x lubridate::setdiff()     masks base::setdiff()
    ## x lubridate::union()       masks base::union()

    ## Loading required package: tidytext

``` r
test
```

    ##       id       date                     president press_secretary
    ## 1 210000 1942-01-26 \n    Franklin D. Roosevelt                 0
    ##   vice_president first_lady
    ## 1              0          0
    ##                                                                                         title
    ## 1 \n    Statement on Raw Materials, Munition Assignments, and Shipping Adjustment Boards.\n  
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               text
    ## 1 \n    To further coordination of the United Nations war effort, the President and Prime Minister Churchill have set up three Boards to deal with munition assignments, shipping adjustment, and raw materials. The functions of these Boards are outlined in the following statements.\nMembers of the Boards will confer with representatives of the Union of Soviet Socialist Republics, China, and such other of the United Nations as are necessary to attain common purposes and provide for the most effective utilization of the joint resources of the United Nations.\nCOMBINED RAW MATERIALS BOARD\nA planned and expeditious utilization of the raw material resources of the United Nations is necessary in the prosecution of the war. To obtain such a utilization of our raw material resources in the most efficient and speediest possible manner, we hereby create the "Combined Raw Materials Board."\nThis Board will:\n(a) Be composed of a representative of the British Government and a representative of the United States Government. The British member will represent and act under the instruction of the Minister of Supply. The Board shall have power to appoint the staff necessary to carry out its responsibilities.\n(b) Plan the best and speediest development, expansion, and use of the raw material resources, under the jurisdiction or control of the two Governments, and make the recommendations necessary to execute such plans. Such recommendations shall be carried out by all parts of the respective Governments.\n(c) In collaboration with others of the United Nations work toward the best utilization of their raw material resources, and, in collaboration with the interested Nation or Nations, formulate plans and recommendations for the development, expansion, purchase, or other effective use of their raw materials.\nMUNITIONS ASSIGNMENTS BOARD\n1. The entire munition resources of Great Britain and the United States will be deemed to be in a common pool, about which the fullest information will be interchanged.\n2. Committees will be formed in Washington and London under the Combined Chiefs of Staff in a manner similar to the South-West Pacific Agreement. These Committees will advise on all assignments both in quantity and priority, whether to Great Britain and the United States or other of the United Nations in accordance with strategic needs.\n3. In order that these Committees may be fully apprised of the policy of their respective Governments, the President will nominate a civil Chairman who will preside over the Committee in Washington, and the Prime Minister of Great Britain will make a similar nomination in respect of the Committee in London. In each case the Committee will be assisted by a Secretariat capable of surveying every branch and keeping in touch with the work of every subcommittee as may be necessary.\n4. The civilian Chairman in Washington and London may invite representatives of the State Department, the Foreign Office, or production ministries or agencies to attend meetings.\nCOMBINED SHIPPING ADJUSTMENT BOARD\n1. In principle, the shipping resources of the two countries will be deemed to be pooled. The fullest information will be interchanged.\n2. Owing to the military and physical facts of the situation around the British Isles, the entire movement of shipping now under the control of Great Britain will continue to be directed by the Ministry of War Transport.\n3. Similarly, the appropriate authority in the United States will continue to direct the movements and allocations of United States shipping, or shipping of other powers under United States control.\n4. In order to adjust and concert in one harmonious policy the work of the British Ministry of War Transport and the shipping authorities of the United States Government, there will be established forthwith in Washington a Combined Shipping Adjustment Board, consisting of a representative of the United States and a representative of the British Government, who will represent and act under the instructions of the British Minister of War Transport.\n5. A similar Adjustment Board will be set up in London consisting of the Minister of War Transport and a representative of the United States Government.\n6. In both cases the executive power will be exercised solely by the appropriate shipping agency in Washington and by the Minister of War Transport in London.\n  
    ##   words
    ## 1   693

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
