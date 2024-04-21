# Web Scraping of TripAdvisor Hotel Reviews (Using Docker)

## Install and Load Required Packages ###################################################################################
library("RSelenium")
library("rvest")
library("tidyverse")
library("sys")
library("rJava")
library("purrr")

## Fire Up Selenium (using Docker) ######################################################################################
shell('docker run -d -p 4445:4444 selenium/standalone-firefox') # Create a new container in docker to run remote server
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")

## Create a Function to iterate web-scraping process across different hotels #############################################
## The function takes iterates through a dataframe with Hotel Name and Tripadvisor URL to gather reviews for hotels ######

tripadvisor_review <- function(csv){ # Function takes in a dataframe (contains Hotel Name and Tripadvisor URL)
  for (i in 1:nrow(csv)){
    hotel_name = csv[i,1]
    url = csv[i,2]
    
    # Scrape reviews from the first Page #####################################################################
    remDr$open()
    remDr$navigate(url)
    
    # Click 'read more' on all
    try({remDr$findElements("xpath", ".//div[contains(@data-test-target, 'expand-review')]")[[1]]$clickElement()}, silent  = TRUE)

    # Sleep 2 seconds to allow DOM to load
    Sys.sleep(2)
    
    # Scrape the HTML and make it available for rvest
    html <- remDr$getPageSource()[[1]]
    nodes <- html %>% read_html() %>%
      html_elements(".YibKl")
    
    # Find total no. of reviews and the no. of pages
    no_of_pages = NULL
    tr = html %>% read_html() %>% html_elements(".POjZy") %>% html_text()
    no_of_reviews = gsub("\\(|\\,|\\)","", tr[2]) %>% as.numeric()
    no_of_pages = ceiling(no_of_reviews/10) # 10 reviews per page
    
    # Create empty vector for each variable to store data
    titles = c()
    reviews = c()
    scores = c()
    trip_types = c()
    dates = c()
    
    # Extract data from each review boxes
    for (i in 1:length(nodes)){
      titles[i] <- nodes[i] %>%
        html_element(".Qwuub") %>%
        html_text()
      
      reviews[i] <- nodes[i] %>%
        html_element(".QewHA") %>%
        html_text()
      
      scores[i] <- nodes[i] %>%
        html_element(".Hlmiy") %>%
        html_children() %>% # Look at the child of the named class
        html_attr("class") %>% # Grab the name of the class of the child
        str_remove_all("ui_bubble_rating bubble_") # Remove unwanted strings
      
      dates[i] <- ifelse(length(html_element(nodes[i], ".teHYY")) == 0, NA, # not all reviews have dates
                         html_element(nodes[i], ".teHYY") %>%
                           html_text() %>%
                           str_remove_all("Date of stay:"))
      
      trip_types[i] = nodes[i] %>% 
        html_element(".TDKzw") %>%
        html_text() %>%
        str_remove_all("Trip type: ")
    }
    
    # IF Condition: to see if there is a need to iterate the remaining pages - if reviews <=10, then the no. of pages is only 1
    if (is.na(no_of_pages)) {
      remDr$close()
      print(paste0(hotel_name, "- no reviews"))
    } else if (no_of_pages == 1){
      # Basic Data Cleaning ##################################################################################################
      reviews_df = cbind.data.frame(Hotel = hotel_name, Titles = titles, Reviews = reviews, Dates = dates, "Review Rating" = scores, "Travel Type" = trip_types)
      
      reviews_df$`Review Rating` = as.numeric(reviews_df$`Review Rating`)/10
      reviews_df$`Travel Type`= stringr::word(trip_types, -1)
      
      filename = paste0(hotel_name, " Reviews")
      write.csv(reviews_df, paste0(filename, ".csv")) # Save reviews for each hotel as a csv
      
      remDr$close()
      Sys.sleep(1)
    } else {
    # Iterate the process through all the remaining pages #######################################################################
    titles_loop = c()
    reviews_loop = c()
    scores_loop = c()
    trip_types_loop = c()
    dates_loop = c()
    
    #remDr$navigate(url)
    #titles = as_tibble()
    #reviews = as_tibble()
    
    # 1500 reviews limit
    pages = min(no_of_pages, 100)
    
    for (i in 2:pages) {
      skip_to_next = FALSE
      tryCatch({
      remDr$findElements("xpath", ".//a[contains(@class,'ui_button nav next primary')]")[[1]]$clickElement() # Click on 'Next' on the Reviews Tab to go to the next page
      Sys.sleep(2)
      
      remDr$findElements("xpath", ".//div[contains(@data-test-target, 'expand-review')]")[[1]]$clickElement() # Click on 'Read More' to expand reviews
      Sys.sleep(2)
      
      html = remDr$getPageSource()[[1]]
      nodes <- html %>% read_html() %>%
        html_elements(".YibKl")
      
      for (i in 1:length(nodes)){
        titles_loop[i] <- nodes[i] %>%
          html_element(".Qwuub") %>%
          html_text()
        
        reviews_loop[i] <- nodes[i] %>%
          html_element(".QewHA") %>%
          html_text()
        
        scores_loop[i] <- nodes[i] %>%
          html_element(".Hlmiy") %>%
          html_children() %>% # Look at the child of the named class
          html_attr("class") %>% # Grab the name of the class of the child
          str_remove_all("ui_bubble_rating bubble_") # Remove unwanted strings
        
        dates_loop[i] <- ifelse(length(html_element(nodes[i], ".teHYY")) == 0, NA, 
                                html_element(nodes[i], ".teHYY") %>%
                                  html_text() %>%
                                  str_remove_all("Date of stay:"))
        
        trip_types_loop[i] = nodes[i] %>% 
          html_element(".TDKzw") %>%
          html_text() %>%
          str_remove_all("Trip type: ")
      }
      titles = c(titles, titles_loop)
      reviews = c(reviews, reviews_loop)
      scores = c(scores, scores_loop)
      trip_types = c(trip_types, trip_types_loop)
      dates = c(dates, dates_loop)
      }, error = function(e) { skip_to_next <<- TRUE})
      if(skip_to_next) { next }   
    }
    # Basic Data Cleaning ##################################################################################################
    reviews_df = cbind.data.frame(Hotel = hotel_name, Titles = titles, Reviews = reviews, Dates = dates, "Review Rating" = scores, "Travel Type" = trip_types)
    
    reviews_df$`Review Rating` = as.numeric(reviews_df$`Review Rating`)/10
    reviews_df$`Travel Type`= stringr::word(trip_types, -1)
    
    filename = paste0(hotel_name, " Reviews")
    write.csv(reviews_df, paste0(filename, ".csv")) # Save reviews for each hotel as a csv
    
    remDr$close()
    Sys.sleep(1)
    }
}}

## Execute the Code #####################################################################################################

tripadvisor_review()
