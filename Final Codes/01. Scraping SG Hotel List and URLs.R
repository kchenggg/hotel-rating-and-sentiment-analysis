# Hotel Data Scraping

## Install and Load Required Packages ###################################################################################
library("RSelenium")
library("rvest")
library("tidyverse")
library("sys")
library("rJava")

## Fire Up Selenium (using Docker) ######################################################################################
shell('docker run -d -p 4445:4444 selenium/standalone-firefox') # Create a new container in docker to run remote server
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")

## Define URLs and necessary variables #########################################
url_hotels = "https://www.tripadvisor.com.sg/Hotels-g294265-zfd21371-a_sort.POPULARITY-Singapore-Hotels.html"
# Sort the Hotels page by popularity, filter by Hotels, and click 'See All' before getting the URL - saves time on clicking elements to expand and filter

no_of_hotels = 386
pages = ceiling(no_of_hotels/30) + 1

## Click 'See All' and extract all nodes #######################################
remDr$open()
remDr$navigate(url_hotels)

## Get the Page Source #########################################################
html_hotels <- remDr$getPageSource()[[1]]

# Get the Nodes ################################################################
nodes_hotels = html_hotels %>%
  read_html() %>%
  html_elements(".listing_title a")

# First Page ###################################################################
hotelname = c()
hotelurls = c()
for (i in 1:length(nodes_hotels)){
  hotelname[i] = nodes_hotels[i] %>%
    html_text()
  hotelurls[i] = nodes_hotels[i] %>%
    html_attr("href")
}

## Get the Hotel Names and Urls ################################################
# Loop through all the pages
#remDr$navigate(url_hotels)
for (i in 2:pages){
  remDr$findElements("xpath", ".//a[contains(@class,'nav next ui_button primary')]")[[1]]$clickElement()
  Sys.sleep(2)
  
  html_hotel <- remDr$getPageSource()[[1]]
  Sys.sleep(2)
  nodes_hotel <- html_hotel %>% read_html() %>%
    html_elements(".listing_title a")
  
  hotelname_loop = c()
  hotelurls_loop = c()
  
  for (i in 1:length(nodes_hotel)){
    hotelname_loop[i] = nodes_hotel[i] %>%
      html_text()
    hotelurls_loop[i] = nodes_hotel[i] %>%
      html_attr("href")
  }
  hotelname = c(hotelname, hotelname_loop)
  hotelurls = c(hotelurls, hotelurls_loop)
}

## Clean Hotel Data ############################################################
hotels_df = cbind.data.frame("Hotel" = hotelname, "URL" = hotelurls)

# Scraped Data includes 'Sponsored' Hotel listing - need to get rid of duplicates
hotels <- hotels_df %>%
  mutate("TripAdvisor Rank" = stringr::word(Hotel, 7), 
         Hotel = word(Hotel, 8, -1),
         URL = paste0("https://www.tripadvisor.com.sg", URL))

hotels$`TripAdvisor Rank` = as.numeric(hotels$`TripAdvisor Rank`)

hotels = hotels %>% drop_na() %>%
  distinct() # drop NAs and remove duplicates
