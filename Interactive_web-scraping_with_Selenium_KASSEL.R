# INTERACTIVE WEB-SCRAPING WITH SELENIUM ----------------------------------

# Simon Kassel
# 08 Feb 2017
# From MUSA620: Data Wrangling and Visualization @UPenn
# Instructor: Max Galka

# This script scrapes home value and square footage data from the Philadelphia
# Property Assessor's database. I have determined the prices per square foot
# for approximately 900 homes within the proximity of Rittenhouse Square.
# It takes as an input:
#   - A dataset of home addresses and unit numbers (columns 1 and 2 identify
#     the address and unit number's respectively)
# And it returns:
#   - The original csv with added fields for home value, square footage, price
#     per square foot and X/Y coordinates
#   - Data visualizations associated with home value/square foot in the area


# PACKAGES/GLOBAL OPTIONS -------------------------------------------------
# Clear environment
rm(list = ls())

# Packages
library(RSelenium)
library(ggplot2)
library(dplyr)
library(ggmap)
library(plyr)

# Global options
options(scipen = "999")
options(stringsAsFactors = FALSE)

# Working directory
setwd("<WORKING DIRECTORY>")


# DATA --------------------------------------------------------------------
# Import csv and add new fields
read_data <- function(csv.link){
  data <- read.csv(csv.link)
  data$valuation <- 0
  data$square.footage <- 0
  data$pp.sqft <- 0
  return(data)
}

# Link to condo dataset csv
rittenhouse <- "https://raw.githubusercontent.com/MUSA-620-Fall-2017/MUSA-620-Week-3/master/rittenhouse-condos.csv"

# read data
data <- read_data(rittenhouse)


# WEB-SCRAPING ------------------------------------------------------------
# Helper functions: 
#   Establish and open remote driver connection
open.rmd <- function(){
  system("kill -9 $(lsof -ti tcp:4567)")
  rD <- rsDriver(port = 4567L, browser = "firefox")
  remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4567L, browserName = "firefox")
}

#   Convert square footage/price strings to numeric
toNum <- function(a.string){
  return(gsub("\\$|,", "", a.string) %>% as.numeric())
}

#   Close server connection
close.server <- function(){
  remDr$close()
}

# Web-scraping constants:
theURL <- "http://property.phila.gov/"
addr.selector <- "#search-address"
unit.selector <- "#search-unit"
price.xpath <- "/html/body/div[1]/main/div[3]/div[1]/div[7]/table/tbody/tr[1]/td[2]/span"
sq.footage.selector <- "div.panel:nth-child(2) > div:nth-child(6) > div:nth-child(2) > strong:nth-child(2)"

# Remote driver
open.rmd()
# Launch browser
remDr$open()

# Scrape data
for (x in 1:nrow(data)){
  
  # Navigate to the URL of the search page
  remDr$navigate(theURL)
  
  # Enter address field
  addressField <- remDr$findElement("css selector", addr.selector)
  addressField$sendKeysToElement(list(data$address[x]))
  
  # Enter unit number
  unitField <- remDr$findElement("css selector", unit.selector)
  unitField$sendKeysToElement(list(data$unit[x]))
  
  # Search for property
  addressField$sendKeysToElement(list(key = 'enter'))
  
  # Wait for page to load
  Sys.sleep(1.5)
  
  # Find the price element with xpath
  price <- remDr$findElement("xpath", price.xpath)
  # Find the square footage with css selector
  square.foot <- remDr$findElement("css selector", sq.footage.selector)
  
  # Assign the price to the appropriate cell in the data frame
  data$valuation[x] <- price$getElementText() %>% 
    unlist %>% 
    toNum()
  
  # Assign the sq. footage to the appropriate cell in the data frame
  data$square.footage[x] <- square.foot$getElementText() %>% 
    unlist() %>% 
    toNum()
  
  # Calculate price per sq foot
  data$pp.sqft[x] <- data$valuation[x] / data$square.footage[x]
  
  # Log loop progress to console
  print(paste("Completed scraping ", x, " of ", "[", nrow(data), "]", sep = ""))
}

# close server
close.server()


# DATA VISUALIZATION ------------------------------------------------------
# Geocode addresses
data <- as.list(data$address) %>%
  ldply(function(x){
    return(geocode(location = paste(x, "Philadelphia, PA", sep = " ")))
  }) %>%
  as.data.frame() %>%
  cbind(data, .)

# Get basemap
basemap <- get_stamenmap(
  bbox = c(min(data$lon) - .001, min(data$lat) - .001, max(data$lon) + .001, max(data$lat) + .001), 
  maptype = "toner-hybrid", 
  zoom = 18)

# Map home prices per square foot
map.homes <- function(){
  map <- ggmap(basemap) + 
    geom_jitter(data = data, aes(x = lon, y = lat, color = pp.sqft), 
                width = 0.0002, height = 0.0002) +
    scale_color_gradientn("Price per \nsq. foot ($)", colours = c("blue", "green", "yellow")) +
    ggtitle("Rittenhouse Sq. Home Prices") +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.background = element_rect(fill = "black"),
      panel.grid = element_blank())
  return(map)
}

# Plot a histogram of prices per square foot
plot.histogram <- function(){
  histogram <- ggplot(data, aes(pp.sqft)) + 
    geom_histogram(bins = 25) + 
    geom_vline(xintercept = mean(data$pp.sqft), color = "red") +
    annotate("text", x = 850, y = 200, 
             label = paste("Average price per \nsquare foot: $", round(mean(data$pp.sqft), 2), sep = ""),
             color = "red") +
    xlab("Price per square foot ($)") + ylab("Count") + 
    ggtitle("Rittenhouse Square Home Values") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
      )
  return(histogram)
}

# Save both plots in WD
ggsave("Kassel_web-scraping_HISTOGRAM.pdf", plot = plot.histogram(), device = "pdf", width = 6, height = 6, units = "in")
ggsave("Kassel_web-scraping_MAP.pdf", plot = map.homes(), device = "pdf", width = 6, height = 6, units = "in")

# Export data to a csv
write.csv(data, "Kassel_web-scraping_OUTPUT.csv")

