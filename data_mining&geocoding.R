# Buildings Age Map of Perm

# Author: Alexander Sheludkov, Institute of Geography, RAS
# Date: 14 March 2018

# Description:
# ....

# libraries

library(sp)
library(rgdal)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(jsonlite)

# ==============
# 1. Data mining
# ==============

# =================================
# 1.1. Objects of cultural heritage

# Source: Russian Ministry of Culture

# Read the data
heritage <- read_csv("data/heritage.csv")

# ========================
# 1.2. Apartment buildings

# Source: reformagkh.ru

# tid - id of the city. We can find it from the href 
# There are 6 753 buildings

res <- data_frame()
for(i in 1:68){
  # Watch your step
  print(i)
  url <- paste0("https://www.reformagkh.ru/myhouse?tid=2299765&sort=name&order=asc&page=", i, "&limit=100")
  # Save as html page
  page <- read_html(url)
  # Extrcat address data
  page %>% 
    html_nodes('td a') %>%  # use css selector 
    html_text() -> address
  # Extract year
  page %>%
    html_nodes('.grid td:nth-child(2)') %>% 
    html_text() -> year
  res <- bind_rows(res,data_frame(address, year))
  Sys.sleep(1)
}

# Replace "н.д." with NA and change year type to integer
res %>% 
  mutate(year = str_replace(year, "н.д.", NA_character_) %>% as.integer()) -> res

# Let's take a look at the data
summary(res) # there are some stranges
res %>% arrange(year) %>% head()
# Fix 
res[res$address == "г. Пермь, ул. Холмогорская, д. 4/2", 2] <- 1975
res[res$address == "г. Пермь, ул. Серпуховская, д. 15", 2] <- 1955
# Remove all na rows
res %>% filter(!is.na(year)) -> res

# Check the distirbution
res %>% ggplot(aes(year))+
  geom_histogram(aes(), binwidth = 2)+
  labs(y = "Number of buildings", x = "Year")+
  scale_x_continuous(breaks = seq(1850, 2017, 10))


# ============
# 2. Geocoding
# ============

perm_buildings <- data_frame()

# Цикл запросов к геокодеру Яндекс
for (n in 1:nrow(res)) {
  # Watch the steps
  print(n)
  # Specifying the url for desired website to be scrapped
  url <- paste0("https://geocode-maps.yandex.ru/1.x/?geocode=", res[n, 1])
  # Reading the HTML code from the website
  page <- read_html(url)
  
  # Parcing with css selector number of found results and their precision
  page %>% html_nodes('found') %>% html_text() %>% as.integer(.) -> found
  page %>% html_nodes('precision') %>% html_text() -> precision
  
  # If there are results and precision is exact, we add coords to our table
  if (found >= 1 && precision[1] == "exact") {
    coords <- page %>% html_nodes('pos') %>% html_text() %>% .[1]
    temp_table <- bind_cols(res[n,], coords = coords)
  } else {
    temp_table <- cbind(res[n,], coords = NA)
  }
  perm_buildings <- bind_rows(perm_buildings, temp_table)
  # Sleep
  Sys.sleep(1)
}

# Split coords column into Lat and Lon
perm_buildings_geocoded <- separate(perm_buildings,
                                    coords, # column to be splitted
                                    into = c("Lon", "Lat"), # new columns' names
                                    sep = " ", # separator
                                    remove = TRUE, # remove splitted column
                                    convert = TRUE) # default type conversion
# Check the results
perm_buildings_geocoded %>% summary()
# Remove NAs
perm_buildings_geocoded %>% 
  filter(!is.na(Lon)) -> perm_buildings_geocoded

# Add date_label column
perm_buildings_geocoded %>% mutate(date_label = year) -> perm_buildings_geocoded

# Add heritage data
perm_buildings_geocoded %>% rbind(., heritage_geocoded) -> perm_buildings_geocoded

# Save the results in csv file
write_csv(perm_buildings_geocoded, "data/perm_buildings_geocoded.csv")


# ============================
# 3. Working with polygon data
# ============================

# We downloaded OSM spatial polygond data.
# Now we need to combine osm polygons with "year" and "address" data from points 

# 3.1. Read the data
points <- readOGR("data/points.geojson")     # geocoded points
polygons <- readOGR("data/polygons.geojson") # osm polygons

# 3.2. Join polygons with data 
# Extract data from points overlaying over polygons 
points_data <- over(polygons, points[,c("address", "year", "date_label")])
# Assign data to polygons attribute dataframe
polygons@data$year <- points_data$year
polygons@data$address <- points_data$address
polygons@data$datelabel <- points_data$date_label
polygons@data %>% select(address, year, datelabel) -> polygons@data
# Remove buildings with NA year
perm_buildings_osm_year <- polygons[!is.na(polygons@data$year), ]

# 3.3. Save SpatilaPolygonsDataFrame as GeoJSON file
writeOGR(perm_buildings_osm_year, "data/perm_buildings_age.geojson", 
         layer = "perm_buildings_age.geojson", driver = "GeoJSON", overwrite_layer = T)
