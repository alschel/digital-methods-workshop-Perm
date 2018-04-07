# Buildings Age Map of Perm
# Author: Alexander Sheludkov, Institute of Geography, RAS
# Date: 7 April 2018

library(jsonlite)
library(tidyr)
library(readr)

# Source: Russian Ministry of Culture

# =======================
# 1. Load and filter data
# =======================

# Read the data
json <- fromJSON("http://opendata.mkrf.ru/opendata/7705851331-egrkn/data-25-structure-2.json")
# Explore the data
json$data$general %>% head()

# Select the only data we need
heritage <- bind_cols(name = json$data$general$name,
                      type = json$data$general$objectType$value,
                      region = json$data$general$region$value,
                      date_label = json$data$general$createDate,
                      address = json$data$general$address$fullAddress) %>% 
  filter(region == "Пермский край", !is.na(address), str_detect(address, "г. Пермь"),
         !is.na(date_label), type != "Достопримечательное место") %>% select(-type, -region)

# Manually get rid of monuments, cemetries etc. 
heritage %>% arrange(name) %>% slice(-c(1,2, 11:18, 38, 49, 52, 76:92, 94:98, 102, 107, 111, 116, 117, 123)) ->
  heritage

# Extract year from 
heritage %>% mutate(year = str_extract(date_label, "[0-9]{4}")) -> heritage

# Since for some buildings we do not know the exact construction year, 
# we just code them as 1901 (beginning of the XX century), 1801 (XVIII and XIX century).
heritage %>% arrange(date_label) %>% slice(-c(1:6)) -> heritage
heritage[c(1:6, 61:75, 79, 84:87), 4] <- "1801"
heritage[c(76:78, 80:83), 4] <- "1901"

#  Change data type
heritage$year <- as.integer(heritage$year)

# Keep only address and year columns
heritage %>% select(address, year, date_label) -> heritage

# Save the file
write_csv(heritage, "data/heritage.csv")

# ============
# 2. Geocoding
# ============

heritage_geocoded <- data_frame()

# Цикл запросов к геокодеру Яндекс
for (n in 1:nrow(heritage)) {
  # Watch the steps
  print(n)
  # Specifying the url for desired website to be scrapped
  url <- paste0("https://geocode-maps.yandex.ru/1.x/?geocode=", heritage[n, 1])
  # Reading the HTML code from the website
  page <- read_html(url)
  
  # Parcing with css selector number of found results and their precision
  page %>% html_nodes('found') %>% html_text() %>% as.integer(.) -> found
  page %>% html_nodes('precision') %>% html_text() -> precision
  
  # If there are results and precision is exact, we add coords to our table
  if (found >= 1 && precision[1] == "exact") {
    coords <- page %>% html_nodes('pos') %>% html_text() %>% .[1]
    temp_table <- bind_cols(heritage[n,], coords = coords)
  } else {
    temp_table <- cbind(heritage[n,], coords = NA)
  }
  heritage_geocoded <- bind_rows(heritage_geocoded, temp_table)
  # Sleep
  Sys.sleep(1)
}

# Split coords column into Lat and Lon
heritage_geocoded <- separate(heritage_geocoded,
                              coords, # column to be splitted
                              into = c("Lon", "Lat"), # new columns' names
                              sep = " ", # separator
                              remove = TRUE, # remove splitted column
                              convert = TRUE) # default type conversion
# Check the results
heritage_geocoded %>% summary()
# Remove NAs
heritage_geocoded %>% 
  filter(!is.na(Lon)) -> heritage_geocoded

# Save the results in csv file
write_csv(heritage_geocoded, "data/heritage_geocoded.csv")
