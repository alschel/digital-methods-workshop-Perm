# Buildings Age Map of Perm

# Author: Alexander Sheludko, Institute of Geography, RAS
# Date: 14 March 2018

# Description:
# ....

# libraries

library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(readr)

# ==============
# 1. Data mining
# ==============

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

# Разобьем строку координат в отдельные столбцы Lat и Lon
perm_buildings_geocoded <- separate(perm_buildings,
                                    coords, # столбец для деления
                                    into = c("Lon", "Lat"), # названия новых столбцов
                                    sep = " ", # основание для разделения
                                    remove = TRUE, # удаляем столбец coords
                                    convert = TRUE) # применяем к новым значениям функцию type.convert (автоматически определяет тип данных)

perm_buildings_geocoded %>% summary() # 293 не нашлись
# Remove NAs
perm_buildings_geocoded %>% 
  filter(!is.na(Lon)) -> perm_buildings_geocoded


# Save the results in csv file
write_csv(perm_buildings_geocoded, "data/perm_buildings_geocoded.csv")
