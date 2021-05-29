library(tidyverse)
library(sf)
library(rvest)
library(httr)
library(polite)

# Data
baseurl <- "https://kartta.hsy.fi/geoserver/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=SeutuRAMAVA_kortteli_12021&format=json"
ramava_wfs <- paste0(baseurl,wfs_request)
data <- st_read(ramava_wfs)

hki_data <- data %>% 
  filter(kunta == '091')

hki_data <- st_set_crs(hki_data, 4326)

hki_data_4326 <- hki_data %>% 
  st_transform(crs = 4326)

rm(data, hki_data)
gc()


# Scraping osa-alue (=kosa in data) from W

site <- "https://fi.wikipedia.org/wiki/Helsingin_alueellinen_jako"

session <- bow(site, user_agent = "sonkkilat@gmail.com")

nodes <- scrape(session) %>% 
  html_nodes(xpath = "//table[@class='wikitable'][3]//tr//td[1]")

text <- nodes %>% 
  html_text() 

# https://stackoverflow.com/a/47259116
text_m <- text %>% 
  .[matches("^[0-9][0-9][0-9]", vars=.)]

# TODO Separate/split




