library(tidyverse)
library(sf)

#-------------
# RAMAVA data
# https://hri.fi/data/fi/dataset/paakaupunkiseudun-tonttivaranto-kortteleittain-seuturamava
#-------------
baseurl <- "https://kartta.hsy.fi/geoserver/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=SeutuRAMAVA_kortteli_12021&format=json"
ramava_wfs <- paste0(baseurl,wfs_request)
data <- st_read(ramava_wfs)

write_rds(data, "ramava_data.RDS")
data <- readRDS("ramava_data.RDS")

# Helsinki
hki_data <- data %>% 
  filter(kunta == '091') %>% 
  st_set_crs(., 4326) %>% 
  st_transform(crs = 4326) %>% 
  mutate(kosa = gsub("^0", "", kosa)) %>% 
  st_drop_geometry()

rm(data)
gc()

#--------------------
# City district data
#--------------------
baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=avoindata:Kaupunginosajako&format=json"
kosat_wfs <- paste0(baseurl, wfs_request)
data_kosat <- st_read(kosat_wfs)

data_kosat <- data_kosat %>% 
  st_set_crs(., 4326) %>% 
  st_transform(crs = 4326) 

write_rds(data_kosat, "data_kosat.RDS")

#---------------
# Join datasets
#---------------
joined <- inner_join(hki_data, data_kosat, by = c("kosa"="tunnus"))

joined <- joined %>% 
  select(kosa, nimi_fi, laskvar_ap, geom) %>% 
  group_by(kosa) %>%
  mutate(laskvar_ap_sum = sum(laskvar_ap)) %>%
  select(-laskvar_ap) %>% 
  distinct_at(vars(kosa), .keep_all = TRUE) %>% 
  ungroup()

rm(hki_data, data_kosat)
gc()

write_rds(joined, "joined.RDS")

#------
# Plot
#------

# TODO geom_sf_text fails
ggplot(joined) + 
  geom_sf(aes(geometry = geom, fill = laskvar_ap_sum)) +
  scale_fill_viridis_c(direction = -1) +
  geom_sf_text(aes(label = nimi_fi, geometry = geom)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
  


