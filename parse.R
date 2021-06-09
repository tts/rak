library(tidyverse)
library(sf)
library(gdalUtilities)

#-------------
# RAMAVA data
# https://hri.fi/data/fi/dataset/paakaupunkiseudun-tonttivaranto-kortteleittain-seuturamava
#-------------
baseurl <- "https://kartta.hsy.fi/geoserver/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=SeutuRAMAVA_kortteli_12021&format=json"
ramava_wfs <- paste0(baseurl,wfs_request)
data <- st_read(ramava_wfs)

# Helsinki
hki_data <- data %>% 
  sf::st_drop_geometry() %>% 
  filter(kunta == '091') %>% 
  rename(tunnus = kosa) %>% 
  dplyr::select(-gml_id, -kunta, -korttunnus, -ktun, -rekpvm)

write_rds(hki_data, "hki_data.RDS")

#--------------------
# City district data
#--------------------
baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=avoindata:Kaupunginosajako&format=json"
kosat_wfs <- paste0(baseurl, wfs_request)
data_kosat <- st_read(kosat_wfs)

data_kosat <- data_kosat %>% 
  mutate(tunnus = gsub("^0", "", tunnus),
         nimi_fi = str_to_title(nimi_fi),
         nimi_se = str_to_title(nimi_se)) %>% 
  filter(nimi_fi != "Aluemeri",
         nimi_fi != "Ulkosaaret") 

st_crs(data_kosat) <- 3067

# https://gis.stackexchange.com/a/389854
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

data_kosat_p <- ensure_multipolygons(data_kosat)

write_rds(data_kosat_p, "data_kosat_p.RDS")


