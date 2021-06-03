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
  filter(kunta == '091') %>% 
  rename(tunnus = kosa)

rm(data)
gc()

# laskvar_ap grouped by district
hki_data_g <- hki_data %>% 
  st_drop_geometry() %>% 
  dplyr::select(tunnus, laskvar_ap) %>% 
  group_by(tunnus) %>%
  mutate(laskvar_ap_sum = sum(laskvar_ap)) %>%
  dplyr::select(-laskvar_ap) %>% 
  distinct_at(vars(tunnus), .keep_all = TRUE) 

rm(hki_data)
gc()

#--------------------
# City district data
#--------------------
baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=avoindata:Kaupunginosajako&format=json"
kosat_wfs <- paste0(baseurl, wfs_request)
data_kosat <- st_read(kosat_wfs)

data_kosat <- data_kosat %>% 
  mutate(tunnus = gsub("^0", "", tunnus),
         nimi_fi = str_to_title(nimi_fi)) %>% 
  filter(tunnus != "Alumeri",
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

rm(data_kosat)
gc()

merged <- merge(data_kosat_p, hki_data_g)

ggplot(merged) +
  geom_sf(aes(fill = laskvar_ap_sum)) +
  geom_sf_text(data = st_point_on_surface(merged), aes(label = nimi_fi), check_overlap = TRUE,
               colour = "orangered", size = 2.5) +
  scale_fill_viridis_c() +
  guides(fill = guide_legend(title = "Pientalovara (m2)")) +
  theme_void()


