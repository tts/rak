library(rvest)
library(httr)
library(polite)

# Scrape osa-alue (=kosa in data) from Wikipedia to get the name of it
# site <- "https://fi.wikipedia.org/wiki/Helsingin_alueellinen_jako"
# session <- bow(site, user_agent = "sonkkilat@gmail.com")
# 
# nodes <- scrape(session) %>% 
#   html_nodes(xpath = "//table[@class='wikitable'][2]//tr//td[1]")
# 
# text <- nodes %>% 
#   html_text() 
# 
# # https://stackoverflow.com/a/47259116
# text_m <- text %>% 
#   .[matches("^[0-9][0-9]", vars=.)]
# 
# write_rds(text_m, "w_hkiosa_raw.RDS")
# text_m <- readRDS("w_hkiosa_raw.RDS")
#
#hkiosa_df <- data.frame(text_m, stringsAsFactors = FALSE)
# 
# hkiosa_df_s <- hkiosa_df %>% 
#   mutate(text_m = gsub("^([0-9][0-9])\\s", "\\1%", text_m)) %>% 
#   separate(text_m, into = c("Nro", "Nimi"), sep = "%") %>% 
#   mutate(Nro = gsub("^0", "", Nro),
#          Nro = as.numeric(Nro),
#          Nimi = gsub("[0-9].*", "", Nimi))# Calculate the sum of laskvar_ap (laskennallinen pientalovaranto) by osa-alue
hki_data_4326_stat <- hki_data_4326 %>%
  group_by(kosa) %>%
  summarise(area_sum = sum(laskvar_ap)) %>%
  ungroup() %>% 
  mutate(lon = map_dbl(geom, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geom, ~st_centroid(.x)[[2]])) %>% 
  st_drop_geometry() %>% 
  arrange(desc(area_sum))

# Join with scraped data 
hki_laskvar_ap <- left_join(x = hki_data_4326_stat, y = hkiosa_df_s, by = c("kosa"="Nro"))

# TODO plot heatmap or density map 
# https://stackoverflow.com/questions/60021868/producing-heat-map-over-geo-locations-in-r

