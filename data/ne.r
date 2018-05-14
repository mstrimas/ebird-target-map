library(tidyverse)
library(sf)
library(rnaturalearth)
library(janitor)
library(here)

# state and country boundaries from natural earth
ne_country <- ne_download(scale = 50, category = "cultural",
                          type = "admin_0_countries_lakes",
                          returnclass = "sf") %>%
  clean_names() %>% 
  # fixes to match ebird
  mutate(region = case_when(
    adm0_a3 == "ALD" ~ "FI",
    adm0_a3 == "ATC" ~ "AC",
    adm0_a3 == "CYN" ~ "CY",
    adm0_a3 == "FRA" ~ "FR",
    adm0_a3 == "IOA" ~ "CX",
    adm0_a3 == "KAS" ~ "IN",
    adm0_a3 == "NOR" ~ "NO",
    adm0_a3 == "SOL" ~ "SO",
    TRUE ~ iso_a2
  )) %>% 
  group_by(region) %>% 
  summarize()
ne_state <- ne_download(scale = 50, category = "cultural",
                        type = "admin_1_states_provinces_lakes",
                        returnclass = "sf") %>%
  clean_names() %>%
  filter(iso_a2 %in% c("US", "CA")) %>%
  select(region = iso_3166_2, name)

# full list of ebird countries
ebird_countries <- "http://ebird.org/ws1.1/ref/location/list?rtype=country" %>% 
  read_csv(na = "") %>% 
  clean_names() %>% 
  mutate(region = country_code) %>% 
  select(region, name = country_name)
ne_country <- inner_join(ne_country, ebird_countries, by = "region")
# these "countries" are missing from natural earth
ne_country %>% 
  st_set_geometry(NULL) %>% 
  anti_join(ebird_countries, .)

f <- here("data", "ne.gpkg")
unlink(f)
write_sf(ne_country, f, layer = "countries")
write_sf(ne_state, f, layer = "states")
