library(tidyverse)
library(sf)
library(janitor)
library(countrycode)
library(rmapshaper)
library(here)

# simplify level 0 and 1 gadm boumdaries
gadm0 <- here("data", "gadm36_levels.gpkg") %>% 
  read_sf(layer = "level0") %>% 
  ms_simplify(keep = 0.01, keep_shapes = TRUE, sys = TRUE)
gadm1 <- here("data", "gadm36_levels.gpkg") %>% 
  read_sf(layer = "level1") %>% 
  ms_simplify(keep = 0.01, keep_shapes = TRUE, sys = TRUE)

# full list of ebird countries
ebird_countries <- "http://ebird.org/ws1.1/ref/location/list?rtype=country" %>% 
  read_csv(na = "") %>% 
  clean_names() %>% 
  mutate(region = country_code) %>% 
  select(region, name = country_name) %>% 
  mutate(iso3c = countrycode(region, "iso2c", "iso3c") %>% 
           coalesce(countrycode(name, "country.name", "iso3c"))) %>% 
  mutate(iso3c = case_when(
    region == "CP" ~ "XCL",
    region == "XK" ~ "XKO",
    TRUE ~ iso3c
  )) %>% 
  # lose Ashmore and Cartier Islands, Coral Sea Islands, High Seas
  filter(!is.na(iso3c))

# tidy up gadm names to match ebird

anti_join(st_set_geometry(gadm0, NULL), ebird_countries, by = c("GID_0"="iso3c"))

f <- here("data", "gadm.gpkg")
unlink(f)
write_sf(gadm0, f, layer = "level0")
write_sf(gadm1, f, layer = "level1")
