library(tidyverse)
library(sf)
library(rnaturalearth)
library(tigris)
library(janitor)
library(here)

# state and country boundaries from natural earth
ne_country <- ne_download(scale = 50, category = "cultural",
                          type = "admin_0_countries_lakes",
                          returnclass = "sf") %>%
  clean_names() %>% 
  # fixes to match ebird
  mutate(region_code = case_when(
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
  group_by(region_code) %>% 
  summarize() %>% 
  ungroup()
ne_state <- ne_download(scale = 50, category = "cultural",
                        type = "admin_1_states_provinces_lakes",
                        returnclass = "sf") %>%
  clean_names() %>%
  filter(iso_a2 %in% c("US", "CA")) %>%
  select(region_code = iso_3166_2, region_name = name)

# full list of ebird countries
ebird_countries <- here("data", "ebird-regions.rds") %>% 
  readRDS() %>% 
  filter(level == "country") %>% 
  select(region_code, region_name)
ne_country <- inner_join(ne_country, ebird_countries, by = "region_code")
# these "countries" are missing from natural earth
ne_country %>% 
  st_set_geometry(NULL) %>% 
  anti_join(ebird_countries, .)


# counties
tf <- tempfile(fileext = ".rds")
download.file("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_USA_2_sf.rds",
              destfile = tf)
counties <- readRDS(tf) %>% 
  transmute(parent = substr(HASC_2, 4, 5),
            match_name = tolower(NAME_2) %>% 
              str_replace("ste\\.", "sainte") %>% 
              str_replace("st\\.", "saint") %>% 
              str_replace_all("[^a-zA-Z]", ""),
            match_name = if_else(match_name == "princeofwalesouterketchi",
                                 "princeofwalesouterketchikan", match_name),
            match_name = if_else(match_name == "shannon" & parent == "SD",
                                 "oglalalakota", match_name),
            in_gadm = TRUE)
#counties <- counties("NY", 2016, resolution = "20m", cb = TRUE)
ebird_counties <- here("data", "ebird-regions.rds") %>% 
  readRDS() %>% 
  filter(level == "subnational2") %>% 
  mutate(parent = substr(region_code, 4, 5),
         match_name = tolower(region_name) %>% 
           str_replace("ste\\.", "sainte") %>% 
           str_replace("st\\.", "saint") %>%
           str_replace_all("[^a-zA-Z]", "")) %>% 
  select(parent, region_name, region_code, match_name)
gadm_county <- full_join(counties, ebird_counties,
                         by = c("parent", "match_name"))
# missing from gadm
filter(gadm_county, is.na(in_gadm))
# missing from ebird
filter(gadm_county, is.na(region_code))

gadm_county <- gadm_county %>% 
  filter(in_gadm,
         !is.na(region_code),
         !parent %in% c("HI", "AK")) %>% 
  select(region_code, region_name)

# save
f <- here("data", "boundaries.gpkg")
unlink(f)
write_sf(ne_country, f, layer = "country")
write_sf(ne_state, f, layer = "subnational1")
write_sf(gadm_county, f, layer = "subnational2")

