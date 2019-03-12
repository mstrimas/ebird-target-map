library(tidyverse)
library(sf)
library(rnaturalearth)
library(janitor)
library(auk)
library(httr)
library(here)
walk(list.files("R", full.names = TRUE), source)

# list of valid ebird country and state codes

ebird_countries <- get_regions("country")
ebird_states <- map_df(c("US", "CA"), ~ get_regions("subnational1", parent = .))
ebird_counties <- get_regions("subnational2", parent = "US")
ebird_regions <- bind_rows(ebird_countries, ebird_states, ebird_counties)

f <- here("data", "ebird-frequency_raw.rds")
if (!file.exists(f)) {
  # apply to each region
  region_freq <- ebird_regions %>% 
    mutate(freq = map(region, get_ebird_frequency))
  saveRDS(region_freq,  f)
}
region_freq <- readRDS(f)

# drop non-species taxa
region_freq_sp <- filter(ebird_taxonomy, category == "species") %>% 
  select(species_code) %>% 
  inner_join(unnest(region_freq), ., by = "species_code") %>% 
  select(region_code, species_code, month, frequency)
here("data", "ebird-frequency.rds") %>% 
  saveRDS(region_freq_sp, .)

# region list with species counts
region_freq_sp %>% 
  count(region_code) %>% 
  rename(n_species = n) %>% 
  inner_join(ebird_regions, ., by = "region_code") %>% 
  saveRDS(here("data", "ebird-regions.rds"))
