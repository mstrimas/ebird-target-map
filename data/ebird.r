library(tidyverse)
library(sf)
library(rnaturalearth)
library(janitor)
library(auk)
library(here)

# list of valid ebird country and state codes
base_url <- "http://ebird.org/ws1.1/ref/location/list?"
ebird_countries <- paste0(base_url, "rtype=country") %>% 
  read_csv(na = "") %>% 
  clean_names() %>% 
  transmute(level = "country",
            region = country_code, 
            parent = NA,
            name = country_name)
ebird_states <- paste0(base_url, "rtype=subnational1") %>% 
  read_csv(na = "") %>% 
  clean_names() %>% 
  filter(country_code %in% c("US", "CA"),
         !str_detect(subnational1_code, "-$")) %>% 
  transmute(level = "state",
            region = subnational1_code, 
            parent = country_code,
            name = subnational1_name)
ebird_counties <- paste0(base_url, "rtype=subnational2") %>% 
  read_csv(na = "") %>% 
  clean_names() %>% 
  filter(country_code %in% c("US", "CA"),
         subnational1_code == "US-NY",
         !str_detect(subnational1_code, "-$")) %>% 
  transmute(level = "county",
            region = subnational2_code, 
            parent = subnational1_code,
            name = subnational2_name)
ebird_regions <- bind_rows(ebird_countries, ebird_states, ebird_counties)

# get frequencies for each country
ebird_frequency <- function(region) {
  message(region)
  Sys.sleep(0.5)
  url <- "http://ebird.org/ebird/barchartData"  
  q <- list(r = region, 
            byr = as.integer(format(Sys.Date(), "%Y")) - 10,
            eyr = as.integer(format(Sys.Date(), "%Y")),
            fmt = "json")
  response <- httr::GET(url, query = q)
  
  httr::stop_for_status(response)
  week_month <- tidyr::crossing(month = 1:12, week = 1:4)
  response <- readBin(response$content, "character") %>% 
    jsonlite::fromJSON() %>% 
    purrr::pluck("dataRows")
  if (is.null(response)) {
    return(dplyr::tibble())
  }
  response %>% 
    dplyr::select(species_code = speciesCode, values) %>% 
    dplyr::mutate(values = purrr::map(values, ~ cbind(week_month, frequency = .x))) %>% 
    tidyr::unnest() %>% 
    dplyr::as_tibble() %>% 
    dplyr::group_by(species_code, month) %>% 
    dplyr::summarize(frequency = mean(frequency, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(region = region) %>% 
    dplyr::select(region, dplyr::everything())
}
safe_ebird_frequency <- function(region) {
  result <- safely(ebird_frequency)(region)$result
  if (is.null(result)) {
    result <- safely(ebird_frequency)(region)$result
  }
  if (is.null(result)) {
    result <- safely(ebird_frequency)(region)$result
  }
  if (is.null(result)) {
    result <- safely(ebird_frequency)(region)$result
  }
  return(result)
}
# apply to each region
region_freq <- ebird_regions %>% 
  mutate(freq = map(region, ebird_frequency))
# drop non-species taxa
region_freq_sp <- filter(ebird_taxonomy, category == "species") %>% 
  select(species_code) %>% 
  inner_join(unnest(region_freq), ., by = "species_code") %>% 
  select(region, species_code, month, frequency)

region_freq_sp <- here("data", "ebird-frequency.rds") %>% 
  saveRDS(region_freq_sp, .)
region_freq_sp %>% 
  count(region) %>% 
  rename(n_species = n) %>% 
  inner_join(ebird_regions, ., by = "region") %>% 
  saveRDS(here("data", "ebird-regions.rds"))
