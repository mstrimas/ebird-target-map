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
  pull(country_code)
ebird_states <- paste0(base_url, "rtype=subnational1") %>% 
  read_csv(na = "") %>% 
  clean_names() %>% 
  filter(country_code %in% c("US", "CA")) %>% 
  pull(subnational1_code)

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
# apply to each country and state
region_freq <- map_df(c(ebird_countries, ebird_states), ebird_frequency)
# drop non-species taxa
region_freq <- filter(ebird_taxonomy, category == "species") %>% 
  select(species_code) %>% 
  inner_join(region_freq, ., by = "species_code")

here("data", "ebird-frequency.csv") %>% 
  write_csv(region_freq, ., na = "")
here("data", "ebird-frequency.rds") %>% 
  saveRDS(region_freq, .)
