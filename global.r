library(shiny)
library(leaflet)
library(dplyr)
library(sf)

list.files("R", full.names = TRUE) %>% 
  sapply(source)

months <- setNames(0:12, c("Full Year", month.name))

# country and state boundaries
f_gis <-"data/boundaries.gpkg"
ne_country <- read_sf(f_gis, layer = "country")
ne_state <- read_sf(f_gis, layer = "subnational1")
us_county <- read_sf(f_gis, layer = "subnational2")
