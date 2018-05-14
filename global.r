library(shiny)
library(leaflet)
library(dplyr)
library(sf)
list.files("R", full.names = TRUE) %>% 
  sapply(source)

months <- setNames(0:12, c("Full Year", month.name))

# country and state boundaries
f_gis <-"data/ne.gpkg"
ne_country <- read_sf(f_gis, layer = "countries")
ne_state <- read_sf(f_gis, layer = "states")