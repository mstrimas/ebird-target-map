region_freq <- readRDS("data/ebird-frequency.rds")
regions <- readRDS("data/ebird-regions.rds")

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>% 
      fitBounds(-100, -40, 50, 50)
  })
  
  # life list
  life_list <- eventReactive(input$submit_button, {
    req(input$ll_file)
    read_lifelist(input$ll_file$datapath)
  })
  
  # targets
  targets <- eventReactive(input$submit_button, {
    req(life_list())
    get_targets(life_list(), region_freq, input$freq_thresh / 100, input$month)
  })
  
  # number of lifers
  target_count <- eventReactive(input$submit_button, {
    req(targets())
    summarize_targets(targets())
  })
  
  # upload message
  output$message <- renderUI({
    ll <- life_list()
    if (is.null(ll) || !is.character(ll)) {
      message <- "Problem uploading life list"
    } else {
      message <- paste(length(ll) %>% format(big.mark = ","), 
                       "life birds uploaded")
    }
    strong(message)
  })
  
  # get correct boundaries
  boundaries <- reactive({
    if (input$geolevel == "county") {
      return(us_county)
    } else if (input$geolevel == "state") {
      return(ne_state)
    } else {
      return(ne_country)
    }
  })
  
  # update map
  observe({
    map_targets <- left_join(boundaries(), target_count(), by = "region_code")

    map_targets <- map_targets %>% 
      mutate(n_lifers = coalesce(n_lifers, 0L),
             n_species = coalesce(n_species, 0L)) %>% 
      make_label() %>% 
      make_popup(period = input$month)
    bb <- st_bbox(map_targets)
    
    pal <- colorNumeric("viridis", domain = map_targets$n_lifers)
    lp <- leafletProxy("map", data = map_targets) %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(color = "#333333", weight = 0.5, smoothFactor = 0.25,
                  opacity = 1.0, fillOpacity = 0.75,
                  layerId = ~ region_code,
                  fillColor = ~ pal(n_lifers),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 1,
                                                      bringToFront = TRUE),
                  popup = ~ popup_html,
                  popupOptions = popupOptions(
                    noWrap = TRUE
                  ),
                  label = ~ label_html,
                  labelOptions = labelOptions(
                    offset = c(-100, -140),
                    textOnly = TRUE,
                    style = list(
                      "background" = "rgba(255,255,255,0.95)",
                      "border-color" = "rgba(0,0,0,1)",
                      "border-radius" = "2px",
                      "border-style" = "solid",
                      "border-width" = "2px"))) %>%
      addLegend("bottomleft", pal = pal, values = ~ n_lifers,
                title = "# of life birds", opacity = 1)
    if (isolate(input$geolevel) == "state") {
      lp <- setView(lp, -98.5, 42, 4)
    } else if (isolate(input$geolevel) == "county") {
      lp <- setView(lp, -98.580, 39.831, 5)
    } else {
      lp <- setView(lp, 0, 0, 3)
    }
    lp
  })
}