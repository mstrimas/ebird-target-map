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
  
  # get correct set of boundaries
  values <- reactiveValues(boundaries = ne_country, 
                           submit = FALSE,
                           selected = NULL,
                           county = FALSE)
  observe({
    input$geolevel
    if (input$geolevel == "Counties") {
      values$boundaries <- us_county
      values$county <- TRUE
    } else if (input$geolevel == "States") {
      values$boundaries <- ne_state
      if (isTRUE(values$county)) {
        r <- isolate(values$selected)
        rc <- filter(regions, region == r)
        if (nrow(rc) > 0) {
          isolate(values$selected <- rc$parent[1])
        }
      }
      values$county <- FALSE
    } else {
      values$boundaries <- ne_country
      isolate(values$selected <- NULL)
      values$county <- FALSE
    }
  })
  
  # reset everything on submit
  observeEvent(input$submit_button, {
    values$submit <- TRUE
    values$selected <- NULL
    values$boundaries = ne_country
    output$selected_region <- renderText("Global")
  })
  
  # update map
  observe({
    if (!isTRUE(values$submit)) {
      return(NULL)
    }
    req(target_count())
    map_targets <- left_join(values$boundaries, target_count(), 
                             by = "region")
    r <- isolate(values$selected)
    if (!is.null(r) && "parent" %in% names(map_targets)) {
      if (r %in% map_targets$parent) {
        map_targets <- filter(map_targets, parent == r)
      }
    }
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
                  layerId = ~ region,
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
    if (isolate(input$geolevel) %in% c("States", "Counties")) {
      lp <- lp %>% 
        fitBounds(lng1 = bb[["xmin"]], lng2 = bb[["xmax"]],
                  lat1 = bb[["ymin"]], lat2 = bb[["ymax"]])
    } else {
      lp <- setView(lp, 0, 0, 3)
    }
    lp
  })
  
  # clicking polygon
  observeEvent(input$map_shape_click, {
    values$selected <- input$map_shape_click$id
  })
  observe({
    region_code <- values$selected
    if (is.null(region_code)) {
      region_name <- "Global"
      updateSelectInput(session, "geolevel", NULL, "Countries",
                        selected = "Countries")
    } else {
      region_name <- filter(regions, region == region_code) %>% 
        slice(1) %>% 
        pull(name)
      
      # find subregions
      children <- filter(regions, parent == region_code, n_species > 0)
      
      if (nrow(children) > 0) {
        if (children$level[1] == "state" && region_code %in% ne_state$parent) {
          updateSelectInput(session, "geolevel", NULL, 
                            c("Countries", "States"),
                            selected = "Countries")
        } else if (children$level[1] == "county" &&
                   region_code %in% us_county$parent) {
          updateSelectInput(session, "geolevel", NULL,
                            c("Countries", "States", "Counties"),
                            selected = "States")
        }
      }
    }
    output$selected_region <- renderText(region_name)
  })
}