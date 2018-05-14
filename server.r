region_freq <- readRDS("data/ebird-frequency.rds")

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
  
  # update map
  observeEvent(input$submit_button, {
    req(target_count())
    map_targets <- inner_join(ne_country, target_count(), by = "region") %>% 
      make_popup(period = input$month)
    
    pal <- colorNumeric("viridis", domain = map_targets$n_lifers)
    leafletProxy("map", data = map_targets) %>%
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
                  popupOptions = labelOptions(
                    #offset = c(-100, -140),
                    #textOnly = TRUE,
                    style = list(
                      'background' = 'rgba(255,255,255,0.95)',
                      'border-color' = 'rgba(0,0,0,1)',
                      'border-radius' = '2px',
                      'border-style' = 'solid',
                      'border-width' = '2px'))) %>%
      addLegend("bottomleft", pal = pal, values = ~ n_lifers,
                title = "# of life birds", opacity = 1)
  })
  
  # clicking polygon
  observeEvent(input$map_shape_click, {
    p <- input$map_shape_click$id
    print(p)
  })
}