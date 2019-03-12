# get frequencies for each country
ebird_frequency <- function(region) {
  #message(region)
  Sys.sleep(10)
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
get_ebird_frequency <- function(region) {
  f <- safely(ebird_frequency)
  result <- f(region)$result
  if (is.null(result)) {
    result <- f(region)$result
  }
  if (is.null(result)) {
    result <- f(region)$result
  }
  if (is.null(result)) {
    result <- f(region)$result
  }
  return(result)
}