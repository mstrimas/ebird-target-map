get_regions <- function(level = c("subnational2", "subnational1", "country"), 
                        parent = "world", key = Sys.getenv("EBIRD_API_KEY")) {
  base_url <- "https://ebird.org/ws2.0/ref/region/list/"
  url <- stringr::str_glue(base_url, "{level}/{parent}.csv")
  get <- httr::GET(url, httr::add_headers(`X-eBirdApiToken` = key))
  df <- readr::read_csv(httr::content(get, as = "text", encoding = "UTF-8"),
                        na = "")
  df <- setNames(df, tolower(names(df)))
  df$level <- level
  df$parent <- parent
  df[, c("level", "parent", "region_name", "region_code")]
}