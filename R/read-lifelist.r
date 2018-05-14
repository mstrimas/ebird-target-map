read_lifelist <- function(x) {
  ll <- readr::read_csv(x)
  if (!"Species" %in% names(ll)) {
    return(NULL)
  }
  ll <- dplyr::select(ll, common_name = Species)
  ll <- dplyr::inner_join(ll, auk::ebird_taxonomy, by = "common_name")
  ll <- dplyr::pull(ll, species_code)
  if (length(ll) == 0) {
    return(NULL)
  } else {
    return(ll)
  }
}