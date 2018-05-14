read_lifelist <- function(x) {
  ll <- readr::read_csv(x)
  if (!"Species" %in% names(ll)) {
    return(NULL)
  }
  # both sci and common?
  if (all(stringr::str_detect(ll$Species, " - "))) {
    ll$Species <- stringr::str_split(ll$Species, " - ", n = 2, 
                                     simplify = TRUE)[, 1]
  }
  ll <- ll %>% 
    dplyr::mutate(scientific_name = auk::ebird_species(Species)) %>% 
    dplyr::select(scientific_name) %>% 
    dplyr::inner_join(auk::ebird_taxonomy, by = "scientific_name") %>% 
    dplyr::pull(species_code)
  if (length(ll) == 0) {
    return(NULL)
  } else {
    return(ll)
  }
}