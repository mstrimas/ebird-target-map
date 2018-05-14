read_lifelist <- function(x) {
  ll <- readr::read_csv(x)
  if (!"Species" %in% names(ll)) {
    return(NULL)
  }
  ll <- dplyr::select(ll, common_name = Species) %>% 
    dplyr::mutate(common_name = stringi::stri_trans_general(common_name, 
                                                            "latin-ascii")) %>% 
    dplyr::inner_join(auk::ebird_taxonomy, by = "common_name") %>% 
    dplyr::pull(species_code)
  if (length(ll) == 0) {
    return(NULL)
  } else {
    return(ll)
  }
}