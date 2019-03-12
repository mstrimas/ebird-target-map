summarize_targets <- function(x) {
  dplyr::group_by(x, region_code) %>% 
    dplyr::summarize(n_lifers = sum(!seen), n_species = n())
}