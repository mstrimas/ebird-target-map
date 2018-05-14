summarize_targets <- function(x) {
  dplyr::group_by(x, region) %>% 
    dplyr::summarize(n_lifers = sum(!seen), n_species = n())
}