get_targets <- function(life_list, region_list, freq_thresh, period) {
  if (period > 0) {
    region_list <- dplyr::filter(region_list, month == period)
  }
  dplyr::group_by(region_list, region, species_code) %>% 
    dplyr::summarise(frequency = mean(frequency)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(frequency >= freq_thresh) %>% 
    dplyr::mutate(seen = (species_code %in% life_list)) %>% 
    dplyr::arrange(region, dplyr::desc(frequency))
}