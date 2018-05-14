make_popup <- function(x, period) {
  if (period == 0) {
    period <- "bmo=1&emo=12"
  } else {
    period <- glue::glue("bmo={period}&emo={period}")
  }
  dplyr::mutate(x, 
                popup_html = glue::glue(popup_template),
                popup_html = purrr::map(popup_html, htmltools::HTML))
}

popup_template <- "
<p style='font-size:16px'>
  <a href='https://ebird.org/region/{region}?yr=all' target='_blank'>Explore region on eBird</a>
  <br/>
  <a href='https://ebird.org/targets?r1={region}&{period}&r2=world&t2=life' target='_blank'>View targets on eBird</a>
</p>
"