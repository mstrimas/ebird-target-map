make_popup <- function(x, period) {
  if (period == 0) {
    period <- "bmo=1&emo=12"
  } else {
    period <- glue::glue("bmo={period}&emo={period}")
  }
  dplyr::mutate(x, 
                popup_html = glue::glue(html_template),
                popup_html = purrr::map(popup_html, htmltools::HTML))
}

# from http://rpubs.com/bhaskarvk/electoral-Map-2016
html_template <- "
<div style='font-size:12px;width:200px;float:left'>
  <a href='https://ebird.org/region/{region}?yr=all' target='_blank'><h4>{name}</h4></a>
  <div style='width:95%'>
    <span style='float:left'>Life Birds</span>
    <span style='float:right'>Species</span>
    <br/>
    <span style='color:#2aa1ec;float:left'>{format(n_lifers, big.mark = ',')}</span>
    <span style='color:#fe6a59;float:right'>{format(n_species, big.mark = ',')}</span>
    <br clear='all'/>
    <span style='background:#2aa1ec;width:{round(100 * n_lifers / n_species)}%;float:left'>&nbsp;</span>
    <span style='background:#fe6a59;width:{round(100 * (1 - n_lifers / n_species))}%;float:right'>&nbsp;</span>
  </div>
  <br/>
  <span style='font-size:10px'>
    <a href='https://ebird.org/targets?r1={region}&{period}&r2=world&t2=life' target='_blank'>View targets on eBird</a>
  </span>
</div>"