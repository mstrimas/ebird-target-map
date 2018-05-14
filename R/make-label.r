make_label <- function(x) {
  dplyr::mutate(x, 
                label_html = glue::glue(label_template),
                label_html = purrr::map(label_html, htmltools::HTML))
}

# from http://rpubs.com/bhaskarvk/electoral-Map-2016
label_template <- "
<div style='font-size:12px;width:200px;float:left'>
  <h4>{name}</h4>
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
  <span style='font-size:10px'>Click to view targets</span>
</div>"