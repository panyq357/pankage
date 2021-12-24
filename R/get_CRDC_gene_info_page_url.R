#' Get Gene Info Page URL in CRDC
#'
#' @param gene_id a gene ID string, e.g. "Os05g0187500".
#'
#' @return corresponding gene info page URL in CRDC.
#' @export
#'
get_CRDC_gene_info_page_url <- function(gene_id) {
    url <- stringr::str_c("https://www.ricedata.cn/gene/accessions_switch.aspx?para=", gene_id)
    page <- rvest::read_html(url)
    element <- rvest::html_element(page, "a")
    info_page_url <- rvest::html_attr(element, "href")
    return(info_page_url)
}
