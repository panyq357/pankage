#' Get Gene Info Page URL in CRDC
#'
#' @param gene_id a gene ID string, e.g. "Os05g0187500".
#'
#' @return corresponding gene info page URL in CRDC.
#' @export
#'
get_CRDC_gene_url <- function(gene_id) {
    cat(sprintf("Processing: %s\n", gene_id))
    url <- stringr::str_c("https://www.ricedata.cn/gene/accessions_switch.aspx?para=", gene_id)
    page <- rvest::read_html(url, encoding = "UTF-8")
    element <- rvest::html_element(page, "a")
    info_page_url <- rvest::html_attr(element, "href")
    if (!is.na(info_page_url) & str_length(info_page_url) >= 4 & str_sub(info_page_url,1,4) == "list")
        info_page_url <- str_c("https://www.ricedata.cn/gene/", info_page_url)
    return(info_page_url)
}
