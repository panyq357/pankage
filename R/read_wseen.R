#' Read WSeen Seed Scan Data
#'
#' @param folder WSeen output folder.
#'
#' @return a tibble containing wseen data.
#' @export
#'
read_wseen <- function(folder){
    all <- dplyr::tibble()
    for (csv in fs::dir_ls(folder, glob = "*.csv")){
        ID <- fs::path_ext_remove(fs::path_file(csv))
        this <- readr::read_csv(csv,
                                skip = 1,
                                locale = readr::locale(encoding = "gbk"),
                                col_types = readr::cols())
        this <- dplyr::transmute(this,
                                 ID = ID,
                                 Length = `长`,
                                 Width = `宽`,
                                 LengthByWidth = `长/宽`)
        all <- base::rbind(all, this)
    }
    return(all)
}
