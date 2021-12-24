#' Reading realplex PCR result .XLS file
#'
#' @param xlsfile realplex result .XLS file.
#'
#' @return a data.frame, resembling 96-well plate.
#' @export
#'
read_realplex <- function(xlsfile, wellfile) {
    raw <- readxl::read_excel(xlsfile)
    bottom <- which(raw[["Pos"]] == "Analysis Parameters") - 2
    df <- data.frame(matrix(ncol = 12, nrow = 8))
    colnames(df) <- 1:12
    rownames(df) <- LETTERS[1:8]
    for (i in 1:(bottom)){
        pos <- raw[[i, "Pos"]]
        ct <- raw[[i, "Ct SYBR"]]
        row <- stringr::str_extract(pos, "[A-H]")
        column <- stringr::str_extract(pos, "\\d+")
        df[row, column] <- ct
    }
    return(df)
}
