#' Prefix ab1 files
#'
#' Prefix Sanger sequencing result ab1 files with a excel file containing a
#' 8 x 12 spreadsheet filled with corresponding sample ID.
#'
#' @param well_file A string, containing path to Excel well_file.
#' @param ab1_folder A string, containing path to ab1_folder.
#' @param re_pattern A regular expression string, indicating well index pattern.
#'
#' @return NULL
#' @export
#'
prefix_ab1 <- function(well_file, ab1_folder, re_pattern = "([A-Z]+)(\\d+)\\.(.+)") {

    # Read-in well_file and set proper col and row name.
    well_info <- readxl::read_excel(well_file,
                                    col_names = c("Letter", as.character(1:12)),
                                    skip = 1)

    # Get paths of ab1 files in ab1_folder.
    ab1_paths <- fs::dir_ls(ab1_folder, glob = "*.ab1")

    # Process ab1 files one by one.
    for (ab1_path in ab1_paths) {

        # Separate file name and folder path.
        dir_path <- fs::path_dir(ab1_path[1])
        ab1_name <- fs::path_file(ab1_path[1])

        # Match A-Z and 1-12.
        matched <- stringr::str_match(ab1_name, re_pattern)

        # Skip miss match.
        if (is.na(matched[1, 1])) next

        # Get corresponding ID.
        ID <- dplyr::select(dplyr::filter(well_info, well_info$Letter == matched[2]), matched[3])

        # ID not found, print a warning and skip.
        if (is.na(ID)) {
            cat("Warning: ", matched[2], matched[3], " Not Found.", sep = "")
            next
        }
        # Rename file.
        dst_path <- stringr::str_c(dir_path, "/", ID, "_", ab1_name)
        fs::file_move(ab1_path, dst_path)
    }
}
