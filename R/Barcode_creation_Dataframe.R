#' @title Create the table containing the Mapped Sample f
#' @description This function take as input the path of a file creating using
#'              the samtools idxstats
#'
#' @param path_file  Path of the file
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map
#' @return return the table containing barcode that have mapped to the samples
#' @export

Barcode_creation_Dataframe <- function(path_file) {
    dataFrames <- purrr::map(.x = list.files(path = path_file, full.names = T),
        .f = ~read.table(file = .x, header = F, sep = "\t",
                         col.names = c("Barcode","Length", "Mapped",
                                       "Unmapped")))
    # remove the useless one
    dataFrames <- purrr::map(.x = dataFrames, .f = ~.x[(names(.x) %in%
        c("Barcode", "Mapped"))])

    Mapped_barcode_all_Samples <- dataFrames %>%
        purrr::reduce(left_join, by = "Barcode")

    name <- basename(list.files(path = path_file, full.names = T))
    name <- unlist(strsplit(name, ".", fixed = TRUE))
    name <- name[seq(1, length(name), 3)]
    name <- strex::str_after_nth(name, "_", 3)
    name <- strex::str_before_last(name, "_")
    name <- strex::str_before_last(name, "_")
    name
    colnames(Mapped_barcode_all_Samples) <- c("Barcode", name)

    Mapped_barcode_all_Samples <- Mapped_barcode_all_Samples %>%
        tidyr::drop_na() %>%
        dplyr::filter(.data$Barcode != "*") %>%
        tibble()

    return(Mapped_barcode_all_Samples)
}
