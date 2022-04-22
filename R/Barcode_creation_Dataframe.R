#' @title Create the table containing the Mapped Sample file
#' @description This function takes as input the path of a list of files
#'              created using the Samtools idxstats tool and return a
#'               count table.
#'
#' @param path_file  Path of the list of files
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map reduce
#' @return Return the table containing  the list of barcodes mapped in each of
#'         the samples (a count table).
#'
#' @export

Barcode_creation_Dataframe <- function(path_file) {
    dataFrames <- purrr::map(.x = list.files(
        path = path_file,
        full.names = TRUE
    ), .f = ~ read.table(
        file = .x, header = FALSE,
        sep = "\t", col.names = c(
            "Barcode", "Length", "Mapped",
            "Unmapped"
        )
    ))
    # remove the useless one
    dataFrames <- purrr::map(.x = dataFrames, .f = ~ .x[(names(.x) %in%
        c("Barcode", "Mapped"))])

    Mapped_barcode_all_Samples <- dataFrames %>%
        purrr::reduce(left_join, by = "Barcode")

    name <- basename(list.files(path = path_file, full.names = TRUE))
    name <- unlist(strsplit(name, ".", fixed = TRUE))
    name <- name[seq(1, length(name), 3)]
    name <- strex::str_after_nth(name, "_", 3)
    name <- strex::str_before_last(name, "_")
    name <- strex::str_before_last(name, "_")
    colnames(Mapped_barcode_all_Samples) <- c("Barcode", name)

    Mapped_barcode_all_Samples <- Mapped_barcode_all_Samples %>%
        tidyr::drop_na() %>%
        dplyr::filter(.data$Barcode != "*") %>%
        tibble::tibble()

    return(Mapped_barcode_all_Samples)
}
