#' @title Essential and non-essential reference genes of a DepMap cell line
#' @description Builds the reference genes of
#'     \code{\link{compute_screen_quality}},
#'     \code{\link{plot_reference_separation}} and
#'     \code{\link{scale_by_reference}} from the CRISPR dependency data of
#'     one cell line of the DepMap project (Broad Institute), and a table
#'     with the dependency of every gene to annotate a table of hits.
#' @details The function reads three files of one DepMap release:
#'     \code{CRISPRGeneDependency.csv} (the probability that each gene is a
#'     dependency of each cell line), \code{CRISPRGeneEffect.csv} (the
#'     continuous gene effect, a Chronos score) and \code{Model.csv} (the
#'     cell lines). There are two ways to give them, and exactly one must be
#'     used:
#'     \itemize{
#'       \item \code{files}: the three files downloaded by you from the
#'             DepMap portal (\url{https://depmap.org/portal/data_page/}),
#'             as a character vector named by the file names, with the
#'             \code{release} label (for example "DepMap Public 26Q3") and
#'             the \code{source} (for example the page or URL you
#'             downloaded them from). The MD5 checksums of the files are
#'             computed and returned. This route does not need the
#'             \pkg{depmap} package, and is the route for the current
#'             releases, which cannot be assumed to be on Figshare.
#'       \item \code{dataset_id}: the integer Figshare identifier of an
#'             archived release (\code{depmap::dmsets()$dataset_id}, for
#'             example 25880521 for DepMap 24Q2 Public). The files are
#'             retrieved and cached by the Bioconductor package
#'             \pkg{depmap} (a suggested package, version 1.19.1 or later,
#'             with \code{DepMapFiles()} and \code{dmget()}). On R 4.3,
#'             \code{BiocManager::install("depmap")} installs version 1.16,
#'             which does not have them: install the verified revision with
#'             \code{remotes::install_github("UCLouvain-CBIO/depmap@735fa8971fd79ec746a43d354073701b3165e58d")},
#'             or use a version of R and Bioconductor that has depmap 1.19.1
#'             or later. Nothing is installed automatically.
#'     }
#'     The files are large (about 400 MB each); only the row of the
#'     requested cell line is read in memory. The format of the files was
#'     checked on DepMap 24Q2 (the first column the model ID, the other
#'     columns "SYMBOL (EntrezID)"); the files of a newer release are
#'     rejected, not guessed, if they differ.
#'
#'     A gene is \emph{essential} when its dependency probability is at least
#'     \code{dependency_cutoff} and \emph{non-essential} when it is at most
#'     \code{nondependency_cutoff}; the genes in between, the genes without a
#'     measured probability and the genes without a valid symbol and
#'     positive Entrez identifier, or whose symbol or Entrez identifier
#'     is not unique, are \emph{unclassified}: a missing value is never
#'     taken as non-essential. The two cutoffs are choices to select
#'     reference genes (0.5 is the convention of DepMap), \strong{not}
#'     p-values, false discovery rates or a guarantee of calibration, and
#'     the non-essential genes are not guaranteed to be neutral under a new
#'     treatment. These are the dependencies of one cell line in one
#'     release: they are not the core essential genes of
#'     \code{\link{hart_reference_genes}}, and they depend on the cell line,
#'     the library and the version of the data.
#'
#'     The cell line is the DepMap model identifier (for example
#'     \code{"ACH-000001"}) or its name (\code{CellLineName} or
#'     \code{StrippedCellLineName} of \code{Model.csv}, without regard to
#'     case). A name that does not match exactly one model is an error: no
#'     other cell line, assay or release is used instead.
#'
#'     \code{genes} keeps every measurement, also of the ambiguous symbols
#'     (one row for each Entrez identifier). To annotate a table of hits by
#'     symbol, first remove every occurrence of a duplicated symbol, so that
#'     a hit never becomes two rows and an ambiguous symbol is unknown:
#'     see the examples. If the hits have the Entrez identifier, join by
#'     \code{c("Gene", "EntrezID")} instead.
#' @param cell_line A DepMap model identifier or the name of the cell line.
#' @param dataset_id The integer Figshare identifier of an archived DepMap
#'     release. There is no default: the release must be chosen. Not used
#'     with \code{files}.
#' @param files The paths of \code{CRISPRGeneDependency.csv},
#'     \code{CRISPRGeneEffect.csv} and \code{Model.csv} of one release, a
#'     character vector named by the file names. Not used with
#'     \code{dataset_id}.
#' @param release With \code{files}, the name of the release, for example
#'     "DepMap Public 26Q3".
#' @param source With \code{files}, where the files come from (the portal
#'     page or URL).
#' @param dependency_cutoff The minimum probability of the essential genes
#'     (default 0.5).
#' @param nondependency_cutoff The maximum probability of the non-essential
#'     genes (default 0.1); it must be lower than \code{dependency_cutoff}.
#' @return A list with the elements
#'     \describe{
#'       \item{essential, nonessential}{unique and disjoint character
#'             vectors of gene symbols, for the functions of the reference
#'             genes.}
#'       \item{genes}{a tibble with one row for each gene and the columns
#'             Gene (symbol), EntrezID, DependencyProbability, GeneEffect
#'             (\code{NA} when not measured) and Class ("essential",
#'             "nonessential" or "unclassified").}
#'       \item{provenance}{a list with the cell_line requested, model_id,
#'             cell_line_name, release (the title of the dataset or the
#'             \code{release} given), source, dataset_id (\code{NA} with
#'             \code{files}), files (name, id and md5 of the files used,
#'             the id \code{NA} with \code{files}), the two cutoffs and the
#'             coverage: n_genes, n_probability, n_effect, n_essential,
#'             n_nonessential, n_unclassified and n_ambiguous (the genes
#'             with an invalid or not unique identifier).}
#'     }
#' @seealso \code{\link{compute_screen_quality}},
#'     \code{\link{plot_reference_separation}},
#'     \code{\link{scale_by_reference}}, \code{\link{hart_reference_genes}}
#' @references Tsherniak A, Vazquez F, Montgomery PG, et al. (2017).
#'     Defining a cancer dependency map. Cell 170:564-576.
#'     \doi{10.1016/j.cell.2017.06.010}
#' @concept compute
#' @export
#' @examples
#' # SYNTHETIC miniature files in the format of DepMap 24Q2 (not real data)
#' dir <- system.file("extdata", "depmap-synthetic", package = "ScreenR")
#' names <- c("CRISPRGeneDependency.csv", "CRISPRGeneEffect.csv", "Model.csv")
#' reference <- get_depmap_reference("ACH-000001",
#'     files = stats::setNames(file.path(dir, names), names),
#'     release = "synthetic example", source = "ScreenR inst/extdata")
#' reference$essential
#'
#' # annotate hits by symbol: drop the duplicated symbols, then left join
#' hits <- data.frame(Gene = c("ESS1", "DUP", "OTHER"))
#' safe <- reference$genes[!(duplicated(reference$genes$Gene) |
#'     duplicated(reference$genes$Gene, fromLast = TRUE)), ]
#' dplyr::left_join(hits, safe, by = "Gene")  # 3 rows; DUP and OTHER unknown
#' \dontrun{
#' # archived release from Figshare (about 800 MB the first time)
#' reference <- get_depmap_reference("A549", dataset_id = 25880521)
#' # compute_screen_quality(metrics, reference$essential, reference$nonessential)
#' }
get_depmap_reference <- function(cell_line, dataset_id = NULL,
    files = NULL, release = NULL, source = NULL,
    dependency_cutoff = 0.5, nondependency_cutoff = 0.1) {
    if (!is.character(cell_line) || length(cell_line) != 1 ||
        is.na(cell_line)) {
        stop("`cell_line` must be one DepMap model ID or cell line name.")
    }
    if (is.null(dataset_id) == is.null(files)) {
        stop("Give either `dataset_id` (archived Figshare release) or ",
            "`files` (with `release` and `source`), not both or none.")
    }
    cutoffs <- c(dependency_cutoff, nondependency_cutoff)
    if (!is.numeric(cutoffs) || length(cutoffs) != 2 ||
        any(!is.finite(cutoffs)) || nondependency_cutoff < 0 ||
        dependency_cutoff > 1 || nondependency_cutoff >= dependency_cutoff) {
        stop("The cutoffs must satisfy 0 <= nondependency_cutoff < ",
            "dependency_cutoff <= 1.")
    }
    if (is.null(files)) {
        if (!is.numeric(dataset_id) || length(dataset_id) != 1 ||
            !is.finite(dataset_id) || dataset_id < 1 ||
            dataset_id > .Machine$integer.max ||
            dataset_id != floor(dataset_id)) {
            stop("`dataset_id` must be the integer Figshare ID of one ",
                "DepMap release.")
        }
        dataset_id <- as.integer(dataset_id)
        data <- depmap_release_files(dataset_id)
        data$source <- paste("Figshare dataset", dataset_id)
    } else {
        data <- depmap_local_files(files, release, source)
        dataset_id <- NA_integer_
    }
    model <- depmap_find_model(data$paths[["Model.csv"]], cell_line)
    probability <- depmap_read_model(
        data$paths[["CRISPRGeneDependency.csv"]], model$ModelID)
    effect <- depmap_read_model(
        data$paths[["CRISPRGeneEffect.csv"]], model$ModelID)
    if (any(probability < 0 | probability > 1, na.rm = TRUE)) {
        stop("CRISPRGeneDependency.csv has values outside [0, 1]: not ",
            "dependency probabilities.")
    }

    labels <- union(names(probability), names(effect))
    mapped <- regmatches(labels, regexec("^(.*) \\(([0-9]+)\\)$", labels))
    parsed <- lengths(mapped) == 3
    symbol <- trimws(ifelse(parsed, vapply(mapped, `[`, "", 2), NA_character_))
    number <- suppressWarnings(as.numeric(
        ifelse(parsed, vapply(mapped, `[`, "", 3), NA_character_)))
    # a usable stable identifier: a symbol and an Entrez ID in 1..integer max
    valid <- parsed & !is.na(symbol) & nzchar(symbol) & !is.na(number) &
        number >= 1 & number <= .Machine$integer.max
    gene <- ifelse(valid, symbol, labels)
    entrez <- ifelse(valid, number, NA)
    unique_id <- function(x) !(duplicated(x) | duplicated(x, fromLast = TRUE))
    ambiguous <- !valid | !unique_id(gene) | !(is.na(entrez) | unique_id(entrez))
    p <- unname(probability[labels])
    genes <- tibble::tibble(
        Gene = gene, EntrezID = as.integer(entrez),
        DependencyProbability = p,
        GeneEffect = unname(effect[labels]),
        Class = ifelse(ambiguous | is.na(p), "unclassified",
            ifelse(p >= dependency_cutoff, "essential",
                ifelse(p <= nondependency_cutoff, "nonessential",
                    "unclassified")))
    )
    provenance <- list(
        cell_line = cell_line, model_id = model$ModelID,
        cell_line_name = model$CellLineName, release = data$title,
        source = data$source, dataset_id = dataset_id, files = data$files,
        dependency_cutoff = dependency_cutoff,
        nondependency_cutoff = nondependency_cutoff,
        n_genes = nrow(genes), n_probability = sum(!is.na(p)),
        n_effect = sum(!is.na(genes$GeneEffect)),
        n_essential = sum(genes$Class == "essential"),
        n_nonessential = sum(genes$Class == "nonessential"),
        n_unclassified = sum(genes$Class == "unclassified"),
        n_ambiguous = sum(ambiguous)
    )
    list(
        essential = genes$Gene[genes$Class == "essential"],
        nonessential = genes$Gene[genes$Class == "nonessential"],
        genes = genes, provenance = provenance
    )
}

# The three files given by the user, in the same form as the files of the
# Figshare route: the checksums are computed here.
depmap_local_files <- function(files, release, source) {
    wanted <- c("CRISPRGeneDependency.csv", "CRISPRGeneEffect.csv",
        "Model.csv")
    for (x in list(release, source)) {
        if (!is.character(x) || length(x) != 1 || is.na(x) ||
            !nzchar(trimws(x))) {
            stop("With `files`, `release` and `source` must be given as ",
                "non-empty strings.")
        }
    }
    files <- unlist(files)
    if (!is.character(files) || !setequal(names(files), wanted) ||
        length(files) != 3 || any(!file.exists(files))) {
        stop("`files` must be the existing paths of ",
            paste(wanted, collapse = ", "), ", named by file name.")
    }
    files <- files[wanted]
    list(title = release, source = source, paths = files,
        files = tibble::tibble(name = wanted, id = NA_integer_,
            md5 = unname(tools::md5sum(files))))
}

# The three files of one release, downloaded or read from the cache of the
# suggested package depmap. Separate from the parsing so the tests can use
# small local files.
depmap_release_files <- function(dataset_id) {
    needed <- c("DepMapDataset", "DepMapFiles", "dmTitle", "dmget")
    if (!requireNamespace("depmap", quietly = TRUE) ||
        !all(needed %in% getNamespaceExports("depmap"))) {
        stop("The dataset_id route needs the Bioconductor package 'depmap' ",
            ">= 1.19.1 (DepMapFiles() and dmget()); BiocManager::",
            "install(\"depmap\") gives 1.16 on R 4.3, which does not have ",
            "them. Install remotes::install_github(\"UCLouvain-CBIO/depmap@",
            "735fa8971fd79ec746a43d354073701b3165e58d\") (built with R ",
            "4.3.0), or use a newer R and Bioconductor; or give the files ",
            "downloaded from the DepMap portal with `files`.", call. = FALSE)
    }
    dataset <- depmap::DepMapDataset(dataset_id)
    files <- depmap::DepMapFiles(dataset)
    wanted <- c("CRISPRGeneDependency.csv", "CRISPRGeneEffect.csv",
        "Model.csv")
    files <- files[files$name %in% wanted, ]
    missing <- setdiff(wanted, files$name)
    if (length(missing) > 0 || anyDuplicated(files$name)) {
        stop("Release ", depmap::dmTitle(dataset), " (", dataset_id,
            ") must have exactly one file each of ",
            paste(wanted, collapse = ", "), "; missing: ",
            paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    paths <- depmap::dmget(files)
    list(title = depmap::dmTitle(dataset),
        paths = stats::setNames(paths, files$name),
        files = files[, c("name", "id", "md5")])
}

depmap_find_model <- function(path, cell_line) {
    models <- utils::read.csv(path, check.names = FALSE,
        stringsAsFactors = FALSE)
    required <- c("ModelID", "CellLineName", "StrippedCellLineName")
    if (!all(required %in% names(models))) {
        stop("Model.csv must have the columns ",
            paste(required, collapse = ", "), ".")
    }
    key <- tolower(cell_line)
    hit <- models$ModelID == cell_line |
        tolower(models$CellLineName) %in% key |
        tolower(models$StrippedCellLineName) %in% key
    if (sum(hit) == 0) {
        stop("Cell line '", cell_line, "' is not in Model.csv.")
    }
    if (sum(hit) > 1) {
        stop("Cell line '", cell_line, "' is ambiguous: ",
            paste(models$ModelID[hit], collapse = ", "),
            ". Use the DepMap model ID.")
    }
    models[hit, c("ModelID", "CellLineName")]
}

# One named numeric vector (gene label -> value) for one model of a
# DepMap matrix (rows models, columns genes). Only that row is kept: the
# matrix is never reshaped to a table of all the cell lines.
depmap_read_model <- function(path, model_id) {
    labels <- scan(path, what = "", sep = ",", nlines = 1, quote = "\"",
        quiet = TRUE)[-1]
    if (anyDuplicated(labels)) {
        stop(basename(path), " has duplicated gene columns.")
    }
    ids <- utils::read.csv(path, colClasses = c("character",
        rep("NULL", length(labels))), header = TRUE)[[1]]
    row <- which(ids == model_id)
    if (length(row) != 1) {
        stop("Model ", model_id, " is ", if (length(row) == 0) "not in " else
            "duplicated in ", basename(path), ".")
    }
    values <- utils::read.csv(path, header = FALSE, skip = row, nrows = 1,
        colClasses = c("NULL", rep("numeric", length(labels))),
        na.strings = c("", "NA"))
    values <- as.numeric(unlist(values))
    values[!is.finite(values)] <- NA
    stats::setNames(values, labels)
}
