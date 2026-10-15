#' @title Correct CRISPR-Cas9 copy-number and proximity bias with CRISPRcleanR
#' @description In CRISPR-Cas9 screens, sgRNAs that cut in amplified regions
#'     of the genome (or in clusters of nearby targets) deplete the cells
#'     whatever the gene they target, producing false essential genes. This
#'     function corrects the counts of a ScreenR object with the
#'     \href{https://github.com/francescojm/CRISPRcleanR}{CRISPRcleanR}
#'     package (Iorio et al. 2018), which finds these regions by segmenting
#'     the sgRNA fold changes along the genome, without needing copy-number
#'     data. ScreenR does not implement the correction itself: it prepares
#'     the input, calls the CRISPRcleanR functions and returns a new ScreenR
#'     object with the corrected counts, ready for
#'     \code{\link{normalize_data}} and the hit-finding functions.
#'
#'     Use it for CRISPR-Cas9 knockout screens only (not for shRNA, CRISPRi
#'     or CRISPRa screens), with a library annotation that gives the genomic
#'     position of each sgRNA.
#' @details CRISPRcleanR is not on CRAN or Bioconductor, so it is not
#'     installed with ScreenR. Install it with
#'     \code{BiocManager::install("francescojm/CRISPRcleanR")} (or
#'     \code{remotes::install_github("francescojm/CRISPRcleanR")}).
#'
#'     The function runs the standard CRISPRcleanR workflow on the given
#'     control and treated samples:
#'     \enumerate{
#'       \item \code{ccr.NormfoldChanges}: removes the sgRNAs with fewer
#'             than \code{min_reads} reads (mean of the controls), normalizes
#'             the counts and computes the sgRNA log2 fold changes;
#'       \item \code{ccr.logFCs2chromPos}: sorts the fold changes by
#'             genomic position;
#'       \item \code{ccr.GWclean}: segments the fold changes and corrects
#'             the segments with a biased response;
#'       \item \code{ccr.correctCounts}: computes corrected counts from the
#'             corrected fold changes.
#'     }
#'     The returned object contains only the control and treated samples and
#'     the sgRNAs kept by CRISPRcleanR. Its counts are the corrected
#'     normalized counts of CRISPRcleanR (rounded to integers, so that edgeR
#'     can model them). The CRISPRcleanR results are in
#'     \code{S4Vectors::metadata(object)$crisprcleanr}.
#' @param screenR_Object A ScreenR object created with
#'     \code{\link{create_screenr_object}} from a CRISPR-Cas9 screen.
#' @param library_annotation The sgRNA library annotation in the format of
#'     CRISPRcleanR: a data frame with the sgRNA identifiers (the barcodes of
#'     the ScreenR object) as row names and at least the columns GENES, CHRM
#'     (chromosome), STARTpos and ENDpos. CRISPRcleanR includes the
#'     annotation of several libraries, for example
#'     \code{data(KY_Library_v1.0, package = "CRISPRcleanR")} or
#'     \code{Brunello_Library}, \code{AVANA_Library},
#'     \code{GeCKO_Library_v2}.
#' @param control The name(s) of the control samples (for example the
#'     plasmid or T0), as in the columns of the count table.
#' @param treatment The names of the treated (or late time point) samples.
#' @param min_reads sgRNAs with fewer reads than this in the controls (mean)
#'     are removed (default 30, the CRISPRcleanR default).
#' @param min_targeted_genes Only segments with sgRNAs targeting at least
#'     this number of different genes are corrected (default 3, the
#'     CRISPRcleanR default).
#' @param label A name for the screen, used by CRISPRcleanR in its messages
#'     and output files.
#' @param outdir The directory where CRISPRcleanR writes its intermediate
#'     files (by default a temporary directory).
#' @param verbose Show the progress messages of CRISPRcleanR (default
#'     FALSE).
#' @return A ScreenR object with the corrected counts of the control and
#'     treated samples. \code{S4Vectors::metadata(object)$crisprcleanr} is a
#'     list with the uncorrected sgRNA log2 fold changes (logFCs), the
#'     corrected fold changes (corrected_logFCs) and the segments
#'     (segments) returned by CRISPRcleanR.
#' @references Iorio F, Behan FM, Goncalves E, et al. (2018). Unsupervised
#'     correction of gene-independent cell responses to CRISPR-Cas9
#'     targeting. BMC Genomics 19:604. \doi{10.1186/s12864-018-4989-y}
#' @seealso \code{\link{compute_screen_quality}} to check the screen with
#'     reference genes before and after the correction.
#' @importFrom S4Vectors metadata metadata<-
#' @concept objects
#' @export
#' @examples
#' # Needs the CRISPRcleanR package, which is not on CRAN or Bioconductor:
#' # BiocManager::install("francescojm/CRISPRcleanR")
#' if (crisprcleanr_available()) {
#'     ccr <- "CRISPRcleanR"
#'     # The KY library annotation distributed with CRISPRcleanR
#'     KY_Library_v1.0 <- get(utils::data("KY_Library_v1.0", package = ccr))
#'     file <- system.file("extdata", "HT-29_counts.tsv", package = ccr)
#'     counts <- read.delim(file)
#'     # A few chromosomes to keep the example fast
#'     keep <- KY_Library_v1.0[counts$sgRNA, "CHRM"] %in% c("8", "17")
#'     counts <- counts[keep, ]
#'     colnames(counts) <- c("Barcode", "Gene", "Plasmid", "R1", "R2", "R3")
#'     object <- create_screenr_object(
#'         table = counts[, -2],
#'         annotation = counts[, c("Barcode", "Gene")],
#'         groups = factor(c("Plasmid", "Late", "Late", "Late")),
#'         sample_table = data.frame(
#'             Sample = c("Plasmid", "R1", "R2", "R3"), Day = "Screen",
#'             Treatment = c("Plasmid", "Late", "Late", "Late")
#'         )
#'     )
#'     corrected <- correct_crispr_bias(object,
#'         library_annotation = KY_Library_v1.0,
#'         control = "Plasmid", treatment = c("R1", "R2", "R3")
#'     )
#'     corrected
#' }
correct_crispr_bias <- function(screenR_Object, library_annotation, control,
    treatment, min_reads = 30, min_targeted_genes = 3, label = "screen",
    outdir = tempfile("crisprcleanr_"), verbose = FALSE) {
    ccr <- crisprcleanr_functions()
    counts <- count_table_of(screenR_Object)
    annotation <- annotation_of(screenR_Object)
    samples <- c(control, treatment)
    missing <- setdiff(samples, colnames(counts))
    if (length(missing) > 0) {
        stop("Samples not found in the count table: ",
            paste(missing, collapse = ", "), call. = FALSE)
    }
    if (length(intersect(control, treatment)) > 0) {
        stop("control and treatment must be different samples.",
            call. = FALSE)
    }
    check_library_annotation(library_annotation, counts$Barcode)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    outdir <- paste0(normalizePath(outdir), .Platform$file.sep)

    # CRISPRcleanR input: sgRNA, gene, control columns, treated columns
    gene <- as.character(annotation$Gene[match(counts$Barcode,
        annotation$Barcode)])
    input <- data.frame(sgRNA = as.character(counts$Barcode), gene = gene,
        counts[, samples, drop = FALSE], check.names = FALSE,
        stringsAsFactors = FALSE)
    input <- input[!is.na(gene), ]

    # CRISPRcleanR prints one line for each corrected segment
    quiet <- function(expr) {
        if (verbose) return(expr)
        utils::capture.output(value <- expr)
        value
    }
    normalized <- quiet(ccr$NormfoldChanges(Dframe = input,
        display = FALSE, outdir = outdir, min_reads = min_reads,
        EXPname = label, libraryAnnotation = library_annotation,
        ncontrols = length(control)))
    sorted <- quiet(ccr$logFCs2chromPos(normalized$logFCs,
        library_annotation))
    cleaned <- quiet(ccr$GWclean(sorted, display = FALSE, label = label))
    corrected <- quiet(ccr$correctCounts(label, normalized$norm_counts,
        cleaned, library_annotation, minTargetedGenes = min_targeted_genes,
        OutDir = outdir, ncontrols = length(control), verbose = 1))

    corrected_counts <- data.frame(Barcode = corrected$sgRNA,
        round(as.matrix(corrected[, samples, drop = FALSE])),
        check.names = FALSE)
    sample_table <- get_sample_table(screenR_Object)
    sample_table <- sample_table[match(samples, sample_table$Sample),
        c("Sample", "Day", "Treatment")]
    groups <- droplevels(groups_of(screenR_Object)[match(samples,
        colnames(counts)[-1])])
    object <- create_screenr_object(
        table = corrected_counts,
        annotation = annotation[annotation$Barcode %in%
            corrected_counts$Barcode, , drop = FALSE],
        groups = groups, sample_table = sample_table
    )
    metadata(object)$crisprcleanr <- list(logFCs = normalized$logFCs,
        corrected_logFCs = cleaned$corrected_logFCs,
        segments = cleaned$segments)
    message(sprintf(paste0("CRISPRcleanR corrected %d sgRNAs in %d ",
        "samples; run normalize_data() and compute_data_table() on the ",
        "returned object."), nrow(corrected_counts), length(samples)))
    object
}

#' @title Is CRISPRcleanR Available
#' @description Tells whether the CRISPRcleanR package, used by
#'     \code{\link{correct_crispr_bias}}, is installed. It is not on CRAN or
#'     Bioconductor: install it with
#'     \code{BiocManager::install("francescojm/CRISPRcleanR")}.
#' @return TRUE or FALSE.
#' @seealso \code{\link{correct_crispr_bias}}
#' @concept objects
#' @export
#' @examples
#' crisprcleanr_available()
crisprcleanr_available <- function() {
    # The package name is in a variable: CRISPRcleanR is an optional
    # dependency that is not on CRAN or Bioconductor
    package <- "CRISPRcleanR"
    requireNamespace(package, quietly = TRUE)
}

#' @title CRISPRcleanR Functions
#' @description Returns the CRISPRcleanR functions used by
#'     \code{\link{correct_crispr_bias}}, or stops with installation
#'     instructions when CRISPRcleanR is not installed.
#' @param package The name of the package (only changed in the tests).
#' @return A list of functions.
#' @keywords internal
crisprcleanr_functions <- function(package = "CRISPRcleanR") {
    if (!requireNamespace(package, quietly = TRUE)) {
        stop("correct_crispr_bias() needs the ", package, " package, ",
            "which is not on CRAN or Bioconductor. Install it with\n",
            "  BiocManager::install(\"francescojm/CRISPRcleanR\")",
            call. = FALSE)
    }
    names <- c("NormfoldChanges", "logFCs2chromPos", "GWclean",
        "correctCounts")
    functions <- lapply(paste0("ccr.", names), getExportedValue,
        ns = package)
    stats::setNames(functions, names)
}

#' @title Check a CRISPRcleanR Library Annotation
#' @description Checks that the library annotation has the columns needed by
#'     CRISPRcleanR and that its row names match the barcodes.
#' @param library_annotation The library annotation.
#' @param barcodes The barcodes of the count table.
#' @return Invisibly TRUE; stops with an informative error otherwise.
#' @keywords internal
check_library_annotation <- function(library_annotation, barcodes) {
    if (!is.data.frame(library_annotation)) {
        stop("library_annotation must be a data frame.", call. = FALSE)
    }
    needed <- c("GENES", "CHRM", "STARTpos", "ENDpos")
    missing <- setdiff(needed, colnames(library_annotation))
    if (length(missing) > 0) {
        stop("library_annotation lacks the columns ",
            paste(missing, collapse = ", "), " (the CRISPRcleanR format: ",
            "see for example data(KY_Library_v1.0, package = ",
            "\"CRISPRcleanR\")).", call. = FALSE)
    }
    shared <- mean(as.character(barcodes) %in% rownames(library_annotation))
    if (shared == 0) {
        stop("No barcode of the count table is a row name of ",
            "library_annotation: the row names must be the sgRNA ",
            "identifiers.", call. = FALSE)
    }
    if (shared < 0.5) {
        warning(sprintf(paste0("Only %.0f%% of the barcodes are in ",
            "library_annotation; the others are removed."), 100 * shared),
            call. = FALSE)
    }
    invisible(TRUE)
}
