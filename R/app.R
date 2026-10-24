# The ScreenR app (see run_screenr_app()). shiny, bslib and DT are only
# suggested, so every call is qualified and the functions below are only
# called after screenr_app() has checked that the packages are installed.

# Example screens ----------------------------------------------------------

app_examples <- c(
    "CRISPR-Cas9 dropout screen (HAP1, TKOv3)" = "hap1",
    "shRNA screen, treated vs control" = "shrna"
)

app_example <- function(name) {
    env <- new.env()
    if (name == "hap1") {
        utils::data("hap1_count_table", "hap1_annotation_table",
            package = "ScreenR", envir = env)
        counts <- env$hap1_count_table
        samples <- data.frame(
            Sample = colnames(counts)[-1],
            Day = c("T0", "T3", "T3", "T3", "T18", "T18", "T18"),
            Treatment = "Untreated",
            Group = c("T0", "T3", "T3", "T3", "T18", "T18", "T18")
        )
        list(counts = counts, annotation = env$hap1_annotation_table,
            samples = samples, crispr = TRUE,
            treatment = "T18", control = "T0")
    } else {
        utils::data("count_table", "annotation_table",
            package = "ScreenR", envir = env)
        sample <- c("Time1", "Time2", paste0(
            rep(c("Time3", "Time4"), each = 6), "_",
            rep(rep(c("TRT", "CTRL"), each = 3), 2), "_", c("A", "B", "C")))
        counts <- env$count_table[env$count_table$Barcode != "*", ]
        colnames(counts) <- c("Barcode", sample)
        day <- c("Time1", "Time2", rep(c("Time3", "Time4"), each = 6))
        treatment <- c("Time1", "Time2",
            rep(rep(c("TRT", "CTRL"), each = 3), 2))
        samples <- data.frame(
            Sample = sample, Day = day, Treatment = treatment,
            Group = ifelse(day %in% c("Time1", "Time2"), "T1/T2",
                paste(day, treatment, sep = "_"))
        )
        list(counts = counts, annotation = env$annotation_table,
            samples = samples, crispr = FALSE,
            treatment = "Time4_TRT", control = "Time4_CTRL")
    }
}

# Reading the files of the user ---------------------------------------------

app_read_table <- function(path, name) {
    sep <- if (grepl("\\.csv$", name, ignore.case = TRUE)) "," else "\t"
    utils::read.table(path, sep = sep, header = TRUE, quote = "\"",
        comment.char = "", check.names = FALSE, stringsAsFactors = FALSE)
}

# A first guess of the sample table from the names of the samples: the
# replicate suffix (_A, -2, _R1, ...) is removed to obtain the group
app_guess_samples <- function(samples) {
    group <- sub("[_.-]+(R|rep)?([0-9]+|[A-Za-z])$", "", samples)
    data.frame(Sample = samples, Day = group, Treatment = "Untreated",
        Group = group)
}

# The object from the tables of the data page
app_build_object <- function(counts, annotation, samples, normalization) {
    groups <- factor(samples$Group, levels = unique(samples$Group))
    object <- create_screenr_object(
        table = counts, annotation = annotation, groups = groups,
        sample_table = samples[, c("Sample", "Day", "Treatment")],
        duplicate_barcodes = "sum", duplicate_annotation = "first",
        palettes = app_palettes(samples)
    )
    normalize_data(remove_all_zero_row(object), method = normalization)
}

# Plots ------------------------------------------------------------------------

# Colours of the website: from sage to teal to clay
app_colors <- function(n) {
    grDevices::colorRampPalette(c("#93B5A3", "#2E7D6E", "#1F5F55",
        "#C9836A", "#8C4A36"))(n)
}

# Each group, day and treatment gets a colour; the samples get the colour
# of their group, lighter or darker for each replicate
app_palettes <- function(samples) {
    named <- function(values) {
        values <- unique(values)
        stats::setNames(app_colors(length(values)), values)
    }
    group <- named(samples$Group)
    sample <- unlist(lapply(names(group), function(g) {
        which <- samples$Sample[samples$Group == g]
        shades <- grDevices::colorRampPalette(c(
            grDevices::adjustcolor(group[[g]], 0.55), group[[g]],
            grDevices::colorRampPalette(c(group[[g]], "black"))(5)[2]),
            alpha = TRUE)(max(length(which), 3))
        stats::setNames(if (length(which) == 1) group[[g]] else
            shades[round(seq(1, length(shades), length.out =
                length(which)))], which)
    }))
    list(Group = group, Day = named(samples$Day),
        Treatment = named(samples$Treatment), Sample = sample)
}

# The plots are drawn on the glass cards: transparent background, and the
# text colour of the light or dark theme
app_theme <- function(dark = FALSE) {
    grid <- if (isTRUE(dark)) "#FFFFFF1A" else "#13332D1A"
    ggplot2::theme_minimal(base_size = 13) + app_surface(dark) +
        ggplot2::theme(panel.grid = ggplot2::element_line(colour = grid))
}

# Only the colours of app_theme(), for plots with a layout of their own
app_surface <- function(dark = FALSE) {
    ink <- if (isTRUE(dark)) "#DCE9E4" else "#13332D"
    ggplot2::theme(
        text = ggplot2::element_text(colour = ink),
        axis.text = ggplot2::element_text(colour = ink),
        strip.text = ggplot2::element_text(colour = ink, face = "bold"),
        plot.background = ggplot2::element_rect(fill = NA, colour = NA),
        panel.background = ggplot2::element_rect(fill = NA, colour = NA),
        legend.background = ggplot2::element_rect(fill = NA, colour = NA),
        legend.key = ggplot2::element_rect(fill = NA, colour = NA)
    )
}

# The R code that repeats the analysis of the app
app_script <- function(source, params) {
    data <- if (source == "hap1") {
        c("data(hap1_count_table)", "data(hap1_annotation_table)",
            "counts <- hap1_count_table",
            "annotation <- hap1_annotation_table")
    } else if (source == "shrna") {
        c("data(count_table)", "data(annotation_table)",
            "counts <- count_table[count_table$Barcode != \"*\", ]",
            sprintf("colnames(counts) <- c(\"Barcode\", %s)",
                app_deparse(params$samples$Sample)),
            "annotation <- annotation_table")
    } else {
        c(sprintf("counts <- read.delim(\"%s\", check.names = FALSE)",
            params$counts_file),
            if (is.null(params$annotation_file)) {
                sprintf(paste0("annotation <- counts[, c(\"%s\", \"%s\")]\n",
                    "counts <- counts[, setdiff(colnames(counts), \"%s\")]"),
                    "Barcode", "Gene", "Gene")
            } else {
                sprintf("annotation <- read.delim(\"%s\")",
                    params$annotation_file)
            })
    }
    samples <- params$samples
    methods <- params$methods
    nb <- params$min_barcodes
    calls <- c(
        if ("fry" %in% methods) sprintf(paste0("    fry = find_roast_hit(",
            "object, design, contrast, method = \"fry\",\n        ",
            "number_barcode = %d, direction = \"%s\", fdr = 1, seed = 1)"),
            nb - 1L, params$direction),
        if ("camera" %in% methods) sprintf(paste0("    camera = ",
            "find_camera_hit(object, design, contrast,\n        ",
            "number_barcode = %d, direction = \"%s\", fdr = 1)"),
            nb - 1L, params$direction),
        if ("zscore" %in% methods) sprintf(paste0("    zscore = ",
            "find_gene_zscore_hit(statistics, reference = \"empirical\",\n",
            "        min_barcodes = %d, direction = \"%s\", fdr = 1)"),
            nb, params$direction),
        if ("zscore_reference" %in% methods) sprintf(paste0(
            "    zscore_reference = find_gene_zscore_hit(statistics,\n",
            "        control_genes = nonessential, min_barcodes = %d,\n",
            "        direction = \"%s\", fdr = 1, seed = 1)"),
            nb, params$direction)
    )
    c("# Analysis made with the ScreenR app",
        sprintf("# %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
        "library(ScreenR)", "", data, "",
        "samples <- data.frame(",
        sprintf("    Sample = %s,", app_deparse(samples$Sample)),
        sprintf("    Day = %s,", app_deparse(samples$Day)),
        sprintf("    Treatment = %s", app_deparse(samples$Treatment)),
        ")",
        sprintf("groups <- factor(%s,\n    levels = %s)",
            app_deparse(samples$Group), app_deparse(unique(samples$Group))),
        "object <- create_screenr_object(table = counts,",
        "    annotation = annotation, groups = groups, sample_table = samples,",
        "    duplicate_barcodes = \"sum\", duplicate_annotation = \"first\")",
        paste0("object <- normalize_data(remove_all_zero_row(object), ",
            "method = \"", params$normalization, "\")"),
        "", "design <- create_design_matrix(object)",
        sprintf("contrast <- create_contrast(design, treatment = \"%s\",",
            params$treatment),
        sprintf("    control = \"%s\")", params$control),
        "statistics <- compute_barcode_statistics(object, design, contrast)",
        if ("zscore_reference" %in% methods) c(
            "data(hart_reference_genes)",
            paste0("nonessential <- hart_reference_genes$Gene[",
                "hart_reference_genes$Class == \"non-essential\"]")),
        "hit_list <- list(", paste(calls, collapse = ",\n"), ")",
        sprintf("hits <- summarise_hits(hit_list, fdr = %s)", params$fdr),
        "head(hits)")
}

app_deparse <- function(x) {
    paste(deparse(x, width.cutoff = 70), collapse = "\n    ")
}

# The help text under a plot
app_hint <- function(...) {
    shiny::div(class = "sr-hint", shiny::icon("lightbulb"), shiny::span(...))
}

app_card <- function(title, ..., hint = NULL, height = NULL) {
    bslib::card(
        full_screen = TRUE, height = height,
        bslib::card_header(title),
        bslib::card_body(..., if (!is.null(hint)) app_hint(hint))
    )
}

# User interface ------------------------------------------------------------

app_ui <- function() {
    theme <- bslib::bs_theme(
        version = 5, primary = "#1F5F55", secondary = "#C9836A",
        success = "#2E7D6E", "border-radius" = "0.75rem",
        base_font = bslib::font_google("Inter", local = FALSE),
        heading_font = bslib::font_google("Inter", local = FALSE),
        code_font = bslib::font_google("JetBrains Mono", local = FALSE)
    )
    brand <- shiny::tags$span(class = "sr-brand",
        shiny::tags$img(src = "screenr-app/logo.png", alt = ""),
        shiny::tags$span("ScreenR"),
        shiny::tags$span(class = "sr-version",
            as.character(utils::packageVersion("ScreenR"))))
    bslib::page_navbar(
        id = "page", title = brand, theme = theme, lang = "en",
        window_title = "ScreenR app", fillable = FALSE,
        header = shiny::tags$head(
            shiny::tags$link(rel = "stylesheet",
                href = "screenr-app/screenr-app.css"),
            shiny::tags$script(src = "screenr-app/screenr-app.js")),
        bslib::nav_panel("1  Data", icon = shiny::icon("table"),
            app_ui_data()),
        bslib::nav_panel("2  Quality control",
            icon = shiny::icon("magnifying-glass-chart"), app_ui_qc()),
        bslib::nav_panel("3  Hits", icon = shiny::icon("bullseye"),
            app_ui_hits()),
        bslib::nav_panel("4  Gene explorer", icon = shiny::icon("dna"),
            app_ui_gene()),
        bslib::nav_spacer(),
        bslib::nav_item(shiny::tags$a(shiny::icon("book"), "Tutorial",
            href = paste0("https://emanuelsoda.github.io/ScreenR/",
                "articles/Tutorial_5_ScreenR_app.html"),
            target = "_blank")),
        bslib::nav_item(bslib::input_dark_mode(id = "dark"))
    )
}

app_ui_data <- function() {
    shiny::tagList(
        shiny::div(class = "sr-hero",
            shiny::h1("Find the hits of your screen"),
            shiny::p("Pooled shRNA and CRISPR-Cas9 screens, from the counts",
                "to the genes, without writing code. Load the counts,",
                "describe the samples, then follow the tabs."),
            shiny::div(class = "sr-steps",
                lapply(list(
                    c("1", "Data", "Counts and samples"),
                    c("2", "Quality control", "Are the samples good?"),
                    c("3", "Hits", "Genes depleted or enriched"),
                    c("4", "Gene explorer", "Check each hit")),
                    function(s) shiny::div(class = "sr-step",
                        shiny::span(class = "sr-step-n", s[1]),
                        shiny::strong(s[2]), shiny::span(s[3]))))),
        bslib::layout_columns(
            col_widths = c(4, 8),
            bslib::card(
                bslib::card_header("Load a screen"),
                shiny::radioButtons("source", NULL, choices = c(
                    app_examples, "My own data" = "upload")),
                shiny::conditionalPanel("input.source == 'upload'",
                    shiny::fileInput("counts_file", "Count table",
                        accept = c(".txt", ".tsv", ".csv", ".gz")),
                    app_hint("One row per barcode (shRNA or sgRNA), one",
                        "column per sample. A MAGeCK count file (sgRNA, Gene,",
                        "samples) works as it is."),
                    shiny::uiOutput("columns_ui"),
                    shiny::fileInput("annotation_file",
                        "Annotation (optional)",
                        accept = c(".txt", ".tsv", ".csv")),
                    app_hint("Only needed when the count table has no gene",
                        "column: a table with the Barcode and Gene columns.")),
                shiny::selectInput("normalization", "Normalization",
                    choices = c("Counts per million (CPM)" = "cpm",
                        "TMM (edgeR)" = "tmm")),
                shiny::checkboxInput("crispr", paste("CRISPR screen in human",
                    "cells: use the core essential and non-essential genes",
                    "as references"), value = TRUE),
                shiny::actionButton("create", "Create the ScreenR object",
                    icon = shiny::icon("wand-magic-sparkles"),
                    class = "btn-primary w-100")),
            bslib::card(
                bslib::card_header("Describe the samples"),
                app_hint("Double-click a cell to edit it. Group: the samples",
                    "that are replicates of each other (the models compare",
                    "groups). Day: the time point. Treatment: the condition,",
                    "or \"Untreated\" in a dropout screen."),
                DT::DTOutput("samples_table"))),
        shiny::uiOutput("object_summary")
    )
}

app_ui_qc <- function() {
    shiny::tagList(
        shiny::uiOutput("need_object_qc"),
        bslib::layout_columns(
            col_widths = c(6, 6, 6, 6, 6, 6),
            app_card("Mapped reads", shiny::plotOutput("qc_reads"),
                hint = paste("The number of reads of each sample. Samples",
                    "with far fewer reads than the others give noisy",
                    "results.")),
            app_card("Barcodes lost",
                shiny::plotOutput("qc_lost"),
                hint = paste("Barcodes with no read. Many lost barcodes in",
                    "the early samples mean that the library was not well",
                    "represented.")),
            app_card("Similarity of the samples (MDS)",
                shiny::plotOutput("qc_mds"),
                hint = paste("Replicates should be close to each other and",
                    "the groups should separate.")),
            app_card("Replicate correlation",
                shiny::plotOutput("qc_correlation"),
                hint = "Replicates of the same group should correlate well."),
            app_card("Distribution of the counts",
                shiny::plotOutput("qc_distribution"),
                hint = paste("Similar distributions after normalization:",
                    "the samples can be compared.")),
            app_card("Evenness of the library (Gini index)",
                shiny::plotOutput("qc_gini"),
                hint = paste("Low at the start of the screen; it grows over",
                    "time as some barcodes drop out."))))
}

app_ui_hits <- function() {
    bslib::layout_sidebar(
        sidebar = bslib::sidebar(
            width = 300, title = "Comparison",
            shiny::selectInput("treatment", "Compare", choices = NULL),
            shiny::selectInput("control", "against", choices = NULL),
            shiny::radioButtons("direction", "Genes",
                choices = c("Depleted" = "Down", "Enriched" = "Up"),
                inline = TRUE),
            shiny::checkboxGroupInput("methods", "Methods", choices = c(
                "FRY (limma)" = "fry", "CAMERA (limma)" = "camera",
                "Z-score, empirical null" = "zscore",
                "Z-score, non-essential genes" = "zscore_reference"),
                selected = c("fry", "camera", "zscore")),
            shiny::sliderInput("fdr", "False discovery rate", min = 0.01,
                max = 0.25, value = 0.05, step = 0.01),
            shiny::numericInput("min_barcodes", "Barcodes per gene (minimum)",
                value = 3, min = 1, max = 10),
            shiny::actionButton("run_hits", "Find hits",
                icon = shiny::icon("bullseye"), class = "btn-primary w-100"),
            app_hint("A gene is a hit when all the methods call it at this",
                "FDR. Click a gene in the plot or the table to open it in",
                "the gene explorer.")),
        shiny::uiOutput("need_object_hits"),
        shiny::uiOutput("hit_boxes"),
        bslib::layout_columns(
            col_widths = c(6, 6),
            app_card("Volcano plot",
                shiny::plotOutput("volcano", click = "volcano_click",
                    height = "420px"),
                hint = paste("Each point is a gene. Above the dashed line:",
                    "FDR below the cut off in every method.")),
            app_card("Agreement of the methods",
                shiny::plotOutput("common", height = "420px"),
                hint = paste("How many genes each method calls, and how they",
                    "overlap."))),
        bslib::card(
            bslib::card_header(class = "d-flex align-items-center",
                "Hits", shiny::div(class = "ms-auto d-flex gap-2",
                    shiny::downloadButton("download_hits", "Table (.csv)",
                        class = "btn-sm btn-outline-primary"),
                    shiny::downloadButton("download_script", "R script",
                        class = "btn-sm btn-outline-primary"))),
            DT::DTOutput("hits_table")),
        shiny::uiOutput("quality_ui"))
}

app_ui_gene <- function() {
    shiny::tagList(
        shiny::uiOutput("need_object_gene"),
        shiny::div(class = "sr-gene-bar",
            shiny::selectizeInput("gene", NULL, choices = NULL, width = "320px",
                options = list(placeholder = "Search a gene...")),
            shiny::uiOutput("gene_badges")),
        bslib::layout_columns(
            col_widths = c(6, 6, 6, 6),
            app_card("Barcodes of the gene among all barcodes",
                shiny::plotOutput("gene_barcode_hit"),
                hint = paste("All the barcodes ranked by their log2 fold",
                    "change: the lines are the barcodes of the gene. Lines",
                    "on one side mean that the barcodes agree.")),
            app_card("Each barcode in each sample",
                shiny::plotOutput("gene_heatmap"),
                hint = paste("A good hit has all its barcodes going the same",
                    "way. One barcode alone can be an off-target effect.")),
            app_card("Over time", shiny::plotOutput("gene_trend"),
                hint = paste("Mean normalized count of the barcodes at each",
                    "time point.")),
            app_card("Normalized counts",
                shiny::plotOutput("gene_boxplot"),
                hint = paste("The normalized counts of the barcodes in each",
                    "sample."))))
}

# Server -----------------------------------------------------------------------

app_server <- function(initial = NULL) {
    function(input, output, session) {
        state <- shiny::reactiveValues(object = initial, samples = NULL,
            counts = NULL, annotation = NULL, results = NULL,
            source = if (is.null(initial)) "hap1" else "object")
        dark <- shiny::reactive(identical(input$dark, "dark"))

        render <- function(expr) {
            expr <- substitute(expr)
            env <- parent.frame()
            shiny::renderPlot({
                shiny::req(state$object)
                plot <- tryCatch(eval(expr, env), error = function(e) {
                    shiny::validate(shiny::need(FALSE, conditionMessage(e)))
                })
                plot & app_theme(dark())
            }, bg = "transparent", res = 110)
        }

        # Data --------------------------------------------------------------

        uploaded <- shiny::reactive({
            shiny::req(input$counts_file)
            app_read_table(input$counts_file$datapath, input$counts_file$name)
        })

        output$columns_ui <- shiny::renderUI({
            table <- uploaded()
            text <- colnames(table)[!vapply(table, is.numeric, logical(1))]
            shiny::tagList(
                shiny::selectInput("barcode_column", "Barcode column", text),
                shiny::selectInput("gene_column", "Gene column",
                    c("(in the annotation)" = "", text),
                    selected = if (length(text) > 1) text[2] else ""))
        })

        # The tables of the chosen source
        shiny::observe({
            source <- input$source
            if (source == "upload") {
                table <- uploaded()
                shiny::req(input$barcode_column)
                numeric <- colnames(table)[vapply(table, is.numeric,
                    logical(1))]
                counts <- table[, c(input$barcode_column, numeric)]
                colnames(counts)[1] <- "Barcode"
                if (nzchar(input$gene_column %||% "")) {
                    annotation <- table[, c(input$barcode_column,
                        input$gene_column)]
                    colnames(annotation) <- c("Barcode", "Gene")
                } else {
                    shiny::req(input$annotation_file)
                    annotation <- app_read_table(
                        input$annotation_file$datapath,
                        input$annotation_file$name)
                }
                state$counts <- counts
                state$annotation <- annotation
                state$samples <- app_guess_samples(numeric)
            } else {
                example <- app_example(source)
                state$counts <- example$counts
                state$annotation <- example$annotation
                state$samples <- example$samples
                shiny::updateCheckboxInput(session, "crispr",
                    value = example$crispr)
            }
        })

        output$samples_table <- DT::renderDT({
            shiny::req(state$samples)
            DT::datatable(shiny::isolate(state$samples), rownames = FALSE,
                selection = "none", class = "compact",
                editable = list(target = "cell", disable = list(columns = 0)),
                options = list(dom = "t", paging = FALSE, ordering = FALSE))
        })
        shiny::observeEvent(input$samples_table_cell_edit, {
            state$samples <- DT::editData(state$samples,
                input$samples_table_cell_edit, rownames = FALSE)
        })

        shiny::observeEvent(input$create, {
            shiny::req(state$counts, state$samples)
            object <- shiny::withProgress(message = "Creating the object", {
                tryCatch(app_build_object(state$counts, state$annotation,
                    state$samples, input$normalization),
                    error = function(e) {
                        shiny::showNotification(conditionMessage(e),
                            type = "error", duration = 10)
                        NULL
                    })
            })
            shiny::req(object)
            state$object <- object
            state$results <- NULL
            state$source <- input$source
            shiny::showNotification("The object is ready: check its quality",
                type = "message")
        })

        # Reference genes of CRISPR screens, among the genes of the screen
        references <- shiny::reactive({
            shiny::req(state$object)
            if (!isTRUE(input$crispr)) return(NULL)
            env <- new.env()
            utils::data("hart_reference_genes", package = "ScreenR",
                envir = env)
            genes <- unique(SummarizedExperiment::rowData(state$object)$Gene)
            reference <- env$hart_reference_genes
            list(
                essential = intersect(reference$Gene[
                    reference$Class == "core essential"], genes),
                nonessential = intersect(reference$Gene[
                    reference$Class == "non-essential"], genes))
        })

        shiny::observeEvent(state$object, {
            groups <- levels(get_groups(state$object))
            example <- if (state$source %in% app_examples) {
                app_example(state$source)
            }
            shiny::updateSelectInput(session, "treatment", choices = groups,
                selected = example$treatment %||% groups[length(groups)])
            shiny::updateSelectInput(session, "control", choices = groups,
                selected = example$control %||% groups[1])
            methods <- c("fry", "camera", "zscore")
            if (length(references()$nonessential) >= 10) {
                methods <- c(methods, "zscore_reference")
            }
            shiny::updateCheckboxGroupInput(session, "methods",
                selected = methods)
            shiny::updateSelectizeInput(session, "gene", server = TRUE,
                choices = sort(unique(
                    SummarizedExperiment::rowData(state$object)$Gene)),
                selected = character(0))
        })

        output$object_summary <- shiny::renderUI({
            shiny::req(state$object)
            object <- state$object
            reads <- sum(SummarizedExperiment::assay(object, "counts"))
            bslib::layout_columns(
                col_widths = c(3, 3, 3, 3),
                bslib::value_box("Samples", ncol(object),
                    showcase = shiny::icon("vial")),
                bslib::value_box("Barcodes", format(nrow(object),
                    big.mark = ","), showcase = shiny::icon("barcode")),
                bslib::value_box("Genes", format(length(unique(
                    SummarizedExperiment::rowData(object)$Gene)),
                    big.mark = ","), showcase = shiny::icon("dna")),
                bslib::value_box("Mapped reads",
                    paste0(format(round(reads / 1e6, 1)), " M"),
                    showcase = shiny::icon("chart-simple")))
        })

        need_object <- function() {
            shiny::renderUI({
                if (is.null(state$object)) {
                    shiny::div(class = "sr-empty", shiny::icon("table"),
                        "Load a screen and create the object in the Data tab",
                        "first.")
                }
            })
        }
        output$need_object_qc <- need_object()
        output$need_object_hits <- need_object()
        output$need_object_gene <- need_object()

        # Quality control ----------------------------------------------------

        output$qc_reads <- render(plot_mapped_reads(state$object) +
            ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)))
        output$qc_lost <- render(plot_barcode_lost(state$object) +
            ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)))
        output$qc_mds <- render(plot_mds(state$object))
        output$qc_correlation <- render(plot_replicate_correlation(
            state$object))
        output$qc_distribution <- render(plot_mapped_reads_distribution(
            state$object) +
            ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)))
        output$qc_gini <- render(plot_gini(state$object) +
            ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)))

        # Hits --------------------------------------------------------------

        shiny::observeEvent(input$run_hits, {
            shiny::req(state$object, input$treatment, input$control)
            if (input$treatment == input$control) {
                shiny::showNotification("Choose two different groups",
                    type = "warning")
                return()
            }
            if (length(input$methods) == 0) {
                shiny::showNotification("Choose at least one method",
                    type = "warning")
                return()
            }
            object <- state$object
            nb <- as.integer(input$min_barcodes)
            direction <- input$direction
            shiny::withProgress(message = "Finding hits", value = 0, {
                result <- tryCatch({
                    design <- create_design_matrix(object)
                    contrast <- create_contrast(design,
                        treatment = input$treatment, control = input$control)
                    statistics <- compute_barcode_statistics(object, design,
                        contrast)
                    hit_list <- list()
                    step <- 1 / (length(input$methods) + 1)
                    for (method in input$methods) {
                        shiny::incProgress(step, detail = method)
                        hit_list[[method]] <- switch(method,
                            fry = find_roast_hit(object, design, contrast,
                                method = "fry", number_barcode = nb - 1L,
                                direction = direction, fdr = 1, seed = 1),
                            camera = find_camera_hit(object, design,
                                contrast, number_barcode = nb - 1L,
                                direction = direction, fdr = 1),
                            zscore = find_gene_zscore_hit(statistics,
                                reference = "empirical", min_barcodes = nb,
                                direction = direction, fdr = 1),
                            zscore_reference = {
                                control <- references()$nonessential
                                if (length(control) < 10) {
                                    stop("The non-essential genes are not in ",
                                        "this screen: use another method")
                                }
                                find_gene_zscore_hit(statistics,
                                    control_genes = control,
                                    min_barcodes = nb, direction = direction,
                                    fdr = 1, seed = 1)
                            })
                    }
                    list(design = design, contrast = contrast,
                        statistics = statistics, hit_list = hit_list,
                        treatment = input$treatment, control = input$control,
                        direction = direction, methods = input$methods,
                        min_barcodes = nb)
                }, error = function(e) {
                    shiny::showNotification(conditionMessage(e),
                        type = "error", duration = 10)
                    NULL
                })
            })
            state$results <- result
        })

        hits <- shiny::reactive({
            shiny::req(state$results)
            hits <- summarise_hits(state$results$hit_list, fdr = input$fdr)
            log2fc <- stats::aggregate(Log2FC ~ Gene,
                data = state$results$statistics, FUN = mean)
            hits$Log2FC <- log2fc$Log2FC[match(hits$Gene, log2fc$Gene)]
            hits[, c("Gene", "Log2FC", setdiff(colnames(hits),
                c("Gene", "Log2FC")))]
        })

        output$hit_boxes <- shiny::renderUI({
            shiny::req(state$results)
            hits <- hits()
            results <- state$results
            bslib::layout_columns(
                col_widths = c(4, 4, 4),
                bslib::value_box("Hits", sum(hits$Hit),
                    paste("genes", if (results$direction == "Down") {
                        "depleted"
                    } else {
                        "enriched"
                    }, "at", paste0(input$fdr * 100, "% FDR")),
                    showcase = shiny::icon("bullseye")),
                bslib::value_box("Comparison", paste(results$treatment, "vs",
                    results$control), paste(length(results$methods),
                    "methods"), showcase = shiny::icon("code-compare")),
                bslib::value_box("Genes tested", format(nrow(hits),
                    big.mark = ","), showcase = shiny::icon("dna")))
        })

        volcano <- shiny::reactive({
            plot_volcano(hits(), statistics = state$results$statistics,
                fdr = input$fdr, genes = input$gene,
                label_color = if (dark()) "#A8D8C9" else "#1F5F55") +
                app_theme(dark()) + ggplot2::theme(legend.position = "top")
        })
        output$volcano <- shiny::renderPlot(volcano(), bg = "transparent",
            res = 110)

        output$common <- shiny::renderPlot({
            shiny::req(state$results)
            if (length(state$results$hit_list) < 2) {
                shiny::validate(shiny::need(FALSE,
                    "Choose two methods or more to compare them"))
            }
            plot <- plot_common_hit(state$results$hit_list, fdr = input$fdr,
                show_elements = FALSE,
                text_color = if (dark()) "#DCE9E4" else "#13332D")
            plot & app_surface(dark())
        }, bg = "transparent", res = 110)

        output$hits_table <- DT::renderDT({
            hits <- hits()
            DT::formatSignif(DT::datatable(hits, rownames = FALSE,
                selection = "single", class = "compact hover",
                filter = "top", options = list(pageLength = 10,
                    scrollX = TRUE)),
                columns = which(vapply(hits, is.double, logical(1)) &
                    colnames(hits) != "NMethods"), digits = 3)
        })

        show_gene <- function(gene) {
            shiny::updateSelectizeInput(session, "gene", server = TRUE,
                choices = sort(unique(
                    SummarizedExperiment::rowData(state$object)$Gene)),
                selected = gene)
            bslib::nav_select("page", "4  Gene explorer")
        }
        shiny::observeEvent(input$volcano_click, {
            point <- shiny::nearPoints(volcano()$data, input$volcano_click,
                xvar = "Log2FC", yvar = "Significance", maxpoints = 1)
            if (nrow(point) == 1) show_gene(point$Gene)
        })
        shiny::observeEvent(input$hits_table_rows_selected, {
            show_gene(hits()$Gene[input$hits_table_rows_selected])
        })

        output$download_hits <- shiny::downloadHandler(
            filename = function() {
                paste0("ScreenR_hits_", state$results$treatment, "_vs_",
                    state$results$control, ".csv")
            },
            content = function(file) {
                utils::write.csv(hits(), file, row.names = FALSE)
            })

        output$download_script <- shiny::downloadHandler(
            filename = "ScreenR_analysis.R",
            content = function(file) {
                shiny::req(state$results)
                results <- state$results
                writeLines(app_script(state$source, list(
                    samples = state$samples,
                    normalization = input$normalization,
                    counts_file = input$counts_file$name,
                    annotation_file = input$annotation_file$name,
                    treatment = results$treatment, control = results$control,
                    methods = results$methods, direction = results$direction,
                    min_barcodes = results$min_barcodes, fdr = input$fdr)),
                    file)
            })

        # Screen quality with the reference genes (CRISPR screens)
        output$quality_ui <- shiny::renderUI({
            shiny::req(state$results)
            reference <- references()
            if (is.null(reference) || length(reference$essential) < 10) {
                return(NULL)
            }
            bslib::layout_columns(
                col_widths = c(7, 5),
                app_card("Screen quality: reference genes",
                    shiny::plotOutput("reference_plot"),
                    hint = paste("The core essential genes (depleted in",
                        "every cell line) should separate from the",
                        "non-essential genes.")),
                app_card("Quality metrics", shiny::tableOutput("quality"),
                    hint = paste("AUC and SSMD: higher is better. An AUC",
                        "above 0.9 is a good CRISPR screen.")))
        })
        output$reference_plot <- shiny::renderPlot({
            reference <- references()
            plot_reference_separation(state$results$statistics,
                reference$essential, reference$nonessential) &
                app_theme(dark())
        }, bg = "transparent", res = 110)
        output$quality <- shiny::renderTable({
            reference <- references()
            quality <- compute_screen_quality(state$results$statistics,
                reference$essential, reference$nonessential)
            data.frame(Metric = colnames(quality),
                Value = vapply(quality, function(x) {
                    if (is.numeric(x)) format(signif(x, 3)) else
                        as.character(x)
                }, character(1)))
        }, striped = FALSE, bordered = FALSE)

        # Gene explorer -----------------------------------------------------

        gene <- shiny::reactive({
            shiny::req(state$object)
            shiny::validate(shiny::need(nzchar(input$gene %||% ""),
                "Pick a gene above, or click one in the Hits tab."))
            input$gene
        })

        output$gene_badges <- shiny::renderUI({
            shiny::req(input$gene)
            gene <- gene()
            if (is.null(state$results)) {
                return(shiny::span(class = "sr-badge",
                    "Run Find hits to see its statistics"))
            }
            row <- hits()[hits()$Gene == gene, ]
            if (nrow(row) == 0) {
                return(shiny::span(class = "sr-badge", "Not tested",
                    "(too few barcodes)"))
            }
            fdr <- grep("^FDR_", colnames(row), value = TRUE)
            shiny::tagList(
                shiny::span(class = if (row$Hit) "sr-badge sr-hit" else
                    "sr-badge", if (row$Hit) "Hit" else "Not a hit"),
                shiny::span(class = "sr-badge",
                    sprintf("log2FC %.2f", row$Log2FC)),
                lapply(fdr, function(column) shiny::span(class = "sr-badge",
                    sprintf("%s FDR %s", sub("^FDR_", "", column),
                        format(signif(row[[column]], 2))))))
        })

        output$gene_barcode_hit <- render({
            g <- gene()
            if (is.null(state$results)) {
                stop("Run Find hits first: the plot uses the comparison")
            }
            plot_barcode_hit(state$object, state$results$design,
                state$results$contrast,
                number_barcode = state$results$min_barcodes - 1L,
                gene = g)
        })
        # Long barcode names (sgRNA sequences) would leave no room for the
        # heatmap
        output$gene_heatmap <- render({
            g <- gene()
            plot_barcode_heatmap(state$object, genes = g,
                barcode_labels = max(nchar(rownames(
                    state$object)[SummarizedExperiment::rowData(
                    state$object)$Gene == g])) <= 16) +
                ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(
                    angle = 45))
        })
        output$gene_trend <- render({
            g <- gene()
            treatments <- unique(get_sample_table(state$object)$Treatment)
            plot_trend(state$object, genes = g, group_var = treatments,
                trend = "mean",
                color_by = if (length(treatments) > 1) "Treatment")
        })
        output$gene_boxplot <- render({
            g <- gene()
            plot_boxplot(state$object, genes = g,
                group_var = unique(get_sample_table(state$object)$Treatment),
                fill_var = "Day") +
                ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(
                    angle = 45))
        })
    }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
