# Competitor methods for the ScreenR benchmark: BAGEL2, gscreend and two
# plain edgeR/limma workflows. Each function takes a count table (Barcode +
# one column per sample), the gene of each barcode and the control and
# treated sample names, and returns one row per gene with PValue and FDR for
# depletion (BAGEL2: a Bayes factor; see run_bagel2()).

suppressMessages({
    library(edgeR)
    library(limma)
})

one_sided_down <- function(p, logfc) ifelse(logfc < 0, p / 2, 1 - p / 2)

# edgeR on gene-level counts: the counts of the barcodes of each gene are
# summed, then the quasi-likelihood F-test of treated vs control.
run_edger_gene <- function(counts, gene, ctrl, trt) {
    time <- system.time({
        gene_counts <- rowsum(as.matrix(counts[, c(ctrl, trt)]), gene)
        group <- factor(rep(c("CTRL", "TRT"), c(length(ctrl), length(trt))),
            levels = c("CTRL", "TRT"))
        y <- DGEList(gene_counts, group = group)
        y <- y[filterByExpr(y, group = group), , keep.lib.sizes = FALSE]
        y <- normLibSizes(y)
        design <- model.matrix(~group)
        fit <- glmQLFit(estimateDisp(y, design), design)
        test <- glmQLFTest(fit, coef = 2)$table
        p <- one_sided_down(test$PValue, test$logFC)
    })[["elapsed"]]
    data.frame(Gene = rownames(test), PValue = p, FDR = p.adjust(p, "BH"),
        LFC = test$logFC, time = time)
}

# limma-voom on the barcode counts and camera with the barcodes of each gene
# as a set (the workflow of Dai et al. 2014 with voom instead of edgeR).
run_voom_camera <- function(counts, gene, ctrl, trt) {
    time <- system.time({
        group <- factor(rep(c("CTRL", "TRT"), c(length(ctrl), length(trt))),
            levels = c("CTRL", "TRT"))
        y <- DGEList(as.matrix(counts[, c(ctrl, trt)]), group = group,
            genes = data.frame(Gene = gene))
        y <- normLibSizes(y)
        design <- model.matrix(~group)
        v <- voom(y, design)
        index <- split(seq_along(gene), gene)
        cam <- camera(v, index, design, contrast = 2)
        p <- ifelse(cam$Direction == "Down", cam$PValue / 2,
            1 - cam$PValue / 2)
        lfc <- vapply(index[rownames(cam)], function(i) {
            median(lmFit(v[i, ], design)$coefficients[, 2])
        }, numeric(1))
    })[["elapsed"]]
    data.frame(Gene = rownames(cam), PValue = p, FDR = p.adjust(p, "BH"),
        LFC = lfc, time = time)
}

# gscreend (Imkeller et al. 2020): controls as T0, treated as T1. gscreend
# accepts a single T0 sample, so the control replicates are summed into one
# reference. (gscreend is deprecated in Bioconductor 3.19.)
run_gscreend <- function(counts, gene, ctrl, trt) {
    suppressMessages({
        library(gscreend)
        library(SummarizedExperiment)
    })
    time <- system.time({
        reference <- rowSums(as.matrix(counts[, ctrl, drop = FALSE]))
        se <- SummarizedExperiment(
            # gscreend fails if the count matrix has dimnames
            assays = list(counts = unname(cbind(reference,
                as.matrix(counts[, trt])))),
            rowData = data.frame(sgRNA_id = counts$Barcode, gene = gene),
            colData = data.frame(samplename = c("T0", trt),
                timepoint = c("T0", rep("T1", length(trt))))
        )
        pse <- suppressMessages(RunGscreend(createPoolScreenExp(se)))
        res <- ResultsTable(pse, direction = "negative")
    })[["elapsed"]]
    data.frame(Gene = res$Name, PValue = res$pval, FDR = res$fdr,
        LFC = res$lfc, time = time)
}

# BAGEL2 (Kim & Hart 2021). It is supervised: it needs training sets of
# essential and non-essential genes. Bayes factors are computed with 10-fold
# cross-validation, so the training genes are scored by models that did not
# see them. Calls at "FDR <= alpha" use BAGEL's own precision-recall on the
# training sets ("pr" command). PValue is a rank transform of -BF, used only
# to rank the genes.
run_bagel2 <- function(counts, gene, ctrl, trt, essentials, nonessentials,
    workdir, bagel, python, seed = 1) {
    dir.create(workdir, showWarnings = FALSE, recursive = TRUE)
    count_file <- file.path(workdir, "counts.txt")
    write.table(data.frame(SEQID = counts$Barcode, GENE = gene,
        counts[, c(ctrl, trt)], check.names = FALSE), count_file, sep = "\t",
        quote = FALSE, row.names = FALSE)
    writeLines(essentials, file.path(workdir, "ess.txt"))
    writeLines(nonessentials, file.path(workdir, "noness.txt"))
    run <- function(args) {
        status <- system2(python, c(bagel, args), stdout = FALSE,
            stderr = file.path(workdir, "bagel.log"))
        if (status != 0) stop("BAGEL2 failed, see ", workdir)
    }
    label <- file.path(workdir, "bagel")
    time <- system.time({
        run(c("fc", "-i", count_file, "-o", label, "-c",
            paste(ctrl, collapse = ",")))
        fc <- read.delim(paste0(label, ".foldchange"), check.names = FALSE)
        columns <- paste(seq_along(trt), collapse = ",")
        run(c("bf", "-i", paste0(label, ".foldchange"), "-o",
            paste0(label, ".bf"), "-e", file.path(workdir, "ess.txt"),
            "-n", file.path(workdir, "noness.txt"), "-c", columns,
            "--seed", seed))
        run(c("pr", "-i", paste0(label, ".bf"), "-o", paste0(label, ".pr"),
            "-e", file.path(workdir, "ess.txt"), "-n",
            file.path(workdir, "noness.txt")))
    })[["elapsed"]]
    bf <- read.delim(paste0(label, ".bf"))
    pr <- read.delim(paste0(label, ".pr"))
    fdr <- pr$FDR[match(bf$GENE, pr$Gene)]
    data.frame(Gene = bf$GENE, PValue = rank(-bf$BF) / nrow(bf), FDR = fdr,
        LFC = NA_real_, BF = bf$BF, time = time)
}
