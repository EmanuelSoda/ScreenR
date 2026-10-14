# Simulation of pooled shRNA screens with known hits and non-targeting
# controls, for the benchmark of ScreenR against MAGeCK.
simulate_screen <- function(n_genes = 500, n_barcodes = 10, n_reps = 3,
    hit_fraction = 0.1, effect = -1, prop_inactive = 0.2,
    dispersion = 0.05, reads_per_barcode = 400, off_target = 0,
    off_target_effect = -1.5, n_controls = 500, seed = 1) {
    set.seed(seed)
    genes <- sprintf("Gene_%04d", seq_len(n_genes))
    gene <- c(rep(genes, each = n_barcodes), rep("non-targeting", n_controls))
    n <- length(gene)
    barcode <- sprintf("bc_%05d", seq_len(n))
    abundance <- rlnorm(n, meanlog = 0, sdlog = 0.8)
    abundance <- abundance / mean(abundance)
    hits <- sample(genes, round(hit_fraction * n_genes))
    is_hit <- gene %in% hits
    efficiency <- rbeta(n, 4, 2)
    efficiency[runif(n) < prop_inactive] <- 0
    log2fc <- ifelse(is_hit, effect * efficiency, 0)
    # Seed (off-target) effects hit targeting and non-targeting barcodes alike
    off <- runif(n) < off_target
    log2fc[off] <- log2fc[off] + off_target_effect
    samples <- c(paste0("CTRL_", seq_len(n_reps)), paste0("TRT_", seq_len(n_reps)))
    depth <- reads_per_barcode * n * runif(length(samples), 0.7, 1.3)
    counts <- vapply(seq_along(samples), function(j) {
        treated <- startsWith(samples[j], "TRT")
        mu <- abundance * 2^(treated * log2fc)
        mu <- mu / sum(mu) * depth[j]
        rnbinom(n, mu = mu, size = 1 / dispersion)
    }, numeric(n))
    colnames(counts) <- samples
    list(
        counts = data.frame(Barcode = barcode, counts, check.names = FALSE),
        annotation = data.frame(Barcode = barcode, Gene = gene),
        truth = data.frame(Gene = genes, hit = genes %in% hits),
        samples = samples, n_reps = n_reps, off_target = off
    )
}
