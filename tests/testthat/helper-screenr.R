# Builds a ScreenR object whose normalized counts are exactly the Frequency
# column of a data table (one row per barcode and sample, with the columns
# Barcode, Gene, Sample, Day, Treatment and Frequency): the counts are the
# frequencies and every effective library size is 1e6.
object_from_frequencies <- function(data_table) {
    samples <- levels(factor(data_table$Sample))
    barcodes <- unique(as.character(data_table$Barcode))
    counts <- matrix(0, nrow = length(barcodes), ncol = length(samples),
        dimnames = list(barcodes, samples))
    counts[cbind(match(as.character(data_table$Barcode), barcodes),
        match(as.character(data_table$Sample), samples))] <-
        data_table$Frequency
    count_table <- data.frame(Barcode = barcodes, counts, check.names = FALSE)
    annotation <- unique(data.frame(Barcode = as.character(data_table$Barcode),
        Gene = data_table$Gene))
    sample_table <- unique(data.frame(Sample = as.character(data_table$Sample),
        Day = data_table$Day, Treatment = data_table$Treatment))
    object <- suppressMessages(create_screenr_object(table = count_table,
        annotation = annotation, sample_table = sample_table))
    set_normalized_count_table(object, count_table)
}
