# Check a CRISPRcleanR Library Annotation

Checks that the library annotation has the columns needed by
CRISPRcleanR and that its row names match the barcodes.

## Usage

``` r
check_library_annotation(library_annotation, barcodes)
```

## Arguments

- library_annotation:

  The library annotation.

- barcodes:

  The barcodes of the count table.

## Value

Invisibly TRUE; stops with an informative error otherwise.
