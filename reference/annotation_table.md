# Table for the annotation of Barcode

Table for the annotation of Barcode

## Usage

``` r
data(annotation_table)
```

## Format

A data frame with 5320 rows and 2 columns obtained from a
loss-of-function genetic screening. This table is used to store
information about the shRNAs:

- Gene:

  It Contains the gene name

- Barcode:

  It contains an ID that identify each barcode (it is an unique
  identifier for an shRNA). I t can be use to merge the annotation table
  with t he count table

- Gene_ID:

  It Contains a unique Gene ID

- Sequence:

  It contains the cDNA sequence of the shRNA associated to the barcode

- Library:

  It contains the library from which the shRNA come from. In this case
  is a pooled from <https://cellecta.com/>cellecta
