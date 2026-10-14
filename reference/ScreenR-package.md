# Tools for analyzing shRNAs screening data

**ScreenR** is an easy and effective package to perform hits
identification in loss of function High Throughput Biological Screening
performed with shRNAs library. ScreenR combines the power of software
like edgeR with the simplicity of the Tidyverse metapackage. ScreenR
executes a pipeline able to find candidate hits from barcode counts data
and integrates a wide range of visualization for each step of the
analysis.

## Details

**ScreenR** takes the a count table as input and create the
screenr_object to perform the analysis. Throught the pipeline
**ScreenR** enable the user to perform quality control, visual
inspection, dimensionality reduction of the data. Using three
statistical methods:

- [ROAST](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2922896)

- [CAMERA](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3458527/)

- [Z-score](https://pubmed.ncbi.nlm.nih.gov/21515799/)

it is able to find new candidate hits. Moreover in order to improve the
quality of the hit found it is also possible to further filter the list
of hit using other filter like the variance and the slope filters.

## Author

Emanuel Michele Soda <emanuelsoda@gmail.com>
