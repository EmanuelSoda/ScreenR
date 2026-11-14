# Real DepMap 24Q2 Public K-562 example

Attribution: Broad DepMap, Broad Institute of Harvard and MIT (2024),
**DepMap 24Q2 Public**, https://doi.org/10.25452/figshare.plus.25880521,
Figshare dataset 25880521. License: Creative Commons Attribution 4.0
International (CC BY 4.0), https://creativecommons.org/licenses/by/4.0/ .
The upstream archive's license was checked on 2026-11-14 at
https://figshare.com/articles/dataset/DepMap_24Q2_Public/25880521 .
These data retain their upstream license, rather than ScreenR's code license.

Derived by ScreenR's vignette author on 2026-11-14 from the implementer's
package-managed cached release files; no new full-matrix download was made.
Each gzip contains the original CSV header and the single original row whose
first field is ACH-000551 (CellLineName K-562; StrippedCellLineName K562).
All original gene columns, missing values, numeric text and metadata fields
are preserved; no measurements were imputed and no dependency calls invented.
Changes from upstream: other models removed, gzip compression applied with
zero timestamp. Approximately 533 KB compressed in total.

`provenance.tsv` records original file IDs and verified original MD5 hashes
separately from the hashes of these derived gzip files. Original file metadata
and downloads: https://api.figshare.com/v2/articles/25880521 .
The cached upstream MD5 hashes were checked before extraction. The derived
measurements and reference vectors were also checked against the reviewer's
independently parsed full-matrix K562 result.

To reproduce: obtain the three original named files through the documented
`depmap::DepMapDataset(25880521)`, `DepMapFiles()` and `dmget()` cache route;
verify each original file against `source_md5`; retain the header and the
single CSV row with first field ACH-000551; gzip that text. Gzip container
hashes can differ with compression versions; numeric measurements must agree.
See `vignettes/DepMap_reference.Rmd` for the runnable offline example.
