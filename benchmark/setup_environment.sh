#!/usr/bin/env bash
# Recreate locally the environment used to develop ScreenR and run the
# benchmark (R packages, CRISPRcleanR, gscreend, BiocCheck, pkgdown, MAGeCK
# and BAGEL2).
#
# Requirements: R >= 4.3 with a compiler toolchain (Rtools on Windows, Xcode
# command line tools on macOS), git and Python >= 3.9.
#
# Usage (from the repository root):
#   bash benchmark/setup_environment.sh [tools directory, default ../screenr_tools]
set -euo pipefail
tools=${1:-../screenr_tools}
mkdir -p "$tools"
tools=$(cd "$tools" && pwd)

echo "== R packages"
Rscript -e '
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
# ScreenR dependencies, development and check tools
BiocManager::install(c("remotes", "devtools", "roxygen2", "testthat",
    "rmarkdown", "knitr", "BiocStyle", "pkgdown", "BiocCheck", "covr",
    "styler", "callr"), update = FALSE, ask = FALSE)
remotes::install_deps(".", dependencies = TRUE, upgrade = "never")
# Optional dependency of correct_crispr_bias() (not on CRAN/Bioconductor)
BiocManager::install("francescojm/CRISPRcleanR", update = FALSE, ask = FALSE)
# Competitor used in the benchmark (deprecated in Bioconductor 3.19)
BiocManager::install("imkeller/gscreend", update = FALSE, ask = FALSE)
# ScreenR itself from this checkout
remotes::install_local(".", upgrade = "never", force = TRUE)
'

echo "== MAGeCK 0.5.9.5 in $tools/mageck"
if [ ! -d "$tools/mageck/src" ]; then
    git clone --depth 1 https://bitbucket.org/liulab/mageck.git "$tools/mageck/src"
fi
python3 -m venv "$tools/mageck/env"
"$tools/mageck/env/bin/pip" install numpy scipy
(cd "$tools/mageck/src" && "$tools/mageck/env/bin/pip" install .)

echo "== BAGEL2 in $tools/bagel"
if [ ! -d "$tools/bagel/src" ]; then
    git clone --depth 1 https://github.com/hart-lab/bagel.git "$tools/bagel/src"
fi
python3 -m venv "$tools/bagel/env"
# BAGEL2 uses numpy.in1d, removed in recent NumPy versions
"$tools/bagel/env/bin/pip" install "numpy<2.3" scipy pandas scikit-learn click

cat <<EOF

Done. Paths for the benchmark scripts (see benchmark/README.md):
  MAGeCK:          $tools/mageck/env/bin/mageck
  BAGEL.py:        $tools/bagel/src/BAGEL.py
  python (BAGEL2): $tools/bagel/env/bin/python
  BAGEL2 data:     $tools/bagel/src (CEGv2.txt, NEGv1.txt, HAP1 example)
EOF
