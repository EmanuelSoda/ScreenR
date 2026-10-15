#!/usr/bin/env bash
# Install MAGeCK (0.5.9.5, from the Bitbucket repository) in a Python
# virtual environment. Usage: bash install_mageck.sh <directory>
set -euo pipefail
dir=${1:-mageck}
mkdir -p "$dir"
git clone --depth 1 https://bitbucket.org/liulab/mageck.git "$dir/src"
python3 -m venv "$dir/env"
"$dir/env/bin/pip" install numpy scipy
(cd "$dir/src" && "../env/bin/pip" install .)
"$dir/env/bin/mageck" -v
