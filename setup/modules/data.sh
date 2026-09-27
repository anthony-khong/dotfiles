#!/usr/bin/env bash
# A global `ipython` with the numeric and ML stack, for scratch work and vim-slime.
# Projects keep their own virtualenvs. To change the list later, edit it and re-run
# with `uv tool install --force ...`.
set -euo pipefail
# shellcheck source=../lib.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/lib.sh"

have uv || die "uv comes from mise: run the mise step first"
# Python 3.13: widest wheel coverage for this stack
uv tool install --python 3.13 ipython \
  --with numpy --with scipy --with pandas --with pyarrow --with fastparquet \
  --with matplotlib --with scikit-learn --with scikit-image \
  --with "dask[complete]" --with jax --with lightgbm --with xgboost \
  --with click --with cytoolz
