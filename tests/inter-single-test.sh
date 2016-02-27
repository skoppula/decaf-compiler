#!/usr/bin/env bash
IN="$1"
arrIN=(${IN//-/ })
$(git rev-parse --show-toplevel)/run.sh --target=inter tests/semantics/"${arrIN[1]}"/"$1".dcf
