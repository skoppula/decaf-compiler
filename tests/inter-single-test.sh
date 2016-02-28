#!/usr/bin/env bash
IN="$1"
arrIN=(${IN//-/ })
$(git rev-parse --show-toplevel)/run.sh --target=inter tests/semantics/"${arrIN[0]}"/"$1".dcf
