#!/usr/bin/env bash
IN="$1"
arrIN=(${IN//-/ })
$(git rev-parse --show-toplevel)/run.sh --target=parse tests/parser/"${arrIN[0]}"/"$1"
