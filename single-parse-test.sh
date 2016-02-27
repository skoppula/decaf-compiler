#!/usr/bin/env bash
IN="$1"
arrIN=(${IN//-/ })
./run.sh --target=parse tests/parser/"${arrIN[0]}"/"$1"
