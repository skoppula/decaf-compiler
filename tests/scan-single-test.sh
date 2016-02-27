#!/usr/bin/env bash

$(git rev-parse --show-toplevel)/run.sh --target=scan tests/scanner/input/"$1" | diff tests/scanner/output/"$1".out -
