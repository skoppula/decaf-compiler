#!/usr/bin/env bash

./run.sh --target=scan tests/scanner/input/"$1" | diff tests/scanner/output/"$1".out -
