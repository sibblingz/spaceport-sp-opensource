#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

############################################################
phase "Add single .as file"

template_as3

build

############################################################
phase "Add .png, .json, .txt files"

EXTENSIONS="png json txt"
for ext in $EXTENSIONS; do
  mkdir "$INPUT/${ext}_assets"
  touch "$INPUT/${ext}_assets/my_asset.$ext"
done

build

for ext in $EXTENSIONS; do
  assert_updated "assets/${ext}_assets/my_asset.$ext"
done

############################################################
end_tests
