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
phase "Adding SGF file in output directory does not change manifest"

touch "$OUTPUT/assets/crud.sgf"

build
assert_untouched "manifest/unoptimized-bundle/manifest.xml"

############################################################
phase "Adding JS file in output directory does not change manifest"

mkdir "$OUTPUT/assets/_spaceport"
touch "$OUTPUT/assets/_spaceport/lol.js"
touch "$OUTPUT/assets/lol.js"

build
assert_untouched "manifest/unoptimized-bundle/manifest.xml"

############################################################
phase "Unbuilt file in output directory does not bundle in .app"

build ipa
assert_untouched "manifest/unoptimized-bundle/manifest.xml"
assert_unbuilt "ios/built.app/assets/crud.sgf"

############################################################
end_tests
