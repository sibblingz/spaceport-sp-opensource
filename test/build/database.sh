#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

############################################################
phase "Add single AS3 file"

template_as3

build
assert_updated "build-cache.database"

############################################################
phase "Add an asset"

touch "$INPUT/asset.png"

build
assert_updated "build-cache.database"

############################################################
phase "No change"

build
assert_updated "build-cache.database"

############################################################
end_tests
