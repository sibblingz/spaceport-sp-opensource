#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

############################################################
phase "Add face.swf"

cp "$HERE/assets/face.swf" "$INPUT/face.swf"

template_as3

build
assert_updated "assets/face.sgf"

############################################################
phase "Corrupt face.swf"

echo "hello world; corruption!" > "$INPUT/face.swf"

build_failing

############################################################
phase "Fix face.swf"

cp "$HERE/assets/face.swf" "$INPUT/face.swf"

build
assert_updated "assets/face.sgf"

############################################################
end_tests
