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
phase "Add source directory"

mkdir "$INPUT/lib"
build --set:source_path "$INPUT/lib"
# See [note Asset rebuild on new source directory].
#assert_untouched "assets/face.sgf"

############################################################
phase "Add face2.swf"

cp "$HERE/assets/face.swf" "$INPUT/face2.swf"

build --set:source_path "$INPUT/lib"
assert_untouched "assets/face.sgf"
assert_updated "assets/face2.sgf"

############################################################
phase "Add lib/face3.swf"

cp "$HERE/assets/face.swf" "$INPUT/lib/face3.swf"

build --set:source_path "$INPUT/lib"
assert_untouched "assets/face.sgf"
assert_untouched "assets/face2.sgf"
assert_updated "assets/lib/face3.sgf"

############################################################
end_tests
