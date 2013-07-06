#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

############################################################
phase "Add fireworks.mp3"

cp "$HERE/assets/fireworks.mp3" "$INPUT/fireworks.mp3"

template_as3

build
assert_updated "assets/fireworks.ogg"

############################################################
phase "Corrupt fireworks.mp3"

echo "hello world; corruption!" > "$INPUT/fireworks.mp3"

build_failing

############################################################
phase "Fix fireworks.mp3"

cp "$HERE/assets/fireworks.mp3" "$INPUT/fireworks.mp3"

build
assert_updated "assets/fireworks.ogg"

############################################################
end_tests
