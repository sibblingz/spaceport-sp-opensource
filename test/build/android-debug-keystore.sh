#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

############################################################
phase "Add single .as file"

template_as3
build apk
assert_updated "android/debug.keystore"

############################################################
phase "No change"

build apk
assert_untouched "android/debug.keystore"

############################################################
phase "Changing code does not update keystore"

echo >> "$INPUT/Main.as"
build apk
assert_untouched "android/debug.keystore"

############################################################
phase "Changing keystore causes it to be rebuilt"

touch "$OUTPUT/android/debug.keystore"
build apk
assert_updated "android/debug.keystore"

############################################################
end_tests
