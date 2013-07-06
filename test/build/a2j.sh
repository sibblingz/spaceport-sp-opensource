#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

############################################################
phase "Add single .as file"

template_as3

build
assert_updated "assets/index.html"
assert_updated "assets/spaceport/spaceport.js"
assert_updated "assets/spaceport/unrequire.js"
assert_updated "js/unoptimized/game.js"
assert_updated "manifest/unoptimized-bundle/manifest.xml"

if ! grep -q 'game\.js' "$OUTPUT/assets/index.html"; then
  fail_msg "game.js should be referenced in index.html; but isn't"
  exit 9
fi

############################################################
phase "No changes"

build
assert_all_untouched

############################################################
phase "Changing unrelated file does not rebuild AS3"

touch "$INPUT/foo.png"

build
assert_untouched "js/unoptimized/game.js"

############################################################
phase "Touch .as file"

# FIXME Maybe this shouldn't happen?
touch "$INPUT/Main.as"
build
assert_updated "js/unoptimized/game.js"
assert_updated "manifest/unoptimized-bundle/manifest.xml"
assert_untouched "assets/index.html"

############################################################
phase "Introduce syntax error"

cat > "$INPUT/Main.as" <<EOF
package {
EOF

build_failing
assert_all_untouched

############################################################
phase "Rebuild with syntax error"

build_failing
assert_all_untouched

############################################################
phase "Correct syntax error"

template_as3

build
assert_updated "js/unoptimized/game.js"

############################################################
phase "No changes"

build
assert_all_untouched

############################################################
phase "Adding .png does not rebuild .as"

touch "$INPUT/asset.png"

build
assert_updated "assets/asset.png"
assert_untouched "js/unoptimized/game.js"

############################################################
phase "Adding .as file rebuilds"

# Maybe it shouldn't rebuild; whatever.
cp "$INPUT/Main.as" "$INPUT/MyMain.as"

build
assert_updated "js/unoptimized/game.js"

############################################################
phase "Changing main file path updates index"

build --set:entry_point "MyMain.as"
assert_updated "js/unoptimized/game.js"
assert_untouched "assets/index.html"
assert_updated "manifest/unoptimized-bundle/manifest.xml"

############################################################
end_tests
