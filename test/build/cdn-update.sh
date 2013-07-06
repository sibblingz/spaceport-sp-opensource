#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

############################################################
phase "Add single .as file"

template_as3

build cdn --set:authorization_key 1234 --set:id dummy.packagename
assert_updated "manifest/unoptimized-cdn/manifest.xml"
assert_updated "game-zip/unoptimized-cdn/game.zip"

if ! grep -q 'cdn=' "$OUTPUT/manifest/unoptimized-cdn/manifest.xml"; then
  fail_msg "cdn attribute should have been specified in the CDN manifest.xml"
  exit 9
fi

extract_tmp="$OUTPUT/extract-tmp/"
mkdir "$extract_tmp"
unzip -q -d "$extract_tmp/" "$OUTPUT/game-zip/unoptimized-cdn/game.zip"
diff -u \
  "$extract_tmp/game/manifest.xml" \
  "$OUTPUT/manifest/unoptimized-cdn/manifest.xml"

if ! [ -e "$extract_tmp/game/game.js" ]; then
  fail_msg "game/game.js should be in update.zip, but doesn't exist"
  exit 5
fi

############################################################
end_tests
