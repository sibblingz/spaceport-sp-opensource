#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

if ! which codesign > /dev/null ; then
  echo "Codesign is not available; skipping codesign test" >&2
  exit 0
fi

function codesign_verify {
  debug "Verifying bundle $1 is properly signed..."
  codesign -v "$1"
}

ios="ios/unoptimized-bundle-spdebug-devsigned"
start_tests

############################################################
phase "Add single .as file"

template_as3

build ipa
assert_updated "$ios/built.app"
assert_updated "$ios/built.app/assets/manifest.xml"
assert_updated "$ios/built.app/assets/index.html"
assert_updated "$ios/built.app/assets/game.js"
assert_updated "$ios/built.ipa"

codesign_verify "$OUTPUT/$ios/built.app"

############################################################
end_tests
