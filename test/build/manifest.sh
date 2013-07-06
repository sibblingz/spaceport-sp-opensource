#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

manifest="manifest/unoptimized-bundle/manifest.xml"
start_tests

############################################################
phase "Add single .as file"

template_as3

build
assert_updated "$manifest"

if [ "$(grep -c "spaceport.js" "$OUTPUT/$manifest")" -ne 1 ]; then
  fail_msg "Expected spaceport.js to appear in manifest.xml once"
  exit 9
fi

if ! grep -q "io.spaceport.plugins.contacts" "$OUTPUT/$manifest"; then
  fail_msg "Expected io.spaceport.plugins.contacts to appear in manifest.xml"
  exit 9
fi

############################################################
phase "Set description"

build --set:app_description "This is a description"
assert_updated "$manifest"

if ! grep -q "This is a description" "$OUTPUT/$manifest"; then
  fail_msg "Expected 'This is a description' to appear in manifest.xml"
  exit 9
fi

############################################################
phase "Change description"

build --set:app_description "This is a new description"
assert_updated "$manifest"

if ! grep -q "This is a new description" "$OUTPUT/$manifest"; then
  fail_msg "Expected 'This is a new description' to appear in manifest.xml"
  exit 9
fi

############################################################
end_tests
