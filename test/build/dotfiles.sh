#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

############################################################
phase "Add single .as file"

template_as3

build
assert_updated "manifest/unoptimized-bundle/manifest.xml"

############################################################
phase "Add .DS_Store"

touch "$INPUT/.DS_Store"

build
# We aren't smart enough to detect that no interesting (.as)
# files were added or removed.  We only know that a
# directory was changed, and that causes a rebuild of
# everything related to JS.
#assert_all_untouched

if [ -e "$OUTPUT/assets/.DS_Store" ]; then
  fail_msg ".DS_Store should not have been copied"
  exit 9
fi

############################################################
phase "Add .svn folder"

mkdir "$INPUT/.svn"
touch "$INPUT/.svn/bullshit.swf"

build
#assert_all_untouched

if [ -e "$OUTPUT/assets/.svn" ]; then
  fail_msg ".svn should not have been copied"
  exit 9
fi

############################################################
end_tests
