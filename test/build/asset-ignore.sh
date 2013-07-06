#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

function test_file_extension {
  lower_extension="$1"
  upper_extension="$(echo "$1" | tr a-z A-Z)"

  phase "Add .$1 file should ignore"

  touch "$INPUT/test_file_lower.$lower_extension"
  touch "$INPUT/test_file_upper.$upper_extension"

  build
  assert_unbuilt "assets/test_file_lower.$lower_extension"
  assert_unbuilt "assets/test_file_upper.$upper_extension"

  rm "$INPUT/test_file_lower.$lower_extension"
  rm "$INPUT/test_file_upper.$upper_extension"

  phase "Add .$1 folder should copy"

  mkdir "$INPUT/test_folder_lower.$lower_extension"
  mkdir "$INPUT/test_folder_upper.$upper_extension"
  touch "$INPUT/test_folder_lower.$lower_extension/foo.png"
  touch "$INPUT/test_folder_upper.$upper_extension/bar.png"

  build
  assert_updated "assets/test_folder_lower.$lower_extension/foo.png"
  assert_updated "assets/test_folder_upper.$upper_extension/bar.png"

  rm "$INPUT/test_folder_lower.$lower_extension/foo.png"
  rm "$INPUT/test_folder_upper.$upper_extension/bar.png"
  rmdir "$INPUT/test_folder_lower.$lower_extension"
  rmdir "$INPUT/test_folder_upper.$upper_extension"
}

function test_folder_extension {
  lower_extension="$1"
  upper_extension="$(echo "$1" | tr a-z A-Z)"

  phase "Add .$1 file should copy"

  touch "$INPUT/test_file_lower.$lower_extension"
  touch "$INPUT/test_file_upper.$upper_extension"

  build
  assert_updated "assets/test_file_lower.$lower_extension"
  assert_updated "assets/test_file_upper.$upper_extension"

  rm "$INPUT/test_file_lower.$lower_extension"
  rm "$INPUT/test_file_upper.$upper_extension"

  phase "Add .$1 folder should ignore"

  mkdir "$INPUT/test_folder_lower.$lower_extension"
  mkdir "$INPUT/test_folder_upper.$upper_extension"
  touch "$INPUT/test_folder_lower.$lower_extension/foo.png"
  touch "$INPUT/test_folder_upper.$upper_extension/bar.png"

  build
  assert_unbuilt "assets/test_folder_lower.$lower_extension"
  assert_unbuilt "assets/test_folder_upper.$upper_extension"
  assert_unbuilt "assets/test_folder_lower.$lower_extension/foo.png"
  assert_unbuilt "assets/test_folder_upper.$upper_extension/bar.png"

  rm "$INPUT/test_folder_lower.$lower_extension/foo.png"
  rm "$INPUT/test_folder_upper.$upper_extension/bar.png"
  rmdir "$INPUT/test_folder_lower.$lower_extension"
  rmdir "$INPUT/test_folder_upper.$upper_extension"
}

start_tests

############################################################
phase "Add single .as file"

template_as3

build

############################################################

for ext in ai psd fla exe htm html; do
  test_file_extension "$ext"
done

for ext in app; do
  test_folder_extension "$ext"
done

############################################################
end_tests
