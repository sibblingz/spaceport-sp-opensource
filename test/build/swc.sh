#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

function as3_referencing_face_swc {
  echo 'package { public class Main extends Face { } }' > "$INPUT/Main.as"
}

manifest="manifest/unoptimized-bundle/manifest.xml"
start_tests

############################################################
phase "Add single .as file"

mkdir "$INPUT/lib/"
template_as3
build

############################################################
phase "Adding .swc directory rebuilds nothing"

build --set:library_dir "$INPUT/lib/"
assert_all_untouched

############################################################
phase "Adding .swc file rebuilds JS, SGF and adds to manifest"

cp "$HERE/assets/face.swc" "$INPUT/lib/face.swc"

build --set:library_dir "$INPUT/lib/"
assert_updated "js/unoptimized/game.js"
assert_updated "$manifest"
assert_updated "library_sgf/face.sgf"
assert_updated "library_swf/face.swf"

if ! grep -q 'library_sgf/face\.sgf' "$OUTPUT/$manifest"; then
  fail_msg "library_sgf/face.sgf should be referenced in manifest.xml, but isn't"
  exit 9
fi

############################################################
phase "Referencing .swc class in AS3"

as3_referencing_face_swc

build --set:library_dir "$INPUT/lib/"
assert_updated "js/unoptimized/game.js"

if ! grep -q 'library_sgf/face\.swf' "$OUTPUT/js/unoptimized/game.js"; then
  fail_msg "library_sgf/face.swf should be referenced in game.js, but isn't"
  exit 9
fi

############################################################
phase "No change"

build --set:library_dir "$INPUT/lib/"
assert_all_untouched

############################################################
phase "Touching .swc rebuilds JS and SGF"

touch "$INPUT/lib/face.swc"

build --set:library_dir "$INPUT/lib/"
assert_updated "js/unoptimized/game.js"
assert_updated "library_sgf/face.sgf"
assert_updated "library_swf/face.swf"

############################################################
############################################################
phase "Invalid ZIP .swc causes build failure"
reset

template_as3
mkdir "$INPUT/lib/"
echo "Invalid ZIP file =D" > "$INPUT/lib/face.swc"

build_failing --set:library_dir "$INPUT/lib/"
assert_unbuilt "library_swf/face.swf"

############################################################
############################################################
phase "Missing library.swf in .swc causes build failure"
reset

template_as3
mkdir "$INPUT/lib/"
zip -q "$INPUT/lib/face.swc" "$HERE/assets/face.swf"

build_failing --set:library_dir "$INPUT/lib/"
assert_unbuilt "library_swf/face.swf"

############################################################
############################################################
phase "Adding single .swc builds JS, SGF, and manifest"
reset

template_as3
mkdir "$INPUT/lib/"
cp "$HERE/assets/face.swc" "$INPUT/lib/face.swc"

build --set:library_file "$INPUT/lib/face.swc"
assert_updated "js/unoptimized/game.js"
assert_updated "$manifest"
assert_updated "library_sgf/face.sgf"
assert_updated "library_swf/face.swf"

if ! grep -q 'library_sgf/face\.sgf' "$OUTPUT/$manifest"; then
  fail_msg "library_sgf/face.swf should be referenced in manifest.xml, but isn't"
  exit 9
fi

############################################################
phase "Referencing single .swc class in AS3"

as3_referencing_face_swc

build --set:library_file "$INPUT/lib/face.swc"
assert_updated "js/unoptimized/game.js"
assert_untouched "library_swf/face.swf"
assert_untouched "library_sgf/face.sgf"

if ! grep -q 'library_sgf/face\.swf' "$OUTPUT/js/unoptimized/game.js"; then
  fail_msg "library_sgf/face.swf should be referenced in game.js, but isn't"
  exit 9
fi

############################################################
phase "Removing .swc fails if AS3 references"

build_failing  # No .swc.

############################################################
phase "Removing .swc removes from manifest and JS"

template_as3

build  # No .swc.
assert_updated "js/unoptimized/game.js"
assert_updated "$manifest"

if grep -q 'library_sgf/face\.sgf' "$OUTPUT/$manifest"; then
  fail_msg "library_sgf/face.swf should not be referenced in manifest.xml, but is"
  exit 9
fi

if grep -q 'library_sgf/face\.swf' "$OUTPUT/js/unoptimized/game.js"; then
  fail_msg "library_sgf/face.swf should not be referenced in game.js, but is"
  exit 9
fi

############################################################
############################################################
phase "Conflicting .swc files fails"
reset

mkdir "$INPUT/lib1"
mkdir "$INPUT/lib2"
cp "$HERE/assets/face.swc" "$INPUT/lib1/face.swc"
cp "$HERE/assets/face.swc" "$INPUT/lib2/face.swc"

build_failing --set:library_dir "$INPUT/lib1/" --set:library_dir "$INPUT/lib2/"

############################################################
############################################################
phase "Duplicate .swc folders are treated as the same"
reset

as3_referencing_face_swc
mkdir "$INPUT/lib"
cp "$HERE/assets/face.swc" "$INPUT/lib/face.swc"

build \
  --set:library_dir "$INPUT/lib/" \
  --set:library_dir "$INPUT/lib/"

############################################################
############################################################
phase "Duplicate .swc files are treated as the same"
reset

as3_referencing_face_swc
mkdir "$INPUT/lib"
cp "$HERE/assets/face.swc" "$INPUT/lib/face.swc"

build \
  --set:library_file "$INPUT/lib/face.swc" \
  --set:library_file "$INPUT/lib/face.swc"

############################################################
############################################################
phase "Coinciding .swc file and folder are treated as the same"
reset

as3_referencing_face_swc
mkdir "$INPUT/lib"
cp "$HERE/assets/face.swc" "$INPUT/lib/face.swc"

build \
  --set:library_dir "$INPUT/lib/" \
  --set:library_file "$INPUT/lib/face.swc"

############################################################
############################################################
#phase "Chinese .swc file path"
#reset
#
#as3_referencing_face_swc
#mkdir "$INPUT/lib_中文/"
#cp "$HERE/assets/face.swc" "$INPUT/lib_中文/Русия.swc"
#
#build --set:library_file "$INPUT/lib_中文/Русия.swc"
#
#if grep -q 'library_sgf/Русия\.sgf' "$OUTPUT/$manifest"; then
#  fail_msg "library_sgf/Русия.sgf should not be referenced in manifest.xml, but is"
#  exit 9
#fi
#
#if grep -q 'library_sgf/Русия\.swf' "$OUTPUT/js/unoptimized/game.js"; then
#  fail_msg "library_sgf/Русия.swf should not be referenced in game.js, but is"
#  exit 9
#fi

############################################################
############################################################
phase "Big .swc which corrupted zip-archive"
reset

echo 'package { public class Main extends Mockup { } }' > "$INPUT/Main.as"

cp "$HERE/assets/mgm.swc" "$INPUT/mgm.swc"

build --set:library_file "$INPUT/mgm.swc"

############################################################
end_tests
