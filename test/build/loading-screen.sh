#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

extract_tmp="$OUTPUT/extract-tmp"
android="android/unoptimized-bundle-spdebug-devsigned"
ios="ios/unoptimized-bundle-spdebug-devsigned"

############################################################
phase "Loading screen is converted into bundle"

cat > "$INPUT/Main.as" <<EOF
package {
  import flash.display.Sprite;
  public class Main extends Sprite { }
}
EOF

mkdir "$INPUT/resources/"
cp "$HERE/assets/loadingscreen.swf" "$INPUT/resources/loadingscreen.swf"

build ipa apk --set:loading_screen_file resources/loadingscreen.swf
assert_updated "loading_screen/loading_screen.sgf"
assert_updated "$ios/built.app/splashscreen.sgf"
assert_updated "$android/res/raw/splashscreen.sgf"

assert_same \
  "$OUTPUT/loading_screen/loading_screen.sgf" \
  "$OUTPUT/$ios/built.app/splashscreen.sgf"

assert_same \
  "$OUTPUT/loading_screen/loading_screen.sgf" \
  "$OUTPUT/$android/res/raw/splashscreen.sgf"

mkdir "$extract_tmp"
unzip -q -d "$extract_tmp/" "$OUTPUT/$android/built.apk"
assert_same "$extract_tmp/res/raw/splashscreen.sgf" "$OUTPUT/loading_screen/loading_screen.sgf"
rm -r "$extract_tmp"

# Save the .sgf so we can ensure it changes later.
cp "$OUTPUT/loading_screen/loading_screen.sgf" "$OUTPUT/old_loading_screen.sgf"

############################################################
phase "Updating loading screen updates bundle"

cp "$HERE/assets/loadingscreen2.swf" "$INPUT/resources/loadingscreen.swf"

build ipa apk --set:loading_screen_file resources/loadingscreen.swf
assert_updated "loading_screen/loading_screen.sgf"
assert_updated "$ios/built.app/splashscreen.sgf"
assert_updated "$android/res/raw/splashscreen.sgf"

assert_different \
  "$OUTPUT/$ios/built.app/splashscreen.sgf" \
  "$OUTPUT/old_loading_screen.sgf"

assert_same \
  "$OUTPUT/loading_screen/loading_screen.sgf" \
  "$OUTPUT/$ios/built.app/splashscreen.sgf"

assert_same \
  "$OUTPUT/loading_screen/loading_screen.sgf" \
  "$OUTPUT/$android/res/raw/splashscreen.sgf"

mkdir "$extract_tmp"
unzip -q -d "$extract_tmp/" "$OUTPUT/$android/built.apk"
assert_same "$extract_tmp/res/raw/splashscreen.sgf" "$OUTPUT/loading_screen/loading_screen.sgf"
rm -r "$extract_tmp"

############################################################
end_tests
