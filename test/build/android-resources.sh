#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

my_resources="$OUTPUT/my_res"
extract_tmp="$OUTPUT/extract-tmp"
android="android/unoptimized-bundle-spdebug-devsigned"

############################################################
phase "No resources specified creates defaults"

cat > "$INPUT/Main.as" <<EOF
package {
  import flash.display.Sprite;
  public class Main extends Sprite { }
}
EOF

build apk
assert_updated "$android/packaged.ap_"

mkdir "$extract_tmp"
unzip -q -d "$extract_tmp/" "$OUTPUT/$android/packaged.ap_"
if ! [ -f "$extract_tmp/res/drawable-hdpi/sp_icon.png" ] ; then
  fail_msg "Resources package should contain drawable-hdpi/sp_icon.png, but doesn't"
  exit 9
fi

rm -r "$extract_tmp"

############################################################
phase "Specifying resources copies correct assets"

cat > "$INPUT/settings" <<EOF
EOF

mkdir -p "$my_resources/drawable/"
cp "$HERE/assets/splash_1024x748.png" "$my_resources/drawable/sp_splash.png"
cp "$HERE/assets/icon_72x72.png" "$my_resources/drawable/sp_icon.png"

build apk --set:android_resources "$my_resources/"
assert_updated "$android/packaged.ap_"

mkdir "$extract_tmp"
unzip -q -d "$extract_tmp/" "$OUTPUT/$android/packaged.ap_"
assert_same "$extract_tmp/res/drawable/sp_splash.png" "$my_resources/drawable/sp_splash.png"
assert_same "$extract_tmp/res/drawable/sp_icon.png" "$my_resources/drawable/sp_icon.png"
rm -r "$extract_tmp"

############################################################
phase "Updating resource recopies assets"

cp "$HERE/assets/splash_640x960.png" "$my_resources/drawable/sp_splash.png"

build apk --set:android_resources "$my_resources/"
assert_updated "$android/packaged.ap_"

mkdir "$extract_tmp"
unzip -q -d "$extract_tmp/" "$OUTPUT/$android/packaged.ap_"
assert_same "$extract_tmp/res/drawable/sp_splash.png" "$my_resources/drawable/sp_splash.png"
assert_same "$extract_tmp/res/drawable/sp_icon.png" "$my_resources/drawable/sp_icon.png"
rm -r "$extract_tmp"

############################################################
phase "No changes"

build apk --set:android_resources "$my_resources/"
assert_untouched "$android/packaged.ap_"

############################################################
end_tests
