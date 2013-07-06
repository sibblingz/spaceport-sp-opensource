#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

extract_tmp="$OUTPUT/extract-tmp"
android="android/unoptimized-bundle-spdebug-devsigned"

############################################################
phase "Add single .as file"

template_as3

build apk
assert_updated "$android/AndroidManifest.xml"
assert_updated "$android/built.apk"
assert_updated "$android/assets/game.js"
assert_updated "$android/assets/index.html"
assert_updated "$android/assets/manifest.xml"
assert_updated "$android/assets/spaceport/spaceport.js"
assert_updated "$android/assets/spaceport/unrequire.js"

jarsigner -verify "$OUTPUT/$android/built.apk" \
  > "$OUTPUT/jarsigner.stdout" \
  2> "$OUTPUT/jarsigner.stderr"
if ! grep -q "verified" "$OUTPUT/jarsigner.stdout" ; then
  echo "Built APK should be signed, but isn't:"
  cat "$OUTPUT/jarsigner.stdout" "$OUTPUT/jarsigner.stderr"
  exit 9
fi

if ! grep -q "android:screenOrientation=\"fullSensor\"" \
  "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Default screen orientation should be \"fullSensor\" in manifest, but isn't"
  exit 9
fi

if ! grep -q "android\.permission\.READ_CONTACTS" \
  "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "READ_CONTACTS permission should be in manifest, but isn't"
  exit 9
fi

mkdir "$extract_tmp"
unzip -q -d "$extract_tmp/" "$OUTPUT/$android/built.apk"

if [ ! -e "$extract_tmp/lib/armeabi/libspaceport.so" ]; then
  fail_msg "APK should contain libspaceport.so, but doesn't"
  exit 9
fi

if [ ! -e "$extract_tmp/lib/armeabi/libopenal.so" ]; then
  fail_msg "APK should contain libopenal.so, but doesn't"
  exit 9
fi

rm -r "$extract_tmp"

############################################################
phase "No changes"

build apk
assert_all_untouched

############################################################
phase "Change in ActionScript file should rebuild"

cat > "$INPUT/Main.as" <<EOF
package {
  import flash.display.Sprite;
  public class Main extends Sprite {
    public function dummy():void {
      trace("dummy");
    }
  }
}
EOF

build apk
assert_updated "js/unoptimized/game.js"
assert_untouched "$android/AndroidManifest.xml"
assert_updated "$android/assets/game.js"
assert_updated "$android/built.apk"

############################################################
phase "Adding new asset should rebuild"

touch "$INPUT/asset.png"

build apk
assert_updated "assets/asset.png"
assert_untouched "js/unoptimized/game.js"
assert_untouched "assets/index.html"
assert_updated "$android/built.apk"

############################################################
phase "Setting version should rebuild & update manifest"

build apk --set:version 2.0.0
assert_updated "$android/built.apk"
assert_updated "$android/AndroidManifest.xml"

if ! grep -q "android:versionName=\"2.0.0\"" \
  "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Version name should be updated in manifest, but isn't"
  exit 9
fi

############################################################
phase "Changing version should rebuild & update manifest"

build apk --set:version 2.0.1
assert_updated "$android/built.apk"
assert_updated "$android/AndroidManifest.xml"

if ! grep -q "android:versionName=\"2.0.1\"" \
  "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Version name should be updated in manifest, but isn't"
  exit 9
fi

############################################################
phase "Setting version code should rebuild & update manifest"

build apk --set:version_code 200
assert_updated "$android/built.apk"
assert_updated "$android/AndroidManifest.xml"

if ! grep -q "android:versionCode=\"200\"" \
  "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Version code should be updated in manifest, but isn't"
  exit 9
fi

############################################################
phase "Changing version code should rebuild & update manifest"

build apk --set:version_code 201
assert_updated "$android/built.apk"
assert_updated "$android/AndroidManifest.xml"

if ! grep -q "android:versionCode=\"201\"" \
  "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Version code should be updated in manifest, but isn't"
  exit 9
fi

############################################################
phase "Changing orientation from default should rebuild & update manifest"

build apk --set:orientations portrait
assert_updated "$android/built.apk"
assert_updated "$android/AndroidManifest.xml"

if ! grep -q "android:screenOrientation=\"portrait\"" \
  "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Screen orientation should be updated in manifest, but isn't"
  exit 9
fi

############################################################
phase "Changing orientation should rebuild & update manifest"

build apk --set:orientations landscape
assert_updated "$android/built.apk"
assert_updated "$android/AndroidManifest.xml"

if ! grep -q "android:screenOrientation=\"landscape\"" \
  "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Screen orientation should be updated in manifest, but isn't"
  exit 9
fi

############################################################
phase "Changing orientation to multiple should rebuild & update manifest"

build apk --set:orientations 'portrait landscape'
assert_updated "$android/built.apk"
assert_updated "$android/AndroidManifest.xml"

if ! grep -q "android:screenOrientation=\"fullSensor\"" \
  "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Screen orientation should be updated in manifest, but isn't"
  exit 9
fi

############################################################
phase "Changing application ID should rebuild & update manifest"

build apk --set:id spaceportshake.tests.android
assert_updated "$android/AndroidManifest.xml"
assert_updated "$android/classes.dex"
assert_updated "$android/built.apk"

if ! grep -q 'spaceportshake\.tests\.android' "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Manifest should contain application ID spaceportshake.tests.android, but doesn't"
  exit 9
fi

if ! grep -q 'spaceportshake/tests/android/MyGame' "$OUTPUT/$android/classes.dex"; then
  fail_msg "classes.dex should contain spaceportshake/tests/android/MyGame, but doesn't"
  exit 9
fi

############################################################
phase "Changing application ID again should rebuild & update manifest"

build apk --set:id spaceportshake.tests.android2
assert_updated "$android/AndroidManifest.xml"
assert_updated "$android/classes.dex"
assert_updated "$android/built.apk"

if ! grep -q 'spaceportshake\.tests\.android2' "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Manifest should contain application ID spaceportshake.tests.android2, but doesn't"
  exit 9
fi

if ! grep -q 'spaceportshake/tests/android2/MyGame' "$OUTPUT/$android/classes.dex"; then
  fail_msg "classes.dex should contain spaceportshake/tests/android2/MyGame, but doesn't"
  exit 9
fi

############################################################
phase "Adding custom URL scheme should rebuild & update manifest"

build apk --set:url_schemes 'mycustomscheme'
assert_updated "$android/AndroidManifest.xml"

if ! grep -q 'mycustomscheme' "$OUTPUT/$android/AndroidManifest.xml"; then
  fail_msg "Manifest should contain custom URL scheme mygame, but doesn't"
  exit 9
fi

############################################################
phase "Display name should accept apostrophe"

build apk --set:display_name "Liran's Lab"
assert_updated "$android/built.apk"

mkdir "$extract_tmp"
unzip -q -d "$extract_tmp/" "$OUTPUT/$android/built.apk"

if ! grep -q "L.i.r.a.n.'.s. .L.a.b" "$extract_tmp/AndroidManifest.xml"; then
  fail_msg "APK should contain display name Liran's Lab, but doesn't"
  exit 9
fi

rm -r "$extract_tmp"

############################################################
end_tests
