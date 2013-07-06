#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

start_tests

ios="ios/unoptimized-bundle-spdebug-devsigned"
assets="$HERE/assets"
app="$OUTPUT/$ios/built.app"

############################################################
phase "Add single .as file"

template_as3

build

############################################################
phase "Build IPA"

build ipa

############################################################
phase "Icons and splash screens are copied into app bundle"

mkdir "$INPUT/resources/"
cp "$assets/icon_114x114.png" "$INPUT/resources/Icon@2x.png"
cp "$assets/icon_72x72.png" "$INPUT/resources/Icon~ipad.png"
cp "$assets/splash_1024x748.png" "$INPUT/resources/Default-Landscape~ipad.png"
cp "$assets/splash_320x480.png" "$INPUT/resources/Default.png"

build ipa --set:ios_resources resources/
assert_updated "$ios/built.app/Info.plist"

assert_updated "$ios/built.app/Icon@2x.png"
assert_updated "$ios/built.app/Icon~ipad.png"

assert_updated "$ios/built.app/Default.png"
assert_updated "$ios/built.app/Default-Landscape~ipad.png"

assert_same "$assets/icon_114x114.png" "$app/Icon@2x.png"
assert_same "$assets/icon_72x72.png" "$app/Icon~ipad.png"

assert_same "$assets/splash_320x480.png" "$app/Default.png"
assert_same "$assets/splash_1024x748.png" "$app/Default-Landscape~ipad.png"

############################################################
phase "Adding splash screens updates app bundle"

cp "$assets/splash_640x960.png" "$INPUT/resources/Default@2x.png"
cp "$assets/splash_1536x2008.png" "$INPUT/resources/Default@2x~ipad.png"

build ipa --set:ios_resources resources/
assert_updated "$ios/built.app/Info.plist"

assert_updated "$ios/built.app/Default@2x.png"
assert_updated "$ios/built.app/Default@2x~ipad.png"

assert_same "$assets/splash_640x960.png" "$app/Default@2x.png"
assert_same "$assets/splash_1536x2008.png" "$app/Default@2x~ipad.png"

############################################################
phase "No change"

build ipa --set:ios_resources resources/
assert_untouched "$ios/built.app/Info.plist"
assert_untouched "$ios/built.app/Default.png"

############################################################
phase "Replacing splash screen file updates app bundle"

cp "$assets/splash_640x960_2.png" "$INPUT/resources/Default@2x.png"

build ipa --set:ios_resources resources/
assert_updated "$ios/built.app/Default.png"

assert_same "$assets/splash_640x960_2.png" "$app/Default@2x.png"

############################################################
end_tests
