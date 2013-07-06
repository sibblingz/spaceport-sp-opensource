#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

ios="ios/unoptimized-bundle-spdebug-devsigned"
ios_cdn="ios/unoptimized-cdn-spdebug-devsigned"
start_tests

############################################################
phase "Add single .as file"

template_as3

build

############################################################
phase "Build IPA"

build ipa
assert_updated "$ios/built.app"
assert_updated "$ios/built.app/assets/manifest.xml"
assert_updated "$ios/built.app/assets/index.html"
assert_updated "$ios/built.app/assets/game.js"
assert_updated "$ios/built.app/Info.plist"
assert_updated "$ios/built.ipa"

assert_same \
  "$OUTPUT/manifest/unoptimized-bundle/manifest.xml" \
  "$OUTPUT/$ios/built.app/assets/manifest.xml"

############################################################
phase "Setting application ID updates Info.plist"

build ipa --set:id spaceportshake.tests.ios
plist_file="$OUTPUT/$ios/built.app/Info.plist"
assert_updated "$ios/built.app/Info.plist"
assert_updated "$ios/built.ipa"
# Everything gets copied over each time, so these are
# updated (but conceptually are not).
#assert_untouched "$ios/built.app/assets/manifest.xml"
#assert_untouched "$ios/built.app/assets/index.html"
#assert_untouched "$ios/built.app/assets/game.js"

# Find a UTF-16BE string in the binary plist.
if ! ( sed -e 's/[^a-z\.]//g' < "$plist_file" | grep -q 'spaceportshake\.tests\.ios' ); then
  fail_msg "Info.plist should contain application ID spaceportshake.tests.ios, but doesn't"
  exit 9
fi

############################################################
phase "Adding custom URL scheme should rebuild & update plist"

build ipa --set:url_schemes 'mycustomscheme'
plist_file="$OUTPUT/$ios/built.app/Info.plist"
assert_updated "$ios/built.app/Info.plist"

if ! grep -q 'm.y.c.u.s.t.o.m.s.c.h.e.m.e' "$plist_file"; then
  fail_msg "Info.plist should contain custom URL scheme mycustomscheme, but doesn't"
  exit 9
fi

############################################################
phase "Change in application ID rebuilds"

build ipa --set:id spaceportshake.tests.ios.cooler
plist_file="$OUTPUT/$ios/built.app/Info.plist"
assert_updated "$ios/built.app/Info.plist"

# Find a UTF-16BE string in the binary plist.
if ! ( sed -e 's/[^a-z\.]//g' < "$plist_file" | grep -q 'spaceportshake\.tests\.ios\.cooler' ); then
  fail_msg "Info.plist should contain application ID spaceportshake.tests.ios.cooler, but doesn't"
  exit 9
fi

############################################################
phase "No change"

build ipa --set:id spaceportshake.tests.ios.cooler
assert_untouched "$ios/built.app/Info.plist"
assert_untouched "$ios/built.ipa"

############################################################
phase "Insignificant change in settings does not rebuild"

build ipa --set:id spaceportshake.tests.ios.cooler --set:authorization_key 42
assert_untouched "$ios/built.app/Info.plist"

############################################################
phase "Build IPA for CDN"

build --debug-cdn ipa \
    --set:authorization_key 1234 \
    --set:id dummy.packagename
assert_updated "$ios_cdn/built.app"
assert_updated "$ios_cdn/built.app/assets/manifest.xml"
assert_updated "$ios_cdn/built.app/assets/index.html"
assert_updated "$ios_cdn/built.app/assets/game.js"
assert_updated "$ios_cdn/built.ipa"

assert_same \
  "$OUTPUT/manifest/unoptimized-cdn/manifest.xml" \
  "$OUTPUT/$ios_cdn/built.app/assets/manifest.xml"

############################################################
end_tests
