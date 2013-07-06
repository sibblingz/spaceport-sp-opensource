#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

function as3_embed_source {
  cat > "$INPUT/Main.as" <<EOF
package {
  public class Main {
    [Embed(source="$1")]
    public static var asset:Class;
  }
}
EOF
}

manifest="manifest/unoptimized-bundle/manifest.xml"

function assert_js_references {
  if ! grep -q "$1" "$OUTPUT/js/unoptimized/game.js"; then
    fail_msg "embedded asset '$1' should be referenced in game.js, but isn't"
    exit 9
  fi
}

function assert_manifest_references {
  if ! grep -q "$1" "$OUTPUT/$manifest"; then
    fail_msg "embedded asset '$1' should be referenced in manifest, but isn't"
    exit 9
  fi
}

start_tests

############################################################
phase "Embed PNG asset"

mkdir -p "$INPUT/assets/"
cp "$HERE/assets/splash_1024x748.png" "$INPUT/assets/splash_1024x748.png"
as3_embed_source "assets/splash_1024x748.png"
build

mangled="_2e__2e__2f_assets_2f_splash__1024x748"

assert_updated "embedded_assets/unoptimized/$mangled.png"
assert_js_references "$mangled\.png"
assert_manifest_references "$mangled\.png"

############################################################
phase "Touch embedded asset"

touch "$INPUT/assets/splash_1024x748.png"
build

assert_updated "embedded_assets/unoptimized/$mangled.png"

############################################################
phase "Embed MP3 asset"

mkdir -p "$INPUT/assets/"
cp "$HERE/assets/fireworks.mp3" "$INPUT/assets/fireworks.mp3"
as3_embed_source "assets/fireworks.mp3"
build

mangled="_2e__2e__2f_assets_2f_fireworks"

assert_updated "embedded_assets/unoptimized/$mangled.ogg"
assert_js_references "$mangled\.mp3"
assert_manifest_references "$mangled\.ogg"

############################################################
phase "Embed SWF asset"

mkdir -p "$INPUT/assets/"
cp "$HERE/assets/face.swf" "$INPUT/assets/face.swf"
as3_embed_source "assets/face.swf"
build_failing

############################################################
phase "Embed unknown asset"

mkdir -p "$INPUT/assets/"
touch "$INPUT/assets/.quux"
as3_embed_source "assets/.quux"
build_failing

############################################################
phase "Embed sound with no extension and explicit MIME type"

mkdir -p "$INPUT/assets"
cp "$HERE/assets/fireworks.mp3" "$INPUT/assets/sound"

cat > "$INPUT/Main.as" <<EOF
package {
  public class Main {
    [Embed(source="assets/sound", mimeType="audio/mpeg")]
    public static var asset:Class;
  }
}
EOF

build

mangled="_2e__2e__2f_assets_2f_sound"

assert_js_references "$mangled"
assert_manifest_references "$mangled"

############################################################
phase "Embed binary with explicit MIME type"

mkdir -p "$INPUT/assets"
echo 'Hello, world!' > "$INPUT/assets/binary.txt"

cat > "$INPUT/Main.as" <<EOF
package {
  public class Main {
    [Embed(source="assets/binary.txt", mimeType="application/octet-stream")]
    public static var asset:Class;
  }
}
EOF

build

mangled="_2e__2e__2f_assets_2f_binary"

assert_js_references "$mangled\.txt"
assert_manifest_references "$mangled\.txt"

############################################################
phase "Embed binary with no MIME type"

mkdir -p "$INPUT/assets"
echo 'Hello, world!' > "$INPUT/assets/binary.txt"

cat > "$INPUT/Main.as" <<EOF
package {
  public class Main {
    [Embed(source="assets/binary.txt")]
    public static var asset:Class;
  }
}
EOF

build_failing

############################################################
phase "Embed multiple assets"

mkdir -p "$INPUT/assets"
echo 'Hello, world!' > "$INPUT/assets/binary.txt"
cp "$HERE/assets/fireworks.mp3"       "$INPUT/assets/fireworks.mp3"
cp "$HERE/assets/splash_1024x748.png" "$INPUT/assets/splash_1024x748.png"

cat > "$INPUT/Main.as" <<EOF
package {
  public class Main {
    [Embed(source="assets/fireworks.mp3")]
    public static var audio:Class;

    [Embed(source="assets/binary.txt", mimeType="application/octet-stream")]
    public static var binary:Class;

    [Embed(source="assets/splash_1024x748.png")]
    public static var image:Class;
  }
}
EOF

build

audio_mangled="_2e__2e__2f_assets_2f_fireworks"
binary_mangled="_2e__2e__2f_assets_2f_binary"
image_mangled="_2e__2e__2f_assets_2f_splash__1024x748"

####

assert_js_references "$audio_mangled\.mp3"
assert_manifest_references "$audio_mangled\.ogg"
assert_js_references "$binary_mangled\.txt"
assert_manifest_references "$binary_mangled\.txt"
assert_js_references "$image_mangled\.png"
assert_manifest_references "$image_mangled\.png"

############################################################
end_tests
