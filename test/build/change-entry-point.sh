#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/common.sh"

function assert_entry_point {
  if ! grep -q "$1" "$OUTPUT/js/unoptimized/game.js"; then
    fail_msg "$1 should be referenced in game.js; but isn't"
    exit 9
  fi
}

start_tests

############################################################
phase "Add single .as file"

template_as3

build
assert_updated "js/unoptimized/game.js"
assert_entry_point "Main"

############################################################
phase "Add another .as file"

cat > "$INPUT/OtherMain.as" <<EOF
package {
  import flash.display.Sprite;
  public class OtherMain extends Sprite {
  }
}
EOF

build
assert_updated "js/unoptimized/game.js"
assert_entry_point "Main"

############################################################
phase "Change entry point"

build --set:entry_point OtherMain.as
assert_updated "js/unoptimized/game.js"
assert_entry_point "OtherMain"

############################################################
phase "Update original application code"

cat > "$INPUT/Main.as" <<EOF
package {
  import flash.display.Sprite;
  public class Main extends Sprite {
    trace("Hello, world!");
  }
}
EOF

build --set:entry_point OtherMain.as
assert_entry_point "OtherMain"

############################################################
phase "Change entry point to original applicaiton"

build --set:entry_point Main.as
assert_updated "js/unoptimized/game.js"
assert_entry_point "Main"

############################################################
end_tests
