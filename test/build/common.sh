#!/bin/bash

set -e -E -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"

# sed, grep, etc. under some locales will get confused if we
# treat binary files as text.  We thus treat all files as
# binary by using the C locale.
export LANG=C

if ! [ -f "$HERE/config.sh" ]; then
  echo "FATAL: No test configuration found" >&2
  echo "Copy $HERE/config.example.sh to $HERE/config.sh and edit appropriately." >&2
  exit 1
fi

source "$HERE/config.sh"

if [ "$#" -lt 1 ]; then
  echo "Usage: $0 path/to/spaceport-shake" >&2
  exit 1
fi

TEST_PHASE="(Unknown phase)"

function debug {
  echo "$@" >&2
}

function fail_msg {
  echo "FAILURE: $@" >&2
  echo "FAILURE: During phase: $TEST_PHASE"
}

# Resets the input and output directories to a clean state.
function reset {
  rm -r "$INPUT" "$OUTPUT" "$LAST_BUILD_START"
  mkdir "$INPUT" "$OUTPUT"
}

function template_as3 {
  echo 'package { public class Main { } }' > "$INPUT/Main.as"
}

function run_spshake {
  spshake \
    --output "$OUTPUT" \
    --ignore-missing-project-file \
    --set:source_path "$INPUT" \
    --set:entry_point Main.as \
    --set:project_root "$INPUT" \
    "$@"
}

function build {
  debug "Building in phase $TEST_PHASE..."
  touch "$LAST_BUILD_START"
  sleep 1
  run_spshake build "$@"
}

function build_failing {
  debug "Building and expecting failure in phase $TEST_PHASE..."
  touch "$LAST_BUILD_START"
  sleep 1
  set +e
  run_spshake build "$@"
  exit_code="$?"
  set -e
  if [ "$exit_code" -eq 0 ]; then
    fail_msg "Expected a failed build, but build was successful"
    exit 6
  fi
}

# Asserts that $OUTPUT/$1 was updated in the last build.
function assert_updated {
  if ! [ -e "$OUTPUT/$1" ]; then
    fail_msg "$1 should have been updated, but doesn't exist"
    exit 5
  fi

  if [ "$OUTPUT/$1" -ot "$LAST_BUILD_START" ]; then
    fail_msg "$1 should have been updated, but was not"
    exit 2
  fi
}

# Asserts that $OUTPUT/$1 was not updated in the last build.
function assert_untouched {
  if ! [ -e "$OUTPUT/$1" ]; then
    fail_msg "$1 should not have been updated, but doesn't exist"
    exit 4
  fi

  if [ "$OUTPUT/$1" -nt "$LAST_BUILD_START" ]; then
    fail_msg "$1 should not have been updated, but was"
    exit 3
  fi
}

# Asserts that $OUTPUT/$1 was not created in the last build.
function assert_unbuilt {
  if [ -e "$OUTPUT/$1" ]; then
    fail_msg "$1 should not exist"
    exit 6
  fi
}

# Asserts that all output files were not updated in the last build.
function assert_all_untouched {
  find "$OUTPUT" -type f -print | sed "s:^$OUTPUT/::" | while read file; do
    if [ "$file" != "build-cache.database" ]; then
      assert_untouched "$file"
    fi
  done
}

# Asserts that two files are identical.
function assert_same {
  if ! cmp --quiet -- "$1" "$2"; then
    fail_msg "$1 should be exactly identical to $2, but they differed"
    exit 12
  fi
}

# Asserts that two files are not identical.
function assert_different {
  if ! [ -e "$1" ]; then
    fail_msg "$1 should be different than $2, but $1 does not exist"
    exit 12
  fi

  if ! [ -e "$2" ]; then
    fail_msg "$1 should be different than $2, but $2 does not exist"
    exit 12
  fi

  if cmp --quiet -- "$1" "$2"; then
    fail_msg "$1 should be different than $2, but they were identical"
    exit 12
  fi
}

function start_tests {
  INPUT="$(mktemp -d -t spaceport-shake.XXXXXXXXXX)"
  OUTPUT="$(mktemp -d -t spaceport-shake.XXXXXXXXXX)"

  LAST_BUILD_START="$(mktemp -t spaceport-shake.XXXXXXXXXX)"
  BUILD_CACHE="$(mktemp -t spaceport-shake.XXXXXXXXXX)"

  debug "Test input directory is: $INPUT"
  debug "Test output directory is: $OUTPUT"
}

function phase {
  TEST_PHASE="$@"
}

function end_tests {
  debug "All tests passed"
  rm -r "$INPUT" "$OUTPUT" "$LAST_BUILD_START"
  exit 0
}
