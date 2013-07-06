#!/bin/bash

parallel="$1"

TMP="$(mktemp -d -t spaceport-parallel.XXXXXXXXXX)"
LOG_DIR="$TMP/log"
CMD_FILE="$TMP/cmds"

function run_parallel {
  "$parallel" "$LOG_DIR" "$CMD_FILE" > /dev/null
}

function phase {
  echo "Running phase $1"
  rm -f "$CMD_FILE"
  touch "$CMD_FILE"
}

function add_cmd {
  echo "$1" >> "$CMD_FILE"
}

function fail {
  echo "$1" >&2
  exit 1
}

############################################################
phase "Success if first finishes last"

add_cmd 'sleep 3 ; true'
add_cmd 'sleep 1 ; true'

run_parallel || fail "Build should have succeeded"

############################################################
phase "Success if first finishes first"

add_cmd 'sleep 1 ; true'
add_cmd 'sleep 3 ; true'

run_parallel || fail "Build should have succeeded"

rm -r "$TMP"
