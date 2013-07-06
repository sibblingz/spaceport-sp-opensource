#!/bin/bash

set -e -E -o pipefail

COMMIT="$(cd "$HERE" ; git rev-parse HEAD)"

if ! "$1" --version | grep -q -- "$COMMIT"; then
  echo "$1 --version is missing version ($COMMIT)" >&2
  exit 1
fi
