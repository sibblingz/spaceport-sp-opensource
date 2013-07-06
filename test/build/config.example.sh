#!/bin/bash

SPSHAKE="$1"

function spshake {
  "$SPSHAKE" \
    --set:support_path '/path/to/spaceport-shake/support' \
    --set:ios_dev_mobile_provision_file '/path/to/myprofile.mobileprovision' \
    --set:ios_dev_identity 'iphone developer: joe shmoe' \
    "$@"
}
