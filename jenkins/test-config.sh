#!/bin/bash -ex

SPSHAKE="$1"

function spshake {
  "$SPSHAKE" \
    --set:support_path '/Users/buildserver/Jenkins/jobs/SDK-Validate-Shake-Master/workspace/spaceport-support' \
    --set:ios_dev_mobile_provision_file '/Users/buildserver/Documents/certs/iOS_Team_Provisioning_Profile_.mobileprovision' \
    --set:ios_dev_identity 'iphone developer: build serer (5r34528xyq)' \
    "$@"
}
