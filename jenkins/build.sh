#!/bin/bash -xe

# NOTE: $WORKSPACE is the root of the project git repo

STAGING_AREA="$1"

#force a clean build if indicated
if [ -d $STAGING_AREA/clean.build ]; then
   echo "Performing CLEAN build."
   git clean -fdx
else
   echo "Performing INCREMENTAL build."
fi

#clean staging area
rm -f $STAGING_AREA/workspace/sdk/osx/package/bin/sp

#copy change documentation to staging area
$SP_SCRIPTS/copy_component_changes.py $WORKSPACE/../builds/$BUILD_NUMBER/changelog.xml $STAGING_AREA/changes/shake.txt

make deps configure build config=release

#move build artifacts to staging area
cp $WORKSPACE/.dist/build/sp/sp $STAGING_AREA/workspace/sdk/osx/package/bin/
