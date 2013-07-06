#!/bin/bash -xe

# NOTE: $WORKSPACE is the root of the project git repo

STAGING_AREA="$1"
SHAKE_WORKSPACE="$2"

#nuke the local workspace
rm -rf $WORKSPACE/*

#remove job status flag
rm -f $STAGING_AREA/status/$JOB_NAME.succeeded

#copy spaceport support folder to local workspace
cp -R $STAGING_AREA/workspace/sdk/osx/package/lib/spaceport-support $WORKSPACE/

#replace old config in test directory
cp -f $SHAKE_WORKSPACE/jenkins/test-config.sh $SHAKE_WORKSPACE/test/build/config.sh

pushd $SHAKE_WORKSPACE

#build all tests
make lint test-par

popd

#notify results job that this test completed successfully
touch $STAGING_AREA/status/$JOB_NAME.succeeded

