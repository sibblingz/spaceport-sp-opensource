#!/bin/bash

# Following is the minimal set of tools from the Android SDK
# required to build a working APK.
#
# $ANDROID_SDK_PATH/platform-tools/aapt
# $ANDROID_SDK_PATH/platform-tools/dx
# $ANDROID_SDK_PATH/platforms/android-15/android.jar
# $ANDROID_SDK_PATH/tools/apkbuilder
# $ANDROID_SDK_PATH/tools/zipalign

set -e -x

ANDROID_SDK_PATH=/Users/jonpurdy/android-sdk-min
PROJECT_SHORT_ID=liranslab
PROJECT_ID=io.spaceport.$PROJECT_SHORT_ID
PROJECT_ROOT=/Users/jonpurdy/lirans-lab-android
PROJECT_PATH=$PROJECT_ROOT/android

# build-setup:

mkdir -p $PROJECT_PATH/res
mkdir -p $PROJECT_PATH/jar
mkdir -p $PROJECT_PATH/bin
mkdir -p $PROJECT_PATH/bin/res
mkdir -p $PROJECT_PATH/gen
mkdir -p $PROJECT_PATH/bin/classes

# code-gen:

$ANDROID_SDK_PATH/platform-tools/aapt \
    package \
    -v \
    -f \
    -m \
    -M $PROJECT_PATH/AndroidManifest.xml \
    -S $PROJECT_PATH/bin/res \
    -S $PROJECT_PATH/res \
    -I $ANDROID_SDK_PATH/platforms/android-15/android.jar \
    -J $PROJECT_PATH/gen \
    --generate-dependencies

# compile:

javac \
    -d $PROJECT_PATH/bin/classes \
    -classpath $PROJECT_PATH/bin/classes:$PROJECT_PATH:$PROJECT_PATH/jar/Spaceport-sub.jar \
    -sourcepath $PROJECT_PATH/src:$PROJECT_PATH/gen \
    -target 1.5 \
    -bootclasspath $ANDROID_SDK_PATH/platforms/android-15/android.jar \
    -encoding UTF-8 \
    -g \
    -source 1.5 \
    $PROJECT_PATH/src/MyGame.java \
    $PROJECT_PATH/gen/io/spaceport/liranslab/R.java

# dex:

$ANDROID_SDK_PATH/platform-tools/dx \
    --dex \
    --output $PROJECT_PATH/bin/classes.dex \
    --verbose \
    $PROJECT_PATH/bin/classes \
    $PROJECT_PATH/jar/Spaceport-sub.jar

# crunch:

$ANDROID_SDK_PATH/platform-tools/aapt \
    crunch \
    -v \
    -S $PROJECT_PATH/res \
    -C $PROJECT_PATH/bin/res

# package-resources:

$ANDROID_SDK_PATH/platform-tools/aapt \
    package \
    -v \
    --no-crunch \
    -f \
    --debug-mode \
    -M $PROJECT_PATH/AndroidManifest.xml \
    -S $PROJECT_PATH/bin/res \
    -S $PROJECT_PATH/res \
    -A $PROJECT_PATH/assets \
    -I $ANDROID_SDK_PATH/platforms/android-15/android.jar \
    -F $PROJECT_PATH/bin/$PROJECT_ID.ap_ \
    --generate-dependencies

# package:

$ANDROID_SDK_PATH/tools/apkbuilder \
    $PROJECT_PATH/bin/$PROJECT_ID-debug-unaligned.apk \
    -v \
    -d \
    -z $PROJECT_PATH/bin/$PROJECT_ID.ap_ \
    -f $PROJECT_PATH/bin/classes.dex \
    -z $PROJECT_PATH/jar/Spaceport-sub.jar

# do-debug:

$ANDROID_SDK_PATH/tools/zipalign \
    -f 4 \
    $PROJECT_PATH/bin/$PROJECT_ID-debug-unaligned.apk \
    $PROJECT_PATH/bin/$PROJECT_ID.apk

# copyapk:

cp $PROJECT_PATH/bin/$PROJECT_ID.apk \
    $PROJECT_PATH/$PROJECT_SHORT_ID.apk
