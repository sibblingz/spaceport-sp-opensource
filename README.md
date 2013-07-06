Spaceport Shake
===============

Spaceport build system for ActionScript 3 projects.

Support directory
-----------------

To use or develop `spaceport-push`, a directory containing
Spaceport build tools is needed.  This directory is called
the support directory.

The files needed and directory structure of the support
directory are codified in the `toolConfigurationFromRoot`
function in `Development.Spaceport.Config`
(`src/Development/Spaceport/Config.hs`).

When distributing, place the support directory next to the
`spaceport-push` executable, naming it `support`.
`spaceport-push` will automatically detect the `support`
directory.

Setup
-----

Set up the spaceport-build tests by copying
`test/build/config.example.sh` to `test/build/config.sh`.
Edit the support path and other options as required.

Running by example
------------------

**DOCUMENTATION OUT OF DATE**

Builds `Main` into `sp-output/` with two source directories,
`src/` and `vendor/mylibrary/`.  `-j4` specifies that four
targets should be built in parallel, if possible.

    spaceport-build -o sp-output/ -i src -j4

Builds an IPA into `sp-output/ios/built.ipa`.  You can use
iTunes to install the built IPA onto an iOS device.

    spaceport-build \
        -o sp-output/ -i src \
        --entry-point MyGame \
        --provision mygame.mobileprovision \
        --certificates Certificates.p12 \
        ipa

Build instructions
------------------

Prerequisites:

 * GHC 7.4.1 or above
 * cabal-install
 * GNU Make
 * GNU Bash

Install the Haskell Platform. Version 2012.2 is supported.

Makefile rule summary:

    # Install dependencies and configure project.
    make deps configure

    # Build and test spaceport-shake.  See above on how to
    # set up testing.
    make

After building and testing, the `spaceport-push` executable
can be found at `.dist/build/spaceport-push/spaceport-push`.
