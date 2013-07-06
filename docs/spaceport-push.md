spaceport-push
==============

Builds an ActionScript 3 application and pushes it to a
device or a simulator.

Usage
-----

    spaceport-push [options...] target...

### Options

`-j jobs`, `--jobs jobs` builds with the given number of
parallel processes.  Default value: 2

`-p project`, `--project project` builds from the given
project file.  Default value: project file is searched for
in the current directory.

### Target

`sim`, `simulator` launches the project on a new Spaceport
Simulator.

`ios` installs the project on a USB-connected iOS device.
