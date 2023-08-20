Catbell developer handbook
==========================

# Tools used #

The following are the tools used for developing the application.

For application development:-
  *  Lazarus IDE 2.0.12
  
For creating graphical elements used in the user interface:-
  * Inkscape
  * Imagemagick
  * Shell

Third party libraries used
  * Lazarus packages
  
  | Library        | Version | Source                                                              |
  |----------------|---------|---------------------------------------------------------------------|
  | BGraBitmap     | 11.5.5  | https://github.com/bgrabitmap/bgrabitmap                            |
  | UniqueInstance | 1.1     | https://github.com/blikblum/luipack/releases/tag/uniqueinstance-1.1 |
  * External (non-FPC)
  
  | Library   | Version |
  |-----------|---------|
  | PortAudio | 2.0     |
  | SndFile   | 1.0.31  |
  | LibMpg123 | 1.27.2  |

# How to setup your development environment #

## IDE

Install Lazarus

### Install additional Lazarus packages

Install bgrabitmappack.lpk and uniqueinstance_package.lpk following the steps below.

Download the packages from the sources mentioned above, and unzip.
Steps:-
* Package > Open Package File (.lpk)
* Select bgrabitmappack.lpk or uniqueinstance_package.lpk
* A dialog box opens, 
  * Browse to the location
    1. bgrabitmappack.lpk under the directory bgrabitmap
    2. uniqueinstance_package.lpk
  * Click on compile
  * Click on Use > Add to project 
(It is also possible to install these packages through the Lazarus package manager. However, that would involve re-compiling the IDE).

### Compiler

#### Assertions

Assertions are used liberally to intercept runtime anomalous behaviour. These checks run during development testing in builds with debug info, but would not be part of the release build.
Compiler options -> Custom options -> -Sa

#### Hints & Notes

fgl.ppl shows a lot of hints by default.
Project > Project Options > Compiler Options > Messages
Uncheck 
- *Hint: Inlining disabled*
- *Hint: "$1" not yet supported inside inline procedure/function*
- *Note: Call to subroutine "$1" marked as inline is not inlined*

## Install libraries ##

### Windows ###

The following DLLs are required in Windows:-
* libportaudio-2.dll
* libogg-0.dll
* libvorbis-0.dll
* libvorbisenc-2.dll
* libsndfile-1.dll
* libopus-0.dll
* libFLAC-8.dll
* libssp-0.dll
* libmpg123-0.dll

#### These are installed through MSYS2. ####

Search for libraries:-
`$ pacman -Ss portaudio | grep -i ucrt
ucrt64/mingw-w64-ucrt-x86_64-portaudio 190600_20161030-3`

Install library:-
`$ pacman -S mingw-w64-ucrt-x86_64-portaudio`

The DLLs are placed in the *lib* folder.

### Linux ###

The following libraries are required in Linux.

# Notes #
Occasionally, when quality of sound ouput became too bad in Linux, had to reset PulseAudio and Alsa.

# Source control #

Version controlled source is kept in Github
