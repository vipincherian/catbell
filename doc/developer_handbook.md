Catbell developer handbook
==========================

# Tools used #

The following are the tools used for developing the application.

For application development:-
  *  Lazarus IDE
  
For creating graphical elements used in the user interface:-
  * Inkscape
  * Imagemagick
  * Shell

Third party libraries used
  * Lazarus packages
  
  | Library        | Version | Source                                                              |
  |----------------|---------|---------------------------------------------------------------------|
  | BGraBitmap     | 11.3.1  | https://github.com/bgrabitmap/bgrabitmap                            |
  | UniqueInstance | 1.1     | https://github.com/blikblum/luipack/releases/tag/uniqueinstance-1.1 |
  * External (non-FPC)
  
  | Library   | Version |
  |-----------|---------|
  | PortAudio |         |
  | SndFile   |         |
  | LibMpg123 |         |

# How to setup your development environment #

Install Lazarus
Install BGRABitmap lkp
Download from and unzip
In Lazarus IDE Package > Open Package file (.lpk)  and select bgrabitmappack.lpk.
Popup titled  Package BGRABitmapPack v9.3 click Compile and then Use > In Project

Install UniqueInstance
Using Lazarus IDE, install through Package > Online Package Manager
Obtain libraries

Logging
Teventlog

Build
Development
Assertions
Assertions are used liberally to intercept runtime anomalous behaviour. These checks run during development testing in builds with debug info, but would not be part of the release build.
Compiler options -> Custom options -> -Sa

# Notes #
Occasionally, when quality of sound ouput became too bad in Linux, had to reset PulseAudio and Alsa.

# Source control #

Version controlled source is kept in Github
