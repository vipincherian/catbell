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
  * BgraBitmap
  * UniqueInstance
  * PortAudio
  * SndFile
  * LibMpg123

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

# Sound #
In Linux I had to creat a .asoundrc file in the home directory with the following content:-
`pcm.!default "plughw:0,0"`


# Source control #

Version controlled source is kept in Github