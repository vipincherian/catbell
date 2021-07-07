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

## Install additional Lazarus packages ##

Download the packages from the sources mentioned above, and unzip.
Steps:-
* Package > Open Package File (.lpk)
* Select bgrabitmappack.lpk or uniqueinstance_package.lpk
* A dialog box opens, 
  * Click on compile
  * Click on Use > Add to project 
(It is also possible to install these packages through the Lazarus package manager. However, that would involve re-compiling the IDE).
Build
Development
Assertions
Assertions are used liberally to intercept runtime anomalous behaviour. These checks run during development testing in builds with debug info, but would not be part of the release build.
Compiler options -> Custom options -> -Sa

# Notes #
Occasionally, when quality of sound ouput became too bad in Linux, had to reset PulseAudio and Alsa.

# Source control #

Version controlled source is kept in Github
