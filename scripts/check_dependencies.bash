#!/bin/bash

#This script checks for dynamic library dependencies, checks
#each against the whitelist and prints any that are not already 
#white-listed.

EXECUTABLE="../src/catbell"

ldd "$EXECUTABLE" | sed 's/^\s*//' | cut -f1 -d" " | \
    grep -v -e ^libX -e ^libgtk -e ^libgdk
exit 0

