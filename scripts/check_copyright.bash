#!/bin/bash

# Copyright (C) 2022 Vipin Cherian
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
# 
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
# Boston, MA  02110-1301, USA.

error_count=0

while read file
do
    # Variable to hold the final error messages for the current file
    message=""

    # This variable tracks if any error was encountered, per file
    errors_found=false

    if [[ -z "$(cat "$file" | \
        grep "Copyright (C) [0-9]* Vipin Cherian")" ]] 
    then
        errors_found=true
        message="does not have Copyright clause"
        ((error_count++))
    fi

    if [[ -z "$(cat "$file" |  grep "GNU")" ]] 
    then
        errors_found=true
        if [[ ! -z "$message" ]]
        then
            message="${message}, license"
        else
            message="does not have license"
        fi
        ((error_count++))
    fi

    if [[ "$errors_found" = true ]]
    then
        echo "$file $message"
    fi

done <<< $(find ../../ -type f \( -iname \*.pas -o -iname \*.bash -o \
    -iname \*.ps1 \) -not -path "*/backup/*" | \
    grep -v -f whitelist_copyright.txt)

if [[ "$error_count" -ne 0 ]]
then
    echo "$error_count errors found."
fi

exit "$error_count"

