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

#sizes="16 24 32"
main_svg="hourglass_flat.svg"
main_ico="${main_svg%.*}.ico"


control_file="$1"

if [ -z "$control_file" ]
then
    echo "Control file not specified"
    exit 1
fi

if [ ! -f "$control_file" ]
then
    echo "Cannot find control file \"$control_file\""
fi


cat "$control_file" | while read line
do
    echo "Processing line: $line"
    svg="$(echo $line | cut -f1 -d"|")"
    sizes="$(echo $line | cut -f2 -d"|")"

    echo "SVG is: $svg"
    if [ ! -f "$svg" ]
    then
        echo "File \"$svg\" not found."
        continue
    fi
    base="${svg%.*}"
    #echo "Debug base: $base"
    
    OIFS="$IFS"
    IFS=","

    
    rebuild_ico="false"
    png_list=""
    
    for size in $sizes
    do
        # cut does not work
        dim="$(echo "$size" | awk -F"_" '{print $1}')"
        grey="$(echo "$size" | awk -F"_" '{print $2}')"
        
        png="${dim}_${base}.png"
        
        if [ "$png" -ot "$svg" ]
        then
            rebuild_ico="true"
            echo "$png" has to be rebuilt
            inkscape -f "$svg" -w "$size" -h "$size" -e "$png"
            echo Completed with status - "$?"
        fi
        png_list="${png_list} $png"
        
        if [ "$grey" = "g" ]
        then
            gpng="${size}_${base}.png"
            if [ "$gpng" -ot "$png" ]
            then
                echo "$gpng" has to be rebuilt
                convert "$png" -colorspace gray "$gpng"
                echo Completed with status - "$?"
            fi            
        fi
    done
       
    IFS="$OIFS"
    
    if [ "$svg" = "$main_svg" ] && [ "$rebuild_ico" = "true" ]
    then
        echo "inside"
        convert $png_list "$main_ico"
    fi    


done
