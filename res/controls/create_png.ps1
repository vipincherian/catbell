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

$main_svg="hourglass.svg"
$main_ico=[System.IO.Path]::ChangeExtension($main_svg, ".ico")


$control_file=$args[0]
if ([string]::IsNullOrEmpty($control_file)) {
    echo "Control file not specified"
	exit 1
}

if(![System.IO.File]::Exists($control_file)) {
    echo "Cannot find control file ""$control_file"""
	exit 1
}


foreach($line in Get-Content $control_file) {
    echo "Processing line: $line"
	$svg=$line.split("|")[0]
	#echo $svg
	$sizes=$line.split("|")[1].split(",")
	#echo $sizes
	echo "SVG is: $svg"
	if(![System.IO.File]::Exists($svg)) {
	    echo "File ""$svg"" not found."
		continue
	}
	$base=[System.IO.Path]::GetFileNameWithoutExtension($svg)
	#echo "Debug base: $base"
	
	$rebuild_ico=$FALSE
	$png_list=""
	
	foreach($size in $sizes) {
		#echo $size
		$dim=$size.split("_")[0]
		$grey=$size.split("_")[1]
		$png="${dim}_${base}.png"
		#echo $png
		
		$rebuild_png=$FALSE
		
		if(![System.IO.File]::Exists($png)) {
		    $rebuild_png=$TRUE
		} else {
		    if((Get-Item $png).LastWriteTime -lt (Get-Item $svg).LastWriteTime) { 
			    $rebuild_png=$TRUE
			}
		}
		#echo $rebuild_png
		if($rebuild_png) {
		    $rebuild_ico=$TRUE
            echo "$png has to be rebuilt"
			try {
				Invoke-Expression "inkscape -f $svg -w $dim -h $dim -e $png"
		    } catch {
				echo $_
			}
            #echo Completed with status - "$?"			
		}
		
		$png_list="${png_list} $png"
		
		if (![string]::IsNullOrEmpty($grey)) {
			#echo "Hoohoo"
		    $gpng="${size}_${base}.png"
			$rebuild_gpng=$FALSE
			# Check if the grey pngs have to be rebuilt
			if(![System.IO.File]::Exists($gpng)) {
				$rebuild_gpng=$TRUE
			} else {
				if((Get-Item $gpng).LastWriteTime -lt (Get-Item $png).LastWriteTime) { 
					$rebuild_gpng=$TRUE
				}
			}
			
			if($rebuild_gpng) {
				echo "$png has to be rebuilt"
				try {
					Invoke-Expression "J:\bin\imagemagick\convert ${png} -colorspace gray ${gpng}"
				} catch {
					echo $_
				}
				#echo Completed with status - "$?"			
			}			
			
			
		}
	}
	
	if(($svg -eq $main_svg) -and ($rebuild_ico -eq $TRUE)){
	    echo "Rebuilding main icon $main_ico"
		try {
			Invoke-Expression "J:\bin\imagemagick\convert $png_list $main_ico"
		} catch {
			echo $_
		}		
	}
	
}
