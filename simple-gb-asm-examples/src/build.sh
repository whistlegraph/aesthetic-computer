#! /bin/bash
# Simple bash script to assemble Game Boy ROMs
# usage: build.sh <sourcefile>

function error {
echo "Build failed."
exit 1
}

fn=${1%.*}

if [ -f $fn.gb ]
  then
   rm $fn.gb
fi

echo "Asset conversion..."
shopt -s nullglob # avoid errors if no assets are present

# Convert *-tilemap.png files to 2bpp format including a tilemap (remove duplicate tiles)
for file in *-tilemap.png; do
  rgbgfx -u -o ${file%.*}.2bpp -t ${file%.*}.tilemap $file;
done

# Convert *-ztiles.png files to 2bpp format column-by-column without a tilemap (keep duplicate tiles)
for file in *-ztiles.png; do
  rgbgfx -Z -o ${file%.*}.2bpp $file;
done

# Convert *-tiles.png files to 2bpp format row-by-row without a tilemap (keep duplicate tiles)
for file in *-tiles.png; do
  rgbgfx -o ${file%.*}.2bpp $file;
done

echo "Assembling..."
rgbasm -I ../../inc -o$fn.o $1 || error
echo "Linking..."
rgblink -n$fn.sym -m$fn.map -o$fn.gb $fn.o || error
echo "Fixing..."
rgbfix -p 255 -v $fn.gb || error

echo "Created: $fn.gb"
rm *.o
