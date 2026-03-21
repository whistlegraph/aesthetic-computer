# this is a shell script for making animated webps (written for whistlegraph spinners)
# (requires webp [installable via homebrew]) https://chromium.googlesource.com/webm/libwebp
# See also: https://stackoverflow.com/questions/58374189/convert-a-bunch-of-png-or-webp-images-to-a-webp-animation

set directory $argv[1] # any directory with sequentially named webp files
set filename $argv[2] # should have .webp at the end

# then grab all webps in the directory and (assuming they are named sequentially, build the image)
set delay $argv[3] # in milliseconds per frame - 250 is a good default
set frames
for file in $directory/*.webp;
  set -a frames -frame $file +$delay+0+0+0-b
end

webpmux $frames -o $filename