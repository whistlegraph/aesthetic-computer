# this is a shell script for converting all images in a directory to webp
# currently supports all files with an "image*" mimetype 

# Usage from directory of images... `fish img-to-webp.fish any-flags`
# Useful flags: `-resize 2048x2048` `-define webp:lossless=true`  
# (requires imagemagick)
# last updated 22.11.28.12.57

# otherwise use imagemagick's defaults
for file in *
    if test (string match "image/*" (file -b --mime-type $file))
        echo "Converting $file to webp with flags: $argv"
        convert $file $argv (path change-extension '' $file).webp
    end
end

# see also: https://imagemagick.org/script/webp.php,
#           https://stackoverflow.com/a/29177261