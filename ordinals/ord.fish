# 🐟 Ordfish build script...  23.05.05.03.44

#!/usr/bin/env fish

# requires `pup`
echo "Let's make an ordfish! >=D"

# Check if there's a first parameter
if not set -q argv[1]
    echo "Please provide a destination address as the first parameter."
    exit 1
end

set destination $argv[1]

# Prompt user to paste input
# echo "Grabbing sat JSON from clipboard..."
# set input_json (xclip -o -selection clipboard)

echo "Fetching latest sat from ordsies..."
# Connect to SSH server and run the command
set json_output (ssh root@ordsies "./ord/target/release/ord wallet sats")
# Parse the JSON output and get the last object in the array
set input_json (echo $json_output | jq '.[-1]')
# Print the last object

# Parse input JSON string into variables
set sat (echo $input_json | jq -r '.sat')
set output (echo $input_json | jq -r '.output')
set offset (echo $input_json | jq -r '.offset')

# Find the highest numbered file in the directory and add 1 to it
set highest_number (ls . | grep -o '^[0-9]*' | sort -n | tail -1)
set number (math $highest_number + 1)

# Fetch high priority fee rate from Mempool Space API
set fee (curl -s "https://mempool.space/api/v1/fees/recommended" | jq -r '.fastestFee')

# Fetch name from Ordinals website
set name (curl -s "https://ordinals.com/sat/$sat" | pup 'dt:contains("name") + dd text{}')

# Construct final string
set out "./ord wallet inscribe --dry-run --fee-rate $fee --satpoint $output:$offset --destination $destination ~/ordfish/$number-$name.webp"

echo "# $number: https://ordinals.com/sat/$name"
echo "# $sat"

echo $name | xclip -selection clipboard
echo "`$name` added to clipboard!"

# Check if PNG file is saved
echo "Have you added a PNG file to this directory? (y/n)"
read answer

if test $answer = "y"
    # rename PNG before converting to webp
    set found_png (find . -maxdepth 1 -type f -iname "*.png")
    set input_png "./$number-$name.png"
    mv $found_png $input_png

    set output_webp (echo $input_png | sed 's/\.[^.]*$//').webp

    cwebp -lossless $input_png -o $output_webp
    echo "Converted..."

    rm $input_png
    scp $output_webp root@ordsies:ordfish
    echo "Uploaded `$output_webp` to ordies."
    echo $out | xclip -selection clipboard
    echo "Added mint string to clipboard!"
else
    echo "Exiting the script. Save a PNG file in the directory and try again."
    exit 1
end

