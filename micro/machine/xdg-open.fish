#!/usr/bin/env fish

echo "Opening..."
echo $argv[1]

set url_file /home/me/aesthetic-computer/micro/machine/url.txt

echo $argv[1] > $url_file
