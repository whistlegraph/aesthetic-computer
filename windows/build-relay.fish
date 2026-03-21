#!/usr/bin/env fish
# Relay build output to both terminal and session server
# This runs in the background and sends lines via curl without blocking

while read -l line
    # Echo to terminal immediately
    echo $line
    
    # Send to session server in background (non-blocking)
    fish -c "
        set escaped (string replace -a '\\' '\\\\' -- '$line' | string replace -a '\"' '\\\"')
        curl -s -X POST https://session-server.aesthetic.computer/build-stream \
            --header 'Content-Type: application/json' \
            --data '{\"line\": \"'(string escape -- \$escaped)'\"}' > /dev/null 2>&1
    " &
end
