#!/usr/bin/env fish
# Live reload watcher for kidlisp.com

echo "👀 Watching kidlisp.com/index.html for changes..."
echo "🌐 Server running at http://localhost:8765"
echo "Press Ctrl+C to stop"

set last_hash (md5sum index.html | cut -d' ' -f1)

while true
    sleep 0.5
    set current_hash (md5sum index.html | cut -d' ' -f1)
    
    if test "$current_hash" != "$last_hash"
        echo "🔄 Detected change in index.html - page will auto-reload"
        set last_hash $current_hash
        # Touch the file to update last-modified header
        touch index.html
    end
end
