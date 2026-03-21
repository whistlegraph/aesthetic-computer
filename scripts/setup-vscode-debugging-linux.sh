#!/bin/bash
# Setup VS Code to always launch with remote debugging enabled on Linux
# Run this script on your Linux host machine (not in the container)

ARGV_FILE="$HOME/.config/Code/User/argv.json"

echo "🩸 Setting up VS Code remote debugging on port 9333..."

mkdir -p "$(dirname "$ARGV_FILE")"

if [ -f "$ARGV_FILE" ] && [ -s "$ARGV_FILE" ]; then
    if grep -q "remote-debugging-port" "$ARGV_FILE"; then
        echo "✅ remote-debugging-port already configured in argv.json"
        cat "$ARGV_FILE"
        exit 0
    fi
    
    cp "$ARGV_FILE" "$ARGV_FILE.backup"
    echo "📦 Backed up existing argv.json to argv.json.backup"
    
    python3 -c "
import json, os
argv_file = os.path.expanduser('~/.config/Code/User/argv.json')
with open(argv_file, 'r') as f:
    lines = [l for l in f.read().split(chr(10)) if not l.strip().startswith('//')]
try:
    data = json.loads(chr(10).join(lines))
except:
    data = {}
data['remote-debugging-port'] = 9333
with open(argv_file, 'w') as f:
    json.dump(data, f, indent=2)
    f.write(chr(10))
print('Updated argv.json')
"
else
    echo '{"remote-debugging-port": 9333}' > "$ARGV_FILE"
    echo "📝 Created new argv.json"
fi

echo ""
echo "✅ Done! Your argv.json now contains:"
cat "$ARGV_FILE"
echo ""
echo "🔄 Please restart VS Code for changes to take effect."
echo ""
echo "After restart, VS Code will ALWAYS launch with CDP on port 9333."
echo "Container rebuilds will preserve CDP access - no need to run 'start' again!"
echo ""
echo "To verify it's working after restart, run in your container:"
echo "  curl -s http://host.docker.internal:9333/json | head -5"
