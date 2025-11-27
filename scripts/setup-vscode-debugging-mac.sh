#!/bin/bash
# Setup VS Code to always launch with remote debugging enabled on Mac
# Run this script on your Mac (not in the container)

ARGV_FILE="$HOME/Library/Application Support/Code/User/argv.json"

echo "🩸 Setting up VS Code remote debugging on port 9222..."

# Create the directory if it doesn't exist
mkdir -p "$(dirname "$ARGV_FILE")"

# Check if file exists and has content
if [ -f "$ARGV_FILE" ] && [ -s "$ARGV_FILE" ]; then
    # File exists - check if it already has the setting
    if grep -q "remote-debugging-port" "$ARGV_FILE"; then
        echo "✅ remote-debugging-port already configured in argv.json"
        cat "$ARGV_FILE"
        exit 0
    fi
    
    # Add the setting to existing file (insert before last closing brace)
    # Backup first
    cp "$ARGV_FILE" "$ARGV_FILE.backup"
    echo "📦 Backed up existing argv.json to argv.json.backup"
    
    # Use Python for reliable JSON manipulation
    python3 << 'PYTHON'
import json
import os

argv_file = os.path.expanduser("~/Library/Application Support/Code/User/argv.json")

with open(argv_file, 'r') as f:
    content = f.read()
    # Handle comments in JSON (VS Code allows them)
    lines = content.split('\n')
    clean_lines = [l for l in lines if not l.strip().startswith('//')]
    clean_content = '\n'.join(clean_lines)
    
try:
    data = json.loads(clean_content)
except:
    data = {}

data['remote-debugging-port'] = 9222

with open(argv_file, 'w') as f:
    json.dump(data, f, indent=2)
    f.write('\n')

print("Updated argv.json")
PYTHON

else
    # Create new file
    cat > "$ARGV_FILE" << 'JSON'
{
  "remote-debugging-port": 9222
}
JSON
    echo "📝 Created new argv.json"
fi

echo ""
echo "✅ Done! Your argv.json now contains:"
cat "$ARGV_FILE"
echo ""
echo "🔄 Please restart VS Code for changes to take effect."
echo ""
echo "To verify it's working after restart, run in your container:"
echo "  curl -s http://host.docker.internal:9222/json | head -5"
