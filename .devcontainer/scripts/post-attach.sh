#!/bin/bash
# Post-attach script - runs when VS Code attaches to the devcontainer.
# The "💻 Aesthetic" task (runOn: folderOpen) should auto-start the emacs TUI,
# but after devcontainer rebuilds VS Code sometimes doesn't fire the folderOpen
# event. This script waits briefly and checks if the task started.

echo "✅ Attached to aesthetic container"

# Give the auto-task a few seconds to start
sleep 5

# Check if the aesthetic task / emacsclient is running
if pgrep -f "aesthetic-launch.sh" > /dev/null || pgrep -f "emacsclient" > /dev/null; then
    echo "✅ Aesthetic platform task is running."
else
    echo ""
    echo "⚠️  The '💻 Aesthetic' task did not auto-start."
    echo "   This can happen after a devcontainer rebuild."
    echo ""
    echo "   👉 Run 'ac-aesthetic' in any fish terminal to connect."
    echo "      Or use: Terminal > Run Task > 💻 Aesthetic"
    echo ""
    # Check if emacs daemon is at least running (entry.fish should have started it)
    if pgrep -f "emacs.*daemon" > /dev/null; then
        echo "   ✅ Emacs daemon is running (pre-started by entry.fish)"
        echo "      'ac-aesthetic' will connect instantly."
    else
        echo "   ⏳ Emacs daemon may still be starting..."
    fi
fi
