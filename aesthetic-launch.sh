#!/usr/bin/env bash

# Ensure ~/.local/bin is in PATH
export PATH="$HOME/.local/bin:$PATH"

# Use a writable XDG config root so fish can create its state files
if [ -z "$XDG_CONFIG_HOME" ]; then
    export XDG_CONFIG_HOME="/workspaces/aesthetic-computer/.xdg"
fi
mkdir -p "$XDG_CONFIG_HOME/fish/functions"

FISH_CONFIG_FILE="$XDG_CONFIG_HOME/fish/config.fish"
if [ ! -f "$FISH_CONFIG_FILE" ]; then
    cat <<'EOF' > "$FISH_CONFIG_FILE"
source /workspaces/aesthetic-computer/.devcontainer/config.fish
EOF
fi

echo "→ \$0 is: $0"
echo "→ SHELL is: $SHELL"
echo "→ USER is: $USER"
echo "→ HOME is: $HOME"
echo "→ PATH is: $PATH"
echo "→ Current directory: $(pwd)"

# if command -v docker >/dev/null; then
#     echo "✅ Found 'docker' in PATH"
# else
#     echo "❌ 'docker' not found in PATH"
# fi

# Detect Fish shell
if [[ "$SHELL" == *fish* ]]; then
    echo "🐟 Detected Fish shell"
    echo "🎬 Launching 'aesthetic' via Fish (login shell)"
    exec fish --login -c "aesthetic"
else
    echo "🐚 Detected non-Fish shell"
    if command -v aesthetic > /dev/null; then
        echo "🎬 Launching 'aesthetic'"
        exec aesthetic
    elif command -v docker > /dev/null; then
        echo "🧹 Pruning Docker system"
        docker system prune -f
    fi
    clear
    echo -e "\n\033[1m 🟪 Please run the Aesthetic Platform from its development container.\033[0m\n"
fi
