# Aesthetic Computer Development Environment Fish Config (macOS Host)
# For Apple Silicon (M1/M2/M3)
set -gx PATH /opt/homebrew/bin $HOME/bin $PATH

# Listen for clipboard from Docker
function clipboard-listen
  while true
    nc -l 12345 | pbcopy
  end
end

# begin starship
starship init fish | source

# reload fish config
alias reload 'source ~/.config/fish/config.fish'

# alias ac-ssl '~/Desktop/code/aesthetic-computer/ssl-dev/ssl-install.fish'
# The above script on macos is copied to ~/bin/ac-ssl and added to sudoers.

# Run the Aesthetic Computer platform.
function start
  set -l workspace ~/Desktop/code/aesthetic-computer

  # Set up the ssl certificate.
  sudo ac-ssl

  # Kill any existing clipboard listener
  pkill -f "nc -l 12345"
  sleep 0.2

  # Start clipboard listener in background
  fish -c 'while true; nc -l 12345 | pbcopy; end' &

  # Remove old container and start devcontainer via CLI
  echo "🧹 Removing old container..."
  docker rm -f aesthetic 2>/dev/null

  echo "🚀 Starting devcontainer..."
  cd $workspace
  devcontainer up --workspace-folder .

  if test $status -eq 0
    echo ""
    echo "✅ Container ready!"
    echo "🔗 Opening VS Code..."
    # Open VS Code, then user attaches manually - auto-attach via CLI is too flaky
    open -a "Visual Studio Code"
    sleep 2
    echo ""
    echo "📋 Press: Cmd+Shift+P → 'attach' → Enter → Enter"
    echo ""
  else
    echo "❌ Failed to start container"
    return 1
  end
end

# empty greeting
function fish_greeting
end

# Emulates vim's cursor shape behavior
# Set the normal and visual mode cursors to a block
set fish_cursor_default block
# Set the insert mode cursor to a line
set fish_cursor_insert line
# Set the replace mode cursor to an underscore
set fish_cursor_replace_one underscore
set fish_vi_force_cursor true

# vim bindings
fish_vi_key_bindings
