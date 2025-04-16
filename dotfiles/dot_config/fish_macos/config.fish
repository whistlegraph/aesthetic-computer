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
  # Set up the ssl certificate.
  sudo ac-ssl

  # Kill any existing clipboard listener
  pkill -f "nc -l 12345"
  sleep 0.2

  # Start clipboard listener in background
  fish -c 'while true; nc -l 12345 | pbcopy; end' &

  # Quit VS Code if it's running
  osascript -e 'tell application "Visual Studio Code" to quit'
  sleep 1

  # Launch VS Code
  open -a "Visual Studio Code"
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
