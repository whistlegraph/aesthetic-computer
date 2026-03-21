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

# ═══════════════════════════════════════════════════════════════════════════════
# 🩸 AESTHETIC COMPUTER - Start Script (macOS)
# ═══════════════════════════════════════════════════════════════════════════════

# AC color palette
function __ac_colors
    set -g RST (set_color normal)
    set -g BOLD (set_color --bold)
    set -g AC_MAGENTA (set_color --bold c81e64)
    set -g FG_GREEN (set_color green)
    set -g FG_RED (set_color red)
    set -g FG_YELLOW (set_color yellow)
    set -g FG_WHITE (set_color white)
end

function __ac_banner
    __ac_colors
    echo
    printf "%s" "$AC_MAGENTA"
    echo '  ╔═══════════════════════════════════════════════════════════╗'
    echo '  ║                                                           ║'
    printf "  ║  %s█▀█ █▀▀ █▀ ▀█▀ █ █ █▀▀ ▀█▀ █ █▀▀   █▀▀ █▀█ █▄▀▄█ █▀█%s  ║\n" "$FG_WHITE$BOLD" "$RST$AC_MAGENTA"
    printf "  ║  %s█▀█ ██▄ ▄█  █  █▀█ ██▄  █  █ █▄▄ ▄ █▄▄ █▄█ █ ▀ █ █▀▀%s  ║\n" "$FG_WHITE$BOLD" "$RST$AC_MAGENTA"
    echo '  ║                                                           ║'
    echo '  ╚═══════════════════════════════════════════════════════════╝'
    printf "%s\n" "$RST"
end

function __ac_log
    set -l icon $argv[1]
    set -l msg $argv[2]
    set -l color $argv[3]
    test -z "$color"; and set color "$AC_MAGENTA"
    printf "%s▌%s %s%s%s\n" "$color" "$RST" "$icon" " " "$msg"
end

function __ac_ok; __ac_log "✓" "$argv[1]" "$FG_GREEN"; end
function __ac_err; __ac_log "✗" "$argv[1]" "$FG_RED"; end
function __ac_info; __ac_log "›" "$argv[1]" "$AC_MAGENTA"; end
function __ac_warn; __ac_log "!" "$argv[1]" "$FG_YELLOW"; end

# 🚀 MAIN START FUNCTION
function start
    set -l workspace ~/Desktop/code/aesthetic-computer
    
    __ac_colors
    __ac_banner
    
    # Set up the ssl certificate
    __ac_info "Setting up SSL..."
    sudo ac-ssl
    __ac_ok "SSL ready"

    # Kill any existing clipboard listener
    pkill -f "nc -l 12345" 2>/dev/null
    sleep 0.2

    # Start clipboard listener in background
    __ac_info "Starting clipboard listener..."
    fish -c 'while true; nc -l 12345 | pbcopy; end' &
    __ac_ok "Clipboard listener started"

    # Remove old container and start devcontainer via CLI
    __ac_info "Removing old container..."
    docker rm -f aesthetic 2>/dev/null

    __ac_info "Starting devcontainer..."
    cd $workspace
    devcontainer up --workspace-folder .

    if test $status -eq 0
        __ac_ok "Container ready"
        
        # Quit VS Code if running (needed for CDP flag to take effect)
        __ac_info "Quitting any running VS Code..."
        osascript -e 'quit app "Visual Studio Code"' 2>/dev/null
        sleep 1
        
        # Launch VS Code with CDP debugging enabled
        __ac_info "Launching VS Code with CDP on port 9333..."
        
        # Use dev-container+ URI format
        set -l hex_path (echo -n "/Users/$USER/Desktop/code/aesthetic-computer" | xxd -c 256 -p)
        set -l uri "vscode-remote://dev-container+$hex_path/workspaces/aesthetic-computer"
        
        # Open VS Code with remote debugging for artery-tui control
        # Must quit VS Code first for --remote-debugging-port to work
        code --folder-uri "$uri" \
             --remote-debugging-port=9333 &
        
        __ac_ok "VS Code launched"
        echo
        printf "%s  ✨ aesthetic.computer ready%s\n" "$AC_MAGENTA$BOLD" "$RST"
        echo
    else
        __ac_err "Failed to start container"
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

# Created by `pipx` on 2026-02-14 21:34:00
set PATH $PATH /Users/jas/.local/bin
