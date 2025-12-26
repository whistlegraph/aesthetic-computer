# @jeffrey's laptop fish config (Fedora Linux Host)

# Enable Wayland support for Java apps
set -x _JAVA_AWT_WM_NONREPARENTING 1
set -x GDK_SCALE 1
set -x GDK_DPI_SCALE 1

# HiDPI settings for JetBrains apps like Android Studio
set -x JETBRAINS_JDK_FLAGS "-Dsun.java2d.uiScale=2 -Dsun.java2d.uiScale.enabled=true"

# Optional: Force scaling for Qt apps if needed
set -x QT_SCALE_FACTOR 1.5

# add homebrew to path (only if we are on linux)
switch (uname)
    case Linux
        eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
end

if status is-interactive
    # Commands to run in interactive sessions can go here
end

# begin starship
starship init fish | source

# alias vim to neovim
alias vim nvim
alias vimdiff 'nvim -d'

# make sure that visual studio code uses wayland
alias chrome 'google-chrome --ozone-platform=wayland'
alias code 'code --ozone-platform=wayland'
alias cursor '~/Downloads/cursor-0.11.7.AppImage --ozone-platform=wayland'
alias mongodb-compass 'mongodb-compass --enable-features=UseOzonePlatform,WaylandWindowDecorations --ozone-platform=wayland --ignore-additional-command-line-flags'

# reload fish config
alias reload 'source ~/.config/fish/config.fish'

# xdg-open wrapper for dev container (opens URLs on Windows host)
function xdg-open
    "$BROWSER" $argv
end

# learn about a command with 't' -> tldr
alias t 'tldr'

# open my agenda.txt
alias agenda 'nvim ~/Desktop/agenda/agenda.txt'
alias paper 'gnome-extensions prefs paperwm@paperwm.github.com'

# open a file with emacs in the tui
alias edit 'fzf | read -l file; and test -n "$file"; and emacs -nw $file'

alias wgeth 'echo 0x238c9c645c6EE83d4323A2449C706940321a0cBf'

# shortcuts for editing dot files
# alias ev 'chezmoi edit ~/.config/nvim/init.vim'
alias config 'nvim ~/.config/fish/config.fish'
alias confoot 'nvim ~/.config/foot/foot.ini'

# alias edit 'fzf | read -l file; and test -n "$file"; and emacs -nw "$file"'

# shortcuts for aesthetic.computer (macOS only)
# alias webp 'fish ~/IdeaProjects/aesthetic.computer/system/public/disks/digitpain/webp.fish'

alias vs 'vim (sk)'
alias js 'vim (find . -name "*.js" -o -name "*.mjs" | sk -m -n !node_modules)'
alias ff "vim (sk -c 'git ls-tree -r --name-only HEAD || ag -l -g \"\"')"

# shortcuts for projects
alias ac 'cd ~/aesthetic-computer; git pull'

alias ac-ssl '~/aesthetic-computer/ssl-dev/ssl-install.fish'

alias acc 'ac; ac-ssl; code .'

function vidinfo
  ffprobe -v error \
    -select_streams v:0 \
    -show_entries "stream=codec_name,width,height,r_frame_rate,avg_frame_rate,duration" \
    -show_entries format=format_name,format_long_name \
    -of default=noprint_wrappers=1:nokey=0 \
    $argv
end

function acd
    set -l workspace ~/aesthetic-computer
    
    ac
    
    # Stop other containers (but not aesthetic)
    set containers (docker ps -q --filter "name!=aesthetic")
    if test -n "$containers"
        docker stop $containers >/dev/null 2>&1
    end
    
    # Kill any existing socat forwarder for CDP
    pkill -f "socat.*9224" 2>/dev/null
    sleep 0.5
    
    # Forward CDP port from localhost:9222 (VS Code) to 0.0.0.0:9224 so container can reach it
    socat TCP-LISTEN:9224,bind=0.0.0.0,fork,reuseaddr TCP:127.0.0.1:9222 &
    
    # Check if aesthetic container exists and start it if stopped
    set -l container_status (docker inspect -f '{{.State.Status}}' aesthetic 2>/dev/null)
    if test "$container_status" = "exited"
        echo "🔄 Starting existing container..."
        docker start aesthetic
        sleep 2
    else if test -z "$container_status"
        # Container doesn't exist - need to build it first
        echo "🚀 Building devcontainer (this may take a moment)..."
        cd $workspace
        devcontainer up --workspace-folder . &
        set -l dc_pid $last_pid
        # Wait up to 120 seconds for container to be running
        set -l timeout 120
        while test $timeout -gt 0
            set container_status (docker inspect -f '{{.State.Status}}' aesthetic 2>/dev/null)
            if test "$container_status" = "running"
                break
            end
            sleep 2
            set timeout (math "$timeout - 2")
        end
        cd -
    end
    
    # Use attached-container approach (connects to running container by name)
    set -l hex_config (printf '{"containerName":"/aesthetic"}' | xxd -p | tr -d '\n')
    code --folder-uri "vscode-remote://attached-container+$hex_config/workspaces/aesthetic-computer" --remote-debugging-port=9222 --remote-allow-origins="*"
end

function ac-event-daemon
    # Check if ac-event-daemon is already running (via cargo watch)
    if not pgrep -f "cargo watch -x run --release" > /dev/null
        set daemon_dev_script "/home/jas/aesthetic-computer/ac-event-daemon/dev.fish"

        if test -f "$daemon_dev_script"
            sudo -E fish "$daemon_dev_script" "$HOME"
        else
            echo "⚠ Event daemon script not found"
        end
    end
end

# ═══════════════════════════════════════════════════════════════════════════════
# 🩸 AESTHETIC COMPUTER - Start Script
# ═══════════════════════════════════════════════════════════════════════════════

# AC color palette (matches prompt.mjs dark/light scheme + artery-tui)
function __ac_colors
    set -g RST (set_color normal)
    set -g BOLD (set_color --bold)
    set -g DIM (set_color --dim)
    # Dark mode palette (from prompt.mjs scheme.dark)
    set -g AC_MAGENTA (set_color --bold c81e64)  # [200, 30, 100] - prompt/block color
    set -g AC_PURPLE (set_color 6432c8)           # purple accent
    set -g AC_BLUE (set_color 387ade)             # [56, 122, 223] - light mode block
    # Standard terminal colors
    set -g FG_RED (set_color red)
    set -g FG_GREEN (set_color green)
    set -g FG_YELLOW (set_color yellow)
    set -g FG_CYAN (set_color cyan)
    set -g FG_WHITE (set_color white)
    set -g FG_GRAY (set_color brblack)
end

# Log a step with AC styling (concatenative, artery-tui inspired)
function __ac_log
    set -l icon $argv[1]
    set -l msg $argv[2]
    set -l color $argv[3]
    test -z "$color"; and set color "$AC_MAGENTA"
    printf "%s▌%s %s%s%s\n" "$color" "$RST" "$icon" " " "$msg"
end

function __ac_ok
    __ac_log "✓" "$argv[1]" "$FG_GREEN"
end

function __ac_err
    __ac_log "✗" "$argv[1]" "$FG_RED"
end

function __ac_info
    __ac_log "›" "$argv[1]" "$AC_MAGENTA"
end

function __ac_warn
    __ac_log "!" "$argv[1]" "$FG_YELLOW"
end

# Print the AC banner
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

# 🚀 MAIN START FUNCTION
function start
    __ac_colors
    __ac_banner
    
    # Kill VS Code gracefully first, then forcefully
    __ac_info "Stopping VS Code..."
    pkill code 2>/dev/null
    pkill code-insiders 2>/dev/null
    sleep 1
    # Force kill any remaining
    pkill -9 code 2>/dev/null
    pkill -9 code-insiders 2>/dev/null
    sleep 1
    __ac_ok "VS Code stopped"
    
    # Symlink prompts
    set -l ps "$HOME/aesthetic-computer/prompts"
    set -l pt "$HOME/.config/Code/User/prompts"
    if test -d "$ps"
        mkdir -p "$HOME/.config/Code/User"
        test -L "$pt"; and rm "$pt"
        test -d "$pt"; and rm -rf "$pt"
        ln -s "$ps" "$pt"
        __ac_ok "Prompts linked"
    end
    
    # SSL
    __ac_info "Starting SSL..."
    ac-ssl &
    __ac_ok "SSL started"
    
    # Dev container
    __ac_info "Opening dev container..."
    acd
    
    if test $status -eq 0
        __ac_ok "Dev container ready"
    else
        __ac_err "Dev container failed"
    end
    
    # Event daemon
    __ac_info "Starting event daemon..."
    ac-event-daemon $argv
    
    echo
    printf "%s  ✨ aesthetic.computer ready%s\n" "$AC_MAGENTA$BOLD" "$RST"
    echo
end

alias acw 'cd ~/aesthetic-computer/system; npm run watch'
alias platform 'cd ~/aesthetic-computer; npm run platform'


# set default editor to nvim
set -gx EDITOR emacs -nw

# include user binaries in the shell path
fish_add_path ~/.local/bin

# add rust binaries to the shell path
fish_add_path ~/.cargo/bin

# 📚 How to use rotation...
# add the user to the wheel group with `usermod -aG wheel me`
# visudo and make sure the user can skip the sudo password
# install ydotool
# `sudo dnf copr enable atim/ydotool`
# `sudo dnf install ydotool`
# install gnome-randr with `cargo install gnome-randr`

alias visudo 'sudo EDITOR=nvim visudo' # always use nvim for visudo

alias upgrade 'sudo dnf upgrade --refresh'

alias prune 'docker system prune -a --volumes'

alias chart 'foot -W 180x60 bpytop &'

function flip
    set rotation (gnome-randr query | grep -o "rotation: [a-z]*" | cut -d' ' -f2)
    if test $rotation = normal
        down
    else if test $rotation = inverted
        up
    else
        echo "Current rotation state is not handled: $rotation"
    end
end

# add android studio
fish_add_path /opt/android-studio/bin

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

# https://github.com/lotabout/skim/issues/3#issuecomment-272785980
set SKIM_DEFAULT_COMMAND 'git ls-tree -r --name-only HEAD || rg --files'

# The next line updates PATH for Netlify's Git Credential Helper.
test -f '/Users/jas/Library/Preferences/netlify/helper/path.fish.inc' && source '/Users/jas/Library/Preferences/netlify/helper/path.fish.inc'

# Set the keyboard repeat and delay in milliseconds. Be careful!
function keyrepeat
    set interval $argv[1]
    gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval $interval
end

function keydelay
    set delay $argv[1]
    gsettings set org.gnome.desktop.peripherals.keyboard delay $delay
end

# a shell-gpt shortcut (must be all lowercase / otherwise quoted)
function umm
    # Use string escape to handle special characters
    set -l args (string join " " $argv)

    # Pass the joined, escaped string to sgpt
    sgpt --chat umm "$args"
end

function umms
    sgpt --show-chat umm
end

function codegen
    if set -q argv[1]
        set -l args (string join " " $argv)
        sgpt --code --chat code "$args"
    else
        sgpt --code --editor --chat code
    end
end

function codes
    sgpt --show-chat code
end

function copy
    # Extract everything from the chat after the last "assistant: " line.
    set content (sgpt --show-chat code | tac | sed '/^assistant: /q' | tac | sed '1s/^assistant: //')
    printf "%s\n" $content | xclip -selection clipboard
end

function done
    rm /tmp/chat_cache/code 2>/dev/null
    echo "bye :)"
end

function ok
    rm /tmp/chat_cache/umm 2>/dev/null
    echo "bye :)"
end

function forget
    rm /tmp/chat_cache/umm 2>/dev/null
    echo "umm, i forgot :)"
end

alias nvm forget

# Install via: `sudo curl -Lo /usr/bin/theme.sh 'https://git.io/JM70M' && sudo chmod +x /usr/bin/theme.sh`
if type -q theme.sh
    if test -e ~/.theme_history
        theme.sh (theme.sh -l|tail -n1)
    end

    function TRAPUSR1 --on-signal USR1
        if test "$scheme" = night
            theme.sh dracula
        else if test "$scheme" = day
            theme.sh belafonte-day
        end
    end

    # Optional
    # Bind C-o to the last theme.
    function last_theme
        theme.sh (theme.sh -l|tail -n2|head -n1)
    end

    bind \co last_theme

    alias th='theme.sh -i'

    # Interactively load a light theme
    alias thl='theme.sh --light -i'

    # Interactively load a dark theme
    alias thd='theme.sh --dark -i'
end

set PATH /home/jas/.fnm $PATH
fnm env | source

# set -gx PATH $PATH $HOME/isomorphic_copy/bin
# set -gx DISPLAY FAKE
# set -gx ISOCP_USE_FILE 1

set -gx TERM xterm-256color
# OPS config
export OPS_DIR="$HOME/.ops"
export PATH="$HOME/.ops/bin:$PATH"
# source "$HOME/.ops/scripts/bash_completion.sh"

function silence_xhost
    xhost +local:docker >/dev/null 2>&1
end

silence_xhost

# Increase Node.js heap size
set -x NODE_OPTIONS "--max-old-space-size=8192"
set -x ELECTRON_EXTRA_ARGS "--max-old-space-size=8192"

# Set HOST_IP for use in the aesthetic devcontainer. 
set -x HOST_IP (hostname -I | awk '{print $1}')

# use tab to autocomplete the first suggestion
bind \t complete-select-first

#function fish_command_not_found
#    # Use string replace to strip special characters like '?' or '*' from the command
#    set cleaned_cmd (string replace -r '\*|\?' '' $argv[1])
#
#    if test -n "$cleaned_cmd"
#        echo "Wildcard match failed, showing tldr page for: $cleaned_cmd"
#        tldr $cleaned_cmd
#    else
#        # Default behavior for commands that can't be found
#        echo "Command not found: $argv[1]"
#    end
#end

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH

# ac-shop - Shopify CLI for Aesthetic Computer
function ac-shop
    node /workspaces/aesthetic-computer/ac-shop/shopify.mjs $argv
end

# ac-notify function - Send notifications to ac-event-daemon
function ac-notify
    if test (count $argv) -eq 0
        # Default success notification
        echo "prompt-complete:success" | nc -u 127.0.0.1 9999 2>/dev/null
    else
        # Custom notification type
        echo "prompt-complete:$argv[1]" | nc -u 127.0.0.1 9999 2>/dev/null
    end
end

# Hook into command success/failure for prompt notifications
function __notify_command_status --on-event fish_postexec
    # Only notify if ac-event-daemon is running
    if pgrep -f "ac-event-daemon" > /dev/null
        if test $status -eq 0
            ac-notify success
        else
            ac-notify error
        end
    end
end
