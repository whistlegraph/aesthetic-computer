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

function acd
    ac
    # Kill any node instances that are running
    # if pgrep node >/dev/null
    #     pkill node
    # end
    # devcontainer build --workspace-folder .
    set containers (docker ps -q)
    if test -n "$containers"
        docker stop $containers
    end
    set container_id (pwd | tr -d '\n' | xxd -c 256 -p)
    set workspace_name (basename (pwd))
    code --folder-uri="vscode-remote://dev-container+$container_id/workspaces/$workspace_name"
    cd -
    # exit
end

function ac-event-daemon
    # Check if ac-event-daemon is already running (via cargo watch)
    if not pgrep -f "cargo watch -x run --release" > /dev/null
        echo "ac-event-daemon (via cargo-watch) not running. Starting..."
        
        set daemon_dev_script "/home/jas/aesthetic-computer/ac-event-daemon/dev.fish"

        if test -f "$daemon_dev_script"
            # Run the dev script
            # Ensure it's executable and then run it.
            # The dev.fish script itself handles cd'ing into the correct directory.
            # Use sudo -E to preserve the user environment, ensuring fish is found.
            # Pass the user's HOME directory as an argument to the script.
            sudo -E fish "$daemon_dev_script" "$HOME"
            echo "ac-event-daemon (via dev.fish) finished."
        else
            echo "Error: $daemon_dev_script not found."
        end
    else
        echo "ac-event-daemon (via cargo-watch) is already running."
    end
end

function start
    echo "✨ Starting Aesthetic Computer development environment..."
    
    # Kill any code instances that are runningdark-window
    if pgrep code >/dev/null
        echo "💀 Killing existing VS Code instances..."
        pkill code
        # Give VS Code a moment to shut down properly
        sleep 1
        echo "   ⏱️  Waiting for graceful shutdown..."
    end
    
    # Create symlink for prompts directory
    set prompts_source "$HOME/aesthetic-computer/prompts"
    set prompts_target "$HOME/.config/Code/User/prompts"
    
    if test -d "$prompts_source"
        # Create the Code/User directory if it doesn't exist
        mkdir -p "$HOME/.config/Code/User"
        
        # Remove existing symlink or directory if it exists
        if test -L "$prompts_target"
            rm "$prompts_target"
            echo "🗑️  Removed existing symlink"
        else if test -d "$prompts_target"
            rm -rf "$prompts_target"
            echo "🗑️  Removed existing directory"
        end
        
        # Create the symlink
        ln -s "$prompts_source" "$prompts_target"
        echo "🔗 Linked prompts directory to VS Code"
    else
        echo "⚠️  Prompts directory not found - skipping symlink"
    end
    
    echo "🔐 Starting SSL certificates..."
    ac-ssl &
    echo "📦 Opening dev container..."
    acd # &
    echo "🎨 aesthetic.computer is ready! Have fun creating! 🚀"
    # ac-event-daemon $argv
end

alias acw 'cd ~/aesthetic-computer/system; npm run watch'
alias platform 'cd ~/aesthetie-computer; npm run platform'


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
