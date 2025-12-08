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
    ac
    
    set containers (docker ps -q)
    if test -n "$containers"
        docker stop $containers >/dev/null 2>&1
    end
    
    set container_id (pwd | tr -d '\n' | xxd -c 256 -p)
    set workspace_name (basename (pwd))
    
    # Kill any existing socat forwarder for CDP
    pkill -f "socat.*9224" 2>/dev/null
    sleep 0.5
    
    # Forward CDP port from localhost:9222 (VS Code) to 0.0.0.0:9224 so container can reach it
    socat TCP-LISTEN:9224,bind=0.0.0.0,fork,reuseaddr TCP:127.0.0.1:9222 &
    
    # Launch VS Code with Chrome DevTools Protocol enabled
    code --remote-debugging-port=9222 --remote-allow-origins="*" --folder-uri="vscode-remote://dev-container+$container_id/workspaces/$workspace_name"
    cd -
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
# 🎰 AESTHETIC COMPUTER - VEGAS MODE LAUNCHER 🎰
# ═══════════════════════════════════════════════════════════════════════════════

# Color setup using fish's set_color (works great in foot)
function __ac_colors
    set -g RST (set_color normal)
    set -g BOLD (set_color --bold)
    set -g DIM (set_color --dim)
    # Foreground
    set -g FG_PINK (set_color f7a)
    set -g FG_PURPLE (set_color b9f)
    set -g FG_CYAN (set_color 8ef)
    set -g FG_GREEN (set_color 5fa)
    set -g FG_YELLOW (set_color ff5)
    set -g FG_ORANGE (set_color fa5)
    set -g FG_RED (set_color f55)
    set -g FG_WHITE (set_color fff)
    set -g FG_GRAY (set_color 678)
    set -g FG_MAGENTA (set_color f9d)
    set -g FG_BLACK (set_color 000)
    # Background (using printf escape for true color bg)
    set -g BG_PINK (printf '\e[48;2;255;121;198m')
    set -g BG_PURPLE (printf '\e[48;2;189;147;249m')
    set -g BG_CYAN (printf '\e[48;2;139;233;253m')
    set -g BG_GREEN (printf '\e[48;2;80;250;123m')
    set -g BG_YELLOW (printf '\e[48;2;241;250;140m')
    set -g BG_ORANGE (printf '\e[48;2;255;184;108m')
    set -g BG_RED (printf '\e[48;2;255;85;85m')
    set -g BG_MAGENTA (printf '\e[48;2;255;146;223m')
    set -g BG_BLACK (printf '\e[48;2;30;30;46m')
    set -g BG_RST (printf '\e[49m')
end

# Get terminal size (portable)
function __ac_size
    set -g ROWS (tput lines 2>/dev/null; or echo 24)
    set -g COLS (tput cols 2>/dev/null; or echo 80)
end

# Clear and reset
function __ac_clear
    printf '\e[2J\e[H'
end

# Move cursor
function __ac_move
    printf '\e[%d;%dH' $argv[1] $argv[2]
end

# Fill screen with bg color and multi-line centered block text
function __ac_flash_frame
    set -l bg $argv[1]
    __ac_size
    __ac_clear
    
    # Fill with background
    printf '%s' "$bg"
    for i in (seq 1 $ROWS)
        printf '%*s' $COLS '' 
        test $i -lt $ROWS; and printf '\n'
    end
    printf '%s\e[49m' "$RST"
end

# Display big block text centered on screen
function __ac_big_text
    set -l bg $argv[1]
    set -l fg $argv[2]
    set -l lines $argv[3..-1]
    
    __ac_size
    __ac_clear
    
    # Fill bg
    printf '%s' "$bg"
    for i in (seq 1 $ROWS)
        printf '%*s' $COLS ''
        test $i -lt $ROWS; and printf '\n'
    end
    
    # Calculate starting row to center the block
    set -l num_lines (count $lines)
    set -l start_row (math "floor(($ROWS - $num_lines) / 2)")
    
    # Print each line centered
    for i in (seq 1 $num_lines)
        set -l line $lines[$i]
        set -l line_len (string length -- "$line")
        set -l start_col (math "floor(($COLS - $line_len) / 2) + 1")
        __ac_move (math "$start_row + $i") $start_col
        printf '%s%s%s' "$fg" "$line" "$RST"
    end
    
    printf '\e[49m'
end

# 🎰 VEGAS STROBE INTRO with BIG TEXT
function __ac_vegas_intro
    __ac_colors
    __ac_size
    
    # Big block text for "aesthetic.computer"
    set -l ac1 '                    █████╗  ██████╗'
    set -l ac2 '                   ██╔══██╗██╔════╝'
    set -l ac3 '         ▄█████▄   ███████║██║     '
    set -l ac4 '         ▀▀▀▀▀▀▀   ██╔══██║██║     '
    set -l ac5 '                   ██║  ██║╚██████╗'
    set -l ac6 '                   ╚═╝  ╚═╝ ╚═════╝'
    set -l ac7 '    a e s t h e t i c . c o m p u t e r'
    
    # Strobe with big text - 6 flashes
    for i in (seq 1 6)
        switch (math "$i % 3")
            case 0
                __ac_big_text "$BG_PINK" "$FG_BLACK$BOLD" $ac1 $ac2 $ac3 $ac4 $ac5 $ac6 "" $ac7
            case 1
                __ac_big_text "$BG_PURPLE" "$FG_WHITE$BOLD" $ac1 $ac2 $ac3 $ac4 $ac5 $ac6 "" $ac7
            case 2
                __ac_big_text "$BG_CYAN" "$FG_BLACK$BOLD" $ac1 $ac2 $ac3 $ac4 $ac5 $ac6 "" $ac7
        end
        sleep 0.1
    end
    
    # Dramatic pause
    __ac_flash_frame "$BG_BLACK"
    sleep 0.2
end

# ASCII logo with gradient
function __ac_show_logo
    __ac_colors
    __ac_size
    __ac_clear
    printf '%s' "$BG_BLACK"
    
    # Fill bg
    for i in (seq 1 $ROWS)
        printf '%*s\n' $COLS ''
    end
    
    set -l logo \
        '   █████╗ ███████╗███████╗████████╗██╗  ██╗███████╗████████╗██╗ ██████╗' \
        '  ██╔══██╗██╔════╝██╔════╝╚══██╔══╝██║  ██║██╔════╝╚══██╔══╝██║██╔════╝' \
        '  ███████║█████╗  ███████╗   ██║   ███████║█████╗     ██║   ██║██║     ' \
        '  ██╔══██║██╔══╝  ╚════██║   ██║   ██╔══██║██╔══╝     ██║   ██║██║     ' \
        '  ██║  ██║███████╗███████║   ██║   ██║  ██║███████╗   ██║   ██║╚██████╗' \
        '  ╚═╝  ╚═╝╚══════╝╚══════╝   ╚═╝   ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚═╝ ╚═════╝'
    
    set -l colors $FG_PINK $FG_PINK $FG_MAGENTA $FG_PURPLE $FG_CYAN $FG_CYAN
    set -l start_row (math "floor($ROWS / 2) - 4")
    set -l logo_width 72
    set -l start_col (math "floor(($COLS - $logo_width) / 2) + 1")
    
    for i in (seq 1 6)
        __ac_move (math "$start_row + $i") $start_col
        printf '%s%s%s' "$colors[$i]" "$logo[$i]" "$RST"
        sleep 0.04
    end
    
    printf '\e[49m'
end

# System info panel
function __ac_show_info
    __ac_colors
    __ac_size
    
    set -l node_ver (node --version 2>/dev/null; or echo "N/A")
    set -l docker_ok (docker info >/dev/null 2>&1; and echo "$FG_GREEN●"; or echo "$FG_RED●")
    set -l branch (git -C ~/aesthetic-computer branch --show-current 2>/dev/null; or echo "N/A")
    set -l mem (free -h 2>/dev/null | awk '/^Mem:/ {print $3"/"$2}'; or echo "N/A")
    
    set -l info_row (math "floor($ROWS / 2) + 4")
    set -l info_col (math "floor($COLS / 2) - 20")
    
    __ac_move $info_row $info_col
    printf '%s┌────────────────────────────────────────┐%s' "$FG_GRAY" "$RST"
    
    __ac_move (math "$info_row + 1") $info_col
    printf '%s│%s %s▌%s OS      %s%-26s %s│%s' "$FG_GRAY" "$RST" "$FG_PINK" "$RST" "$FG_WHITE" (uname -o) "$FG_GRAY" "$RST"
    
    __ac_move (math "$info_row + 2") $info_col
    printf '%s│%s %s▌%s Node    %s%-26s %s│%s' "$FG_GRAY" "$RST" "$FG_PURPLE" "$RST" "$FG_WHITE" "$node_ver" "$FG_GRAY" "$RST"
    
    __ac_move (math "$info_row + 3") $info_col
    printf '%s│%s %s▌%s Docker  %s %-25s %s│%s' "$FG_GRAY" "$RST" "$FG_CYAN" "$RST" "$docker_ok" "running" "$FG_GRAY" "$RST"
    
    __ac_move (math "$info_row + 4") $info_col
    printf '%s│%s %s▌%s Branch  %s%-26s %s│%s' "$FG_GRAY" "$RST" "$FG_GREEN" "$RST" "$FG_GREEN" "$branch" "$FG_GRAY" "$RST"
    
    __ac_move (math "$info_row + 5") $info_col
    printf '%s│%s %s▌%s Memory  %s%-26s %s│%s' "$FG_GRAY" "$RST" "$FG_YELLOW" "$RST" "$FG_WHITE" "$mem" "$FG_GRAY" "$RST"
    
    __ac_move (math "$info_row + 6") $info_col
    printf '%s└────────────────────────────────────────┘%s' "$FG_GRAY" "$RST"
end

# Full-screen status flash with BIG TEXT
function __ac_status
    set -l icon $argv[1]
    set -l msg $argv[2]
    set -l bg $argv[3]
    
    __ac_colors
    __ac_size
    
    # Build big decorative frame around message
    set -l border_top    '╔══════════════════════════════════════════════════════════╗'
    set -l border_mid    '║                                                          ║'
    set -l border_bot    '╚══════════════════════════════════════════════════════════╝'
    set -l msg_line      "║            $icon   $msg   $icon            ║"
    
    # Flash twice
    for i in (seq 1 2)
        __ac_big_text "$bg" "$FG_WHITE$BOLD" "" $border_top $border_mid "$msg_line" $border_mid $border_bot ""
        sleep 0.12
        __ac_flash_frame "$BG_BLACK"
        sleep 0.06
    end
    
    # Hold
    __ac_big_text "$bg" "$FG_WHITE$BOLD" "" $border_top $border_mid "$msg_line" $border_mid $border_bot ""
    sleep 0.4
end

# Sparkle celebration with BIG finale
function __ac_celebrate
    __ac_colors
    __ac_size
    
    set -l sparkles ✦ ✧ ★ ☆ ✶ ✷ ❋ ❊
    set -l colors $FG_PINK $FG_MAGENTA $FG_PURPLE $FG_CYAN $FG_GREEN $FG_YELLOW
    
    # Big finale text
    set -l fin1 '═══════════════════════════════════════════════════════════'
    set -l fin2 '   ✨  a e s t h e t i c . c o m p u t e r  ✨'
    set -l fin3 '═══════════════════════════════════════════════════════════'
    set -l fin4 ''
    set -l fin5 '              🚀  H A V E   F U N   C R E A T I N G !  🚀'
    
    for frame in (seq 1 6)
        __ac_flash_frame "$BG_BLACK"
        
        # Random sparkles overlay
        for s in (seq 1 25)
            set -l r (random 1 $ROWS)
            set -l c (random 1 $COLS)
            set -l sp $sparkles[(random 1 8)]
            set -l clr $colors[(random 1 6)]
            __ac_move $r $c
            printf '%s%s%s' "$clr" "$sp" "$RST"
        end
        
        # Center message block
        set -l mid (math "floor($ROWS / 2)")
        set -l col (math "floor(($COLS - 59) / 2)")
        
        __ac_move (math "$mid - 2") $col
        printf '%s%s%s' "$FG_GRAY" "$fin1" "$RST"
        __ac_move (math "$mid - 1") $col
        printf '%s%s%s' "$FG_PINK$BOLD" "$fin2" "$RST"
        __ac_move $mid $col
        printf '%s%s%s' "$FG_GRAY" "$fin3" "$RST"
        __ac_move (math "$mid + 2") (math "floor(($COLS - 55) / 2)")
        printf '%s%s%s' "$FG_CYAN$BOLD" "$fin5" "$RST"
        
        sleep 0.12
    end
    
    # Final clean frame
    __ac_flash_frame "$BG_BLACK"
    
    set -l mid (math "floor($ROWS / 2)")
    set -l col (math "floor(($COLS - 59) / 2)")
    
    __ac_move (math "$mid - 2") $col
    printf '%s%s%s' "$FG_GRAY" "$fin1" "$RST"
    __ac_move (math "$mid - 1") $col
    printf '%s%s%s' "$FG_PINK$BOLD" "$fin2" "$RST"
    __ac_move $mid $col
    printf '%s%s%s' "$FG_GRAY" "$fin3" "$RST"
    __ac_move (math "$mid + 2") (math "floor(($COLS - 55) / 2)")
    printf '%s%s%s' "$FG_CYAN$BOLD" "$fin5" "$RST"
    __ac_move (math "$mid + 5") (math "floor(($COLS - 45) / 2)")
    printf '%sCtrl+C to stop  •  reload to refresh config%s' "$FG_GRAY" "$RST"
    
    __ac_move $ROWS 1
    printf '\e[49m\n'
end

# 🚀 MAIN START FUNCTION
function start
    __ac_colors
    
    # IMMEDIATELY kill VS Code - no mercy!
    pkill -9 code 2>/dev/null
    pkill -9 code-insiders 2>/dev/null
    
    # 🎰 VEGAS INTRO!
    __ac_vegas_intro
    
    # Show logo
    __ac_show_logo
    sleep 0.3
    
    # System info
    __ac_show_info
    sleep 0.8
    
    # Status: VS Code killed
    __ac_status "💀" "VS CODE TERMINATED" "$BG_RED"
    
    # Prompts symlink
    set -l ps "$HOME/aesthetic-computer/prompts"
    set -l pt "$HOME/.config/Code/User/prompts"
    if test -d "$ps"
        mkdir -p "$HOME/.config/Code/User"
        test -L "$pt"; and rm "$pt"
        test -d "$pt"; and rm -rf "$pt"
        ln -s "$ps" "$pt"
    end
    
    __ac_status "🔗" "PROMPTS LINKED" "$BG_PURPLE"
    
    # SSL
    __ac_status "🔐" "STARTING SSL" "$BG_CYAN"
    ac-ssl &
    
    # Dev container
    __ac_show_logo
    __ac_size
    __ac_move (math "floor($ROWS / 2) + 4") (math "floor(($COLS - 30) / 2)")
    printf '%s📦 OPENING DEV CONTAINER...%s' "$FG_YELLOW$BOLD" "$RST"
    
    acd
    
    __ac_status "✅" "DEV CONTAINER READY" "$BG_GREEN"
    
    # 🎉 CELEBRATE!
    __ac_celebrate
    
    # Event daemon
    ac-event-daemon $argv
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
