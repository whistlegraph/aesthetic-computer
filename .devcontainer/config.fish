# Aesthetic Computer Development Environment Fish Config (Docker)

# Set environment variables to prevent ETXTBSY errors
set -gx NETLIFY_CLI_TELEMETRY_DISABLED 1
set -gx NODE_DISABLE_COMPILE_CACHE 1

if test -d /home/me/envs
    source /home/me/envs/load_envs.fish
    load_envs # Load devcontainer envs conditionally.
end

# Add Deno to PATH
set -gx PATH /home/me/.deno/bin $PATH

# Enable `fnm` support.

if test -f $HOME/.fnm/fnm
    set -gx PATH $HOME/.fnm $PATH
    fnm env --use-on-cd --log-level=quiet | source
    fnm use lts-jod --silent-if-unchanged 2>/dev/null
end

# Symlink a VSCode workspace as needed.

if test -d /workspaces/aesthetic-computer
    if test ! -L ~/aesthetic-computer
        echo "Symlinking /workspaces/aesthetic-computer to ~"
        ln -s /workspaces/aesthetic-computer ~
    end
end

function fish_prompt
    echo -n '> '
end

function dns
    set -l domain $argv[1]
    echo "https://iwantmyname.com/dashboard/dns/$domain"
end

function fish_greeting
    if test "$nogreet" = true
        return
    end

    printf "🧩 Hi @$AESTHETIC!\n\n"
    
    # Show the new KidLisp source tree shortcut


    # printf "Ask with 'umm' and forget with 'nvm'\n"
    # printf "or use 'code' and 'done' with 'copy'\n"
    # printf "to generate and get code.\n\n"
    # printf "🆕 Try 'aider' to make edits: https://github.com/paul-gauthier/aider?tab=readme-ov-file#usage\n\n"
    # printf "📋 Clipboard also requires `xhost +local:docker` to be set on the host."
end

# KidLisp Source Tree Tool - allows running source-tree from anywhere
# Usage: st cow, st $cow, st cow --source, st --test-colors, or st --test-css-colors
function st
    # Check for special commands first
    if test "$argv[1]" = "--test-colors"
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs --test-colors
        return
    end
    
    if test "$argv[1]" = "--test-css-colors"
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs --test-css-colors
        return
    end
    
    if test "$argv[1]" = "--debug-colors"
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs --debug-colors
        return
    end
    
    set -l piece_name $argv[1]
    set -l extra_args $argv[2..-1]
    
    # Remove $ prefix if present
    if string match -q '$*' $piece_name
        set piece_name (string sub -s 2 $piece_name)
    end
    
    # Check if --source flag is explicitly provided
    set -l has_source false
    for arg in $extra_args
        if test "$arg" = "--source"
            set has_source true
            break
        end
    end
    
    # Run the source-tree tool with clean output (no Node.js warnings)
    if test $has_source = true
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs $piece_name $extra_args
    else
        node --no-warnings /workspaces/aesthetic-computer/kidlisp/tools/source-tree.mjs $piece_name $extra_args
    end
end

# Alternative function for those who prefer the $ syntax
function dollarpiece
    st $argv
end

# AC Pack - Package pieces for Teia with cover GIF generation
# Usage: ac-pack '$ceo' or ac-pack 'ceo' (from any directory)
function ac-pack
    if test (count $argv) -ne 1
        echo "Usage: ac-pack PIECE_NAME"
        echo "Example: ac-pack '\$ceo' or ac-pack 'ceo'"
        return 1
    end
    
    set -l piece_name $argv[1]
    set -l current_dir (pwd)
    
    # Store original directory
    echo "📂 Packaging $piece_name from $current_dir"
    
    # Run the ac-pack.mjs script from the teia directory
    cd /workspaces/aesthetic-computer/teia
    
    # Run the packaging with the piece name
    node ac-pack.mjs $piece_name
    
    # Check if packaging was successful
    if test $status -eq 0
        # Find the generated zip file and cover GIF
        set -l zip_files (find /workspaces/aesthetic-computer/teia/output -name "*.zip" -newer /workspaces/aesthetic-computer/teia/ac-pack.mjs 2>/dev/null)
        set -l cover_files (find /workspaces/aesthetic-computer/teia/output -name "*-cover.gif" -newer /workspaces/aesthetic-computer/teia/ac-pack.mjs 2>/dev/null)
        
        # Copy files to the original directory
        for zip_file in $zip_files
            if test -f $zip_file
                cp $zip_file $current_dir/
                echo "📦 Copied (basename $zip_file) to $current_dir"
            end
        end
        
        for cover_file in $cover_files
            if test -f $cover_file
                cp $cover_file $current_dir/
                echo "🖼️ Copied (basename $cover_file) to $current_dir"
            end
        end
        
        echo "✅ AC Pack complete! Files copied to $current_dir"
    else
        echo "❌ AC Pack failed with exit code $status"
    end
    
    # Return to original directory
    cd $current_dir
end

# always start in aesthetic-computer directory if there was a greeting
if not test "$nogreet" = true
    cd ~/aesthetic-computer
end

# rebuild the container after exiting with a special code ;)

# alias reload 'exit 70'

# reload fish config
alias reload 'source ~/.config/fish/config.fish'
alias refish 'source ~/.config/fish/config.fish'

# set default editor to emacs
set -gx TERM xterm-256color
set -gx EDITOR emacs
set -gx PATH $PATH /home/me/.local/bin

# add dotnet tools to path
set -Ux PATH $PATH $HOME/.dotnet/tools
# add stuff to path
# set -gx PATH $PATH $HOME/isomorphic_copy/bin
# use temporary clipboard file
# set -gx ISOCP_USE_FILE 1
# set -gx DISPLAY FAKE

set -gx DENO_INSTALL /home/me/.deno
set -gx PATH $PATH $DENO_INSTALL/bin

# enable vi support
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_vi_force_cursor true
fish_vi_key_bindings

# add homebrew to path (only if we are on linux)
#switch (uname)
#    case Linux
#        eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
#end

# include user binaries in the shell path
fish_add_path ~/.local/bin

# add rust binaries to the shell path
fish_add_path ~/.cargo/bin

# add nanos ops binaries to the shell path
fish_add_path ~/.ops/bin

# Assume the daemon is running when entering emacs.

# For fast config reloading - simple approach
function platform
    emacsclient -nw -c --eval '(aesthetic-backend (quote "status"))'
end

# ⏲️ Wait on `entry.fish` to touch the `.waiter` file.

function aesthetic
    # Check if --no-wait flag is passed
    if test "$argv[1]" = "--no-wait"
        echo "Skipping wait for .waiter file..."
    else
        clear
        set -l config_count 0
        while not test -f /home/me/.waiter
            set -l message
            if test (math $config_count % 2) -eq 0
                set message "Configuring..."
            else
                set message "Configuring. . ."
            end
            toilet $message -f future | lolcat -x -r
            sleep 0.25
            clear
            set config_count (math $config_count + 1)
        end
        sudo rm /home/me/.waiter
    end
    
    # Start emacs daemon if not running
    if not pgrep -f "emacs.*daemon" >/dev/null
        echo "Starting emacs daemon..."
        emacs -q --daemon -l ~/aesthetic-computer/dotfiles/dot_config/emacs.el &
        
        # Wait for daemon to be ready with timeout and better error handling
        set -l timeout 30
        set -l count 0
        echo "Waiting for emacs daemon to start..."
        
        while test $count -lt $timeout
            if emacsclient -e t >/dev/null 2>&1
                echo "✅ Emacs daemon is ready!"
                break
            end
            
            printf "⏳ Waiting for daemon (%d/%d)...\n" (math $count + 1) $timeout
            sleep 1
            set count (math $count + 1)
        end
        
        if test $count -eq $timeout
            echo "❌ Timeout waiting for emacs daemon to start"
            echo "Trying to start daemon again..."
            pkill -f "emacs.*daemon" 2>/dev/null
            sleep 2
            emacs -q --daemon -l ~/aesthetic-computer/dotfiles/dot_config/emacs.el &
            sleep 3
        end
    else
        echo "✅ Emacs daemon already running"
        # Double-check that it's actually responsive
        if not emacsclient -e t >/dev/null 2>&1
            echo "⚠️  Daemon found but not responsive, restarting..."
            pkill -f "emacs.*daemon" 2>/dev/null
            sleep 2
            emacs -q --daemon -l ~/aesthetic-computer/dotfiles/dot_config/emacs.el &
            sleep 3
        end
    end
    
    # Final check before connecting
    if not emacsclient -e t >/dev/null 2>&1
        echo "❌ Cannot connect to emacs daemon. Please check your emacs configuration."
        return 1
    end
    
    # Connect to emacs with aesthetic-backend
    echo "🚀 Connecting to aesthetic platform..."
    emacsclient -nw -c --eval '(aesthetic-backend (quote "status"))'
end

# Convenience alias for skipping the wait
function aesthetic-now
    aesthetic --no-wait
end

# Direct aesthetic function that skips waiting entirely
function aesthetic-direct
    echo "🚀 Starting aesthetic directly (no wait)..."
    
    # Start emacs daemon if not running
    if not pgrep -f "emacs.*daemon" >/dev/null
        echo "Starting emacs daemon..."
        emacs -q --daemon -l ~/aesthetic-computer/dotfiles/dot_config/emacs.el &
        
        # Wait with timeout
        set -l timeout 20
        set -l count 0
        while test $count -lt $timeout
            if emacsclient -e t >/dev/null 2>&1
                echo "✅ Emacs daemon ready!"
                break
            end
            printf "⏳ Waiting (%d/%d)...\n" (math $count + 1) $timeout
            sleep 1
            set count (math $count + 1)
        end
        
        if test $count -eq $timeout
            echo "❌ Timeout waiting for daemon"
            return 1
        end
    else
        echo "✅ Emacs daemon already running"
    end
    
    # Connect to emacs with aesthetic-backend
    emacsclient -nw -c --eval '(aesthetic-backend (quote "status"))'
end

# TODO: Automatically kill online mode and go to offline mode if necessary.

# Helper function to check emacs daemon status
function check-daemon
    if pgrep -f "emacs.*daemon" >/dev/null
        echo "✅ Emacs daemon process found"
        if emacsclient -e t >/dev/null 2>&1
            echo "✅ Emacs daemon is responsive"
        else
            echo "❌ Emacs daemon found but not responsive"
        end
    else
        echo "❌ No emacs daemon process found"
    end
end

# Helper function to restart emacs daemon
function restart-daemon
    echo "🔄 Restarting emacs daemon..."
    pkill -f "emacs.*daemon" 2>/dev/null
    sleep 2
    echo "🚀 Starting fresh daemon..."
    emacs -q --daemon -l ~/aesthetic-computer/dotfiles/dot_config/emacs.el &
    sleep 3
    check-daemon
end

function ac-site
    echo "🐱 Starting online mode..."
    ac
    npm run site
end

function ac-offline
    echo "🐭 Starting offline mode..."
    ac
    cd system/public
    cp system/offline-index.html system/public/index.html
    npx http-server -p 8888 -c-1 -g -b -S -C ../../ssl-dev/localhost.pem -K ../../ssl-dev/localhost-key.pem
end

function ac-url
    clear
    ac
    npm run -s url $argv
    fish
end

alias ac 'cd ~/aesthetic-computer'
alias watch 'ac; npm run watch' # check for new deployments
alias ac-watch 'ac; npm run watch'

# 📱 Dev log monitoring function with dynamic file detection
function ac-dev-log
    set log_dir "/tmp/dev-logs"
    
    if not test -d $log_dir
        echo "🔍 No dev logs directory found at $log_dir"
        echo "💡 Try touching/drawing on your iPhone to generate logs"
        return 1
    end
    
    echo "📱 Monitoring ALL device logs in real-time..."
    echo "🎨 Touch/draw on any device to see live logs..."
    echo "🔄 Watching for new files in: $log_dir"
    echo "----------------------------------------"
    
    # Kill any existing tail/inotify processes for clean start
    pkill -f "tail.*$log_dir" 2>/dev/null
    pkill -f "inotifywait.*$log_dir" 2>/dev/null
    
    # Function to start monitoring a single log file
    function monitor_log_file
        set logfile $argv[1]
        set log_name (basename "$logfile" .log)
        
        tail -f "$logfile" 2>/dev/null | while read line
            # Extract JSON part (everything after first '{')
            set json_start (string match -r '\{.*' "$line")
            if test -n "$json_start"
                # Try to format with jq for colors
                set formatted (echo "$json_start" | jq -C . 2>/dev/null)
                if test $status -eq 0
                    # Extract timestamp/prefix part
                    set prefix (string replace -r '\{.*' '' "$line")
                    echo "📱 $log_name: $prefix$formatted"
                else
                    echo "📱 $log_name: $line"  
                end
            else
                echo "📱 $log_name: $line"
            end
        end &
    end
    
    # Monitor existing log files
    for logfile in $log_dir/*.log
        if test -f "$logfile"
            echo "🔍 Found existing log: "(basename "$logfile")
            monitor_log_file "$logfile"
        end
    end
    
    # Monitor for new log files being created (fallback if inotify not available)
    if command -v inotifywait >/dev/null 2>&1
        echo "�️  Using inotifywait for new file detection"
        inotifywait -m -e create -e moved_to --format '%w%f' "$log_dir" 2>/dev/null | while read new_file
            if string match -q "*.log" "$new_file"
                echo "🆕 New log file detected: "(basename "$new_file")
                monitor_log_file "$new_file"
            end
        end &
    else
        echo "⏰ Using polling for new file detection (inotify not available)"
        # Fallback: poll for new files every 2 seconds
        while true
            for logfile in $log_dir/*.log
                if test -f "$logfile"
                    set log_name (basename "$logfile" .log)
                    # Check if we're already monitoring this file
                    if not pgrep -f "tail.*$logfile" >/dev/null
                        echo "🆕 New log file detected: $log_name"
                        monitor_log_file "$logfile"
                    end
                end
            end
            sleep 2
        end &
    end
    
    # Keep the function running with proper interrupt handling
    echo "✅ Monitoring started. Press Ctrl+C to stop."
    
    # Use read to wait for interrupt (Ctrl+C) - this is more reliable in Fish
    # The read command will be interrupted by Ctrl+C and return control
    echo "Press any key to stop monitoring, or Ctrl+C..."
    read -s
    
    # Cleanup on exit
    echo "🛑 Stopping all monitoring processes..."
    pkill -f "tail.*$log_dir" 2>/dev/null
    pkill -f "inotifywait.*$log_dir" 2>/dev/null
    echo "✅ Monitoring stopped"
end

# 📱 List all available device logs
function ac-dev-logs
    set log_dir "/tmp/dev-logs"
    
    if not test -d $log_dir
        echo "🔍 No dev logs directory found"
        return 1
    end
    
    echo "📱 Available device logs:"
    ls -la $log_dir/*.log 2>/dev/null | while read line
        echo "  $line"
    end
end

# 📱 Clean old device logs
function ac-dev-log-clean
    set log_dir "/tmp/dev-logs"
    
    if test -d $log_dir
        echo "🧹 Cleaning old device logs..."
        rm -f $log_dir/*.log
        echo "✨ Done!"
    else
        echo "🔍 No dev logs directory found"
    end
end

# 📱 Improved dev log monitoring with proper Ctrl+C support (test version)
function ac-dev-log-new
    set log_dir "/tmp/dev-logs"
    
    if not test -d $log_dir
        echo "🔍 No dev logs directory found at $log_dir"
        echo "💡 Try touching/drawing on your iPhone to generate logs"
        return 1
    end
    
    echo "📱 Monitoring ALL device logs in real-time..."
    echo "🎨 Touch/draw on any device to see live logs..."
    echo "🔄 Watching for new files in: $log_dir"
    echo "✅ Press Ctrl+C to stop monitoring"
    echo "----------------------------------------"
    
    # Kill any existing monitoring processes for clean start
    pkill -f "tail.*$log_dir" 2>/dev/null
    pkill -f "inotifywait.*$log_dir" 2>/dev/null
    
    # Trap Ctrl+C to clean up properly
    function cleanup_logs --on-signal INT
        echo ""
        echo "🛑 Stopping log monitoring..."
        pkill -f "tail.*$log_dir" 2>/dev/null
        pkill -f "inotifywait.*$log_dir" 2>/dev/null
        echo "✨ Monitoring stopped!"
        return 0
    end
    
    # Function to start monitoring a single log file
    function start_monitor
        set logfile $argv[1]
        set log_name (basename "$logfile" .log)
        
        tail -f "$logfile" 2>/dev/null | while read line
            # Extract JSON part (everything after first '{')
            set json_start (string match -r '\{.*' "$line")
            if test -n "$json_start"
                # Try to format with jq for colors
                set formatted (echo "$json_start" | jq -C . 2>/dev/null)
                if test $status -eq 0
                    # Extract timestamp/prefix part
                    set prefix (string replace -r '\{.*' '' "$line")
                    echo "📱 $log_name: $prefix$formatted"
                else
                    echo "📱 $log_name: $line"  
                end
            else
                echo "📱 $log_name: $line"
            end
        end &
    end
    
    # Start monitoring existing files
    set -l monitored_files
    for logfile in $log_dir/*.log
        if test -f "$logfile"
            echo "🔍 Found existing log: "(basename "$logfile")
            start_monitor "$logfile"
            set -a monitored_files "$logfile"
        end
    end
    
    # Simple loop that responds quickly to Ctrl+C
    if command -v inotifywait >/dev/null 2>&1
        echo "👁️  Using inotifywait for new file detection"
        
        # Watch for new files
        inotifywait -m -e create -e moved_to --format '%w%f' "$log_dir" 2>/dev/null &
        set inotify_pid $last_pid
        
        while true
            # Read from inotify if available
            if jobs -q %$inotify_pid
                # Process new files...
                # (simplified for testing)
            end
            
            # Check for new files manually as backup
            for logfile in $log_dir/*.log
                if test -f "$logfile"; and not contains "$logfile" $monitored_files
                    set log_name (basename "$logfile" .log)
                    echo "🆕 New log file detected: $log_name"
                    start_monitor "$logfile"
                    set -a monitored_files "$logfile"
                end
            end
            
            sleep 1
        end
    else
        echo "⏰ Using polling for file detection"
        
        while true
            # Check for new files
            for logfile in $log_dir/*.log
                if test -f "$logfile"; and not contains "$logfile" $monitored_files
                    set log_name (basename "$logfile" .log)
                    echo "🆕 New log file detected: $log_name"
                    start_monitor "$logfile"
                    set -a monitored_files "$logfile"
                end
            end
            
            sleep 2
        end
    end
end
# 📱 Simple improved dev log monitoring (test version)
function ac-dev-log-simple
    set log_dir "/tmp/dev-logs"
    
    if not test -d $log_dir
        echo "🔍 No dev logs directory found at $log_dir"
        return 1
    end
    
    echo "📱 Monitoring device logs..."
    echo "✅ Press Ctrl+C to stop"
    echo "----------------------------------------"
    
    # Kill existing processes
    pkill -f "tail.*$log_dir" 2>/dev/null
    
    # Simple approach - just monitor all existing files
    for logfile in $log_dir/*.log
        if test -f "$logfile"
            set log_name (basename "$logfile" .log)
            echo "🔍 Monitoring: $log_name"
            tail -f "$logfile" | while read line
                echo "📱 $log_name: $line"
            end &
        end
    end
    
    # Simple loop that can be interrupted
    echo "🔄 Running... (Ctrl+C to stop)"
    while true
        sleep 1
    end
end
# alias ac-kidlisp 'ac; npm run test:kidlisp'
alias ac-session 'ac; npm run server:session'
alias ac-stripe-print 'ac; npm run stripe-print-micro'
alias ac-stripe-ticket 'ac; npm run stripe-ticket-micro'
alias ac-extension 'ac; cd vscode-extension; npm run build; ac'

# kidlisp test function - supports watch mode or direct run
function ac-kidlisp
    ac
    if test "$argv[1]" = "watch"
        echo "🔍 Running kidlisp tests in watch mode..."
        npm run test:kidlisp
    else
        echo "🚀 Running kidlisp tests directly..."
        npm run test:kidlisp:direct
    end
end
# alias ac-shell 'ac; ac-url; ac-tunnel; fish'
# alias ac-offline 'ac; cd system/public; npx http-server -p 8888 -c-1 -g -b -S -C ../../ssl-dev/localhost.pem -K ../../ssl-dev/localhost-key.pem'
alias ac-redis 'clear; ac; npm run redis'
alias ac-udp 'ssh root@157.245.134.225' # ac monolith udp server management

# Send aesthetic.computer playlist to TV cast coordination
function ac-ff-playlist
    cd ~/aesthetic-computer
    ./rebroadcast.sh
end
alias ac-servers 'clear; ac; npm run -s servers; env nogreet=true fish'
alias ac-chat-system 'clear; ac; npm run -s chat; cd nanos; npm run chat-system:dev; fish'
alias ac-chat-sotce 'clear; ac; npm run -s chat; cd nanos; npm run chat-sotce:dev; fish'
alias ac-chat-clock 'clear; ac; npm run -s chat; cd nanos; npm run chat-clock:dev; fish'
alias ac-tunnel 'ac; npm run tunnel; fish'
alias ac-logger 'ac; cd system; npx netlify logs:function index'
alias sotce-net 'ac; cd system; npx netlify logs:function sotce-net'
alias acw 'cd ~/aesthetic-computer/system; npm run watch'

alias cat 'bat -p' # use bat for syntax highlighting instead of the `cat` default

# set up an ngrok tunnel

function ac-tunnel
    set tmp (mktemp)
    ngrok start --config ngrok.yml --all 2>$tmp
    set ngrok_exit $status
    set err (cat $tmp)
    rm -f $tmp

    if test $ngrok_exit -ne 0
        if string match -q '*ERR_NGROK_334*' $err
            clear
            echo "🟢 tunnel already online — watching..."
            while true
                sleep 5
                if not curl --silent --max-time 2 --output /dev/null https://local.aesthetic.computer
                    echo "🔁 tunnel down, restarting..."
                    # Kill any existing ngrok processes to ensure clean restart
                    pkill -f ngrok 2>/dev/null
                    sleep 2
                    ac-tunnel
                    return
                end
            end
        else
            echo "❌ ngrok error:"
            echo $err
        end
    else
        echo "🚀 tunnel started successfully!"
    end
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

function code
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

# Increase Node.js heap size

set -x NODE_OPTIONS "--max-old-space-size=4096"

# Set Google Application Credentials

set -x GOOGLE_APPLICATION_CREDENTIALS /home/me/aesthetic-computer/nanos/gcp-service.key.json

set -x PATH /google-cloud-sdk/bin $PATH

alias nvm forget

# use tab to autocomplete the first suggestion

bind \t complete-select-first

function clipboard
    set content $argv
    set -l hosts host.docker.internal $HOST_IP 172.17.0.1
    for host in $hosts
        echo "🧪 trying $host..."
        if echo "" | nc -z -w 0.15 $host 12345 2>/dev/null
            echo "✅ sending to $host"
            printf "%s\n" $content | nc $host 12345
            return
        end
    end
    echo "❌ clipboard: No reachable host for clipboard relay"
end

# Automatically reload the fish config when it changes. 25.04.28.00.12

# if status --is-interactive

#     function __fish_watch_config --description "Watch and reload config.fish on save"

#         while inotifywait --quiet -e close_write ~/.config/fish/config.fish

#             echo "🐟 Reloading config.fish..."

#             source ~/.config/fish/config.fish

#         end

#     end

#     set pidfile ~/.cache/fish-config-watcher.pid

#     if not test -e $pidfile; or not kill (cat $pidfile) ^/dev/null

#         __fish_watch_config & disown

#         echo $last_pid > $pidfile

#     end

# end
