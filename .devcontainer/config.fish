# Aesthetic Computer Development Environment Fish Config (Docker)

if test -d /home/me/envs
    source /home/me/envs/load_envs.fish
    load_envs # Load devcontainer envs conditionally.
end

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

    # printf "Ask with 'umm' and forget with 'nvm'\n"
    # printf "or use 'code' and 'done' with 'copy'\n"
    # printf "to generate and get code.\n\n"
    # printf "🆕 Try 'aider' to make edits: https://github.com/paul-gauthier/aider?tab=readme-ov-file#usage\n\n"
    # printf "📋 Clipboard also requires `xhost +local:docker` to be set on the host."
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
    clear
    while not test -f /home/me/.waiter
        toilet "Configuring..." -f future | lolcat -x -r
        sleep 1
        clear
    end
    sudo rm /home/me/.waiter
    
    # Start emacs daemon if not running
    if not pgrep -f "emacs.*daemon" >/dev/null
        echo "Starting emacs daemon..."
        emacs -q --daemon -l ~/aesthetic-computer/dotfiles/dot_config/emacs.el
        while not emacsclient -e t >/dev/null 2>&1
            sleep 1
        end
    end
    
    # Connect to emacs with aesthetic-backend
    emacsclient -nw -c --eval '(aesthetic-backend (quote "status"))'
end

# TODO: Automatically kill online mode and go to offline mode if necessary.

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
alias ac-kidlisp 'ac; npm run test:kidlisp'
alias ac-session 'ac; npm run server:session'
alias ac-stripe-print 'ac; npm run stripe-print-micro'
alias ac-stripe-ticket 'ac; npm run stripe-ticket-micro'
alias ac-extension 'ac; cd vscode-extension; npm run build; ac'
# alias ac-shell 'ac; ac-url; ac-tunnel; fish'
# alias ac-offline 'ac; cd system/public; npx http-server -p 8888 -c-1 -g -b -S -C ../../ssl-dev/localhost.pem -K ../../ssl-dev/localhost-key.pem'
alias ac-redis 'clear; ac; npm run redis'
alias ac-udp 'ssh root@157.245.134.225' # ac monolith udp server management
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
                    ac-tunnel
                    return
                end
            end
        else
            echo "❌ ngrok error:"
            echo $err
        end
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
