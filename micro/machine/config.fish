# empty greeting
function fish_greeting
  printf "\n  🧩 hi $AESTHETIC welcome 2 aesthetic micro ;)"
  printf "\n     ask with 'umm' and forget with 'nvm'\n\n"
end

# rebuild the container after exiting with a special code ;)
alias reload 'exit 70' 

# set default editor to emacs
set -gx EDITOR emacs
set -gx PATH $PATH /home/me/.local/bin

# enable vi support
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_vi_force_cursor true
fish_vi_key_bindings

# add homebrew to path (only if we are on linux)
switch (uname)
  case Linux
    eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
end

# include user binaries in the shell path
fish_add_path ~/.local/bin

# add rust binaries to the shell path
fish_add_path ~/.cargo/bin

alias ac 'cd ~/aesthetic-computer'
alias ac-code 'cd ~/aesthetic-computer; clear; npm run code'
alias ac-session 'cd ~/aesthetic-computer; clear; npm run server:session'
alias ac-edge 'cd ~/aesthetic-computer; clear; npm run edge-micro'
alias ac-stripe-print 'cd ~/aesthetic-computer; clear; npm run stripe-print-micro'
alias ac-stripe-ticket 'cd ~/aesthetic-computer; clear; npm run stripe-ticket-micro'
alias ac-url 'cd ~/aesthetic-computer; clear; npm run url'

alias acw 'cd ~/aesthetic-computer/system; npm run watch'

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

alias nvm 'forget'

clear
