# empty greeting
function fish_greeting
  printf "\n  🧩 welcome 2 aesthetic micro ;)"
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

# a shell-gpt shortcut (must be all lowercase / otherwise quoted)
function umm
    # Use string escape to handle special characters
    set -l args (string join " " $argv)

    # Pass the joined, escaped string to sgpt
    sgpt --chat umm "$args"
end

function forget
  rm /tmp/chat_cache/umm
  echo "umm, i forgot :)"
end

alias nvm 'forget'
