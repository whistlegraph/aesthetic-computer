# empty greeting
function fish_greeting
  echo "🧩 welcome 2 aesthetic micro ;)"
end

# set default editor to emacs
set -gx EDITOR emacs

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
