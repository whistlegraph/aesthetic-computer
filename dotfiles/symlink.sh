#!/bin/sh

# -e: exit on error
# -u: exit on unset variables
set -eu

create_symlinks() {
  # Get the directory in which this script lives.
  script_dir=$(dirname "$(readlink -f "$0")")

  # link fish config
  rm -rf ~/.config/fish
  ln -s $script_dir/dot_config/fish ~/.config/fish

  # link vim config
  rm -rf ~/.config/nvim
  ln -s $script_dir/dot_config/nvim ~/.config/nvim

  # link starship config
  rm -rf ~/.config/starship.toml
  ln -s $script_dir/dot_config/starship.toml ~/.config/starship.toml

  # link foot (a linux terminal client) config
  rm -rf ~/.config/foot
  ln -s $script_dir/dot_config/foot ~/.config/foot

  # link emacs
  rm -rf ~/.emacs
  ln -s $script_dir/dot_config/emacs.el ~/.emacs
}

create_symlinks
