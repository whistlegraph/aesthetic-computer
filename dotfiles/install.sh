#!/bin/sh

# todo: install_emacs and auto-link emacs/init.el 

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

  # emacs
  ln -s $script_dir/dot_config/emacs.el ~.emacs
}

install_fish() {
  sudo add-apt-repository ppa:fish-shell/release-3 -y
  sudo apt-get update
  sudo apt-get install fish -y
}

install_neovim() {
  sudo add-apt-repository ppa:neovim-ppa/unstable -y
  sudo apt-get update
  sudo apt-get install neovim -y
}

install_emacs() {
  sudo apt-get install emacs -y
}

install_homebrew() {
  NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' >> /home/codespace/.profile
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
  sudo apt-get install build-essential
  brew install gcc
}

install_starship() {
  brew install starship
}

install_fnm() {
  brew install fnm
}

install_mkcert() {
  brew install mkcert
}

install_fonts() {
  mkdir -p ~/.local/share/fonts
  cd ~/.local/share/fonts
  wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip 
  rm -rf FiraCode
  mkdir FiraCode
  unzip FiraCode.zip -d FiraCode 
  rm FiraCode.zip
  cd -
}

install_fish
install_neovim
install_emacs
install_homebrew
install_starship
install_fnm
install_mkcert
install_fonts

create_symlinks
