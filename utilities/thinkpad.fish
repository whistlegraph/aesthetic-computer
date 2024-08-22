#!/usr/bin/env fish

# ⚠️ This script is for installing Aesthetic Computer development dependencies on
#    a Thinkpad running Fedora.

echo "🟣 Installing VS Code package repositories"

# Import the Microsoft GPG key for Visual Studio Code
sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc

# Add the Visual Studio Code repository
echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" | sudo tee /etc/yum.repos.d/vscode.repo >/dev/null

# Update the package cache
dnf check-update

echo "🟣 Installing `foot`, `google-chrome`, `code`, `docker`, `bpytop`, `emacs`, `rust`, `cargo` and `xxd`"

# Install dependencies
sudo dnf install -y foot google-chrome code docker bpytop emacs rust cargo xxd
echo "🟣 Installing `dbus-devel`, `pkgconf-pkg-config`"
sudo dnf install -y dbus-devel pkgconf-pkg-config
# todo: mkcert as well?

echo "🟣 Enabling `docker` system services"

# Enable and start Docker service
sudo systemctl enable docker
sudo systemctl start docker

echo "🟣 Installing `brew`"
# Install Linuxbrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

echo "🟣 Symlinking dotfiles"
# Add Linuxbrew to PATH
# set -U fish_user_paths /home/linuxbrew/.linuxbrew/bin $fish_user_paths
# Symlink dotfiles
bash ~/aesthetic-computer/dotfiles/symlink.sh

echo "🟣 Installing `gnome-randr`, `starship`, `nvim`, and `fnm`"

# Install gnome-randr using Cargo
cargo install gnome-randr

# TODO: ❓ Could I programmaticaly add my system shortcuts here? 24.08.21.21.43

brew install starship

brew install nvim

# Install fnm (Fast Node Manager)
brew install fnm

echo "🟣 Sourcing fish config"
# reload fish configuration, which will add brew and cargo to path, etc.
source ~/.config/fish/config.fish

# Set the default Node.js version to most recent LTS
fnm install lts/iron
fnm default lts/iron

echo "🟣 Installing `devcontainer` cli"

# Install devcontainer CLI globally using npm
npm install -g @devcontainers/cli

# todo: Install mkcert certificates locally
# mkcert -install

echo "🟣 Installing `Fira Code` font"

# Install system font (optional, depending on your needs)
mkdir -p ~/.local/share/fonts
cd ~/.local/share/fonts
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/FiraCode.zip
rm -rf FiraCode
mkdir FiraCode
unzip FiraCode.zip -d FiraCode
rm FiraCode.zip
cd -

echo "🟣 Installing `theme.sh` utility"

# Install theme.sh tools
sudo curl -Lo /usr/bin/theme.sh 'https://git.io/JM70M'
sudo chmod +x /usr/bin/theme.sh`

echo "🟣 Installing Aesthetic Computer system daemon"
# Install AC JavaScript system daemon for watching SSL and perhaps
# performing other tasks... on command, which could accept
# messages from the client.
# Is this still being used actively? 24.08.21.21.46
fish ~/aesthetic-computer/daemon/install-daemon.fish
