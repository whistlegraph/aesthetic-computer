#!/usr/bin/env fish

# ⚠️ This script is for installing Aesthetic Computer development dependencies on
#    a Thinkpad running Fedora.

# Import the Microsoft GPG key for Visual Studio Code
rpm --import https://packages.microsoft.com/keys/microsoft.asc

# Add the Visual Studio Code repository
echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" | tee /etc/yum.repos.d/vscode.repo >/dev/null

# Update the package cache
dnf check-update

# Install dependencies
dnf install -y foot google-chrome code docker bpytop emacs rust cargo xxd
# todo: mkcert as well?

# Enable and start Docker service
systemctl enable docker
systemctl start docker

# Install Linuxbrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Add Linuxbrew to PATH
# set -U fish_user_paths /home/linuxbrew/.linuxbrew/bin $fish_user_paths
# Symlink dotfiles
bash ~/aesthetic-computer/dotfiles/symlink.sh

# reload fish configuration, which will add brew and cargo to path, etc.
source ~/.config/fish/config.fish

# Install gnome-randr using Cargo
cargo install gnome-randr

# TODO: ❓ Could I programmaticaly add my system shortcuts here? 24.08.21.21.43

brew install starship

brew install nvim

# Install fnm (Fast Node Manager)
brew install fnm

# Set the default Node.js version to most recent LTS
fnm install --lts
fnm default --lts

# Install devcontainer CLI globally using npm
npm install -g @devcontainers/cli

# todo: Install mkcert certificates locally
# mkcert -install

# Install system font (optional, depending on your needs)
mkdir -p ~/.local/share/fonts
cd ~/.local/share/fonts
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/FiraCode.zip
rm -rf FiraCode
mkdir FiraCode
unzip FiraCode.zip -d FiraCode
rm FiraCode.zip
cd -

# Install theme.sh tools
curl -Lo /usr/bin/theme.sh 'https://git.io/JM70M'
chmod +x /usr/bin/theme.sh`

# Install AC JavaScript system daemon for watching SSL and perhaps
# performing other tasks... on command, which could accept
# messages from the client.
# Is this still being used actively? 24.08.21.21.46
fish ~/aesthetic-computer/daemon/install_daemon.fish

echo "All development dependencies installed successfully."