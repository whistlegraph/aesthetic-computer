#!/bin/bash

# Install Fish shell
sudo dnf install -y fish

# Change the default shell to Fish
chsh -s /usr/bin/fish

# Run the Fish script to install other software
fish /home/me/aesthetic-computer/utilities/thinkpad.fish

echo "Fish shell installed, set as default, and additional software installed."

echo "Please log out and log back in for the shell change to take effect."