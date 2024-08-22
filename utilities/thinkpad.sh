#!/bin/bash

echo "🟠 Beginning \`Aesthetic Computer\` System Installation"

echo "🟣 Installing \`fish\` shell"

# Install Fish shell
sudo dnf install -y fish

echo "🟣 Setting \`fish\` shell as default"
# Change the default shell to Fish
chsh -s /usr/bin/fish me

# Run the Fish script to install other software
fish /home/me/aesthetic-computer/utilities/thinkpad.fish

echo "🟢 Installation complete"