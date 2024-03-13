#/usr/bin/env fish

# Send a welcome message!
echo "*** Aesthetic Computer Setup ***"

# Authenticate with Github.
gh auth login

# Clone the aesthetic-computer-vault repository.
git clone https://github.com/whistlegraph/aesthetic-computer-vault

# TODO: Apply the 'vault' credentials to the mounted aesthetic-computer volume.

# Clone the aesthetic-computer repository.
# git clone https://github.com/whistlegraph/aesthetic-computer

# Initialize fnm and use the specified Node.js version.
cd aesthetic-computer

# Install latest npm version before doing anything.
npm install -g npm@latest --no-fund --no-audit --silent

# Install npm packages for emacs support.
npm install -g prettier typescript-language-server javascript-typescript-langserver

# Install Node.js dependencies
npm install --no-fund --no-audit

cd ..

clear
