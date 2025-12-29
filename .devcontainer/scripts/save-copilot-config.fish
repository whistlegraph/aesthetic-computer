#!/usr/bin/env fish
# Save Copilot CLI config to vault for persistence across container rebuilds

set vault_dest /home/me/aesthetic-computer/aesthetic-computer-vault/home/.copilot

if test -f ~/.copilot/config.json
    mkdir -p $vault_dest
    cp -f ~/.copilot/config.json $vault_dest/config.json
    chmod 600 $vault_dest/config.json
    echo "✅ Copilot CLI config saved to vault"
else
    echo "❌ No ~/.copilot/config.json found - run copilot and use /login first"
    exit 1
end
