#!/usr/bin/env fish
# with-env.fish
# Load environment variables from vault and run a command
# Usage: ./with-env.fish node create-account.mjs auth0|12345

set VAULT_ENV /workspaces/aesthetic-computer/aesthetic-computer-vault/at/.env

if not test -f $VAULT_ENV
    echo "❌ Vault env file not found: $VAULT_ENV"
    exit 1
end

# Export all variables from the .env file
for line in (cat $VAULT_ENV | grep -v '^#' | grep -v '^$')
    set parts (string split -m 1 = $line)
    if test (count $parts) -eq 2
        set -gx $parts[1] $parts[2]
    end
end

# Run the command
eval $argv
