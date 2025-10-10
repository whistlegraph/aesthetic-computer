#!/usr/bin/env fish
# Load credentials from vault and run bulk account creation

# Load environment variables from vault
set vault_env /workspaces/aesthetic-computer/aesthetic-computer-vault/at/.env

if not test -f $vault_env
    echo "❌ Vault .env not found: $vault_env"
    exit 1
end

# Export each variable from .env
for line in (cat $vault_env | grep -v "^\#" | grep -v "^\$")
    set parts (string split "=" $line)
    if test (count $parts) -eq 2
        set -x $parts[1] $parts[2]
    end
end

echo "✅ Loaded credentials from vault"
echo "🚀 Starting bulk account creation..."
echo ""

# Run the bulk creation script
node create-bulk-accounts.mjs 10000
