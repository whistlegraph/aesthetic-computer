#!/usr/bin/env bash
# with-env.sh
# Load environment variables from vault and run a command
# Usage: ./with-env.sh node create-account.mjs "auth0|12345"

VAULT_ENV="/workspaces/aesthetic-computer/aesthetic-computer-vault/at/.env"

if [ ! -f "$VAULT_ENV" ]; then
    echo "❌ Vault env file not found: $VAULT_ENV"
    exit 1
fi

# Export all variables from the .env file (ignoring comments and empty lines)
while IFS='=' read -r key value || [ -n "$key" ]; do
    # Skip comments and empty lines
    [[ "$key" =~ ^#.*$ ]] && continue
    [[ -z "$key" ]] && continue
    
    # Remove leading/trailing whitespace
    key=$(echo "$key" | xargs)
    value=$(echo "$value" | xargs)
    
    # Export the variable
    export "$key=$value"
done < "$VAULT_ENV"

# Run the command
exec "$@"
