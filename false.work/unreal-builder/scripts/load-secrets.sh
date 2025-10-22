#!/bin/bash
# Load secrets from vault for false.work UE5 builder operations
# Usage: source ./load-secrets.sh

VAULT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../aesthetic-computer-vault" && pwd)"
SECRETS_FILE="$VAULT_DIR/false.work/ue5-builder.env"

if [ ! -f "$SECRETS_FILE" ]; then
    echo "❌ Secrets file not found: $SECRETS_FILE"
    echo "Make sure aesthetic-computer-vault is available"
    return 1
fi

# Load secrets
set -a
source "$SECRETS_FILE"
set +a

echo "✓ Loaded secrets from vault"
echo "  VM: $VM_IP"
echo "  P4 Server: $P4_SERVER"
echo "  Project: $PROJECT_NAME"

# Set up gcloud authentication if service key exists
if [ -f "$VAULT_DIR/$GCP_SERVICE_ACCOUNT_KEY" ]; then
    export GOOGLE_APPLICATION_CREDENTIALS="$VAULT_DIR/$GCP_SERVICE_ACCOUNT_KEY"
    echo "✓ GCP service key configured"
fi
