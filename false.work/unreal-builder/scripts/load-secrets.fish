# Load secrets from vault for false.work UE5 builder operations
# Usage: source ./load-secrets.fish

set VAULT_DIR (cd (dirname (status --current-filename))/../../../aesthetic-computer-vault; and pwd)
set SECRETS_FILE $VAULT_DIR/false.work/ue5-builder.env

if not test -f $SECRETS_FILE
    echo "❌ Secrets file not found: $SECRETS_FILE"
    echo "Make sure aesthetic-computer-vault is available"
    return 1
end

# Load secrets (fish style)
for line in (cat $SECRETS_FILE | grep -v '^#' | grep '=')
    set -gx (echo $line | cut -d= -f1) (echo $line | cut -d= -f2-)
end

echo "✓ Loaded secrets from vault"
echo "  VM: $VM_IP"
echo "  P4 Server: $P4_SERVER"
echo "  Project: $PROJECT_NAME"

# Set up gcloud authentication if service key exists
if test -f $VAULT_DIR/$GCP_SERVICE_ACCOUNT_KEY
    set -gx GOOGLE_APPLICATION_CREDENTIALS $VAULT_DIR/$GCP_SERVICE_ACCOUNT_KEY
    echo "✓ GCP service key configured"
end
