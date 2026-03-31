#!/usr/bin/env fish
# Deploy Aesthetic Computer Tangled Knot
# Wraps deploy.sh with vault environment loading

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR (cd "$SCRIPT_DIR/../../../aesthetic-computer-vault" 2>/dev/null && pwd; or echo "")

echo "╔════════════════════════════════════════════════════════╗"
echo "║   Aesthetic Computer Knot Deployment (Tangled)        ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""

# Load knot-specific vault env
if test -f "$VAULT_DIR/at/knot.env"
    echo "✓ Loading knot config from vault..."
    for line in (cat "$VAULT_DIR/at/knot.env" | grep -v '^#' | grep -v '^$')
        set -l parts (string split '=' -- $line)
        if test (count $parts) -ge 2
            set -gx $parts[1] (string join '=' -- $parts[2..-1])
        end
    end
else
    echo "! Vault knot config not found: $VAULT_DIR/at/knot.env"
    echo ""
    echo "  Create it with:"
    echo "    KNOT_OWNER_DID=did:plc:your-did-here"
    echo ""
    echo "  Find your DID at https://tangled.org/settings"
    echo ""

    # Default to @aesthetic.computer DID (jeffrey)
    if test -z "$KNOT_OWNER_DID"
        set -gx KNOT_OWNER_DID "did:plc:k3k3wknzkcnekbnyde4dbatz"
        echo "  Using default DID: $KNOT_OWNER_DID"
    end
end

# Load shared deploy env (for Cloudflare creds, DO token, etc.)
if test -f "$VAULT_DIR/at/deploy.env"
    echo "✓ Loading shared deploy config..."
    for line in (cat "$VAULT_DIR/at/deploy.env" | grep -v '^#' | grep -v '^$')
        set -l parts (string split '=' -- $line)
        if test (count $parts) -ge 2
            set -gx $parts[1] (string join '=' -- $parts[2..-1])
        end
    end
end

# Export for bash script
export KNOT_OWNER_DID
export DROPLET_IP

echo ""
echo "→ Starting knot deployment..."
echo ""

set -x AUTO_CONFIRM yes

bash "$SCRIPT_DIR/deploy.sh" $argv
