#!/usr/bin/fish

# Enhanced Tezos CLI setup with personal wallet support and beautiful JSON

echo "🔧 Enhanced Tezos CLI Setup"
echo ""

# Source JSON formatting functions
source format.fish

# Source personal wallet functions
source personal-wallet.fish

# Load environment variables
if test -f .env
    export (cat .env | grep -v '^#' | grep -v '^$' | xargs)
else
    echo "❌ No .env file found. Run 'devault.fish' from vault first."
    exit 1
end

# Set up basic Ghostnet connection
echo "🌐 Setting up Ghostnet connection..."
octez-client --endpoint https://ghostnet.ecadinfra.com config update

# Import admin key (test key) - only if not already exists
echo "🔑 Setting up admin test key..."
if not octez-client show address admin >/dev/null 2>&1
    octez-client import secret key admin unencrypted:edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq --force
end

# Import personal wallet if mnemonic is provided
if test -n "$PERSONAL_WALLET_MNEMONIC"
    echo "👤 Importing personal wallet from mnemonic..."
    if not octez-client show address personal >/dev/null 2>&1
        # For mnemonic import, we need to use a different approach
        echo "Setting up personal wallet... (this requires manual import)"
        echo "Run: octez-client import secret key personal"
        echo "Then paste your mnemonic when prompted"
    else
        echo "✅ Personal wallet already imported!"
        echo "   Address:" (octez-client show address personal)
        echo "   Balance:" (octez-client get balance for personal)
    end
else
    echo "ℹ️  No personal mnemonic provided (set PERSONAL_WALLET_MNEMONIC in vault/.env)"
end

echo ""
echo "🎨 JSON Display Tools Available:"
echo "   pretty_json     - Beautiful JSON display"
echo "   json_to_table   - Array to table format"  
echo "   compact_json    - Compact display for large objects"
echo ""

echo "📊 Available accounts:"
octez-client list known addresses

echo ""
echo "🚀 Enhanced setup complete!"
echo ""
echo "💡 Examples:"
echo "   octez-client get balance for personal | pretty_json"
echo "   octez-client rpc get /chains/main/blocks/head/header | pretty_json"
echo "   curl -s 'https://api.ghostnet.tzkt.io/v1/accounts/[address]' | pretty_json"
