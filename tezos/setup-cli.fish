#!/usr/bin/fish

# Tezos CLI Setup Script
# Configures octez-client for aesthetic.computer development

echo "🔧 Setting up Tezos CLI tools..."

# Set up environment variables for Tezos
set -gx TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER Y

# Configure Ghostnet endpoint (testnet)
echo "🌍 Configuring Ghostnet (testnet) endpoint..."
octez-client --endpoint https://ghostnet.ecadinfra.com config update

# Import the test admin key if it doesn't exist
if not octez-client list known addresses | grep -q "admin"
    echo "🔑 Importing test admin key..."
    # This is the test key from our .env file
    echo "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq" | octez-client import secret key admin --force
end

# Show configuration
echo "✅ Tezos CLI configured!"
echo "📍 Current endpoint: "(octez-client config show | grep endpoint || echo "default")
echo "🔑 Available keys:"
octez-client list known addresses

echo ""
echo "🚀 Ready to explore Tezos! Try these commands:"
echo "   octez-client get balance for admin"
echo "   octez-client rpc get /chains/main/blocks/head"
echo "   octez-client list known contracts"
