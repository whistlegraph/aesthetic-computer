#!/usr/bin/env fish
# Setup script for Tezos development environment

echo "🔧 Setting up Tezos development environment..."

# Install Python dependencies
echo "📦 Installing Python dependencies..."
pip install -r requirements.txt

# Verify installations
echo "✅ Verifying installations..."

# Check pytezos
python -c "import pytezos; print('✓ pytezos:', pytezos.__version__)" || echo "✗ pytezos failed"

# Check octez-client
if command -v octez-client &> /dev/null
    echo "✓ octez-client: "(octez-client --version | head -n 1)
else
    echo "✗ octez-client not found"
end

# Create .env if it doesn't exist
if not test -f .env
    echo "📝 Creating .env template..."
    echo "# Tezos Configuration" > .env
    echo "TEZOS_RPC_URL=https://mainnet.api.tez.ie" >> .env
    echo "TEZOS_TESTNET_RPC_URL=https://ghostnet.ecadinfra.com" >> .env
    echo "" >> .env
    echo "# Wallet keys (DO NOT COMMIT - keep in aesthetic-computer-vault)" >> .env
    echo "# KIDLISP_WALLET_ADDRESS=" >> .env
    echo "# KIDLISP_WALLET_PRIVATE_KEY=" >> .env
    echo "# KIDLISP_WALLET_MNEMONIC=" >> .env
    echo "">> .env
    echo "✓ Created .env template"
else
    echo "✓ .env already exists"
end

echo ""
echo "🎉 Tezos environment setup complete!"
echo "📖 Next steps:"
echo "   1. Run: python create_kidlisp_wallet.py"
echo "   2. Store keys in aesthetic-computer-vault"
echo "   3. Register kidlisp.tez domain"
