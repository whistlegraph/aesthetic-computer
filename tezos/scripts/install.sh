#!/bin/bash

# Install Tezos Development Tools
# This script sets up the complete Tezos development environment

set -e

echo "🚀 Setting up Tezos development environment..."

# Check if we're in the right directory
if [ ! -f "package.json" ]; then
    echo "❌ Error: Run this script from the /tezos directory"
    exit 1
fi

# Install Node.js dependencies
echo "📦 Installing Node.js dependencies..."
npm install

# Check if SmartPy is installed
if ! command -v smartpy &> /dev/null; then
    echo "🐍 SmartPy CLI not found. Installing..."
    
    # Detect OS
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        # Linux
        echo "🐧 Detected Linux"
        wget https://smartpy.io/cli/install.sh -O - | bash
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        echo "🍎 Detected macOS"
        curl https://smartpy.io/cli/install.sh | bash
    else
        echo "⚠️ Unsupported OS. Please install SmartPy manually:"
        echo "   Visit: https://smartpy.io/cli"
        echo "   Or use: curl https://smartpy.io/cli/install.sh | bash"
    fi
else
    echo "✅ SmartPy CLI already installed"
fi

# Check if octez-client is available (optional)
if ! command -v octez-client &> /dev/null; then
    echo "⚠️ octez-client not found (optional)"
    echo "   Install from: https://tezos.gitlab.io/introduction/howtoget.html"
    echo "   Or use package manager:"
    echo "     Ubuntu/Debian: sudo apt install octez-client"
    echo "     macOS: brew install tezos"
else
    echo "✅ octez-client found"
fi

# Create .env file if it doesn't exist
if [ ! -f ".env" ]; then
    echo "📝 Creating .env file from template..."
    cp .env.example .env
    echo "   Please edit .env with your configuration"
else
    echo "✅ .env file exists"
fi

# Create output directory for compiled contracts
echo "📁 Creating output directories..."
mkdir -p output
mkdir -p deployments

# Build TypeScript
echo "🔨 Building TypeScript..."
npm run build

# Test SmartPy installation
echo "🧪 Testing SmartPy installation..."
if command -v smartpy &> /dev/null; then
    echo "🐍 SmartPy version: $(smartpy --version)"
    
    # Test contract compilation
    echo "🧪 Testing contract compilation..."
    npm run compile:contract
    
    if [ $? -eq 0 ]; then
        echo "✅ Contract compilation successful"
    else
        echo "❌ Contract compilation failed"
        exit 1
    fi
else
    echo "❌ SmartPy installation failed"
    exit 1
fi

echo ""
echo "🎉 Tezos development environment setup complete!"
echo ""
echo "Next steps:"
echo "1. Edit .env with your configuration"
echo "2. Run 'npm run test:contract' to test the smart contract"
echo "3. Deploy to testnet: 'npm run deploy:ghostnet'"
echo "4. Integrate with store-kidlisp.js"
echo ""
echo "Documentation:"
echo "- README: ../README.md"
echo "- TODO: ./TODO.md"
echo "- SmartPy docs: https://smartpy.io/docs"
echo "- Taquito docs: https://taquito.io/docs"
