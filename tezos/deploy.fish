#!/usr/bin/fish

# Deploy KidLisp Contract to Tezos
# Usage: ./deploy.fish ghostnet|mainnet

if test (count $argv) -eq 0
    echo "Usage: ./deploy.fish <network>"
    echo "Networks: ghostnet, mainnet"
    exit 1
end

set network $argv[1]

# Load configuration
source configure.fish

# Check if sensitive env is available (from vault)
if not test -f .env
    echo "❌ No .env file found. Run 'devault.fish' from aesthetic-computer-vault to symlink sensitive environment variables."
    exit 1
end

# Load sensitive environment variables
export (cat .env | grep -v '^#' | xargs)

echo "🚀 Deploying KidLisp contract to $network..."

# Run the Node.js deployment script
node scripts/deploy.js --network $network
