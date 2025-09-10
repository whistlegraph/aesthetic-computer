#!/usr/bin/fish

# Tezos Network Explorer
# Demo script showing what we can do with our setup

echo "🌍 === Tezos Ghostnet Explorer Demo ==="
echo ""

# Show current setup
echo "📊 Current Configuration:"
echo "  Network: Ghostnet"
echo "  Admin Balance:" (octez-client get balance for admin)
echo "  Admin Address:" (octez-client show address admin)
echo ""

# Show network info
echo "🔗 Network Status:"
octez-client rpc get /chains/main/blocks/head/header | jq '{level: .level, timestamp: .timestamp, protocol: .protocol}'
echo ""

# Show what we can do next
echo "🚀 What you can do now:"
echo ""
echo "1. Deploy KidLisp contract:"
echo "   ./deploy.fish ghostnet"
echo ""
echo "2. Explore existing contracts:"
echo "   octez-client get contract storage for <address>"
echo ""  
echo "3. Generate new keys:"
echo "   octez-client gen keys mykey"
echo ""
echo "4. Get testnet tokens:"
echo "   Visit: https://faucet.ghostnet.teztnets.xyz/"
echo ""
echo "5. Inspect contract code:"
echo "   octez-client get script for <contract>"
echo ""

echo "📚 Ready to deploy and test KidLisp on Tezos!"
