#!/usr/bin/fish

# Tezos Web3 Suite Demonstration
# Shows off all the integrated APIs and tools

echo "🚀 Tezos Web3 Suite Demonstration"
echo "=================================="
echo ""

# Load the toolkit
source tezos-toolkit.fish

echo "📊 Current Network Status:"
echo "--------------------------"
tezos-status
echo ""

echo "🔍 Exploring a Famous FA2 Contract (hic et nunc):"
echo "------------------------------------------------"
set hen_contract "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton"
echo "Contract: $hen_contract"
tzkt-contract $hen_contract | jq '{alias: .alias, balance: .balance, tokens: .activeTokensCount}'
echo ""

echo "🎨 Checking Objkt for Recent NFTs:"
echo "----------------------------------"
objkt-token "1" | jq '{name: .name, creators: .creators}' 2>/dev/null || echo "Token not found or API rate limited"
echo ""

echo "🏷️  Testing Tezos Domains:"
echo "--------------------------"
tezos-domain "alice.tez" 2>/dev/null || echo "Domain lookup (may be rate limited)"
echo ""

echo "🛠️  Available CLI Tools:"
echo "------------------------"
echo "✅ octez-client - Tezos CLI client"
echo "✅ TzKT API - Blockchain explorer"
echo "✅ Objkt API - NFT marketplace data"
echo "✅ Better Call Dev - Contract analysis"
echo "✅ Tezos Domains - Name resolution"
echo ""

echo "📋 Quick Reference:"
echo "------------------"
echo "npm run help     - Show all commands"
echo "npm run status   - Check API status"  
echo "npm run network  - Show network info"
echo "npm run deploy   - Deploy KidLisp contract"
echo ""

echo "🔧 Development Commands:"
echo "-----------------------"
echo "tzkt-account <addr>      - Get account info"
echo "analyze-fa2 <addr>       - Analyze FA2 contract"
echo "find-kidlisp-contracts   - Search for KidLisp contracts"
echo "tezos-domain <name>      - Resolve domain"
echo ""

echo "✨ Ready for KidLisp development on Tezos!"
