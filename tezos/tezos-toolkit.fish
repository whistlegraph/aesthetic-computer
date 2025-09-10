#!/usr/bin/fish

# Tezos API Toolkit
# Comprehensive functions for interacting with Tezos APIs

# Load API configuration
source setup-apis.fish

echo "🔧 Tezos API Toolkit Loaded"
echo "Type 'tezos-help' for available commands"

# Help function
function tezos-help
    echo "🔧 Tezos API Toolkit Commands"
    echo "============================"
    echo ""
    echo "📊 TzKT API (Blockchain Explorer):"
    echo "  tzkt-account <address>     - Get account info"
    echo "  tzkt-contract <address>    - Get contract details"
    echo "  tzkt-operations <address>  - Get recent operations"
    echo "  tzkt-tokens <address>      - Get token balances"
    echo "  tzkt-search <query>        - Search blockchain"
    echo ""
    echo "🎨 Objkt API (NFT Marketplace):"
    echo "  objkt-token <id>           - Get token details"
    echo "  objkt-collection <id>      - Get collection info"
    echo "  objkt-creator <address>    - Get creator's works"
    echo "  objkt-search <query>       - Search NFTs"
    echo ""
    echo "🔍 Better Call Dev (Contract Explorer):"
    echo "  bcd-contract <address>     - Get contract analysis"
    echo "  bcd-entrypoints <address>  - Get contract entrypoints"
    echo "  bcd-storage <address>      - Get contract storage"
    echo ""
    echo "🏷️  Tezos Domains:"
    echo "  tezos-domain <name>        - Resolve domain to address"
    echo "  reverse-domain <address>   - Get domain for address"
    echo ""
    echo "🛠️  Utility:"
    echo "  tezos-status               - Check all API status"
    echo "  tezos-network              - Show current network info"
end

# TzKT API Functions
function tzkt-account
    if test (count $argv) -eq 0
        echo "Usage: tzkt-account <address>"
        return 1
    end
    curl -s "$CURRENT_TZKT_API/v1/accounts/$argv[1]" | jq '.'
end

function tzkt-contract
    if test (count $argv) -eq 0
        echo "Usage: tzkt-contract <address>"
        return 1
    end
    curl -s "$CURRENT_TZKT_API/v1/contracts/$argv[1]" | jq '.'
end

function tzkt-operations
    if test (count $argv) -eq 0
        echo "Usage: tzkt-operations <address>"
        return 1
    end
    curl -s "$CURRENT_TZKT_API/v1/accounts/$argv[1]/operations?limit=10" | jq '.'
end

function tzkt-tokens
    if test (count $argv) -eq 0
        echo "Usage: tzkt-tokens <address>"
        return 1
    end
    curl -s "$CURRENT_TZKT_API/v1/tokens/balances?account=$argv[1]&limit=20" | jq '.'
end

function tzkt-search
    if test (count $argv) -eq 0
        echo "Usage: tzkt-search <query>"
        return 1
    end
    curl -s "$CURRENT_TZKT_API/v1/suggest/accounts/$argv[1]" | jq '.'
end

# Better Call Dev API Functions
function bcd-contract
    if test (count $argv) -eq 0
        echo "Usage: bcd-contract <address>"
        return 1
    end
    curl -s "$BETTER_CALL_DEV_API/contract/$CURRENT_NETWORK/$argv[1]" | jq '.'
end

function bcd-entrypoints
    if test (count $argv) -eq 0
        echo "Usage: bcd-entrypoints <address>"
        return 1
    end
    curl -s "$BETTER_CALL_DEV_API/contract/$CURRENT_NETWORK/$argv[1]/entrypoints" | jq '.'
end

function bcd-storage
    if test (count $argv) -eq 0
        echo "Usage: bcd-storage <address>"
        return 1
    end
    curl -s "$BETTER_CALL_DEV_API/contract/$CURRENT_NETWORK/$argv[1]/storage" | jq '.'
end

# Objkt API Functions (GraphQL)
function objkt-token
    if test (count $argv) -eq 0
        echo "Usage: objkt-token <token_id>"
        return 1
    end
    set query "query { token(where: {token_id: {_eq: \"$argv[1]\"}}) { token_id name description artifact_uri creators { creator_address } } }"
    curl -s -X POST -H "Content-Type: application/json" \
        -d "{\"query\": \"$query\"}" \
        "$OBJKT_API" | jq '.data.token[0]'
end

function objkt-creator
    if test (count $argv) -eq 0
        echo "Usage: objkt-creator <address>"
        return 1
    end
    set query "query { token(where: {creators: {creator_address: {_eq: \"$argv[1]\"}}}, limit: 10) { token_id name artifact_uri } }"
    curl -s -X POST -H "Content-Type: application/json" \
        -d "{\"query\": \"$query\"}" \
        "$OBJKT_API" | jq '.data.token'
end

# Tezos Domains Functions
function tezos-domain
    if test (count $argv) -eq 0
        echo "Usage: tezos-domain <domain.tez>"
        return 1
    end
    set query "query { domain(name: \"$argv[1]\") { name address } }"
    curl -s -X POST -H "Content-Type: application/json" \
        -d "{\"query\": \"$query\"}" \
        "$TEZOS_DOMAINS_API" | jq '.data.domain'
end

function reverse-domain
    if test (count $argv) -eq 0
        echo "Usage: reverse-domain <address>"
        return 1
    end
    set query "query { reverseRecord(address: \"$argv[1]\") { domain { name } } }"
    curl -s -X POST -H "Content-Type: application/json" \
        -d "{\"query\": \"$query\"}" \
        "$TEZOS_DOMAINS_API" | jq '.data.reverseRecord.domain.name'
end

# Utility Functions
function tezos-status
    echo "🔍 Checking API Status..."
    echo ""
    echo "TzKT API ($CURRENT_TZKT_API):"
    curl -s "$CURRENT_TZKT_API/v1/head" | jq '{level: .level, timestamp: .timestamp}' || echo "❌ Failed"
    echo ""
    echo "Better Call Dev API:"
    curl -s "$BETTER_CALL_DEV_API/stats/$CURRENT_NETWORK" | jq '{contracts: .contracts, operations: .operations}' || echo "❌ Failed"
    echo ""
    echo "Objkt API:"
    curl -s -X POST -H "Content-Type: application/json" \
        -d '{"query": "query { token(limit: 1) { token_id } }"}' \
        "$OBJKT_API" | jq '.data' && echo "✅ OK" || echo "❌ Failed"
end

function tezos-network
    echo "🌐 Current Network Configuration"
    echo "==============================="
    echo "Network: $CURRENT_NETWORK"
    echo "TzKT API: $CURRENT_TZKT_API"
    echo "RPC Endpoint: $TEZOS_RPC_URL_GHOSTNET"
    echo ""
    octez-client config show
end

# FA2 Contract Analysis Functions
function analyze-fa2
    if test (count $argv) -eq 0
        echo "Usage: analyze-fa2 <contract_address>"
        return 1
    end
    echo "🔍 Analyzing FA2 Contract: $argv[1]"
    echo "=================================="
    echo ""
    echo "📋 Contract Info:"
    tzkt-contract $argv[1] | jq '{address: .address, kind: .kind, balance: .balance, numTransactions: .numTransactions}'
    echo ""
    echo "🎯 Entrypoints:"
    bcd-entrypoints $argv[1] | jq '.[].name' | head -10
    echo ""
    echo "💾 Recent Operations:"
    tzkt-operations $argv[1] | jq '.[0:3] | .[] | {type: .type, amount: .amount, timestamp: .timestamp}'
end

# KidLisp specific functions
function find-kidlisp-contracts
    echo "🔍 Searching for KidLisp-style contracts..."
    tzkt-search "KidLisp" | jq '.'
    echo ""
    echo "🔍 Searching for Meme Coin contracts..."
    curl -s "$CURRENT_TZKT_API/v1/contracts?kind=smart_contract&limit=20" | jq '.[] | select(.alias != null) | {address: .address, alias: .alias}' | grep -i meme || echo "No meme contracts found with aliases"
end
