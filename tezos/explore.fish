#!/usr/bin/fish

# Tezos Network Explorer Script
# Helpful commands for exploring Tezos Ghostnet

echo "🔍 Tezos Ghostnet Explorer"
echo "========================"

function explore_contract
    set contract_addr $argv[1]
    echo "📋 Exploring contract: $contract_addr"
    echo "Storage:"
    octez-client get contract storage for $contract_addr
    echo ""
    echo "Script:"
    octez-client get script $contract_addr | head -20
    echo ""
end

function get_fa2_contracts
    echo "🎨 Looking for FA2 token contracts on Ghostnet..."
    echo "   (You can find these on https://ghostnet.tzkt.io/)"
    echo ""
    echo "   Some known FA2 contracts on Ghostnet:"
    echo "   - KT1... (add real contract addresses here)"
    echo ""
end

function check_balance_all
    echo "💰 Account balances:"
    octez-client list known addresses | while read line
        set name (echo $line | cut -d: -f1)
        set addr (echo $line | cut -d: -f2 | xargs)
        echo "$name ($addr):"
        octez-client get balance for $name
        echo ""
    end
end

function network_info
    echo "🌍 Network Information:"
    set header (octez-client rpc get /chains/main/blocks/head/header)
    echo "Chain ID: NetXnHfVqm9iesp (Ghostnet)" 
    echo "Current Block: "(echo $header | jq -r '.level')
    echo "Protocol: "(echo $header | jq -r '.protocol' | cut -c1-10)"..."
    echo "Timestamp: "(echo $header | jq -r '.timestamp')
end

# Show available functions
echo "Available commands:"
echo "  network_info          - Show network information"
echo "  check_balance_all     - Check all account balances"  
echo "  get_fa2_contracts     - List known FA2 contracts"
echo "  explore_contract ADDR - Explore a specific contract"
echo ""
echo "Example usage:"
echo "  source explore.fish"
echo "  network_info"
echo "  explore_contract KT1SomeContractAddress"
