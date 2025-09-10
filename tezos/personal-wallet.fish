#!/usr/bin/fish

# Personal Wallet Functions for aesthetic.tez
# Supports both mainnet and testnet operations

function setup_personal_wallet
    echo "👤 Setting up personal wallet: aesthetic.tez"
    echo "   Address: tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"
    echo ""
    
    # Import from private key if provided
    if test -n "$PERSONAL_WALLET_PRIVATE_KEY"
        echo "🔑 Importing wallet from private key..."
        
        # Check if already imported
        if octez-client show address aesthetic >/dev/null 2>&1
            set current_addr (octez-client show address aesthetic | grep -o 'tz1[A-Za-z0-9]*')
            if test "$current_addr" = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"
                echo "✅ Aesthetic wallet already imported correctly!"
            else
                echo "🔄 Re-importing with correct key..."
                octez-client forget address aesthetic --force >/dev/null 2>&1
                octez-client import secret key aesthetic "unencrypted:$PERSONAL_WALLET_PRIVATE_KEY" --force >/dev/null 2>&1
                echo "✅ Aesthetic wallet imported!"
            end
        else
            echo "📥 Importing aesthetic wallet..."
            octez-client import secret key aesthetic "unencrypted:$PERSONAL_WALLET_PRIVATE_KEY" --force >/dev/null 2>&1
            echo "✅ Aesthetic wallet imported!"
        end
        
        # Verify the import worked
        set imported_addr (octez-client show address aesthetic 2>/dev/null | grep -o 'tz1[A-Za-z0-9]*')
        if test "$imported_addr" = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"
            echo "🎉 Address verification: ✅ MATCH!"
        else
            echo "❌ Address mismatch: got $imported_addr"
        end
        
    else
        echo "ℹ️  No private key provided (set PERSONAL_WALLET_PRIVATE_KEY in vault/.env)"
    end
    
    echo ""
    echo "🧪 TESTNET WALLET:"
    if octez-client show address aesthetic-test >/dev/null 2>&1
        set testnet_addr (octez-client show address aesthetic-test | grep -o 'tz1[A-Za-z0-9]*')
        echo "   ✅ Ready: $testnet_addr"
        echo "   💰 Balance: "(octez-client get balance for aesthetic-test)
    else
        echo "   ❌ Not set up - run 'create_testnet_wallet'"
    end
    
    echo ""
    echo "🌐 Network endpoints:"
    echo "   Mainnet:  https://mainnet.api.tez.ie"  
    echo "   Ghostnet: https://ghostnet.ecadinfra.com"
end

# Check balance on both networks
function check_aesthetic_balance
    echo "💰 Checking aesthetic.tez balance on both networks..."
    echo ""
    
    echo "🌍 MAINNET:"
    curl -s "https://api.tzkt.io/v1/accounts/tz1gkf8EexComFBJvjtT1zdsisdah791KwBE" | jq -r '"Balance: " + ((.balance // 0) / 1000000 | tostring) + " tz"'
    echo ""
    
    echo "🧪 GHOSTNET:"
    curl -s "https://api.ghostnet.tzkt.io/v1/accounts/tz1gkf8EexComFBJvjtT1zdsisdah791KwBE" | jq -r '"Balance: " + ((.balance // 0) / 1000000 | tostring) + " tz"' 2>/dev/null || echo "Not active on Ghostnet (normal for mainnet addresses)"
    echo ""
end

# Get account info with beautiful formatting
function aesthetic_account_info
    set network $argv[1]
    
    if test -z "$network"
        set network "mainnet"
    end
    
    if test "$network" = "mainnet"
        set api_url "https://api.tzkt.io/v1/accounts/tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"
    else
        set api_url "https://api.ghostnet.tzkt.io/v1/accounts/tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"  
    end
    
    echo "👤 aesthetic.tez account info ($network):"
    echo ""
    
    curl -s "$api_url" | jq '{
        address,
        type,
        balance: ((.balance // 0) / 1000000 | tostring + " tz"),
        revealed,
        counter,
        numTransactions,
        numContracts,
        activeTokensCount,
        firstActivity: .firstActivityTime,
        lastActivity: .lastActivityTime
    }' | fx
end

# Create testnet wallet helper
function create_testnet_wallet
    echo "🧪 Creating testnet wallet for development..."
    echo ""
    
    # Switch to ghostnet
    octez-client --endpoint https://ghostnet.ecadinfra.com config update
    
    echo "1. Generate new testnet key:"
    octez-client gen keys aesthetic-test
    
    echo ""
    echo "2. Get the address:"
    set testnet_address (octez-client show address aesthetic-test | grep -o 'tz1[A-Za-z0-9]*')
    echo "   Testnet address: $testnet_address"
    
    echo ""
    echo "3. Get testnet funds:"
    echo "   Visit: https://faucet.ghostnet.teztnets.xyz/"
    echo "   Enter address: $testnet_address"
    echo "   Request testnet tez"
    
    echo ""
    echo "4. Verify funds received:"
    echo "   octez-client get balance for aesthetic-test"
end

# Quick wallet switcher
function use_mainnet
    octez-client --endpoint https://mainnet.api.tez.ie config update
    echo "🌍 Switched to MAINNET"
    echo "⚠️  WARNING: This is real money!"
end

function use_testnet  
    octez-client --endpoint https://ghostnet.ecadinfra.com config update
    echo "🧪 Switched to GHOSTNET (testnet)"
    echo "✅ Safe for development"
end

# Show current network
function current_network
    set endpoint (octez-client config show | jq -r .endpoint)
    if string match -q "*mainnet*" $endpoint
        echo "🌍 Current network: MAINNET ⚠️"
    else if string match -q "*ghostnet*" $endpoint  
        echo "🧪 Current network: GHOSTNET (testnet) ✅"
    else
        echo "❓ Current network: $endpoint"
    end
end

echo "👤 Personal wallet functions loaded!"
echo "Usage:"
echo "  setup_personal_wallet      - Import aesthetic.tez wallet"
echo "  check_aesthetic_balance     - Check balance on both networks"
echo "  aesthetic_account_info      - Full account info with fx"
echo "  create_testnet_wallet       - Set up testnet wallet"
echo "  use_mainnet / use_testnet   - Switch networks"
echo "  current_network             - Show active network"
