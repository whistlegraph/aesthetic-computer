#!/usr/bin/fish

# KidLisp Tezos Configuration
# This script sets up configuration for KidLisp on Tezos
# Usage: source configure.fish

echo "🔧 Configuring KidLisp Tezos environment..."

# KidLisp Configuration (baked into deployment)
set -gx KIDLISP_COIN_NAME "KidLisp"
set -gx KIDLISP_COIN_SYMBOL "KIDLISP"
set -gx CREATOR_ROYALTY_PERCENTAGE "500"  # 5% royalty
set -gx MINTING_FEE "100000"  # 0.1 tez in mutez
set -gx DEFAULT_MINT_AMOUNT "1000000"  # 1 token with 6 decimals

# Environment Detection
if test "$CONTEXT" = "dev"
    set -gx TEZOS_ENVIRONMENT "development"
    echo "🌍 Environment: DEVELOPMENT (Ghostnet)"
else
    set -gx TEZOS_ENVIRONMENT "production"  
    echo "🌍 Environment: PRODUCTION (Mainnet)"
end

echo "✅ KidLisp configuration loaded"
echo "   Coin: $KIDLISP_COIN_NAME ($KIDLISP_COIN_SYMBOL)"
echo "   Royalty: "(math $CREATOR_ROYALTY_PERCENTAGE / 100)"%"
echo "   Mint fee: "(math $MINTING_FEE / 1000000)" tez"
