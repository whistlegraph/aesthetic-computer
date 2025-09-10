#!/usr/bin/fish

# Tezos API Integration Suite
# Integrates major Tezos APIs and services for comprehensive blockchain exploration

echo "🌐 Tezos API Integration Suite"
echo "=============================="

# Load configuration
source configure.fish

# Set API endpoints
set -gx TZKT_API_MAINNET "https://api.tzkt.io"
set -gx TZKT_API_GHOSTNET "https://api.ghostnet.tzkt.io"
set -gx OBJKT_API "https://data.objkt.com/v3/graphql"
set -gx BETTER_CALL_DEV_API "https://api.better-call.dev/v1"
set -gx DIPDUP_API "https://metadata.dipdup.net"
set -gx TEZOS_DOMAINS_API "https://api.tezos.domains/graphql"

# Set current network APIs based on environment
if test "$TEZOS_ENVIRONMENT" = "development"
    set -gx CURRENT_TZKT_API $TZKT_API_GHOSTNET
    set -gx CURRENT_NETWORK "ghostnet"
    echo "🧪 Using Ghostnet APIs"
else
    set -gx CURRENT_TZKT_API $TZKT_API_MAINNET
    set -gx CURRENT_NETWORK "mainnet"
    echo "🌐 Using Mainnet APIs"
end

echo "✅ API endpoints configured"
echo "   TzKT: $CURRENT_TZKT_API"
echo "   Objkt: $OBJKT_API"
echo "   Better Call Dev: $BETTER_CALL_DEV_API"
echo "   DipDup: $DIPDUP_API"
echo "   Tezos Domains: $TEZOS_DOMAINS_API"
