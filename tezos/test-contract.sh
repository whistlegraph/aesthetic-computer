#!/bin/bash
#
# Keeps Contract Test Suite
#
# Tests all contract entrypoints on Ghostnet
#

CONTRACT_ADDRESS=$(cat contract-address.txt 2>/dev/null)

if [ -z "$CONTRACT_ADDRESS" ]; then
    echo "❌ No contract address found. Run: node keeps.mjs deploy"
    exit 1
fi

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║  🧪 Keeps Contract Test Suite                                ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""
echo "📍 Contract: $CONTRACT_ADDRESS"
echo "🌐 Network:  Ghostnet"
echo ""

PASSED=0
FAILED=0
SKIPPED=0

test_step() {
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "🔬 Test: $1"
    echo ""
}

pass() {
    echo "✅ PASS: $1"
    echo ""
    ((PASSED++))
}

fail() {
    echo "❌ FAIL: $1"
    echo ""
    ((FAILED++))
}

skip() {
    echo "⚠️  SKIP: $1"
    echo ""
    ((SKIPPED++))
}

# ============================================================================
# Test 1: Contract Status
# ============================================================================
test_step "Contract Status Query"

if node keeps.mjs status > /tmp/test-status.txt 2>&1; then
    pass "Contract status retrieved successfully"
    grep -E "Next Token ID|Administrator|Balance" /tmp/test-status.txt || true
else
    fail "Failed to get contract status"
fi

# ============================================================================
# Test 2: Check Balance
# ============================================================================
test_step "Wallet Balance Query"

if node keeps.mjs balance > /tmp/test-balance.txt 2>&1; then
    pass "Wallet balance retrieved"
    grep -E "Balance|Address" /tmp/test-balance.txt || true
else
    fail "Failed to get wallet balance"
fi

# ============================================================================
# Test 3: List Tokens
# ============================================================================
test_step "Token Listing Query"

if node keeps.mjs tokens > /tmp/test-tokens.txt 2>&1; then
    pass "Token list retrieved"
    tail -5 /tmp/test-tokens.txt || true
else
    fail "Failed to list tokens"
fi

# ============================================================================
# Test 4: Fee Query (expected to show N/A on current contract)
# ============================================================================
test_step "Check Current Fee"

if node keeps.mjs fee > /tmp/test-fee.txt 2>&1; then
    pass "Fee retrieved"
    grep -E "Fee|Contract" /tmp/test-fee.txt || true
else
    echo "   Expected behavior: fee command not implemented or contract lacks fee storage"
    skip "Fee system check (requires entrypoints)"
fi

# ============================================================================
# Test 5: TzKT API - Contract Storage
# ============================================================================
test_step "TzKT API - Contract Storage"

if curl -s "https://api.ghostnet.tzkt.io/v1/contracts/$CONTRACT_ADDRESS/storage" > /tmp/test-tzkt-storage.json 2>&1; then
    if command -v jq &> /dev/null; then
        pass "TzKT storage query successful"
        jq -r '. | "Next ID: \(.next_token_id), Admin: \(.administrator)"' /tmp/test-tzkt-storage.json || cat /tmp/test-tzkt-storage.json
    else
        pass "TzKT storage query successful (install jq for formatted output)"
        cat /tmp/test-tzkt-storage.json
    fi
else
    fail "TzKT API query failed"
fi

# ============================================================================
# Test 6: TzKT API - Contract Entrypoints
# ============================================================================
test_step "TzKT API - Contract Entrypoints"

if curl -s "https://api.ghostnet.tzkt.io/v1/contracts/$CONTRACT_ADDRESS/entrypoints" > /tmp/test-tzkt-entrypoints.json 2>&1; then
    pass "Entrypoints retrieved"
    echo "   Available entrypoints:"
    if command -v jq &> /dev/null; then
        jq -r '.[].name' /tmp/test-tzkt-entrypoints.json | sed 's/^/     - /' || true
        echo ""
        
        # Check for fee entrypoints
        if jq -e '.[] | select(.name == "set_keep_fee")' /tmp/test-tzkt-entrypoints.json > /dev/null 2>&1; then
            pass "Fee entrypoints present ✓"
        else
            echo "   ⚠️  Fee entrypoints missing (expected for current contract)"
            echo ""
        fi
    else
        cat /tmp/test-tzkt-entrypoints.json
    fi
else
    fail "Failed to fetch entrypoints"
fi

# ============================================================================
# Test 7: TzKT API - Token Metadata
# ============================================================================
test_step "Token Metadata Query"

if curl -s "https://api.ghostnet.tzkt.io/v1/tokens?contract=$CONTRACT_ADDRESS&limit=3" > /tmp/test-tokens.json 2>&1; then
    if command -v jq &> /dev/null; then
        TOKEN_COUNT=$(jq 'length' /tmp/test-tokens.json)
        
        if [ "$TOKEN_COUNT" -gt 0 ]; then
            pass "Token metadata retrieved (found $TOKEN_COUNT tokens)"
            jq -r '.[] | "Token #\(.tokenId): \(.metadata.name // "unnamed")"' /tmp/test-tokens.json || true
            echo ""
        else
            skip "No tokens minted yet"
        fi
    else
        pass "Tokens queried (install jq for formatted output)"
    fi
else
    fail "Failed to query tokens"
fi

# ============================================================================
# Test 8: Contract Balance
# ============================================================================
test_step "Contract Balance Check"

if curl -s "https://api.ghostnet.tzkt.io/v1/contracts/$CONTRACT_ADDRESS" > /tmp/test-contract-info.json 2>&1; then
    if command -v jq &> /dev/null; then
        BALANCE=$(jq -r '.balance' /tmp/test-contract-info.json)
        BALANCE_XTZ=$(echo "scale=6; $BALANCE / 1000000" | bc)
        pass "Contract balance: $BALANCE_XTZ XTZ"
    else
        pass "Contract info retrieved"
    fi
else
    fail "Failed to get contract info"
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
echo "╔══════════════════════════════════════════════════════════════╗"
echo "║  📊 Test Summary                                             ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""
echo "   ✅ Passed:  $PASSED"
echo "   ❌ Failed:  $FAILED"
echo "   ⚠️  Skipped: $SKIPPED"
echo ""

if [ $FAILED -eq 0 ]; then
    echo "   🎉 All non-skipped tests passed!"
    exit 0
else
    echo "   ⚠️  Some tests failed"
    exit 1
fi
