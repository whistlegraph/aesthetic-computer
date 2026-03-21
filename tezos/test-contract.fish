#!/usr/bin/env fish
#
# Keeps Contract Test Suite
#
# Tests all contract entrypoints on Ghostnet
#

set -l CONTRACT_ADDRESS (cat contract-address.txt 2>/dev/null)

if test -z "$CONTRACT_ADDRESS"
    echo "❌ No contract address found. Run: node keeps.mjs deploy"
    exit 1
end

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║  🧪 Keeps Contract Test Suite                                ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""
echo "📍 Contract: $CONTRACT_ADDRESS"
echo "🌐 Network:  Ghostnet"
echo ""

set -l PASSED 0
set -l FAILED 0
set -l SKIPPED 0

function test_step
    set -l name $argv[1]
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "🔬 Test: $name"
    echo ""
end

function pass
    set -l msg $argv[1]
    echo "✅ PASS: $msg"
    echo ""
    set -g PASSED (math $PASSED + 1)
end

function fail
    set -l msg $argv[1]
    echo "❌ FAIL: $msg"
    echo ""
    set -g FAILED (math $FAILED + 1)
end

function skip
    set -l msg $argv[1]
    echo "⚠️  SKIP: $msg"
    echo ""
    set -g SKIPPED (math $SKIPPED + 1)
end

# ============================================================================
# Test 1: Contract Status
# ============================================================================
test_step "Contract Status Query"

node keeps.mjs status > /tmp/test-status.txt 2>&1
if test $status -eq 0
    pass "Contract status retrieved successfully"
    cat /tmp/test-status.txt | grep -E "Next Token ID|Administrator|Balance"
else
    fail "Failed to get contract status"
end

# ============================================================================
# Test 2: Check Balance
# ============================================================================
test_step "Wallet Balance Query"

node keeps.mjs balance > /tmp/test-balance.txt 2>&1
if test $status -eq 0
    pass "Wallet balance retrieved"
    cat /tmp/test-balance.txt | grep -E "Balance|Address"
else
    fail "Failed to get wallet balance"
end

# ============================================================================
# Test 3: List Tokens
# ============================================================================
test_step "Token Listing Query"

node keeps.mjs tokens > /tmp/test-tokens.txt 2>&1
if test $status -eq 0
    pass "Token list retrieved"
    cat /tmp/test-tokens.txt | tail -5
else
    fail "Failed to list tokens"
end

# ============================================================================
# Test 4: Check if piece exists (should be available)
# ============================================================================
test_step "Check Piece Availability (test-piece-99)"

# We'll add this command to keeps.mjs
echo "⚠️  SKIP: check-piece command not yet implemented"
set -g SKIPPED (math $SKIPPED + 1)
echo ""

# ============================================================================
# Test 5: Mint a test token
# ============================================================================
test_step "Mint Test Token (keep)"

echo "🚀 Attempting to mint test token..."
echo ""

# Create a simple test piece
set -l TEST_PIECE "test-"(date +%s)
echo "   Piece name: $TEST_PIECE"
echo ""

# Note: This will prompt for confirmation in interactive mode
echo "   Run manually to test: node keeps.mjs mint $TEST_PIECE"
skip "Minting requires manual confirmation (would take 30+ seconds)"

# ============================================================================
# Test 6: Fee Management (expected to fail on current contract)
# ============================================================================
test_step "Check Current Fee"

node keeps.mjs fee > /tmp/test-fee.txt 2>&1
if test $status -eq 0
    pass "Fee retrieved"
    cat /tmp/test-fee.txt | grep -E "Fee|Contract"
else
    echo "Expected to fail on contract without fee entrypoints"
    skip "Fee system not available (requires recompile)"
end

# ============================================================================
# Test 7: Set Fee (expected to fail)
# ============================================================================
test_step "Set Keep Fee (admin operation)"

skip "Requires fee entrypoints (contract needs recompile)"

# ============================================================================
# Test 8: Withdraw Fees (expected to fail)
# ============================================================================
test_step "Withdraw Fees (admin operation)"

skip "Requires fee entrypoints (contract needs recompile)"

# ============================================================================
# Test 9: Query via TzKT API
# ============================================================================
test_step "TzKT API - Contract Storage"

curl -s "https://api.ghostnet.tzkt.io/v1/contracts/$CONTRACT_ADDRESS/storage" > /tmp/test-tzkt-storage.json 2>&1
if test $status -eq 0
    pass "TzKT storage query successful"
    cat /tmp/test-tzkt-storage.json | jq -r '. | "Next ID: \(.next_token_id), Admin: \(.administrator)"'
else
    fail "TzKT API query failed"
end

# ============================================================================
# Test 10: Query Contract Entrypoints
# ============================================================================
test_step "TzKT API - Contract Entrypoints"

curl -s "https://api.ghostnet.tzkt.io/v1/contracts/$CONTRACT_ADDRESS/entrypoints" > /tmp/test-tzkt-entrypoints.json 2>&1
if test $status -eq 0
    pass "Entrypoints retrieved"
    echo "   Available entrypoints:"
    cat /tmp/test-tzkt-entrypoints.json | jq -r '.[].name' | sed 's/^/     - /'
    echo ""
    
    # Check for fee entrypoints
    if cat /tmp/test-tzkt-entrypoints.json | jq -e '.[] | select(.name == "set_keep_fee")' > /dev/null
        pass "Fee entrypoints present ✓"
    else
        echo "   ⚠️  Fee entrypoints missing (expected for current contract)"
    end
else
    fail "Failed to fetch entrypoints"
end

# ============================================================================
# Test 11: Query Token Metadata (if any tokens exist)
# ============================================================================
test_step "Token Metadata Query"

curl -s "https://api.ghostnet.tzkt.io/v1/tokens?contract=$CONTRACT_ADDRESS&limit=1" > /tmp/test-tokens.json 2>&1
set -l token_count (cat /tmp/test-tokens.json | jq 'length')

if test $token_count -gt 0
    pass "Token metadata retrieved (found $token_count tokens)"
    cat /tmp/test-tokens.json | jq -r '.[0] | "Token #\(.tokenId): \(.metadata.name // "unnamed")"'
else
    skip "No tokens minted yet"
end

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

if test $FAILED -eq 0
    echo "   🎉 All non-skipped tests passed!"
else
    echo "   ⚠️  Some tests failed"
    exit 1
end
