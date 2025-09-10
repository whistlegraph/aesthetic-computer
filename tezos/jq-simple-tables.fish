#!/usr/bin/fish

# Simple JQ table functions that work reliably

# Basic table - flatten all values to strings
function jq_simple_table
    jq -r '
        (.[0] | keys_unsorted) as $keys | 
        ($keys | @tsv),
        (.[] | [.[$keys[]] | tostring] | @tsv)'
end

# Aligned table using column
function jq_aligned_table  
    jq_simple_table | column -t
end

# Tezos-specific tables with selected fields

# Accounts table
function jq_accounts_table
    jq -r '
        ["ADDRESS", "TYPE", "BALANCE_TEZ", "ACTIVE", "COUNTER"],
        (.[] | [
            .address[0:25] + "...", 
            .type, 
            ((.balance // 0) / 1000000 | tostring), 
            .active // false, 
            .counter // 0
        ] | @tsv)' | column -t
end

# Contracts table  
function jq_contracts_table
    jq -r '
        ["ADDRESS", "ALIAS", "BALANCE_TEZ", "KIND", "TRANSACTIONS"],
        (.[] | [
            .address[0:25] + "...", 
            .alias // "N/A", 
            ((.balance // 0) / 1000000 | tostring), 
            .kind // "N/A",
            .numTransactions // 0
        ] | @tsv)' | column -t
end

# Operations table
function jq_operations_table
    jq -r '
        ["HASH", "TYPE", "LEVEL", "TIMESTAMP", "SENDER"],
        (.[] | [
            .hash[0:15] + "...", 
            .type, 
            .level, 
            .timestamp[0:19], 
            (.sender.address[0:10] + "..." // "N/A")
        ] | @tsv)' | column -t
end

# Tokens table
function jq_tokens_table
    jq -r '
        ["TOKEN_ID", "CONTRACT", "NAME", "SUPPLY", "HOLDERS"],
        (.[] | [
            .tokenId // "N/A", 
            .contract.address[0:15] + "...", 
            .metadata.name // "Unknown", 
            .totalSupply // "N/A", 
            .holdersCount // 0
        ] | @tsv)' | column -t
end

# Generic key-value table for any object
function jq_keyvalue_table
    jq -r 'to_entries | .[] | "\(.key): \(.value)"'
end

# CSV output
function jq_csv
    jq -r '
        (.[0] | keys_unsorted) as $keys | 
        ($keys | @csv),
        (.[] | [.[$keys[]] | tostring] | @csv)'
end

echo "📊 Simple JQ table functions loaded!"
echo "Usage examples:"
echo "  curl -s 'accounts_api' | jq_accounts_table"
echo "  curl -s 'contracts_api' | jq_contracts_table"  
echo "  curl -s 'operations_api' | jq_operations_table"
echo "  curl -s 'tokens_api' | jq_tokens_table"
