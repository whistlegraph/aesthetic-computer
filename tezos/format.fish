#!/usr/bin/fish

# Simple JSON beauty formatter for Tezos
function pretty_json
    if command -v fx >/dev/null
        fx
    else
        jq -C .
    end
end

# Interactive fx viewer (if available)
function interactive_json
    if command -v fx >/dev/null
        fx
    else
        echo "❌ fx not available. Install with: npm install -g fx"
        jq -C .
    end
end

# Table formatter for arrays - use jq for reliable tables
function json_to_table
    jq -r '
        (.[0] | keys_unsorted) as $keys | 
        ($keys | @tsv),
        (.[] | [.[$keys[]] | tostring] | @tsv)' | column -t
end

# Tezos-specific table formatters
function tezos_accounts_table
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

function tezos_contracts_table  
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

# Compact JSON for large objects
function compact_json
    jq -C -c .
end
