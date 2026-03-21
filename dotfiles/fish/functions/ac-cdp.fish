#!/usr/bin/env fish
# ac-cdp - Fish shell commands for CDP management

function ac-cdp-list --description "List all CDP targets"
    node /workspaces/aesthetic-computer/artery/cdp-cli.mjs list
end

function ac-cdp-find --description "Find a CDP target by URL pattern"
    if test (count $argv) -eq 0
        echo "Usage: ac-cdp-find <url-pattern>"
        return 1
    end
    node /workspaces/aesthetic-computer/artery/cdp-cli.mjs find $argv[1]
end

function ac-cdp-connect --description "Test connection to a CDP target"
    set -l url $argv[1]
    if test -z "$url"
        set url "https://localhost:8888/prompt"
    end
    node /workspaces/aesthetic-computer/artery/cdp-cli.mjs connect $url
end

function ac-cdp-eval --description "Evaluate JavaScript in the connected page"
    if test (count $argv) -eq 0
        echo "Usage: ac-cdp-eval <expression>"
        return 1
    end
    node /workspaces/aesthetic-computer/artery/cdp-cli.mjs eval "$argv"
end

function ac-cdp-cache --description "Cache current aesthetic.computer page"
    node /workspaces/aesthetic-computer/artery/cdp-cli.mjs cache
end

function ac-cdp-test --description "Run CDP connection tests"
    node /workspaces/aesthetic-computer/artery/cdp-cli.mjs test
end

function ac-cdp-status --description "Show CDP tunnel and connection status"
    echo "🔍 CDP Status Check"
    echo ""
    
    # Check if CDP port is accessible
    if curl -s http://localhost:9333/json > /dev/null 2>&1
        echo "✅ CDP port 9333 is accessible"
        
        # Count targets
        set -l total (curl -s http://localhost:9333/json | jq '. | length')
        set -l ac_pages (curl -s http://localhost:9333/json | jq '[.[] | select(.url | contains("localhost:8888") or contains("aesthetic.computer"))] | length')
        
        echo "   Total targets: $total"
        echo "   AC pages: $ac_pages"
        
        if test $ac_pages -gt 0
            echo ""
            echo "🎨 Aesthetic Computer pages ready for connection"
        else
            echo ""
            echo "⚠️  No aesthetic.computer pages found"
            echo "   Make sure the app is open in VS Code Simple Browser"
        end
    else
        echo "❌ CDP port 9333 not accessible"
        echo "   Is VS Code running with --remote-debugging-port=9333?"
    end
    
    echo ""
    
    # Check cache
    if test -f /workspaces/aesthetic-computer/artery/.cdp-cache.json
        set -l cache_age (math (date +%s) - (stat -c %Y /workspaces/aesthetic-computer/artery/.cdp-cache.json))
        if test $cache_age -lt 300
            echo "💾 Cached target available (age: {$cache_age}s)"
        else
            echo "⏰ Cached target expired (age: {$cache_age}s)"
        end
    else
        echo "📭 No cached target"
    end
end

function ac-cdp-help --description "Show CDP command help"
    echo "CDP Management Commands for Aesthetic Computer"
    echo ""
    echo "Available commands:"
    echo "  ac-cdp-list        - List all available CDP targets"
    echo "  ac-cdp-find        - Find a specific target by URL pattern"
    echo "  ac-cdp-connect     - Test connection to a target"
    echo "  ac-cdp-eval        - Evaluate JavaScript in the page"
    echo "  ac-cdp-cache       - Cache current page for fast reconnection"
    echo "  ac-cdp-test        - Run connection tests"
    echo "  ac-cdp-status      - Show CDP tunnel and connection status"
    echo "  ac-cdp-boot-perf   - Show boot performance timeline"
    echo "  ac-cdp-help        - Show this help message"
    echo ""
    echo "Examples:"
    echo "  ac-cdp-list"
    echo "  ac-cdp-find prompt"
    echo "  ac-cdp-connect"
    echo "  ac-cdp-eval 'document.title'"
    echo "  ac-cdp-cache"
    echo "  ac-cdp-test"
    echo "  ac-cdp-status"
    echo "  ac-cdp-boot-perf"
end

function ac-cdp-boot-perf --description "Show boot performance timeline"
    node /workspaces/aesthetic-computer/artery/show-boot-perf.mjs
end

# Make ac-cdp an alias to ac-cdp-help
function ac-cdp --description "CDP management (show help)"
    ac-cdp-help
end
