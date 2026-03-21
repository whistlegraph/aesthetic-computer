#!/usr/bin/env fish

# Script to programmatically set ports 8888 and 8889 to public in GitHub Codespaces
# This runs on container startup to ensure the ports are always public

# Check if we're running in a Codespace
if test -z "$CODESPACE_NAME"
    echo "⚠️  Not running in a GitHub Codespace, skipping port visibility setup"
    exit 0
end

echo "🔓 Setting ports 8888 and 8889 to public visibility..."

# Function to set a port to public
function set_single_port_public
    set -l port $argv[1]
    set -l max_attempts 60
    set -l attempt 0
    
    while test $attempt -lt $max_attempts
        # Try using gh CLI without GH_TOKEN (it uses its own auth)
        if env -u GH_TOKEN gh codespace ports visibility $port:public -c $CODESPACE_NAME 2>/dev/null
            echo "✅ Port $port set to public"
            return 0
        end
        
        set attempt (math $attempt + 1)
        
        # Log progress every 10 seconds
        if test (math $attempt % 10) -eq 0
            echo "⏳ Waiting for port $port to be forwarded... ($attempt/$max_attempts seconds)"
        end
        
        sleep 1
    end
    
    echo "⚠️  Could not set port $port to public after 60 seconds"
    return 1
end

# Function to set both ports to public
function set_ports_public
    set -l success_count 0
    
    # Set port 8888 (main site)
    if set_single_port_public 8888
        set success_count (math $success_count + 1)
    end
    
    # Set port 8889 (session server)
    if set_single_port_public 8889
        set success_count (math $success_count + 1)
    end
    
    if test $success_count -eq 2
        echo "✅ All ports configured successfully"
        return 0
    else
        echo "⚠️  Some ports could not be configured"
        echo "📝 Please manually set ports to public in the Ports panel"
        echo "📝 Or run: env -u GH_TOKEN gh codespace ports visibility PORT:public -c $CODESPACE_NAME"
        return 1
    end
end

# Run the function in the background so it doesn't block container startup
set_ports_public &
