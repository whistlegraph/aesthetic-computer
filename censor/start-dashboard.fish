#!/usr/bin/env fish

# Content Filter Dashboard Startup Script

echo "🛡️  Starting Content Filter Dashboard..."
echo ""

# Check if Ollama is running
if not pgrep -f "ollama serve" > /dev/null
    echo "❌ Ollama daemon is not running!"
    echo "   Start it with: ac-llama start"
    exit 1
end

echo "✅ Ollama daemon is running"

# Check if gemma2:2b model is available
if not ollama list | grep -q "gemma2:2b"
    echo "⚠️  Model gemma2:2b not found!"
    echo "   Pull it with: ollama pull gemma2:2b"
    exit 1
end

echo "✅ Model gemma2:2b is available"

# Start the API server in the background
echo ""
echo "🚀 Starting API server on port 3000..."
node /workspaces/aesthetic-computer/censor/api-server.mjs &
set api_pid $last_pid

sleep 2

# Start Caddy
echo "🌐 Starting Caddy web server on port 8080..."
caddy run --config /workspaces/aesthetic-computer/censor/Caddyfile &
set caddy_pid $last_pid

sleep 2

echo ""
echo "═══════════════════════════════════════════════"
echo "✨ Content Filter Dashboard is ready!"
echo "═══════════════════════════════════════════════"
echo ""
echo "📊 Dashboard: http://localhost:8080"
echo "🔌 API Server: http://localhost:3000/api/filter"
echo ""
echo "Press Ctrl+C to stop all services"
echo ""

# Wait for interrupt
function cleanup
    echo ""
    echo "🛑 Stopping services..."
    kill $api_pid $caddy_pid 2>/dev/null
    echo "✅ All services stopped"
end

trap cleanup INT TERM

# Keep script running
while true
    sleep 1
end
