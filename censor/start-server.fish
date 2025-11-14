#!/usr/bin/env fish

# Start the Caddy server for the censor endpoint
echo "Starting Caddy server on port 8080..."
echo "Censor endpoint available at: http://localhost:8080/censor"
echo "Health check available at: http://localhost:8080/health"
echo ""
echo "Press Ctrl+C to stop the server"

cd (dirname (status -f))
caddy run --config Caddyfile
