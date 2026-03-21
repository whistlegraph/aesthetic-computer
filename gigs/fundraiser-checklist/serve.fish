#!/usr/bin/env fish

# Serve with Caddy on port 8000 with no caching
cd (dirname (status -f))
caddy run --config Caddyfile
