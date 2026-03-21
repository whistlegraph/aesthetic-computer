#!/bin/bash
# Add missing environment variables to Netlify production
# NOTE: Actual secrets should be sourced from the vault, not hardcoded here.
# Usage: Source your vault .env first, then run this script.

cd /workspaces/aesthetic-computer/system

# Add DigitalOcean Spaces credentials (from vault)
npx netlify env:set ART_KEY "$ART_KEY" --context production
npx netlify env:set ART_SECRET "$ART_SECRET" --context production

# Add bucket names
npx netlify env:set ART_SPACE_NAME "art-aesthetic-computer" --context production
npx netlify env:set USER_SPACE_NAME "user-aesthetic-computer" --context production
npx netlify env:set WAND_SPACE_NAME "wand-aesthetic-computer" --context production

# Add endpoints
npx netlify env:set ART_ENDPOINT "sfo3.digitaloceanspaces.com" --context production
npx netlify env:set USER_ENDPOINT "sfo3.digitaloceanspaces.com" --context production
npx netlify env:set WAND_ENDPOINT "sfo3.digitaloceanspaces.com" --context production

# Add MongoDB credentials (from vault)
npx netlify env:set MONGODB_CONNECTION_STRING "$MONGODB_CONNECTION_STRING" --context production
npx netlify env:set MONGODB_NAME "$MONGODB_NAME" --context production

echo "Done! Environment variables added to Netlify production."
