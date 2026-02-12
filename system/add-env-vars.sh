#!/bin/bash
# Add missing environment variables to Netlify production

cd /workspaces/aesthetic-computer/system

# Add DigitalOcean Spaces credentials
npx netlify env:set ART_KEY "REDACTED_DO_SPACES_KEY" --context production
npx netlify env:set ART_SECRET "REDACTED_DO_SPACES_SECRET" --context production

# Add bucket names
npx netlify env:set ART_SPACE_NAME "art-aesthetic-computer" --context production
npx netlify env:set USER_SPACE_NAME "user-aesthetic-computer" --context production
npx netlify env:set WAND_SPACE_NAME "wand-aesthetic-computer" --context production

# Add endpoints (optional but recommended)
npx netlify env:set ART_ENDPOINT "sfo3.digitaloceanspaces.com" --context production
npx netlify env:set USER_ENDPOINT "sfo3.digitaloceanspaces.com" --context production
npx netlify env:set WAND_ENDPOINT "sfo3.digitaloceanspaces.com" --context production

# Add MongoDB credentials (Silo - self-hosted)
npx netlify env:set MONGODB_CONNECTION_STRING "mongodb://aesthetic_app:REDACTED_SILO_PASSWORD@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic&replicaSet=rs0" --context production
npx netlify env:set MONGODB_NAME "aesthetic" --context production

echo "Done! Environment variables added to Netlify production."
