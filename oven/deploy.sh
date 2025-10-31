#!/bin/bash
# Deploy Oven to production and restart service

set -e

echo "🚀 Deploying Oven to production..."

# Deploy files
scp -i ~/.ssh/oven-deploy-key baker.mjs root@137.184.237.166:/opt/oven/baker.mjs
scp -i ~/.ssh/oven-deploy-key server.mjs root@137.184.237.166:/opt/oven/server.mjs

echo "✅ Files deployed"

# Restart service
ssh -i ~/.ssh/oven-deploy-key root@137.184.237.166 "systemctl restart oven.service"

echo "🔥 Oven service restarted"
echo "✨ Deployment complete!"
