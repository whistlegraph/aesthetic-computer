# DP1 Feed Deployment Checklist

## ✅ Pre-Deployment (Completed)

- [x] Installed ripgrep (rg) in Docker container
- [x] Updated Dockerfile to include ripgrep
- [x] Cloned dp1-feed repository
- [x] Installed wrangler CLI (v4.42.0)
- [x] Installed dp1-feed dependencies
- [x] Found Cloudflare API credentials
- [x] Updated wrangler.toml for aesthetic.computer
- [x] Created setup scripts (Fish shell)
- [x] Created deployment scripts
- [x] Added npm scripts to main package.json
- [x] Created comprehensive documentation

## 📋 Deployment Steps (To Do)

### Step 1: Authenticate with Cloudflare
- [ ] Run: `cd /workspaces/aesthetic-computer/dp1-feed`
- [ ] Run: `wrangler login` (opens browser)
- [ ] Complete OAuth flow
- [ ] Verify: Should see "Successfully logged in"

**Alternative (API Token):**
```fish
set -x CLOUDFLARE_API_TOKEN "0346704765b61e560b36592010c98a23bc2c6"
set -x CLOUDFLARE_EMAIL "me@jas.life"
wrangler whoami
```

### Step 2: Create Cloudflare Resources
- [ ] Run: `npm run feed:setup` (from root)
- [ ] OR: `cd dp1-feed && ./setup-resources.fish`
- [ ] Choose: Option 2 (Production)
- [ ] **IMPORTANT**: Copy the namespace IDs from output
- [ ] Example output to look for:
  ```
  Created namespace with title "aesthetic-feed-DP1_PLAYLISTS"
   ID: xxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ```

### Step 3: Update Configuration
- [ ] Open: `dp1-feed/wrangler.toml`
- [ ] Find section: `[[env.production.kv_namespaces]]`
- [ ] Replace placeholder IDs with real IDs from Step 2
- [ ] Update all 3 namespaces:
  - [ ] DP1_PLAYLISTS
  - [ ] DP1_CHANNELS
  - [ ] DP1_PLAYLIST_ITEMS
- [ ] Save file

### Step 4: Generate and Set Secrets
- [ ] Run: `cd /workspaces/aesthetic-computer/dp1-feed`
- [ ] Generate API secret:
  ```fish
  set api_secret (openssl rand -hex 32)
  echo "Save this API Secret: $api_secret"
  echo $api_secret | wrangler secret put API_SECRET --env production
  ```
- [ ] **SAVE THE API SECRET** - write it down!
- [ ] Generate Ed25519 keys:
  ```fish
  npm run jwt:generate-keys
  ```
- [ ] Set Ed25519 private key:
  ```fish
  cat private-key.txt | wrangler secret put ED25519_PRIVATE_KEY --env production
  ```
- [ ] (Optional) Set JWT config if using JWT auth:
  ```fish
  echo "your-issuer" | wrangler secret put JWT_ISSUER --env production
  echo "your-audience" | wrangler secret put JWT_AUDIENCE --env production
  ```

### Step 5: Test Locally (Optional but Recommended)
- [ ] Run: `npm run feed:dev`
- [ ] Open: http://localhost:8787/api/v1
- [ ] Test health: `curl http://localhost:8787/api/v1/health`
- [ ] Press Ctrl+C to stop
- [ ] Verify no errors in console

### Step 6: Deploy to Production
- [ ] Run: `npm run feed:deploy` (from root)
- [ ] OR: `cd dp1-feed && ./deploy-feed.fish production`
- [ ] Confirm deployment when prompted
- [ ] Wait for deployment to complete
- [ ] Look for: "Published aesthetic-feed"
- [ ] Note the worker URL (e.g., aesthetic-feed.xxx.workers.dev)

### Step 7: Configure Custom Domain
- [ ] Open: https://dash.cloudflare.com/
- [ ] Navigate to: Workers & Pages
- [ ] Click: aesthetic-feed
- [ ] Go to: Settings → Domains & Routes
- [ ] Click: "Add Custom Domain"
- [ ] Enter: `feed.aesthetic.computer`
- [ ] Click: "Add Domain"
- [ ] Wait: ~2-5 minutes for DNS propagation

### Step 8: Verify Deployment
- [ ] Test health endpoint:
  ```fish
  curl https://feed.aesthetic.computer/api/v1/health
  ```
- [ ] Expected response:
  ```json
  {
    "status": "healthy",
    "timestamp": "2024-10-06T...",
    "version": "1.0.0"
  }
  ```
- [ ] Test API info:
  ```fish
  curl https://feed.aesthetic.computer/api/v1
  ```
- [ ] Test creating a playlist (use your API_SECRET):
  ```fish
  curl -X POST https://feed.aesthetic.computer/api/v1/playlists \
    -H "Authorization: Bearer YOUR_API_SECRET" \
    -H "Content-Type: application/json" \
    -d '{
      "dpVersion": "1.0.0",
      "title": "test-playlist",
      "items": [{
        "source": "https://aesthetic.computer/prompt",
        "duration": 300,
        "license": "open"
      }]
    }'
  ```

### Step 9: Monitor & Verify
- [ ] Watch logs: `npm run feed:logs`
- [ ] Check dashboard: https://dash.cloudflare.com/
- [ ] Verify metrics are showing requests
- [ ] Check for any errors in logs
- [ ] Test a few API endpoints

### Step 10: Documentation & Integration
- [ ] Save API_SECRET in secure location
- [ ] Document the subdomain in your DNS records
- [ ] Update any internal documentation
- [ ] Share API endpoint with team if needed
- [ ] Test integration with aesthetic.computer pieces

## 🔍 Verification Commands

Run these after deployment to verify everything:

```fish
# Check DNS resolution
dig feed.aesthetic.computer

# Test API health
curl https://feed.aesthetic.computer/api/v1/health

# List playlists (should return empty array initially)
curl https://feed.aesthetic.computer/api/v1/playlists

# Check worker logs
cd /workspaces/aesthetic-computer/dp1-feed
wrangler tail --env production

# List secrets (verify they're set)
wrangler secret list --env production

# Check deployments
wrangler deployments list --env production
```

## 📊 Success Criteria

- [ ] ✅ API responds at https://feed.aesthetic.computer
- [ ] ✅ Health check returns 200 OK
- [ ] ✅ Can list playlists (GET)
- [ ] ✅ Can create playlist with API_SECRET (POST)
- [ ] ✅ DNS resolves correctly
- [ ] ✅ SSL certificate is active (https works)
- [ ] ✅ Logs show successful requests
- [ ] ✅ No errors in Cloudflare dashboard

## 🚨 Common Issues & Solutions

### Issue: "Worker not found"
**Solution:** Wait 1-2 minutes after deployment, DNS needs to propagate

### Issue: "Unauthorized" when creating playlist
**Solution:** Verify API_SECRET is set correctly:
```fish
wrangler secret list --env production
```

### Issue: "KV namespace not found"
**Solution:** Check namespace IDs in wrangler.toml match created namespaces

### Issue: Custom domain not working
**Solution:** 
1. Check domain is added in Cloudflare dashboard
2. Verify DNS record was created automatically
3. Wait up to 5 minutes for propagation

### Issue: "Module not found" error
**Solution:** 
1. Ensure dependencies are installed: `npm install`
2. Try: `npm run worker:build` to verify TypeScript compiles

## 📝 Important Notes

1. **API Secret**: Save it securely - you can't retrieve it later
2. **Ed25519 Keys**: Keep private key secure, back up both keys
3. **KV Namespace IDs**: Must match in wrangler.toml
4. **DNS Propagation**: Can take 2-5 minutes
5. **CORS**: Configured to allow requests from aesthetic.computer
6. **Rate Limits**: Default Cloudflare Worker limits apply

## 🎯 Quick Reference

### Key Files
- `dp1-feed/wrangler.toml` - Configuration
- `dp1-feed/worker.ts` - Entry point
- `dp1-feed/app.ts` - Main logic

### Key Commands
```fish
npm run feed:setup         # One-time setup
npm run feed:deploy        # Deploy to production
npm run feed:dev           # Local development
npm run feed:logs          # Watch logs
npm run feed:test          # Run tests
```

### Key URLs
- API: https://feed.aesthetic.computer/api/v1
- Dashboard: https://dash.cloudflare.com/
- DNS: https://dash.cloudflare.com/.../dns/records

## ⏱️ Estimated Time

- **Total Setup Time**: 10-15 minutes
- **Step 1 (Auth)**: 1-2 minutes
- **Step 2 (Resources)**: 2-3 minutes
- **Step 3 (Config)**: 1 minute
- **Step 4 (Secrets)**: 2-3 minutes
- **Step 5 (Test)**: 2 minutes (optional)
- **Step 6 (Deploy)**: 1-2 minutes
- **Step 7 (Domain)**: 1-2 minutes
- **Step 8 (Verify)**: 2-3 minutes

---

**Ready to deploy?** Start with Step 1! 🚀

**Need help?** See `DP1-FEED-SETUP.md` for detailed instructions.
