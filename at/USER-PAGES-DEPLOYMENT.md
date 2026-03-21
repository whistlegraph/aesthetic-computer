# User Pages Deployment Checklist

Pre-deployment verification and deployment steps for user-specific ATProto subdomain pages.

## ✅ Pre-Deployment Checklist

### Files Ready
- [ ] `user-page.html` - Created and tested
- [ ] `deploy-user-pages.fish` - Created and executable
- [ ] `landing-page.html` - Exists and up to date
- [ ] Documentation complete:
  - [ ] USER-PAGES.md
  - [ ] USER-PAGES-SUMMARY.md
  - [ ] USER-PAGES-ARCHITECTURE.md
  - [ ] USER-PAGES-API-EXAMPLES.md

### Access Verified
- [ ] SSH access to PDS server (root@138.197.35.160)
- [ ] SSH key available at `~/.ssh/aesthetic_pds`
- [ ] Can connect: `ssh -i ~/.ssh/aesthetic_pds root@138.197.35.160`
- [ ] Caddy is running: `docker ps | grep caddy`

### Testing
- [ ] API endpoints working:
  - [ ] `curl https://at.aesthetic.computer/xrpc/_health`
  - [ ] `curl https://at.aesthetic.computer/xrpc/com.atproto.identity.resolveHandle?handle=jeffrey.at.aesthetic.computer`
- [ ] Test handle exists and has records
- [ ] Blob storage accessible

### Backup
- [ ] Current Caddyfile backed up (script does this automatically)
- [ ] Current landing page backed up (optional)

## 🚀 Deployment Steps

### 1. Navigate to Directory
```fish
cd /workspaces/aesthetic-computer/at
```

### 2. Verify Files
```fish
ls -lh user-page.html landing-page.html deploy-user-pages.fish
```

Expected:
- `user-page.html` - ~25KB
- `landing-page.html` - ~15KB
- `deploy-user-pages.fish` - ~2KB (executable)

### 3. Run Deployment Script
```fish
./deploy-user-pages.fish
```

### 4. Monitor Output
Expected output sequence:
```
🚀 Deploying user pages to at.aesthetic.computer

📤 Uploading pages to PDS server...
landing-page.html                     100%   15KB
user-page.html                        100%   25KB
✅ Pages uploaded successfully

🔧 Configuring Caddy to serve custom user pages...
✅ Deployment complete!

🌐 Landing page: https://at.aesthetic.computer
👤 User pages: https://[handle].at.aesthetic.computer
🔍 Health check: https://at.aesthetic.computer/xrpc/_health

Example user pages:
  https://fifi.at.aesthetic.computer
  https://jeffrey.at.aesthetic.computer
```

## ✅ Post-Deployment Verification

### 1. Test Main Landing Page
```fish
curl -I https://at.aesthetic.computer
```

Expected:
- Status: `200 OK`
- Content-Type: `text/html`

### 2. Test User Pages
```fish
# Replace with actual handle that has records
curl -I https://jeffrey.at.aesthetic.computer
```

Expected:
- Status: `200 OK`
- Content-Type: `text/html`

### 3. Test API Proxy Through Subdomain
```fish
curl https://jeffrey.at.aesthetic.computer/xrpc/_health
```

Expected:
```json
{
  "version": "0.4.x"
}
```

### 4. Browser Testing

Visit in browser:
- [ ] `https://at.aesthetic.computer` - Should show landing page
- [ ] `https://jeffrey.at.aesthetic.computer` - Should show user page
- [ ] Check browser console for errors (F12)
- [ ] Verify records load correctly
- [ ] Test tab navigation
- [ ] Verify images load
- [ ] Test dark mode

### 5. Test Multiple Users
- [ ] `https://fifi.at.aesthetic.computer`
- [ ] `https://jeffrey.at.aesthetic.computer`
- [ ] Try a user with many records
- [ ] Try a user with few/no records

### 6. Test Error Handling
- [ ] `https://nonexistent.at.aesthetic.computer` - Should show error message
- [ ] User with no ATProto account - Should show error
- [ ] Network tab shows proper error responses

## 🔧 Troubleshooting

### Issue: "Permission denied" when running script
```fish
chmod +x deploy-user-pages.fish
```

### Issue: SSH connection fails
```fish
# Verify SSH key
ls -l ~/.ssh/aesthetic_pds

# Test connection
ssh -i ~/.ssh/aesthetic_pds root@138.197.35.160 "echo Connected"
```

### Issue: File not found errors
```fish
# Verify you're in the right directory
pwd  # Should show: /workspaces/aesthetic-computer/at

# List files
ls -lh user-page.html landing-page.html
```

### Issue: Caddy reload fails
SSH into server and check:
```fish
ssh -i ~/.ssh/aesthetic_pds root@138.197.35.160

# Check Caddy container
docker ps | grep caddy

# View Caddy logs
docker logs caddy --tail 50

# Manually reload
docker exec caddy caddy reload --config /etc/caddy/Caddyfile

# Validate Caddyfile
docker exec caddy caddy validate --config /etc/caddy/Caddyfile
```

### Issue: Pages not serving correctly
```fish
ssh -i ~/.ssh/aesthetic_pds root@138.197.35.160

# Check files uploaded
ls -lh /var/www/at.aesthetic.computer/
# Should show: index.html, user.html

# View Caddyfile
cat /pds/Caddyfile

# Check file permissions
ls -lh /var/www/at.aesthetic.computer/
# Should be readable by all
```

### Issue: API calls fail
- Check PDS is running: `docker ps`
- Check PDS health: `curl https://at.aesthetic.computer/xrpc/_health`
- Check Caddy proxy config
- View browser console for CORS errors

## 🔄 Rollback Procedure

If something goes wrong:

### 1. SSH to Server
```fish
ssh -i ~/.ssh/aesthetic_pds root@138.197.35.160
```

### 2. Restore Previous Caddyfile
```bash
# Script creates backup at /pds/Caddyfile.backup
cp /pds/Caddyfile.backup /pds/Caddyfile
docker exec caddy caddy reload --config /etc/caddy/Caddyfile
```

### 3. Remove User Page (Optional)
```bash
rm /var/www/at.aesthetic.computer/user.html
```

### 4. Verify Rollback
```fish
curl -I https://at.aesthetic.computer
curl -I https://fifi.at.aesthetic.computer
```

## 📊 Monitoring

### Check Logs
```fish
ssh -i ~/.ssh/aesthetic_pds root@138.197.35.160

# Caddy logs
docker logs caddy --tail 100 -f

# PDS logs
docker logs pds --tail 100 -f
```

### Monitor Traffic
```fish
# On server
tail -f /var/log/caddy/access.log
```

### Check Metrics
- Page load times
- API response times
- Error rates
- 404 rates for invalid handles

## 🎉 Success Criteria

✅ Landing page loads at https://at.aesthetic.computer  
✅ User pages load at https://[handle].at.aesthetic.computer  
✅ Records display correctly with images  
✅ Tab navigation works  
✅ Links to pdsls.dev work  
✅ Dark mode activates properly  
✅ Error handling works for invalid handles  
✅ No console errors  
✅ XRPC APIs accessible through subdomains  

## 📝 Post-Deployment Tasks

- [ ] Update QUICKSTART.md with user page info ✅ (Done)
- [ ] Announce feature to users
- [ ] Monitor for issues in first 24 hours
- [ ] Collect user feedback
- [ ] Plan Phase 1 enhancements (authentication)

## 📚 Related Documentation

- [USER-PAGES.md](USER-PAGES.md) - Full documentation
- [USER-PAGES-ARCHITECTURE.md](USER-PAGES-ARCHITECTURE.md) - Architecture diagrams
- [USER-PAGES-API-EXAMPLES.md](USER-PAGES-API-EXAMPLES.md) - API examples
- [ADMIN.md](ADMIN.md) - PDS administration

---

**Ready to deploy? Run:** `./deploy-user-pages.fish`
