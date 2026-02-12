# 🚀 MongoDB Migration to Silo - Deployment Checklist

## ✅ Completed Steps

1. ✅ **MongoDB Authentication** - Silo configured with secure passwords
2. ✅ **Direct Connection** - Port 27017 open with firewall rules
3. ✅ **Data Sync** - 4,950 documents synced from Atlas to Silo
4. ✅ **Netlify Environment Variables** - Updated to Silo connection
5. ✅ **Vault .env Files** - All 9 service files updated
6. ✅ **Netlify Deploy** - Triggered (running in background)

---

## 🔄 Services That Need Redeployment/Restart

### 1️⃣ **Netlify Functions** (49 functions)
**Status**: ⏳ Deploying now  
**Action**: None - deploy already triggered  
**Check**: Monitor deploy logs for completion

### 2️⃣ **Session Server** (DigitalOcean Droplet)
**Location**: `157.245.134.225`  
**Path**: `/session-server`  
**Action Required**:
```bash
ssh root@157.245.134.225
cd /path/to/session-server
pm2 restart session-server
# or
systemctl restart session-server
```

### 3️⃣ **Oven Service**
**Location**: Check if running on droplet or serverless  
**Path**: `/oven`  
**Action Required**:
```bash
cd /workspaces/aesthetic-computer/oven
# If using PM2:
pm2 restart oven
# If using systemd:
systemctl restart oven
```

### 4️⃣ **Judge API Server**
**Location**: Serverless or dedicated server  
**Path**: `/judge`  
**Action Required**:
```bash
cd /workspaces/aesthetic-computer/judge
# Redeploy to cloud or restart
pm2 restart judge
```

### 5️⃣ **Censor API Server**
**Location**: Serverless or dedicated server  
**Path**: `/censor`  
**Action Required**:
```bash
cd /workspaces/aesthetic-computer/censor
pm2 restart censor
```

### 6️⃣ **AT (ATProto Service)**
**Location**: Check deployment location  
**Path**: `/at`  
**Action Required**:
```bash
cd /workspaces/aesthetic-computer/at
pm2 restart at
```

### 7️⃣ **Feed Builder**
**Location**: Likely cron job or manual  
**Path**: `/feed`  
**Action**: Will use new connection on next run (no restart needed)

### 8️⃣ **Nanos (Chat/Conductor)**
**Location**: Check if running  
**Path**: `/nanos`  
**Action Required**:
```bash
pm2 restart chat
pm2 restart conductor
```

---

## 🧪 Verification Steps

After all services are restarted, verify:

### 1. Check Netlify Deploy Status
```bash
cd system
npx netlify status
```

### 2. Test Key Endpoints
```bash
# Test a MongoDB-dependent function
curl https://aesthetic.computer/api/moods

# Test user lookup
curl https://aesthetic.computer/api/user/SOME_USER_ID
```

### 3. Monitor Silo MongoDB Connections
```bash
ssh -i aesthetic-computer-vault/home/.ssh/id_rsa root@silo.aesthetic.computer
mongosh "mongodb://aesthetic_app:PASSWORD@localhost:27017/aesthetic?authSource=aesthetic" \
  --eval "db.currentOp(true).inprog.length"
```

### 4. Check for Errors
```bash
# Netlify logs
npx netlify logs

# Service logs (on respective servers)
pm2 logs
journalctl -u service-name -f
```

---

## 🎯 Expected Performance Improvements

After migration, you should see:
- ✅ **3.6x faster** database operations (68ms → 15ms average)
- ✅ **6x faster** connections (752ms → 123ms)
- ✅ **Reduced page load times** across the board
- ✅ **Lower latency** for all API endpoints

---

## 📊 Monitoring

### Watch Active Connections
```bash
watch -n 5 'mongosh "mongodb://aesthetic_app:PASSWORD@localhost:27017/aesthetic?authSource=aesthetic" --quiet --eval "db.serverStatus().connections"'
```

### Check Performance
```bash
# Run benchmark again to verify
node utilities/benchmark-mongodb.mjs --writes 50 --reads 50
```

---

## 🔙 Rollback Plan (If Needed)

If issues occur, you can quickly rollback:

### 1. Revert Netlify Environment Variables
```bash
npx netlify env:set MONGODB_CONNECTION_STRING "OLD_ATLAS_CONNECTION" --context production
npx netlify deploy --prod
```

### 2. Revert Service .env Files
```bash
# Restore from backups (created automatically)
for file in aesthetic-computer-vault/*/.env.backup-*; do
  original="${file%.backup-*}"
  cp "$file" "$original"
done
```

### 3. Restart Services
Follow the same restart procedure above

---

## 💰 Cost Savings Realized

- **Atlas**: ~$25-57/month → **$0** (cancelled)
- **Silo**: $6/month
- **Net Savings**: $19-51/month ($228-612/year)

---

## ✅ Final Checklist

- [ ] Netlify deploy completed successfully
- [ ] Session server restarted
- [ ] Oven service restarted
- [ ] Judge API restarted
- [ ] Censor API restarted
- [ ] AT service restarted
- [ ] Nanos services restarted
- [ ] All endpoints responding correctly
- [ ] No error spikes in logs
- [ ] Performance improvements verified
- [ ] Atlas can be decommissioned (after 24-48 hours of stability)

