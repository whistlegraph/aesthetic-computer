# PDS Maintenance Guide

Ongoing maintenance procedures for Aesthetic Computer PDS.

## Daily Tasks (Automated)

These should be automated via cron:

### Health Checks (Every 5 minutes)
```bash
*/5 * * * * /root/health-check.sh >> /var/log/pds-health.log 2>&1
```

### Backups (Daily at 2 AM)
```bash
0 2 * * * /root/backup.sh >> /var/log/pds-backup.log 2>&1
```

## Weekly Tasks

### Review Logs
```bash
# SSH to server
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# Check PDS logs for errors
docker logs pds --tail 200 | grep -i error

# Check health logs
tail -100 /var/log/pds-health.log

# Check backup logs
tail -50 /var/log/pds-backup.log
```

### Check Storage Usage
```bash
# From local machine
cd /workspaces/aesthetic-computer/at/pds
fish scripts/storage-manager.fish usage

# Monitor costs
# Alert if approaching 250GB (cost increases after that)
```

### Update PDS
```bash
# SSH to server
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# Backup first
/root/backup.sh

# Update
pdsadmin update

# Verify
docker logs pds --tail 50
curl https://pds.aesthetic.computer/xrpc/_health
```

### Review Accounts
```bash
# SSH to server
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# List accounts
pdsadmin account list

# Check for suspicious activity
sqlite3 /pds/accounts.sqlite "SELECT email, created_at FROM account ORDER BY created_at DESC LIMIT 10;"
```

## Monthly Tasks

### Full System Update
```bash
# SSH to server
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# Backup everything
/root/backup.sh

# Update system packages
apt update
apt upgrade -y

# Update PDS
pdsadmin update

# Reboot if kernel updated
reboot

# Wait 2 minutes, then verify
curl https://pds.aesthetic.computer/xrpc/_health
```

### Review Security
```bash
# Check failed login attempts
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>
grep "Failed password" /var/log/auth.log | tail -20

# Review firewall rules
doctl compute firewall list

# Check SSL certificate expiry
echo | openssl s_client -servername pds.aesthetic.computer -connect pds.aesthetic.computer:443 2>/dev/null | openssl x509 -noout -dates
```

### Clean Old Backups
```bash
# On server (keeps 30 days by default)
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# Check backup size
du -sh /backups/pds

# Manually clean if needed (older than 60 days)
find /backups/pds -name "pds-backup-*.tar.gz" -mtime +60 -delete
```

### Review Performance
```bash
# SSH to server
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# Check resource usage
htop

# Check disk usage
df -h

# Check database sizes
du -sh /pds/*.sqlite

# Check blob storage
fish scripts/storage-manager.fish usage
```

## Quarterly Tasks

### Review Costs
```bash
# Check DigitalOcean billing
doctl invoice list

# Review Spaces costs
# Check if approaching limits

# Evaluate if need to scale up
```

### Test Backup Restoration
```bash
# Important: Test that backups actually work!

# On a test droplet:
# 1. Create fresh PDS installation
# 2. Restore latest backup
# 3. Verify accounts and posts restored
# 4. Test login

# See DEPLOYMENT.md for restoration steps
```

### Review Scaling Needs
```bash
# Check user growth
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>
sqlite3 /pds/accounts.sqlite "SELECT COUNT(*) FROM account;"

# Check post volume
sqlite3 /pds/blocks.sqlite "SELECT COUNT(*) FROM record;"

# If >50 active users, consider scaling to 2GB droplet
# If >100 active users, consider 4GB droplet
```

### Security Audit
```bash
# Review access logs
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>
tail -200 /var/log/auth.log

# Check for unauthorized access
who
last | head -20

# Review SSH keys
cat ~/.ssh/authorized_keys

# Rotate API keys if needed
# Update vault with new keys
```

## Emergency Procedures

### PDS Down
```bash
# 1. Check server status
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP> 'systemctl status pds'

# 2. Check logs
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP> 'docker logs pds --tail 100'

# 3. Restart if needed
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP> 'systemctl restart pds'

# 4. If still down, check DNS and firewall
dig pds.aesthetic.computer A
doctl compute firewall list
```

### Database Corruption
```bash
# 1. Stop PDS
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP> 'systemctl stop pds'

# 2. Check database integrity
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>
sqlite3 /pds/blocks.sqlite "PRAGMA integrity_check;"
sqlite3 /pds/accounts.sqlite "PRAGMA integrity_check;"

# 3. If corrupted, restore from backup
cd /backups/pds
tar xzf pds-backup-YYYYMMDD-HHMMSS.tar.gz
cp blocks-YYYYMMDD-HHMMSS.sqlite /pds/blocks.sqlite
cp accounts-YYYYMMDD-HHMMSS.sqlite /pds/accounts.sqlite

# 4. Restart PDS
systemctl start pds
```

### Out of Disk Space
```bash
# 1. Check what's using space
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP> 'du -sh /*'

# 2. Clean old logs
find /var/log -name "*.gz" -mtime +30 -delete
journalctl --vacuum-time=7d

# 3. Clean old backups
find /backups/pds -name "*.tar.gz" -mtime +30 -delete

# 4. If still full, resize disk
doctl compute volume resize <VOLUME_ID> --size 40
```

### SSL Certificate Issues
```bash
# Usually auto-renews via Caddy, but if not:

# 1. Check Caddy logs
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP> 'docker logs pds | grep -i caddy'

# 2. Verify DNS is correct
dig pds.aesthetic.computer A

# 3. Restart PDS (triggers new cert request)
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP> 'systemctl restart pds'

# 4. Wait 60 seconds and test
curl -I https://pds.aesthetic.computer
```

### Spaces Connection Issues
```bash
# 1. Test Spaces connectivity
fish scripts/storage-manager.fish test

# 2. Check credentials
source /workspaces/aesthetic-computer/aesthetic-computer-vault/at/deploy.env
echo $SPACES_KEY

# 3. Verify bucket exists
s3cmd ls s3://$SPACES_BUCKET

# 4. Update PDS config if needed
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>
nano /pds/pds.env
# Update Spaces credentials
systemctl restart pds
```

## Monitoring Alerts

Set up alerts for:

### Critical (Immediate Action)
- PDS health check fails 3 times in a row
- SSL certificate expires in < 7 days
- Disk usage > 90%
- Memory usage > 95%

### Warning (Review Soon)
- Storage usage > 200GB
- Response time > 1 second
- Error rate > 5%
- Backup fails

### Info (Regular Review)
- New account created
- PDS updated
- Weekly usage report

## Performance Optimization

### If Response Times Increase
```bash
# 1. Check server load
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP> 'uptime'

# 2. Check database size
du -h /pds/*.sqlite

# 3. Consider:
#    - Scaling to larger droplet
#    - Adding database indexes
#    - Enabling query caching
```

### If Storage Costs Increase
```bash
# 1. Audit storage usage
fish scripts/storage-manager.fish usage

# 2. Clean old/unused blobs
fish scripts/storage-manager.fish clean

# 3. Implement lifecycle policies
# See STORAGE.md for details
```

## Useful Queries

### Account Statistics
```sql
-- Total accounts
SELECT COUNT(*) FROM account;

-- Accounts by creation date
SELECT DATE(created_at) as date, COUNT(*) as count 
FROM account 
GROUP BY DATE(created_at) 
ORDER BY date DESC 
LIMIT 30;

-- Most active users
SELECT handle, COUNT(*) as posts 
FROM record 
JOIN account ON record.did = account.did 
GROUP BY handle 
ORDER BY posts DESC 
LIMIT 10;
```

### Storage Statistics
```sql
-- Database sizes
SELECT 
  'blocks' as db, 
  (SELECT page_count * page_size FROM pragma_page_count(), pragma_page_size()) / 1024.0 / 1024.0 as size_mb 
FROM blocks.sqlite
UNION ALL
SELECT 
  'accounts' as db,
  (SELECT page_count * page_size FROM pragma_page_count(), pragma_page_size()) / 1024.0 / 1024.0 as size_mb 
FROM accounts.sqlite;
```

## Documentation

Keep these docs updated:
- Server IP address
- Droplet ID and specs
- Spaces bucket names
- SMTP configuration
- Last update date
- Known issues

Store in vault or team wiki.

## Support Contacts

- **PDS Admins Discord**: https://discord.gg/e7hpHxRfBP
- **AT Protocol Docs**: https://atproto.com
- **DigitalOcean Support**: https://www.digitalocean.com/support
- **Bluesky Status**: https://status.bsky.app

---

**Last Updated**: October 9, 2025

Remember: Always backup before making changes!
