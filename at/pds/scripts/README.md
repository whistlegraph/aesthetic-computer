# PDS Management Scripts

Collection of scripts for managing your Aesthetic Computer PDS instance.

## Available Scripts

### Deployment

#### `deployment/digitalocean/deploy.fish`
Main deployment script that provisions infrastructure on DigitalOcean.

```bash
cd /workspaces/aesthetic-computer/at/pds/deployment/digitalocean
fish deploy.fish
```

**What it does:**
- Creates SSH key
- Provisions Spaces bucket
- Creates droplet
- Configures firewall
- Installs PDS

#### `deployment/digitalocean/generate-pds-env.fish`
Generates PDS environment file from vault credentials.

```bash
fish generate-pds-env.fish [output-file]
```

### Monitoring

#### `scripts/health-check.sh`
Comprehensive health check for your PDS instance.

```bash
# Local check
./scripts/health-check.sh

# On server (via cron)
*/5 * * * * /root/health-check.sh >> /var/log/pds-health.log 2>&1
```

**Checks:**
- HTTP health endpoint
- WebSocket connectivity
- SSL certificate expiry
- DNS resolution
- Response time

### Backup

#### `scripts/backup.sh`
Backs up SQLite databases and configuration.

```bash
# Manual backup
./scripts/backup.sh

# Automated backup (add to crontab)
0 2 * * * /root/backup.sh >> /var/log/pds-backup.log 2>&1
```

**Features:**
- Consistent SQLite backups
- Configuration backup
- Compressed archives
- Automatic cleanup (30 days)
- Optional Spaces upload

### Storage Management

#### `scripts/storage-manager.fish`
Manage DigitalOcean Spaces blob storage.

```bash
# Setup s3cmd configuration
fish scripts/storage-manager.fish setup

# Check usage and cost
fish scripts/storage-manager.fish usage

# List recent uploads
fish scripts/storage-manager.fish recent 50

# Test connectivity
fish scripts/storage-manager.fish test

# Backup blobs
fish scripts/storage-manager.fish backup

# Clean test files
fish scripts/storage-manager.fish clean

# Show CDN info
fish scripts/storage-manager.fish cdn
```

## Setup Instructions

### 1. Install Dependencies

**Local machine:**
```bash
# macOS
brew install doctl s3cmd

# Linux
apt install doctl s3cmd

# Or use package manager of choice
```

**On server:**
```bash
apt install sqlite3 jq curl
```

### 2. Configure Authentication

```bash
# Initialize doctl
doctl auth init
# Enter DO token from vault

# Configure s3cmd (or use storage-manager.fish setup)
fish scripts/storage-manager.fish setup
```

### 3. Deploy PDS

```bash
cd /workspaces/aesthetic-computer/at/pds/deployment/digitalocean

# Generate environment file
fish generate-pds-env.fish

# Review configuration
cat ../../config/pds.env

# Deploy
fish deploy.fish
```

### 4. Set Up Monitoring

```bash
# Copy scripts to server
scp -i ~/.ssh/aesthetic_pds scripts/health-check.sh root@<SERVER_IP>:/root/
scp -i ~/.ssh/aesthetic_pds scripts/backup.sh root@<SERVER_IP>:/root/

# SSH to server
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# Make executable
chmod +x /root/*.sh

# Add to crontab
crontab -e

# Add these lines:
*/5 * * * * /root/health-check.sh >> /var/log/pds-health.log 2>&1
0 2 * * * /root/backup.sh >> /var/log/pds-backup.log 2>&1
```

## Common Tasks

### Check PDS Health

```bash
# From local machine
./scripts/health-check.sh

# Or directly
curl https://pds.aesthetic.computer/xrpc/_health
```

### View Logs

```bash
# SSH to server
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# PDS logs
docker logs pds --tail 100 --follow

# Health check logs
tail -f /var/log/pds-health.log

# Backup logs
tail -f /var/log/pds-backup.log
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

### Restore from Backup

```bash
# SSH to server
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# Stop PDS
systemctl stop pds

# Restore databases
cd /backups/pds
tar xzf pds-backup-YYYYMMDD-HHMMSS.tar.gz
cp *.sqlite /pds/

# Start PDS
systemctl start pds

# Verify
docker logs pds --tail 50
```

### Check Storage Usage

```bash
# From local machine
fish scripts/storage-manager.fish usage

# Output:
# Storage Used: 45.23 GB
# Objects: 1,234
# Estimated Cost: $5.00/month
```

### Create Account

```bash
# SSH to server
ssh -i ~/.ssh/aesthetic_pds root@<SERVER_IP>

# Create account directly
pdsadmin account create

# Or create invite code
pdsadmin create-invite-code

# List accounts
pdsadmin account list
```

## Troubleshooting

### Script Fails to Run

```bash
# Ensure executable
chmod +x scripts/*.sh
chmod +x scripts/*.fish
chmod +x deployment/digitalocean/*.fish

# Check dependencies
which doctl s3cmd fish bash

# Load vault environment
source /workspaces/aesthetic-computer/aesthetic-computer-vault/at/deploy.env
```

### s3cmd Not Configured

```bash
# Use storage manager to set up
fish scripts/storage-manager.fish setup

# Or manually
s3cmd --configure
```

### Health Check Fails

```bash
# Check DNS
dig pds.aesthetic.computer A

# Check server is running
ssh root@<SERVER_IP> 'systemctl status pds'

# Check firewall
doctl compute firewall list

# Check SSL
echo | openssl s_client -servername pds.aesthetic.computer -connect pds.aesthetic.computer:443
```

### Backup Fails

```bash
# Check disk space
ssh root@<SERVER_IP> 'df -h'

# Check backup directory exists
ssh root@<SERVER_IP> 'mkdir -p /backups/pds'

# Check SQLite databases exist
ssh root@<SERVER_IP> 'ls -lh /pds/*.sqlite'
```

## Automation Examples

### Daily Health Report

```bash
# Add to crontab
0 8 * * * /root/health-check.sh && mail -s "PDS Health Report" me@jas.life < /var/log/pds-health.log
```

### Weekly Storage Report

```bash
# Add to crontab on local machine
0 9 * * 1 cd /workspaces/aesthetic-computer/at/pds && fish scripts/storage-manager.fish usage | mail -s "PDS Storage Report" me@jas.life
```

### Monthly Backup to External Storage

```bash
# Add to crontab on server
0 3 1 * * /root/backup.sh && rclone copy /backups/pds/ gdrive:pds-backups/
```

## Security Notes

1. **SSH Keys**: Never commit SSH keys. Store in `~/.ssh/` with 600 permissions.
2. **Vault Files**: Keep vault files private. They contain API keys and secrets.
3. **Script Permissions**: Only make scripts executable, not readable by others.
4. **Backup Encryption**: Consider encrypting backups before uploading to external storage.

```bash
# Encrypt backup
gpg --encrypt --recipient me@jas.life backup.tar.gz

# Decrypt backup
gpg --decrypt backup.tar.gz.gpg > backup.tar.gz
```

## Script Maintenance

### Update Scripts from Git

```bash
cd /workspaces/aesthetic-computer
git pull origin main

# Copy updated scripts to server
scp -i ~/.ssh/aesthetic_pds scripts/*.sh root@<SERVER_IP>:/root/
```

### Add Custom Scripts

Create new scripts in `scripts/` directory and follow these conventions:

- **Shell scripts**: Use `.sh` extension, start with `#!/usr/bin/env bash`
- **Fish scripts**: Use `.fish` extension, start with `#!/usr/bin/env fish`
- **Make executable**: `chmod +x scripts/your-script.sh`
- **Document**: Add description and usage in this README

## Contributing

When adding new scripts:

1. Test thoroughly in development
2. Add usage documentation to this file
3. Include error handling and logging
4. Use vault configuration where possible
5. Make scripts idempotent (safe to run multiple times)

---

**Last Updated**: October 9, 2025
