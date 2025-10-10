# PDS Deployment Guide - DigitalOcean

Step-by-step guide to deploy Aesthetic Computer's PDS on DigitalOcean.

## Prerequisites Checklist

Before starting, ensure you have:

- [x] DigitalOcean account
- [x] DigitalOcean API token (in vault)
- [x] DigitalOcean Spaces keys (in vault)
- [x] Domain: aesthetic.computer (DNS access)
- [ ] SMTP service (Resend, SendGrid, or Gmail)
- [ ] `doctl` CLI installed
- [ ] SSH access configured

## Cost Summary

| Service | Spec | Monthly Cost |
|---------|------|--------------|
| Droplet | 1GB RAM, 1 CPU, 25GB SSD | $6 |
| Spaces | 250GB storage + CDN | $5 |
| Backups | Optional snapshots | $1 |
| SMTP | Resend free tier | $0 |
| **Total** | | **$12/month** |

## Step 1: Configure SMTP

Email verification is **required** for PDS. You need to set up SMTP before deploying.

### Option A: Resend (Recommended)

**Free tier: 3,000 emails/month**

1. Sign up: https://resend.com
2. Verify your domain (aesthetic.computer)
3. Create API key
4. Add to vault:

```bash
# Edit vault deployment config
nano /workspaces/aesthetic-computer/aesthetic-computer-vault/at/deploy.env

# Add these lines:
SMTP_URL=smtps://resend:re_YOUR_API_KEY@smtp.resend.com:465/
SMTP_FROM_ADDRESS=noreply@aesthetic.computer
```

### Option B: SendGrid

1. Sign up: https://sendgrid.com
2. Create API key
3. Add to vault:

```bash
SMTP_URL=smtps://apikey:SG.YOUR_API_KEY@smtp.sendgrid.net:465/
SMTP_FROM_ADDRESS=noreply@aesthetic.computer
```

### Option C: Gmail (Testing Only)

1. Enable 2FA: https://myaccount.google.com/security
2. Create app password: https://myaccount.google.com/apppasswords
3. Add to vault:

```bash
SMTP_URL=smtps://your-email@gmail.com:YOUR_APP_PASSWORD@smtp.gmail.com:465/
SMTP_FROM_ADDRESS=your-email@gmail.com
```

## Step 2: Install doctl

DigitalOcean CLI tool for automation.

```bash
# macOS
brew install doctl

# Linux
cd ~
wget https://github.com/digitalocean/doctl/releases/download/v1.104.0/doctl-1.104.0-linux-amd64.tar.gz
tar xf doctl-*.tar.gz
sudo mv doctl /usr/local/bin

# Initialize
doctl auth init
# Enter your DO token from vault: DO_TOKEN_PLACEHOLDER
```

## Step 3: Generate PDS Environment File

This creates the PDS configuration from your vault credentials:

```bash
cd /workspaces/aesthetic-computer/at/pds/deployment/digitalocean
fish generate-pds-env.fish
```

This will create `config/pds.env` with your Spaces keys and settings.

## Step 4: Run Deployment Script

The automated deployment script will:
1. Create SSH key
2. Create Spaces bucket
3. Provision droplet
4. Configure firewall
5. Install PDS

```bash
cd /workspaces/aesthetic-computer/at/pds/deployment/digitalocean
fish deploy.fish
```

The script will prompt you to configure DNS.

## Step 5: Configure DNS

While the script is running, configure these DNS records:

### DNS Records to Add

**In your DNS provider (Cloudflare, Namecheap, etc.):**

| Type | Name | Value | TTL |
|------|------|-------|-----|
| A | `pds` | `<DROPLET_IP>` | 600 |
| A | `*.pds` | `<DROPLET_IP>` | 600 |

Replace `<DROPLET_IP>` with the IP shown by the deployment script.

### Verify DNS

```bash
# Check root domain
dig pds.aesthetic.computer A

# Check wildcard
dig jeffrey.pds.aesthetic.computer A
dig test.pds.aesthetic.computer A

# All should return the same IP
```

## Step 6: Complete Installation

After the script provisions the droplet, you'll need to complete the PDS installation:

### SSH to Server

```bash
# IP will be shown by deployment script
ssh -i ~/.ssh/aesthetic_pds root@<DROPLET_IP>
```

### Upload Configuration

```bash
# From your local machine
scp -i ~/.ssh/aesthetic_pds \
  /workspaces/aesthetic-computer/at/pds/config/pds.env \
  root@<DROPLET_IP>:/pds/pds.env
```

### Restart PDS

```bash
# On the server
systemctl restart pds
systemctl status pds

# Check logs
docker logs pds --tail 50
```

## Step 7: Verify Health

From your local machine:

```bash
# Check health endpoint
curl https://pds.aesthetic.computer/xrpc/_health

# Should return: {"version":"0.4.x"}

# Check WebSocket
wsdump "wss://pds.aesthetic.computer/xrpc/com.atproto.sync.subscribeRepos?cursor=0"
```

## Step 8: Create First Account

On the server:

```bash
# Create admin account
pdsadmin account create

# Enter:
#   Handle: jeffrey.pds.aesthetic.computer
#   Email: me@jas.life
#   Password: (secure password)

# Or create invite code for later
pdsadmin create-invite-code
```

## Step 9: Test with Bluesky App

1. Open Bluesky app or go to https://bsky.app
2. Click "Sign in"
3. Choose "Sign in with another account"
4. Enter hosting provider: `pds.aesthetic.computer`
5. Sign in with your handle and password

## Step 10: Migrate @aesthetic.computer

If you want to migrate the existing @aesthetic.computer account from bsky.social:

```bash
# On the server
pdsadmin account migrate did:plc:YOUR_DID

# Follow the prompts
```

See [ACCOUNT_MIGRATION.md](https://github.com/bluesky-social/pds/blob/main/ACCOUNT_MIGRATION.md) for details.

## Post-Deployment Tasks

### Set Up Automated Backups

```bash
# On the server
crontab -e

# Add daily backup at 2 AM
0 2 * * * /root/backup.sh > /dev/null 2>&1
```

### Enable Monitoring

```bash
# Copy monitoring scripts to server
scp -i ~/.ssh/aesthetic_pds \
  /workspaces/aesthetic-computer/at/pds/scripts/health-check.sh \
  root@<DROPLET_IP>:/root/

# Set up cron for health checks
crontab -e

# Add health check every 5 minutes
*/5 * * * * /root/health-check.sh > /tmp/health.log 2>&1
```

### Configure Firewall (Restrict SSH)

```bash
# Get your current IP
curl ifconfig.me

# Update firewall to allow only your IP for SSH
doctl compute firewall update <FIREWALL_ID> \
  --inbound-rules "protocol:tcp,ports:22,address:YOUR_IP/32"
```

## Troubleshooting

### PDS Won't Start

```bash
# Check logs
docker logs pds

# Common issues:
# 1. SMTP not configured → Add to pds.env
# 2. DNS not propagated → Wait 10 minutes
# 3. Ports blocked → Check firewall
```

### Can't Create Account

```bash
# Verify SMTP
docker logs pds | grep -i smtp

# Test email sending
curl -X POST https://pds.aesthetic.computer/xrpc/com.atproto.server.requestEmailConfirmation \
  -H "Content-Type: application/json" \
  -d '{"email":"me@jas.life"}'
```

### SSL Certificate Issues

```bash
# Check Caddy logs
docker logs pds | grep -i caddy

# Verify DNS is correct
dig pds.aesthetic.computer A

# Wait for Let's Encrypt (can take 30-60 seconds)
```

### WebSocket Not Working

```bash
# Check that WebSocket endpoint is accessible
curl -i -N -H "Connection: Upgrade" -H "Upgrade: websocket" \
  https://pds.aesthetic.computer/xrpc/com.atproto.sync.subscribeRepos
```

## Useful Commands

```bash
# On the server

# Restart PDS
systemctl restart pds

# View logs
docker logs pds --tail 100 --follow

# Update PDS
pdsadmin update

# Create invite code
pdsadmin create-invite-code

# List accounts
pdsadmin account list

# Check disk usage
df -h /pds

# Check Spaces usage
du -sh /pds/blobs

# Export account
pdsadmin account export jeffrey.pds.aesthetic.computer

# Database info
sqlite3 /pds/accounts.sqlite "SELECT * FROM account;"
```

## Maintenance Schedule

| Task | Frequency | Command |
|------|-----------|---------|
| Update PDS | Weekly | `pdsadmin update` |
| Backup databases | Daily | `/root/backup.sh` |
| Check health | Hourly | `/root/health-check.sh` |
| Review logs | Daily | `docker logs pds` |
| Check disk space | Weekly | `df -h` |
| Update droplet | Monthly | `apt update && apt upgrade` |

## Scaling

When you outgrow a 1GB droplet:

```bash
# Resize droplet
doctl compute droplet-action resize <DROPLET_ID> --size s-2vcpu-2gb --wait

# Or create larger droplet and migrate
# See INFRASTRUCTURE.md for scaling strategies
```

## Rollback

If something goes wrong:

```bash
# Destroy droplet
doctl compute droplet delete aesthetic-pds

# Spaces and data persist
# Re-run deployment script to create new droplet

# Restore from backup
scp -i ~/.ssh/aesthetic_pds \
  /backups/pds-backup-latest.tar.gz \
  root@<NEW_IP>:/root/

# Extract and restore
tar xzf pds-backup-latest.tar.gz
cp *.sqlite /pds/
systemctl restart pds
```

## Next Steps

- Set up monitoring alerts (Datadog, Grafana Cloud)
- Configure off-site backups (DigitalOcean Spaces in different region)
- Create custom lexicons for AC-specific features
- Build admin dashboard
- Document migration process for existing users

---

**Support:**
- [PDS Admins Discord](https://discord.gg/e7hpHxRfBP)
- [AT Protocol Docs](https://atproto.com)
- [PDS GitHub](https://github.com/bluesky-social/pds)

**Last Updated**: October 9, 2025
