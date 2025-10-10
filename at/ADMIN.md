# ATProto PDS Management

Quick reference for managing the aesthetic.computer Personal Data Server (PDS).

## Quick Commands

### Access PDS Admin Console

```fish
# Drop into interactive SSH shell on PDS server
ac-pds

# Run pdsadmin commands directly
ac-pds account create          # Create a new account
ac-pds account list             # List all accounts
ac-pds create-invite-code       # Generate an invite code
ac-pds update                   # Update PDS to latest version
```

## Server Details

- **Hostname**: `pds.aesthetic.computer`
- **Wildcard**: `*.pds.aesthetic.computer`
- **Droplet**: `aesthetic-computer-pds` (DigitalOcean NYC3)
- **IP**: `138.197.35.160`
- **Storage**: `pds-blobs-aesthetic-computer` (DigitalOcean Spaces)
- **Admin Email**: `mail@aesthetic.computer`

## Common Tasks

### Create Your First Account

```fish
# Interactive mode
ac-pds
# Then on server:
pdsadmin account create

# Or directly from dev container:
ac-pds account create
```

You'll be prompted for:
- Email address
- Handle (e.g., `you.pds.aesthetic.computer`)
- Password

### Generate Invite Code

```fish
ac-pds create-invite-code
```

### Check PDS Health

```fish
# From dev container
curl https://pds.aesthetic.computer/xrpc/_health

# Should return: {"version":"0.4.x"}
```

### Check Service Status

```fish
ac-pds
# Then on server:
systemctl status pds
docker ps
```

### View PDS Logs

```fish
ac-pds
# Then on server:
docker logs pds
journalctl -u pds -f
```

### Update PDS

```fish
ac-pds update
```

## Configuration

### PDS Environment File

Located at: `/pds/pds.env` on server

Key settings:
- `PDS_HOSTNAME`: Domain name
- `PDS_EMAIL_SMTP_URL`: SMTP settings (Gmail configured)
- `PDS_EMAIL_FROM_ADDRESS`: Email sender
- `PDS_DATA_DIRECTORY`: SQLite database location
- `PDS_BLOBSTORE_*`: DigitalOcean Spaces configuration

To edit:
```fish
ac-pds
nano /pds/pds.env
systemctl restart pds
```

### Spaces Credentials (Blob Storage)

Already configured in `/pds/pds.env`:
- Region: `nyc3`
- Bucket: `pds-blobs-aesthetic-computer`
- Endpoint: `https://nyc3.digitaloceanspaces.com`

## Bluesky App Integration

1. Open [Bluesky app](https://bsky.app) or mobile app
2. Click "Sign in" → "Hosting Provider"
3. Enter: `https://pds.aesthetic.computer`
4. Use your account credentials or invite code

Note: First time subdomain certificates take 10-30s to provision.

## Deployment

See `/at/pds/` directory for:
- `QUICKSTART.md` - 15-minute deployment guide
- `DEPLOYMENT.md` - Complete setup instructions
- `MAINTENANCE.md` - Operations guide
- `deployment/digitalocean/` - Automated deployment scripts

### Redeploy from Scratch

```fish
cd /workspaces/aesthetic-computer/at/pds/deployment/digitalocean
fish deploy.fish
```

## Troubleshooting

### DNS Not Resolving

```fish
# Check DNS propagation
dig pds.aesthetic.computer
dig random.pds.aesthetic.computer

# Should both return: 138.197.35.160
```

### PDS Not Responding

```fish
ac-pds
systemctl status pds
docker ps
docker logs pds
```

### Email Not Sending

Check SMTP configuration in `/pds/pds.env`:
```fish
ac-pds
grep SMTP /pds/pds.env
systemctl restart pds
```

### Spaces Connection Issues

```fish
ac-pds
grep BLOBSTORE /pds/pds.env
# Verify credentials match vault: ~/aesthetic-computer/aesthetic-computer-vault/at/deploy.env
```

## Backup & Recovery

### Manual Backup

```fish
ac-pds
/pds/pds-backup.sh
# Backups stored in: pds-blobs-aesthetic-computer/backups/
```

### Restore from Backup

```fish
ac-pds
# Stop PDS
systemctl stop pds

# Restore databases
aws s3 cp s3://pds-blobs-aesthetic-computer/backups/latest/accounts.sqlite /pds/
aws s3 cp s3://pds-blobs-aesthetic-computer/backups/latest/pds.sqlite /pds/

# Restart
systemctl start pds
```

## Security

- SSH access via key authentication only (`~/.ssh/aesthetic_pds`)
- Firewall configured for ports 22, 80, 443 only
- Let's Encrypt SSL/TLS via Caddy (auto-renewed)
- SMTP password stored in vault, not committed to git

## Monitoring

### Health Check Script

Located at: `/at/pds/scripts/health-check.sh`

```fish
cd /workspaces/aesthetic-computer/at/pds/scripts
bash health-check.sh
```

### Metrics to Watch

- Disk usage: `df -h` on server
- Memory: `free -h`
- Docker status: `docker stats pds`
- Log size: `du -sh /pds/*.log`

## Cost

- Droplet: $6/month (1GB RAM, 1 vCPU, 25GB SSD)
- Spaces: $5/month (250GB storage, 1TB transfer)
- **Total: ~$11/month**

## References

- Official PDS Docs: https://github.com/bluesky-social/pds
- ATProto Specs: https://atproto.com/
- Discord: https://discord.gg/e7hpHxRfBP (AT Protocol PDS Admins)

## Related Tools

- Account management: `/at/post-to-bluesky.mjs`
- Painting sharing: `/at/share-latest-painting.mjs`
- Profile queries: `/at/query-profile.mjs`
- Post management: `/at/delete-all-posts.mjs`
