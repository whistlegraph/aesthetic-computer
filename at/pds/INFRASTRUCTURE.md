# PDS Infrastructure Guide

This document provides detailed technical information for deploying and managing the Aesthetic Computer PDS infrastructure.

## Table of Contents

1. [Server Requirements](#server-requirements)
2. [Network Configuration](#network-configuration)
3. [Storage Architecture](#storage-architecture)
4. [Security Considerations](#security-considerations)
5. [Scaling Strategy](#scaling-strategy)

## Server Requirements

### Minimum Specifications (1-20 users)

```yaml
CPU: 1 core (x86_64 or ARM64)
RAM: 1 GB
Storage: 20 GB SSD
OS: Ubuntu 24.04 LTS (recommended)
Network: 1 Gbps
IPv4: Required (public)
IPv6: Optional but recommended
```

### Recommended Specifications (20-100 users)

```yaml
CPU: 2 cores
RAM: 2 GB
Storage: 40 GB SSD
OS: Ubuntu 24.04 LTS
Network: 1 Gbps
```

### Operating System Support

**Fully Supported:**
- Ubuntu 24.04 LTS ✅ (Recommended)
- Ubuntu 22.04 LTS
- Ubuntu 20.04 LTS
- Debian 12
- Debian 11

**Not Recommended:**
- CentOS/RHEL (limited testing)
- Alpine Linux (Docker base only)
- Windows Server (not supported)

## Network Configuration

### Required DNS Records

Configure these DNS records for `aesthetic.computer`:

```dns
# Root domain
pds.aesthetic.computer.     A     <SERVER_IP>     600

# Wildcard for user handles
*.pds.aesthetic.computer.   A     <SERVER_IP>     600
```

**Important Notes:**
- TTL of 600 (10 minutes) allows for quick changes
- Wildcard record is **required** for user handle federation
- Both records must point to the same IP

### Example with dig

```bash
# Verify root domain
dig pds.aesthetic.computer A

# Verify wildcard
dig jeffrey.pds.aesthetic.computer A
dig test123.pds.aesthetic.computer A
```

All should return your server's IP address.

### Firewall Rules

#### Inbound Rules (Required)

| Port | Protocol | Source | Purpose |
|------|----------|--------|---------|
| 443 | TCP | 0.0.0.0/0 | HTTPS (main traffic) |
| 80 | TCP | 0.0.0.0/0 | HTTP (Let's Encrypt only) |
| 22 | TCP | YOUR_IP | SSH (restrict to your IP) |

#### Outbound Rules (Required)

| Port | Protocol | Destination | Purpose |
|------|----------|-------------|---------|
| 443 | TCP | Any | HTTPS (federation, relay) |
| 587 | TCP | SMTP server | Email (STARTTLS) |
| 465 | TCP | SMTP server | Email (SSL/TLS) |

**Security Notes:**
- Do NOT expose port 22 to the world
- Do NOT expose Docker ports (2375, 2376)
- Do NOT expose PostgreSQL or SQLite ports

### DigitalOcean Firewall Setup

```bash
# Create firewall
doctl compute firewall create \
  --name "pds-aesthetic-computer" \
  --inbound-rules "protocol:tcp,ports:80,address:0.0.0.0/0 protocol:tcp,ports:443,address:0.0.0.0/0 protocol:tcp,ports:22,address:YOUR_IP/32" \
  --outbound-rules "protocol:tcp,ports:all,address:0.0.0.0/0 protocol:udp,ports:all,address:0.0.0.0/0"
```

## Storage Architecture

### SQLite Database

The PDS uses SQLite for all relational data:

```
/pds/
├── blocks.sqlite       # User data, posts, blocks
├── accounts.sqlite     # User accounts, auth
└── pds.sqlite         # PDS metadata
```

**Characteristics:**
- Single-file databases
- No separate DB server needed
- Suitable for <100 users
- Automatic backups recommended

**Backup Strategy:**
```bash
# Daily backup script
sqlite3 /pds/blocks.sqlite ".backup /backups/blocks-$(date +%Y%m%d).sqlite"
```

### Blob Storage (Images, Videos)

Blobs are stored separately from SQLite:

#### Option 1: DigitalOcean Spaces (Recommended)

```yaml
Provider: DigitalOcean Spaces
API: S3-compatible
Pricing: $5/month for 250GB + CDN
Regions: NYC3, SFO3, SGP1, FRA1
```

**Configuration:**
```env
# In pds.env
PDS_BLOBSTORE_DISK_LOCATION=s3
PDS_BLOBSTORE_S3_ENDPOINT=https://nyc3.digitaloceanspaces.com
PDS_BLOBSTORE_S3_BUCKET=aesthetic-pds-blobs
PDS_BLOBSTORE_S3_ACCESS_KEY_ID=<spaces-key>
PDS_BLOBSTORE_S3_SECRET_ACCESS_KEY=<spaces-secret>
```

**Advantages:**
- CDN included (no extra cost)
- S3-compatible API
- Simple pricing
- Integrated with DO droplets

#### Option 2: Google Cloud Storage

```yaml
Provider: Google Cloud Storage
API: GCS native
Pricing: ~$0.02/GB/month + egress
Regions: Multi-region options
```

**Configuration:**
```env
# In pds.env
PDS_BLOBSTORE_DISK_LOCATION=gcs
PDS_BLOBSTORE_GCS_BUCKET=aesthetic-pds-blobs
PDS_BLOBSTORE_GCS_PROJECT_ID=aesthetic-computer
GOOGLE_APPLICATION_CREDENTIALS=/pds/gcs-service-account.json
```

**Advantages:**
- Better for large scale
- Advanced features (lifecycle, versioning)
- Global CDN available
- Integrated with GCP services

#### Option 3: Local Disk (Not Recommended for Production)

```env
PDS_BLOBSTORE_DISK_LOCATION=/pds/blobs
```

**Use Cases:**
- Development/testing only
- No redundancy
- Limited by disk size
- No CDN

### Storage Growth Estimates

| Users | Posts/Day | Avg Post Size | Blobs/Day | Storage/Month |
|-------|-----------|---------------|-----------|---------------|
| 10 | 50 | 2KB + 500KB | 25 images | ~15 GB |
| 50 | 250 | 2KB + 500KB | 125 images | ~75 GB |
| 100 | 500 | 2KB + 500KB | 250 images | ~150 GB |

**Notes:**
- SQLite grows slowly (~50MB/month per 10 users)
- Blobs dominate storage (images, videos)
- Plan for 3-6 months of growth

## Security Considerations

### TLS/SSL Certificates

**Automatic via Caddy:**
- PDS includes Caddy web server
- Auto-obtains Let's Encrypt certificates
- Auto-renews before expiration
- Handles wildcard certificates

**No manual configuration needed!**

### Authentication

**User Authentication:**
- App passwords (recommended for clients)
- OAuth (future support)
- Email verification required

**Admin Authentication:**
- SSH key-based (disable password auth)
- pdsadmin CLI (local only)
- No web admin panel

### Rate Limiting

Built into PDS:
```env
PDS_RATE_LIMIT_ENABLED=true
PDS_RATE_LIMIT_REQUESTS_PER_MINUTE=100
```

### Backup Security

```bash
# Encrypt backups
tar czf - /pds/*.sqlite | gpg --encrypt --recipient admin@aesthetic.computer > backup.tar.gz.gpg

# Store off-site
rclone copy backup.tar.gz.gpg digitalocean:aesthetic-backups/
```

## Scaling Strategy

### Vertical Scaling (Easier)

Increase droplet size as needed:

```
1GB RAM → 2GB RAM → 4GB RAM → 8GB RAM
$6/mo → $12/mo → $24/mo → $48/mo
```

**When to scale:**
- CPU usage > 80% sustained
- Memory usage > 90%
- Disk I/O wait times increasing

### Horizontal Scaling (Advanced)

For 100+ users:

```
┌─────────┐     ┌─────────┐     ┌─────────┐
│  PDS 1  │────▶│  Relay  │◀────│  PDS 2  │
└─────────┘     └─────────┘     └─────────┘
     │               │               │
     └───────────────┴───────────────┘
                     │
              ┌──────▼──────┐
              │ Shared Blob │
              │   Storage   │
              └─────────────┘
```

**Considerations:**
- Multiple PDS instances
- Shared blob storage
- Custom relay/firehose
- Load balancer
- Complex to maintain

**Recommendation:** Start with vertical scaling, consider horizontal only if >100 active users.

## Monitoring Requirements

### Health Checks

Monitor these endpoints:

```bash
# PDS health
curl https://pds.aesthetic.computer/xrpc/_health

# WebSocket health
wsdump "wss://pds.aesthetic.computer/xrpc/com.atproto.sync.subscribeRepos?cursor=0"
```

### Metrics to Track

| Metric | Threshold | Action |
|--------|-----------|--------|
| CPU Usage | > 80% | Scale up |
| Memory Usage | > 90% | Scale up |
| Disk Usage | > 80% | Add storage |
| Response Time | > 500ms | Investigate |
| Error Rate | > 1% | Alert |

### Log Management

```bash
# Docker logs
docker logs pds --tail 100 --follow

# Or file-based
tail -f /pds/pds.log
```

**Log Rotation:**
```env
LOG_DESTINATION=/pds/pds.log
LOG_LEVEL=info
```

Use logrotate or similar to prevent disk fill.

## Maintenance Windows

**Recommended Schedule:**
- Updates: Weekly, off-peak hours
- Backups: Daily, automated
- Full maintenance: Monthly

**Update Process:**
```bash
# Simple update
sudo pdsadmin update

# With backup first
./scripts/backup.sh && sudo pdsadmin update
```

## Disaster Recovery

### Backup Strategy

**Daily:**
- SQLite databases
- PDS configuration

**Weekly:**
- Full system snapshot (DigitalOcean)

**Monthly:**
- Off-site backup verification

### Recovery Procedures

**Database Corruption:**
```bash
# Restore from backup
cp /backups/blocks-20251009.sqlite /pds/blocks.sqlite
systemctl restart pds
```

**Server Failure:**
1. Provision new server
2. Restore latest snapshot
3. Update DNS if IP changed
4. Verify health endpoints

**Blob Storage Loss:**
- Blobs are stored redundantly in Spaces/GCS
- No manual recovery needed
- Verify bucket versioning enabled

## Cost Optimization

### DigitalOcean Tips

**Reserved Instances:**
- Not available on DO
- Pay monthly for lower costs

**Spaces Optimization:**
- Use lifecycle policies to delete old blobs
- Enable CDN caching (included)
- Monitor bandwidth usage

**Snapshot Strategy:**
- Keep 7 daily snapshots: ~$1/month
- Delete older snapshots: saves $0.10/GB/month

### Bandwidth Savings

```bash
# Enable compression in Caddy
# Already enabled in default PDS config
encode gzip
```

**CDN Benefits:**
- Spaces includes CDN
- Reduces origin bandwidth
- Faster global delivery

## Next Steps

- Review [DEPLOYMENT.md](./DEPLOYMENT.md) for deployment instructions
- Review [STORAGE.md](./STORAGE.md) for detailed storage setup
- Review [MAINTENANCE.md](./MAINTENANCE.md) for ongoing operations

---

**Last Updated**: October 9, 2025
