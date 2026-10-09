# Aesthetic Computer PDS (Personal Data Server)

This directory contains everything needed to deploy, manage, and maintain Aesthetic Computer's own ATProto Personal Data Server.

## Overview

Running our own PDS gives us:

- **Full Control**: Own our data and infrastructure
- **Custom Handles**: `*.aesthetic.computer` handles for all users
- **Custom Lexicons**: Build AC-specific schemas and features
- **Privacy**: No third-party data hosting
- **Federation**: Still interoperate with Bluesky and other ATProto networks

## Current Status — 2026-10-08

Live at **https://at.aesthetic.computer**, serving 4,203 repositories marked
active (account state, not daily activity). The running version is `0.4.5037`;
pulling the official `ghcr.io/bluesky-social/pds:0.4` image confirmed it is current.
Your `jeffrey.at.aesthetic.computer` repository's latest revision and CID match
Bluesky's relay. The separate `aesthetic.computer` account remains Bluesky-hosted.

The droplet also hosts [Tangled Knot](../knot/README.md). Run
`bash at/pds/scripts/health-check.sh` from the repository root for public checks.
The deployment sizing and initial setup notes below are historical planning;
the live hostname is `at.aesthetic.computer`, not `pds.aesthetic.computer`.

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                  Aesthetic Computer                  │
│                   pds.aesthetic.computer             │
└─────────────────────────────────────────────────────┘
                          │
         ┌────────────────┼────────────────┐
         │                │                │
    ┌────▼────┐     ┌────▼────┐     ┌────▼────┐
    │  PDS    │     │ Blob    │     │  SMTP   │
    │ Server  │     │ Storage │     │ Service │
    └─────────┘     └─────────┘     └─────────┘
         │                │
    ┌────▼────┐     ┌────▼────────────┐
    │ SQLite  │     │ DigitalOcean    │
    │ Database│     │ Spaces or GCS   │
    └─────────┘     └─────────────────┘
```

## Directory Structure

```
pds/
├── README.md              # This file
├── INFRASTRUCTURE.md      # Detailed infrastructure guide
├── DEPLOYMENT.md          # Step-by-step deployment guide
├── STORAGE.md            # Blob storage configuration
├── MAINTENANCE.md        # Ongoing maintenance guide
├── deployment/           # Deployment automation
│   ├── digitalocean/    # DO-specific scripts
│   ├── gcp/             # Google Cloud scripts
│   └── terraform/       # Infrastructure as code
├── config/              # Configuration templates
│   ├── pds.env.example  # PDS environment vars
│   ├── caddy/           # Caddy config
│   └── docker/          # Docker compose files
├── scripts/             # Admin and utility scripts
│   ├── deploy.sh        # Main deployment script
│   ├── backup.sh        # Backup automation
│   ├── health-check.sh  # Health monitoring
│   └── migrate.sh       # Account migration
└── monitoring/          # Monitoring and alerting
    ├── metrics.mjs      # Metrics collection
    └── alerts.mjs       # Alert configuration
```

## Quick Decision Guide

### Cloud Provider Choice

**DigitalOcean** ✅ Recommended
- Simpler pricing
- Spaces (S3-compatible) for blob storage
- Good for 1-100 users
- $6/month droplet + $5/month Spaces
- Already have account and keys

**Google Cloud Platform**
- More complex but scalable
- Better for 100+ users
- Cloud Storage for blobs
- More expensive at small scale
- Overkill for initial deployment

**Recommendation**: Start with DigitalOcean, migrate to GCP if needed later.

### Blob Storage

**DigitalOcean Spaces** ✅ Recommended
- S3-compatible API
- Simple pricing: $5/month for 250GB
- Easy to set up
- Integrated with DO droplets
- CDN included

**Google Cloud Storage**
- More complex pricing
- Better for large scale
- More features (lifecycle, versioning)
- Requires GCP account

### Server Specs

**For 1-20 Users** ✅ Start Here
- 1 CPU core
- 1 GB RAM
- 20 GB SSD
- ~$6/month on DigitalOcean

**For 20-100 Users**
- 2 CPU cores
- 2 GB RAM
- 40 GB SSD
- ~$12/month on DigitalOcean

## Prerequisites

Before deploying your PDS:

### Required
- [x] Domain name (aesthetic.computer)
- [ ] DNS access (configure A records)
- [x] DigitalOcean account
- [x] DigitalOcean API token (in vault ✓)
- [x] DigitalOcean Spaces keys (in vault ✓)
- [x] SMTP service (Gmail configured ✓)

### Dev Container Tools
The following tools are included in the dev container for PDS deployment:
- ✅ `doctl` - DigitalOcean CLI (v1.109.0)
- ✅ `s3cmd` - S3-compatible storage management
- ✅ Fish shell - Deployment scripts
- ✅ Node.js with @atproto packages

**Note:** If you're not using the dev container, install `doctl` manually:
```bash
# Linux/macOS
wget https://github.com/digitalocean/doctl/releases/download/v1.109.0/doctl-1.109.0-linux-amd64.tar.gz
tar xf doctl-*.tar.gz
sudo mv doctl /usr/local/bin
```

### Optional but Recommended
- [ ] Monitoring service (Datadog, Grafana Cloud)
- [ ] Backup storage (separate from main server)
- [ ] CDN for blob storage (included with DO Spaces)

## Next Steps

1. **Review Documentation**
   - Read [INFRASTRUCTURE.md](./INFRASTRUCTURE.md) for technical details
   - Read [DEPLOYMENT.md](./DEPLOYMENT.md) for deployment guide
   - Read [STORAGE.md](./STORAGE.md) for storage configuration

2. **Prepare Infrastructure**
   - Set up DNS records
   - Create DigitalOcean droplet
   - Configure Spaces bucket
   - Set up SMTP service

3. **Deploy PDS**
   - Run deployment scripts
   - Configure environment
   - Create first account
   - Test federation

4. **Migrate @aesthetic.computer**
   - Export data from bsky.social
   - Import to self-hosted PDS
   - Update DID document
   - Verify federation

## Cost Estimate

### DigitalOcean Setup (Recommended)

| Service | Specs | Monthly Cost |
|---------|-------|--------------|
| Droplet | 1GB RAM, 1 CPU, 25GB SSD | $6 |
| Spaces | 250GB storage + CDN | $5 |
| Bandwidth | 1TB included | $0 |
| **Total** | | **$11/month** |

### Additional Costs

| Service | Monthly Cost |
|---------|--------------|
| SMTP (Resend) | Free tier: 3,000 emails/month |
| Domain | Already own aesthetic.computer |
| Backups | $1/month (DO backup service) |
| **Total** | **$12/month** |

## Support and Resources

- [Official PDS Docs](https://github.com/bluesky-social/pds)
- [AT Protocol Docs](https://atproto.com)
- [PDS Admins Discord](https://discord.gg/e7hpHxRfBP)
- [Bluesky API Docs](https://docs.bsky.app)

## Status Updates

Track progress in the todo list and update this section as deployment proceeds.

---

**Last Updated**: October 9, 2025
**Status**: Planning Phase
**Target Deployment**: TBD
