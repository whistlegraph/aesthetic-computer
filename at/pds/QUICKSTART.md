# PDS Quick Start

The fastest way to get your Aesthetic Computer PDS running on DigitalOcean.

## Prerequisites

- [x] DigitalOcean account with API token (in vault ✓)
- [x] DigitalOcean Spaces keys (in vault ✓)
- [x] Domain: aesthetic.computer (DNS access needed)
- [ ] **SMTP service** (required - see below)

## SMTP Setup (Required)

Before deploying, you **must** configure SMTP. Recommended: Resend (free tier).

1. **Sign up**: https://resend.com
2. **Get API key** from dashboard
3. **Add to vault**:

```bash
nano /workspaces/aesthetic-computer/aesthetic-computer-vault/at/deploy.env

# Add:
SMTP_URL=smtps://resend:re_YOUR_KEY@smtp.resend.com:465/
SMTP_FROM_ADDRESS=noreply@aesthetic.computer
```

## Deploy in 3 Commands

```bash
cd /workspaces/aesthetic-computer/at/pds/deployment/digitalocean

# 1. Generate config from vault
fish generate-pds-env.fish

# 2. Deploy infrastructure
fish deploy.fish

# 3. Follow prompts to configure DNS
```

That's it! The script will:
- ✓ Create Spaces bucket for blobs
- ✓ Provision $6/month droplet
- ✓ Configure firewall
- ✓ Install PDS

## DNS Configuration

When prompted, add these DNS records:

| Type | Name | Value | TTL |
|------|------|-------|-----|
| A | `pds` | `<DROPLET_IP>` | 600 |
| A | `*.pds` | `<DROPLET_IP>` | 600 |

The `<DROPLET_IP>` will be shown by the deployment script.

## Verify Deployment

```bash
# Health check
curl https://pds.aesthetic.computer/xrpc/_health

# Should return:
# {"version":"0.4.x"}
```

## Create Your Account

```bash
# SSH to server (key created by deploy script)
ssh -i ~/.ssh/aesthetic_pds root@<DROPLET_IP>

# Create account
pdsadmin account create
# Handle: jeffrey.pds.aesthetic.computer
# Email: me@jas.life
# Password: (secure password)
```

## Use with Bluesky

1. Open https://bsky.app
2. Sign in → "Sign in with another account"
3. Hosting provider: `pds.aesthetic.computer`
4. Enter your handle and password

## Cost

| Service | Monthly |
|---------|---------|
| Droplet (1GB) | $6 |
| Spaces (250GB) | $5 |
| SMTP (Resend free) | $0 |
| **Total** | **$11/month** |

## Next Steps

**Read the full guides:**
- [DEPLOYMENT.md](./DEPLOYMENT.md) - Complete deployment guide
- [INFRASTRUCTURE.md](./INFRASTRUCTURE.md) - Technical details
- [STORAGE.md](./STORAGE.md) - Blob storage setup
- [scripts/README.md](./scripts/README.md) - Management scripts

**Set up monitoring:**
```bash
# See scripts/README.md for automation
fish scripts/storage-manager.fish usage
./scripts/health-check.sh
```

## Troubleshooting

**Deployment fails?**
- Check SMTP is configured in vault
- Ensure `doctl` is installed and authenticated
- Verify DNS is accessible

**Can't create account?**
- Check PDS logs: `docker logs pds`
- Verify SMTP: Look for "email sent" in logs
- Wait 30 seconds for SSL certificate

**Need help?**
- [PDS Admins Discord](https://discord.gg/e7hpHxRfBP)
- [AT Protocol Docs](https://atproto.com)

---

**Estimated setup time**: 15-20 minutes

**Last Updated**: October 9, 2025
