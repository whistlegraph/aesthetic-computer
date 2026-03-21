# Aesthetic.Computer DP-1 Feed Deployment

This directory contains deployment documentation and scripts for deploying the DP-1 Feed API to aesthetic.computer's Cloudflare Workers infrastructure.

**Location**: `feed/dp1-deploy/` (outside the dp1-feed subproject to keep aesthetic.computer-specific configs separate)

## Files

- `wrangler.production.toml` - **Backup copy** of Cloudflare Worker config
- `deploy-aesthetic.sh` - Deployment script wrapper
- `AESTHETIC-DEPLOY.md` - This file

**Note**: The actual `wrangler.production.toml` used by wrangler must be in `feed/dp1-feed/` (it's gitignored there). This directory keeps a backup copy that can be auto-copied if needed.

## Quick Deploy

From anywhere in the aesthetic-computer repo:

```bash
bash feed/dp1-deploy/deploy-aesthetic.sh
```

Or from this directory:

```bash
bash deploy-aesthetic.sh
```

This will:
1. Check if `wrangler.production.toml` exists in dp1-feed (copy from backup if needed)
2. Run validation tests in the dp1-feed directory
3. Deploy to Cloudflare
4. Deploy to: `https://aesthetic-feed.aesthetic-computer.workers.dev`

To skip validation (faster, but risky):
```bash
bash deploy-aesthetic.sh --skip-validation
```

## Manual Deploy

If you need to deploy manually:
```bash
cd feed/dp1-feed
wrangler deploy --config wrangler.production.toml
```

## Configuration

The `wrangler.production.toml` file contains:
- Worker name: `aesthetic-feed`
- KV Namespaces for aesthetic.computer
- Queue bindings for write operations
- Environment variables

## Directory Structure

```
feed/
├── dp1-deploy/              # Aesthetic.computer-specific deployment (tracked in git)
│   ├── deploy-aesthetic.sh  # Deployment wrapper script
│   ├── wrangler.production.toml  # Backup copy of config
│   └── AESTHETIC-DEPLOY.md  # This file
│
└── dp1-feed/                # Upstream dp1-feed repository
    ├── wrangler.toml        # Default config (tracked upstream)
    ├── wrangler.production.toml  # Your config (gitignored, copied from backup)
    └── ...
```

## Why This Structure?

- Keeps aesthetic.computer-specific configs in the aesthetic-computer repo
- Doesn't pollute the upstream dp1-feed repository
- The backup copy in `dp1-deploy/` is tracked in git
- The working copy in `dp1-feed/` is gitignored there but required by wrangler
