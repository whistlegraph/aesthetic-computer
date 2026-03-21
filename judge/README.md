# Judge Deployment

This directory contains deployment configuration for the Judge AI moderation service.

## Quick Start

1. **Copy example configs:**
   ```bash
   cp deploy.env.example deploy.env
   cp .env.example .env
   ```

2. **Edit `deploy.env` with your credentials:**
   - `DO_TOKEN`: DigitalOcean API token
   - `CLOUDFLARE_ZONE_ID`: Cloudflare zone ID for aesthetic.computer
   - `CLOUDFLARE_API_TOKEN`: Cloudflare API token with DNS edit permissions

3. **Edit `.env` with MongoDB credentials:**
   - `MONGODB_CONNECTION_STRING`: MongoDB Atlas connection string

4. **Run deployment:**
   ```bash
   cd /workspaces/aesthetic-computer/judge
   ./deploy.fish
   ```

## Files

- `deploy.env` - DigitalOcean and DNS configuration (gitignored)
- `.env` - Service environment variables (gitignored)
- `deploy.env.example` - Template for deploy.env
- `.env.example` - Template for .env

## Security

⚠️ **Never commit `deploy.env` or `.env` to git!**

These files contain sensitive credentials and are gitignored by default.
