# Scripts Setup & Requirements

## 🔧 How These Scripts Work

All scripts in `scripts/` are designed to run from the **workspace root** (`/workspaces/aesthetic-computer/`).

### Import Resolution
- ✅ **Backend modules**: `import { connect } from "../../system/backend/database.mjs"`
  - Scripts access backend utilities via relative paths
  - Path goes up 2 levels from `scripts/atproto/` to root, then into `system/backend/`
  
- ✅ **Node modules**: `import { AtpAgent } from "@atproto/api"`
  - Installed in root `node_modules/` (shared with system/node_modules)
  - Node.js walks up from script location to find packages
  - Run `npm install` at root to ensure deps are available

### Environment Variables

Scripts load `.env` from `system/.env` using:
```javascript
import { config } from 'dotenv';
config({ path: './system/.env' });
```

**Note**: Path is relative to where you run the script (workspace root).

**Required in `system/.env`:**
```bash
# MongoDB
MONGODB_CONNECTION_STRING=mongodb+srv://...
MONGODB_NAME=aesthetic

# ATProto PDS
PDS_URL=https://at.aesthetic.computer

# DigitalOcean Spaces (for recovery scripts)
ART_SPACES_KEY=your_key
ART_SPACES_SECRET=your_secret
ART_SPACES_BUCKET=art-aesthetic-computer
ART_SPACES_REGION=nyc3
ART_SPACES_ENDPOINT=https://nyc3.digitaloceanspaces.com

AT_BLOBS_SPACES_KEY=your_key
AT_BLOBS_SPACES_SECRET=your_secret
AT_BLOBS_SPACES_BUCKET=at-blobs-aesthetic-computer
AT_BLOBS_SPACES_REGION=nyc3
AT_BLOBS_SPACES_ENDPOINT=https://nyc3.digitaloceanspaces.com

# Anonymous art account password
ANON_PASSWORD=your_hash
```

**User ATProto Credentials** (stored in MongoDB `users` collection):
```javascript
{
  "atproto": {
    "did": "did:plc:...",
    "handle": "user.at.aesthetic.computer",
    "password": "encrypted_app_password"
  }
}
```

### Using aesthetic-computer-vault for Secrets

The `aesthetic-computer-vault/` directory contains **actual secrets**:

```bash
# Instead of editing system/.env manually, use:
cd aesthetic-computer-vault
source devault.fish  # Loads secrets from vault/.env

# Or copy vault env to system:
cp aesthetic-computer-vault/.env system/.env
```

**Vault contains:**
- `ANON_PASSWORD` - Anonymous art account password hash
- `OVEN_CALLBACK_SECRET` - Oven webhook authentication
- SSH credentials for friend's server

## 📦 Dependencies

**Scripts share dependencies with the main workspace** - packages are installed at root level and duplicated in system/ for Netlify functions.

### Installing Dependencies

```bash
# Install at root (needed for scripts)
cd /workspaces/aesthetic-computer
npm install

# Key packages (already in package.json):
# - @atproto/api ^0.17.2 (ATProto client)
# - @aws-sdk/client-s3 ^3.896.0 (DigitalOcean Spaces)
# - mongodb ^6.20.0 (MongoDB driver)
# - dotenv ^16.4.7 (Environment variable loading)
# - node-fetch ^3.3.2 (HTTP requests)
```

These same packages are also in `system/package.json` for Netlify functions, which is fine - small duplication for clean architecture.

## 🚀 Running Scripts

**Always run from workspace root** so paths resolve correctly:

```bash
# ✅ Correct (from /workspaces/aesthetic-computer/)
node scripts/atproto/sync-atproto.mjs live --kidlisp-only
node scripts/recovery/check-tape-zips-in-spaces.mjs @jeffrey

# ❌ Wrong (wrong working directory)
cd scripts/atproto
node sync-atproto.mjs  # Can't find system/.env or system/backend/
```

## 🔍 Troubleshooting

### "Cannot find module '../system/backend/database.mjs'"
- **Cause**: Running script from wrong directory
- **Fix**: `cd /workspaces/aesthetic-computer && node scripts/...`

### "MONGODB_CONNECTION_STRING is not set"
- **Cause**: Missing or incorrect .env file
- **Fix**: 
  1. Check `system/.env` exists
  2. Verify it contains `MONGODB_CONNECTION_STRING`
  3. Or copy from vault: `cp aesthetic-computer-vault/.env system/.env`

### "Cannot use a session that has ended"
- **Cause**: ATProto credentials invalid or expired
- **Fix**: 
  1. Check user record in MongoDB has valid `atproto.password`
  2. Regenerate app password at https://at.aesthetic.computer
  3. Update MongoDB user record

### "AccessDenied" from DigitalOcean Spaces
- **Cause**: Missing or invalid DO Spaces credentials
- **Fix**: Add `ART_SPACES_*` and `AT_BLOBS_SPACES_*` to `system/.env`

## 🔐 Security Notes

- ❌ **Never commit** `system/.env` (already in `.gitignore`)
- ✅ **Always use** `aesthetic-computer-vault/.env` for secrets
- 🔒 **Vault is private** - only accessible to authorized developers
- 🚨 **Rotate credentials** if exposed in logs or commits
