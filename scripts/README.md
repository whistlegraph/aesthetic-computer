# System Scripts Omakase Menu 🍱

Operational and maintenance scripts for Aesthetic Computer backend systems.

## 📁 Directory Structure

- **`atproto/`** - ATProto sync, backfill, and verification scripts
- **`recovery/`** - Data recovery and repair scripts
- **`migration/`** - One-off database migrations (historical)

## 🦋 ATProto Scripts (`atproto/`)

### Sync & Backfill
- **`sync-atproto.mjs`** - Master sync script for all content types (kidlisp, paintings, tapes, moods)
  - Usage: `node scripts/atproto/sync-atproto.mjs [live] [--type] [--user] [--anonymous-only]`
  - Syncs MongoDB content to ATProto PDS and updates rkey backrefs

- **`backfill-kidlisp-rkeys.mjs`** - Match existing ATProto kidlisp to MongoDB and update rkeys
  - Usage: `node scripts/atproto/backfill-kidlisp-rkeys.mjs [live]`
  - For @jeffrey, art-guest, and @fifi accounts

- **`sync-jeffrey-kidlisp.mjs`** - Focused sync for @jeffrey's missing kidlisp
  - Usage: `node scripts/atproto/sync-jeffrey-kidlisp.mjs [live]`
  - Creates ATProto records for kidlisp missing from PDS

- **`sync-fifi-kidlisp.mjs`** - Focused sync for @fifi's missing kidlisp
  - Usage: `node scripts/atproto/sync-fifi-kidlisp.mjs [live]`
  - Batch processes kidlisp records (10 at a time)

### Verification & Debugging
- **`check-kidlisp-atproto.mjs`** - Compare MongoDB vs PDS state for @jeffrey and art-guest
  - Usage: `node scripts/atproto/check-kidlisp-atproto.mjs`
  - Reports discrepancies and missing rkeys

- **`check-fifi-kidlisp.mjs`** - Compare MongoDB vs PDS state for @fifi
  - Usage: `node scripts/atproto/check-fifi-kidlisp.mjs`
  - Detailed analysis of sync status

### Development & Testing
- **`sync-mongodb-from-atproto.mjs`** - Reverse sync: pull data from PDS into MongoDB
  - Usage: `node scripts/atproto/sync-mongodb-from-atproto.mjs`
  - Useful for testing and data recovery scenarios

- **`test-mood-sync.mjs`** - Test mood syncing to ATProto
  - Usage: `node scripts/atproto/test-mood-sync.mjs`
  - Development/debugging script

## 🔧 Recovery Scripts (`recovery/`)

- **`recover-deleted-tapes.mjs`** - Recover tapes from DigitalOcean Spaces backups
  - Usage: `node scripts/recovery/recover-deleted-tapes.mjs [--dry-run] [--codes=...] [--rebake] [--recreate]`
  - Locates ZIP/MP4 files and optionally recreates MongoDB + ATProto records
  - Requires: DO_SPACES_KEY, DO_SPACES_SECRET env vars

- **`check-tape-zips-in-spaces.mjs`** - List all tape ZIP files in DigitalOcean Spaces
  - Usage: `node scripts/recovery/check-tape-zips-in-spaces.mjs`
  - Quick inventory of available tape backups

## 🎯 Common Workflows

### Check sync status for all users
```bash
node scripts/atproto/check-kidlisp-atproto.mjs
node scripts/atproto/check-fifi-kidlisp.mjs
```

### Backfill missing rkeys
```bash
# Dry run first
node scripts/atproto/backfill-kidlisp-rkeys.mjs

# Then live
node scripts/atproto/backfill-kidlisp-rkeys.mjs live
```

### Sync specific content types
```bash
# Kidlisp only
node scripts/atproto/sync-atproto.mjs live --kidlisp-only

# Anonymous content only (uses art-guest account)
node scripts/atproto/sync-atproto.mjs live --anonymous-only

# Specific user
node scripts/atproto/sync-atproto.mjs live --user=jeffrey
```

### Recover deleted tapes
```bash
# Check what can be recovered
node scripts/recovery/recover-deleted-tapes.mjs --dry-run

# Recover specific codes
node scripts/recovery/recover-deleted-tapes.mjs --codes=abc,xyz --recreate
```

## 📝 Notes

- All scripts support `--dry-run` or run without `live` argument for safe preview
- Scripts automatically use MongoDB from MONGODB_CONNECTION_STRING env var
- ATProto scripts require PDS_URL and PDS_ADMIN_PASSWORD env vars
- Always run verification scripts after sync/backfill operations

## 🔗 Related

- Backend modules: `system/backend/`
- Netlify functions: `system/netlify/functions/`
- AT Protocol tools: `at/scripts/atproto/` (external federation tools)
