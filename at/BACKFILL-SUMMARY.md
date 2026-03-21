# ATProto Backfill Implementation Summary

## What Was Created

Two backfill scripts to sync existing MongoDB data to ATProto:

### 1. Moods Backfill
**File**: `at/scripts/atproto/backfill-moods-to-atproto.mjs`

- Syncs all moods without ATProto records
- Fast (~1 per second)
- Processes in batches of 10 (configurable)
- ~25-30 minutes for ~1,500 moods

### 2. Paintings Backfill  
**File**: `at/scripts/atproto/backfill-paintings-to-atproto.mjs`

- Syncs all paintings without ATProto records
- Slow (downloads/uploads images)
- Processes in batches of 5 (configurable)
- ~4-8 hours for ~2,800 paintings

## Features

Both scripts include:

✅ **Dry-run mode** - Test without making changes (`--dry-run`)
✅ **Limiting** - Process only N records (`--limit N`)
✅ **User filtering** - Process specific user (`--user @handle`)
✅ **Batch processing** - Configurable batch sizes and delays
✅ **Progress tracking** - Real-time progress display
✅ **Error handling** - Continue on failures, collect errors
✅ **Summary report** - Detailed success/failure counts
✅ **Safe execution** - Only syncs missing records, doesn't modify existing

## Quick Start

```bash
cd /workspaces/aesthetic-computer

# Test moods backfill (dry run)
node at/scripts/atproto/backfill-moods-to-atproto.mjs --dry-run --limit 10

# Run moods backfill (fast)
node at/scripts/atproto/backfill-moods-to-atproto.mjs

# Test paintings backfill (dry run)
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --dry-run --limit 5

# Run paintings backfill (SLOW - several hours!)
node at/scripts/atproto/backfill-paintings-to-atproto.mjs
```

## Options Reference

### Moods
- `--dry-run` - Preview without syncing
- `--limit N` - Only process N moods
- `--user @handle` - Only process specific user
- `--batch-size N` - Process N at a time (default: 10)
- `--delay MS` - Wait MS between batches (default: 1000)

### Paintings
- `--dry-run` - Preview without syncing
- `--limit N` - Only process N paintings
- `--user @handle` - Only process specific user
- `--batch-size N` - Process N at a time (default: 5)
- `--delay MS` - Wait MS between batches (default: 2000)
- `--skip-blobs` - Skip image uploads (faster, no thumbnails)

## What Gets Synced?

### Moods
- ✅ Have a user (not anonymous)
- ✅ Not deleted
- ✅ Missing `atproto.rkey`
- ✅ User has ATProto account

### Paintings
- ✅ Have a user (not guest)
- ✅ Not nuked
- ✅ Missing `atproto.rkey`
- ✅ User has ATProto account

## Example Output

### Moods
```
🔄 Backfill Moods to ATProto

Mode: ✍️  LIVE
Batch size: 10
Delay: 1000ms

📊 Found 1524 moods without ATProto records

Processing 1524 moods...

📦 Batch 1/153
[1/1524] @jeffrey - feeling creative... ✅ Synced (3m3irrx4aac2z)
[2/1524] @fifi - picture ✅ Synced (3m3jssq2bbc3a)
...

✨ Backfill Summary
✅ Synced: 1450
⏭️  Skipped: 74
❌ Failed: 0
```

### Paintings
```
🎨 Backfill Paintings to ATProto

Mode: ✍️  LIVE
Batch size: 5
Delay: 2000ms

📊 Found 2837 paintings without ATProto records

Processing 2837 paintings...

📦 Batch 1/568
[1/2837] @jeffrey - a3b ✅ Synced (3m3luvx9dde5c)
[2/2837] @fifi - 4Xz ✅ Synced (3m3mwwx0eef6d)
...

✨ Backfill Summary
✅ Synced: 2650
⏭️  Skipped: 187
❌ Failed: 0
```

## Performance Tips

### Moods (Fast)
- Use larger batches: `--batch-size 20`
- Shorter delays: `--delay 500`
- Should complete in ~25-30 minutes

### Paintings (Slow)
- Use smaller batches: `--batch-size 3`
- Longer delays: `--delay 3000`
- Run in background: `nohup ... &`
- Monitor: `tail -f backfill.log`
- Will take several hours (4-8 hours typical)

## Strategy

### Recommended Order

1. **Test moods** (dry run, small limit)
2. **Backfill moods** (full run, ~30 minutes)
3. **Test paintings** (dry run, small limit)
4. **Backfill paintings incrementally**:
   - Start with one user: `--user @jeffrey`
   - Or do small batches: `--limit 100`
   - Or run full backfill overnight

### For Large Backfills

```bash
# Background process with logging
nohup node at/scripts/atproto/backfill-paintings-to-atproto.mjs > paintings.log 2>&1 &

# Monitor progress
tail -f paintings.log

# Check process
ps aux | grep backfill

# Kill if needed
kill <pid>
```

## Verification

After backfill:

1. **Check MongoDB** for `atproto.rkey` fields:
```javascript
db.moods.countDocuments({ 'atproto.rkey': { $exists: true } })
db.paintings.countDocuments({ 'atproto.rkey': { $exists: true } })
```

2. **Query PDS** for records:
```bash
curl "https://at.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=<did>&collection=computer.aesthetic.mood&limit=100"
```

3. **Spot check** some records for data integrity

## Documentation

- `BACKFILL-ATPROTO-GUIDE.md` - Comprehensive guide
- `BACKFILL-QUICK-REF.md` - Quick reference card
- This file - Implementation summary

## Safety Features

- ✅ Dry-run mode for testing
- ✅ Only processes records missing ATProto rkeys
- ✅ Doesn't modify existing ATProto records
- ✅ Continues on individual failures
- ✅ MongoDB updated only after ATProto success
- ✅ Detailed error reporting
- ✅ Graceful handling of missing ATProto accounts

## Prerequisites

- ✅ Environment variables set (`.env` in `system/`)
- ✅ MongoDB connection working
- ✅ ATProto PDS accessible
- ✅ User ATProto credentials in database

## Common Errors

| Error | Meaning | Action |
|-------|---------|--------|
| No ATProto account | User doesn't have PDS credentials | Expected, skip |
| Failed to fetch image | Image URL not accessible | Check S3/Spaces |
| Login failed | ATProto password invalid | Check credentials |
| Network timeout | Rate limiting or connectivity | Increase delays |

## Timeline Estimate

| Dataset | Records | Time |
|---------|---------|------|
| Moods | ~1,500 | 25-30 min |
| Paintings | ~2,800 | 4-8 hours |
| **Total** | **~4,300** | **~5-8 hours** |

## Date
Created: 2025-10-19
