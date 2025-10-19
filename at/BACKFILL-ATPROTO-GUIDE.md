# Backfilling ATProto Records

Guide for syncing existing MongoDB moods and paintings to ATProto PDS.

## Overview

Two backfill scripts are available to sync historical data:

1. **`backfill-moods-to-atproto.mjs`** - Sync existing moods
2. **`backfill-paintings-to-atproto.mjs`** - Sync existing paintings

Both scripts:
- ✅ Only sync records that don't have ATProto records yet
- ✅ Only sync for users with ATProto accounts
- ✅ Skip guest content and deleted items
- ✅ Process in batches with configurable delays
- ✅ Support dry-run mode for testing
- ✅ Update MongoDB with ATProto rkeys
- ✅ Provide detailed progress and error reporting

## Prerequisites

1. **Environment variables** must be set (in `system/.env`):
   - `MONGODB_CONNECTION_STRING`
   - `MONGODB_NAME`
   - `PDS_URL`
   - `PDS_ADMIN_PASSWORD`

2. **Run from project root** or adjust dotenv path

## Moods Backfill

### Basic Usage

```bash
cd /workspaces/aesthetic-computer

# Dry run (see what would be synced)
node at/scripts/atproto/backfill-moods-to-atproto.mjs --dry-run

# Sync all moods
node at/scripts/atproto/backfill-moods-to-atproto.mjs

# Test with just 10 moods
node at/scripts/atproto/backfill-moods-to-atproto.mjs --limit 10

# Sync moods for specific user
node at/scripts/atproto/backfill-moods-to-atproto.mjs --user @jeffrey

# Custom batch size and delay
node at/scripts/atproto/backfill-moods-to-atproto.mjs --batch-size 20 --delay 500
```

### Options

- `--dry-run` - Show what would be synced without making changes
- `--limit N` - Only process N moods (for testing)
- `--user @handle` - Only process moods for specific user
- `--batch-size N` - Process N moods at a time (default: 10)
- `--delay MS` - Delay between batches in ms (default: 1000)

### Example Output

```
🔄 Backfill Moods to ATProto

Mode: ✍️  LIVE
Batch size: 10
Delay: 1000ms

📊 Found 1524 moods without ATProto records

Processing 1524 moods...

📦 Batch 1/153
Processing moods 1-10 of 1524

[1/1524] @jeffrey - feeling creative today... ✅ Synced (3m3irrx4aac2z)
[2/1524] @fifi - picture ✅ Synced (3m3jssq2bbc3a)
[3/1524] @mangoghoul - coding late night... ✅ Synced (3m3kttx8ccz4b)
...

✨ Backfill Summary

✅ Synced: 1450
⏭️  Skipped (no ATProto account): 74
❌ Failed: 0
📊 Total: 1524
```

## Paintings Backfill

### Basic Usage

```bash
cd /workspaces/aesthetic-computer

# Dry run (see what would be synced)
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --dry-run

# Sync all paintings (this will take a while!)
node at/scripts/atproto/backfill-paintings-to-atproto.mjs

# Test with just 5 paintings
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --limit 5

# Sync paintings for specific user
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --user @jeffrey

# Smaller batches with longer delays (be nice to the API)
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --batch-size 3 --delay 3000
```

### Options

- `--dry-run` - Show what would be synced without making changes
- `--limit N` - Only process N paintings (for testing)
- `--user @handle` - Only process paintings for specific user
- `--batch-size N` - Process N paintings at a time (default: 5)
- `--delay MS` - Delay between batches in ms (default: 2000)
- `--skip-blobs` - Skip uploading image blobs (faster, no thumbnails)

### Example Output

```
🎨 Backfill Paintings to ATProto

Mode: ✍️  LIVE
Batch size: 5
Delay: 2000ms

📊 Found 2837 paintings without ATProto records

Processing 2837 paintings...

📦 Batch 1/568
Processing paintings 1-5 of 2837

[1/2837] @jeffrey - a3b ✅ Synced (3m3luvx9dde5c)
[2/2837] @fifi - 4Xz ✅ Synced (3m3mwwx0eef6d)
[3/2837] @digitpain - 7Yz ⏭️  No ATProto account
...

✨ Backfill Summary

✅ Synced: 2650
⏭️  Skipped (no ATProto account or handle): 187
❌ Failed: 0
📊 Total: 2837

⚠️  Note: This backfill downloads and uploads images, so it may take a while.
Consider using --batch-size and --delay to tune performance vs. API limits.
```

## Performance Considerations

### Moods
- **Fast**: Just text data, no image processing
- **Recommended batch size**: 10-20
- **Recommended delay**: 500-1000ms
- **Time estimate**: ~1 mood per second = ~25 minutes for 1500 moods

### Paintings
- **Slow**: Downloads and uploads images (can be several MB each)
- **Recommended batch size**: 3-5
- **Recommended delay**: 2000-3000ms
- **Time estimate**: ~5-10 seconds per painting = **several hours for 2800+ paintings**

⚠️ **Warning**: Paintings backfill will take a LONG time and use significant bandwidth!

## Strategy for Large Backfills

### Option 1: Prioritize Recent Content
```bash
# Start with one user's content first
node at/scripts/atproto/backfill-moods-to-atproto.mjs --user @jeffrey
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --user @jeffrey

# Then do others incrementally
```

### Option 2: Incremental Backfill
```bash
# Process in chunks with breaks
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --limit 100
# ... wait, check results ...
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --limit 100
# ... repeat ...
```

### Option 3: Background Processing
```bash
# Run in background with nohup
nohup node at/scripts/atproto/backfill-paintings-to-atproto.mjs > backfill.log 2>&1 &

# Monitor progress
tail -f backfill.log
```

## Monitoring

### Check Progress
```bash
# Count remaining moods without ATProto records
mongo <connection-string> --eval '
  db.moods.countDocuments({
    deleted: { $ne: true },
    $or: [
      { atproto: { $exists: false } },
      { "atproto.rkey": { $exists: false } }
    ]
  })
'

# Count remaining paintings without ATProto records
mongo <connection-string> --eval '
  db.paintings.countDocuments({
    user: { $exists: true, $ne: null },
    nuked: { $ne: true },
    $or: [
      { atproto: { $exists: false } },
      { "atproto.rkey": { $exists: false } }
    ]
  })
'
```

### Verify ATProto Records
```bash
# Query PDS for mood records
curl "https://at.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=<did>&collection=computer.aesthetic.mood&limit=100"

# Query PDS for painting records
curl "https://at.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=<did>&collection=computer.aesthetic.painting&limit=100"
```

## Error Handling

Both scripts:
- ✅ Continue on individual failures
- ✅ Collect failure details
- ✅ Show summary at end
- ✅ Don't crash on network errors

Common errors:
- **"No ATProto account"** - User doesn't have PDS credentials (expected, skipped)
- **"Failed to fetch image"** - Painting image not accessible (check S3/Spaces)
- **"Login failed"** - User's ATProto password invalid (check credentials)
- **Network timeout** - Increase `--delay` between batches

## Post-Backfill Verification

1. **Check MongoDB** for `atproto.rkey` fields:
```javascript
// Moods with ATProto records
db.moods.countDocuments({ 'atproto.rkey': { $exists: true } })

// Paintings with ATProto records  
db.paintings.countDocuments({ 'atproto.rkey': { $exists: true } })
```

2. **Check PDS** for record counts per user

3. **Spot check** a few records to verify data integrity

4. **Monitor logs** for any delayed errors

## Rollback

If you need to remove backfilled records:

```bash
# Remove ATProto records for a specific user
node remove-user-atproto-records.mjs @handle

# Clear ATProto rkeys from MongoDB
mongo <connection-string> --eval '
  db.moods.updateMany(
    { "atproto.rkey": { $exists: true } },
    { $unset: { atproto: "" } }
  )
'
```

## Best Practices

1. ✅ **Always dry-run first** with `--dry-run`
2. ✅ **Test with small limit** before full backfill
3. ✅ **Start with moods** (faster, less risk)
4. ✅ **Monitor during execution** for errors
5. ✅ **Use conservative batch sizes** for paintings
6. ✅ **Run during off-peak hours** for large backfills
7. ✅ **Keep logs** of backfill runs
8. ✅ **Verify results** before declaring complete

## Timeline Estimate

For a typical aesthetic.computer dataset:

| Content | Count | Time Estimate |
|---------|-------|---------------|
| Moods | ~1,500 | 25-30 minutes |
| Paintings | ~2,800 | 4-8 hours |
| **Total** | **~4,300** | **~5-8 hours** |

*Assuming network is stable and no significant errors.*

## Date
Created: 2025-10-19
