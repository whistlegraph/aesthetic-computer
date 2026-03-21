# ATProto Backfill Quick Reference

## TL;DR

```bash
cd /workspaces/aesthetic-computer

# 1. Test moods (dry run)
node at/scripts/atproto/backfill-moods-to-atproto.mjs --dry-run --limit 10

# 2. Backfill moods (fast, ~30 minutes)
node at/scripts/atproto/backfill-moods-to-atproto.mjs

# 3. Test paintings (dry run)
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --dry-run --limit 5

# 4. Backfill paintings (slow, ~5-8 hours!)
node at/scripts/atproto/backfill-paintings-to-atproto.mjs
```

## Common Commands

### Moods

```bash
# Dry run first
node at/scripts/atproto/backfill-moods-to-atproto.mjs --dry-run

# Backfill everything
node at/scripts/atproto/backfill-moods-to-atproto.mjs

# Test with 10 moods
node at/scripts/atproto/backfill-moods-to-atproto.mjs --limit 10

# Specific user
node at/scripts/atproto/backfill-moods-to-atproto.mjs --user @jeffrey

# Fast (larger batches)
node at/scripts/atproto/backfill-moods-to-atproto.mjs --batch-size 20 --delay 500
```

### Paintings

```bash
# Dry run first
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --dry-run

# Test with 5 paintings
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --limit 5

# Specific user
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --user @jeffrey

# Slow and careful (recommended for full backfill)
node at/scripts/atproto/backfill-paintings-to-atproto.mjs --batch-size 3 --delay 3000

# Background process
nohup node at/scripts/atproto/backfill-paintings-to-atproto.mjs > paintings-backfill.log 2>&1 &
tail -f paintings-backfill.log
```

## What Gets Synced?

✅ **Moods:**
- Have a user (not anonymous)
- Not deleted
- Don't have `atproto.rkey` yet
- User has ATProto account

✅ **Paintings:**
- Have a user (not guest)
- Not nuked
- Don't have `atproto.rkey` yet
- User has ATProto account

## Time Estimates

- **Moods**: ~1 per second = 25-30 min for 1,500 moods
- **Paintings**: ~5-10 per painting = 4-8 hours for 2,800 paintings

## Monitoring Progress

```bash
# Watch MongoDB for synced records
watch 'mongo $MONGODB_CONNECTION_STRING --eval "db.moods.countDocuments({\"atproto.rkey\": {\$exists: true}})"'

# Count remaining
mongo $MONGODB_CONNECTION_STRING --eval 'db.moods.countDocuments({deleted: {$ne: true}, $or: [{atproto: {$exists: false}}, {"atproto.rkey": {$exists: false}}]})'
```

## Troubleshooting

**"No ATProto account"** - Expected, user doesn't have PDS credentials
**"Failed to fetch image"** - Check image URL and S3/Spaces access
**Network timeout** - Increase `--delay`, decrease `--batch-size`

## Safety

- ✅ Dry run available
- ✅ Only syncs missing records
- ✅ Doesn't modify existing ATProto records
- ✅ Doesn't break on errors
- ✅ MongoDB always updated last (after ATProto success)

## Files

- `at/scripts/atproto/backfill-moods-to-atproto.mjs`
- `at/scripts/atproto/backfill-paintings-to-atproto.mjs`
- `BACKFILL-ATPROTO-GUIDE.md` (detailed docs)
