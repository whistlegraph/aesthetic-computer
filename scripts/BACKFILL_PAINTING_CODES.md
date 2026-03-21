# Painting Codes Backfill Migration

## Problem
Paintings created before the short code generation feature (or while it was broken) don't have `code` fields in the database. This means they can't be referenced via `#code` syntax.

## Solution

### Part 1: Fix Future Uploads ✅
Modified `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs` to call `POST /api/track-media` after S3 upload succeeds. This ensures all new paintings get short codes automatically.

### Part 2: Backfill Existing Paintings

Run the migration script to add codes to existing paintings:

```bash
# From the aesthetic-computer directory
cd scripts

# Run the migration (requires MONGODB_CONNECTION_STRING env var)
node backfill-painting-codes.mjs
```

## Environment Variables Required

```bash
export MONGODB_CONNECTION_STRING="mongodb+srv://..."
export MONGODB_NAME="aesthetic"  # Optional, defaults to "aesthetic"
```

## What the Migration Does

1. Connects to MongoDB
2. Finds all paintings without a `code` field
3. For each painting:
   - Generates a unique 3-character short code
   - Checks for collisions with existing codes
   - Updates the painting record with the new code
4. Reports progress and results

## Safety Features

- Uses the same code generation logic as `track-media.mjs`
- Checks for code collisions before assigning
- Doesn't fail if individual updates fail
- Reports detailed progress
- Doesn't modify paintings that already have codes

## Expected Output

```
🚀 Starting painting codes backfill migration...

✅ Connected to MongoDB
✅ Ensured code index exists

📊 Found 1,234 paintings without codes
📦 Loaded 567 existing codes into memory

✅ 1/1234: 2025.10.20.23.28.07.334 → #a3b (user: auth0|...)
✅ 2/1234: 2025.10.20.23.21.29.817 → #x7y (user: auth0|...)
...

📊 Migration complete:
   ✅ Updated: 1234
   ❌ Failed: 0
   📦 Total codes in database: 1801

✅ Disconnected from MongoDB

🎉 Migration completed successfully!
```

## Testing

After running the migration, verify a few paintings:

```bash
# Check in MongoDB
db.paintings.find({ slug: "2025.10.20.23.28.07.334" })

# Should now have a "code" field
```

Or test in the browser:
- Visit `aesthetic.computer/painting~@user/2025.10.20.23.28.07.334`
- Should redirect to `aesthetic.computer/painting#abc` (where abc is the generated code)

## Rollback

If needed, remove all generated codes:

```javascript
// In MongoDB shell - BE CAREFUL!
db.paintings.updateMany(
  { code: { $exists: true } },
  { $unset: { code: "" } }
)
```

(But you probably don't want to do this - codes are meant to be permanent!)
