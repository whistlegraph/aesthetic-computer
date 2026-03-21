# Tape Blob Cleanup - @jeffrey - Summary

**Date:** November 1, 2025

## Problem
Some tapes on the @jeffrey handle in the ATProto PDS instance had missing or incomplete blob references, likely due to early oven development issues.

## Investigation

### Initial State (before cleanup)
- **MongoDB tapes:** 11 total
  - 9 missing ATProto rkeys
  - 2 with rkeys
- **ATProto records:** 9 total
  - 3 with valid video blobs
  - 6 missing video blobs
  - 1 duplicate record (ez2 appeared twice)

### Root Cause
9 tapes (`pNf`, `val`, `79x`, `j75`, `egl`, `wk7`, `yi7`, `znz`, `nj9`) had no source files:
- No MP4 URL in MongoDB
- No ZIP URL in MongoDB
- These were created during early oven development and never completed processing
- 6 of them had ATProto records created but with no video blob data

## Actions Taken

### 1. Created Verification Script
**File:** `at/scripts/atproto/verify-tapes-sync.mjs`
- Checks MongoDB tapes vs ATProto records
- Verifies blob references are valid
- Detects duplicates and orphaned records
- Properly handles ATProto's blob deserialization format

### 2. Created Batch Fix Script
**File:** `at/scripts/atproto/batch-fix-tape-blobs.mjs`
- Downloads MP4/thumbnails from URLs
- Uploads as blobs to ATProto PDS
- Handles duplicates by deleting extras
- Updates MongoDB with correct rkeys
- Has `--dry-run` mode for safety

**Result:** Deleted 1 duplicate ATProto record for tape `ez2`

### 3. Created Cleanup Script
**File:** `at/scripts/atproto/delete-orphaned-tapes.mjs`
- Identifies tapes with no source files (no MP4 or ZIP)
- Deletes from MongoDB
- Deletes matching ATProto records
- Deletes oven-bakes history records

**Result:** 
- Deleted 9 MongoDB tape records
- Deleted 6 ATProto records
- 0 failures

## Final State (after cleanup)

✅ **Perfectly synchronized:**
- **MongoDB tapes:** 2 (cet, ez2)
  - All have ATProto rkeys
  - All have MP4 URLs
- **ATProto records:** 2
  - All have valid video blobs
  - No duplicates
  - No orphaned records

## Scripts Created

All scripts support `--dry-run` and `--verbose` flags for safety:

1. **`verify-tapes-sync.mjs`** - Audit tape sync status
   ```bash
   node at/scripts/atproto/verify-tapes-sync.mjs @handle [--verbose]
   ```

2. **`batch-fix-tape-blobs.mjs`** - Fix tapes with missing blobs
   ```bash
   node at/scripts/atproto/batch-fix-tape-blobs.mjs @handle [--dry-run] [--verbose]
   ```

3. **`delete-orphaned-tapes.mjs`** - Clean up tapes with no source files
   ```bash
   node at/scripts/atproto/delete-orphaned-tapes.mjs @handle [--dry-run] [--verbose]
   ```

4. **`check-tape-zips-in-spaces.mjs`** - Check Digital Ocean Spaces for source files
   ```bash
   node system/backend/check-tape-zips-in-spaces.mjs @handle
   ```
   _(Note: Requires AWS SDK credentials to be configured)_

## Key Learnings

1. **ATProto Blob Deserialization:** When records are retrieved from the PDS, blob references with `$link` CIDs are deserialized into objects with `code`, `version`, `multihash`, and `bytes` properties.

2. **Oven Development Phase:** Some tapes were created during early oven development before the full MP4 conversion pipeline was stable, resulting in incomplete records.

3. **Proper Cleanup Process:**
   - Always verify first
   - Use dry-run mode
   - Handle duplicates before deletion
   - Clean up both MongoDB and ATProto in sync
   - Update related collections (oven-bakes)

4. **Exit Cleanly:** Scripts must call `process.exit()` and properly disconnect from MongoDB to avoid hanging.

## Future Recommendations

1. Add monitoring to detect when tapes fail to complete processing
2. Consider adding a background job to auto-cleanup stale incomplete tapes
3. Add validation in track-media to ensure ZIP uploads complete before creating MongoDB records
4. Implement retry logic in the oven callback webhook for failed MP4 conversions
