# Old Tools Archive

This directory contains one-off diagnostic and fix scripts that were created to resolve specific issues. They are kept for historical reference and potential future use but are not part of regular operations.

## Scripts in This Directory

### `test-user-creation-flow.mjs`
**Created:** October 14, 2025  
**Purpose:** Backfill missing user records and ATProto accounts  
**Used to fix:** 5 users affected by index conflict issue  
**Keep because:** May be useful if similar backfill needed in future

### `diagnose-user-code-generation.mjs`
**Created:** October 14, 2025  
**Purpose:** Test user code generation and identify index conflicts  
**Used to find:** MongoDB `code_1` index naming conflict  
**Keep because:** Good diagnostic tool for user code generation issues

### `check-code-1-user.mjs`
**Created:** October 14, 2025  
**Purpose:** Verify index naming vs user code data  
**Used to confirm:** No user had code "code_1", it was just an index name  
**Keep because:** Educational reference for MongoDB index naming

## When to Use These Tools

⚠️ **These are NOT for regular operations!**

Only use these if:
- Similar user creation issues arise
- Need to backfill historical data
- Debugging MongoDB index problems
- Learning how the system works

## Regular Operations Use These Instead

For day-to-day monitoring, use the tools in `/at/scripts/`:
- `audit-user-creation-sync.mjs` - Check user sync status
- `check-auth0-webhook-config.mjs` - Verify configuration
- `query-auth0-signups.mjs` - Query Auth0 users

See `/at/TOOLS-README.md` for details.

---

**Archive Date:** October 14, 2025  
**Related Issue:** MongoDB index conflict in user-code.mjs  
**Resolution:** Fixed in commit with updated `ensureUserCodeIndex()`
