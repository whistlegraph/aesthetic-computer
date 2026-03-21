# Account Deletion Feature Audit

**Generated:** February 3, 2026  
**Author:** GitHub Copilot

---

## Overview

Both Aesthetic Computer and Sotce Net have account deletion features, but they work differently and have different implications due to the **cross-platform identity system**. This report analyzes what currently happens and suggests improvements.

---

## Current Implementation

### Aesthetic Computer: `delete-erase-and-forget-me`

**Location:** 
- UI: [system/public/aesthetic.computer/disks/delete-erase-and-forget-me.mjs](../system/public/aesthetic.computer/disks/delete-erase-and-forget-me.mjs)
- Backend: [system/netlify/functions/delete-erase-and-forget-me.mjs](../system/netlify/functions/delete-erase-and-forget-me.mjs)

**User Flow:**
1. User types `delete-erase-and-forget-me` in the prompt
2. User must tap/click a "Delete @handle" button **3 times** to confirm
3. Account deletion begins with a loading animation

**What Gets Deleted:**

| Data | Action | Notes |
|------|--------|-------|
| S3 User Files | ✅ Deleted | All files in user's bucket directory |
| Paintings (MongoDB) | ✅ Deleted | `paintings` collection |
| Pieces (MongoDB) | ✅ Deleted | `pieces` collection |
| Moods (MongoDB) | ✅ Deleted | `moods` collection |
| Verification Count | ✅ Deleted | `verifications` collection |
| Chat Messages | ⚠️ Anonymized | Text cleared, user field blanked (not deleted) |
| Logs | ❌ Kept | Intentionally preserved for debugging |
| Handle (Redis) | ✅ Deleted | `@handles` and `userIDs` cache |
| Handle (MongoDB) | ⚠️ Conditional | See cross-platform logic below |
| ATProto/PDS Account | ✅ Deleted | `deleteAtprotoAccount()` called |
| Auth0 Account | ✅ Deleted | `deleteUser()` called |

**Cross-Platform Handle Logic:**
- If user has a **Sotce Net account** with the same email AND verified email:
  - Handle ownership transfers to the Sotce Net account (`sotce-{userID}`)
  - Handle is NOT deleted, just re-keyed
- If no Sotce Net account exists:
  - Handle is fully deleted

---

### Sotce Net: Delete Account Button

**Location:** 
- UI: Embedded in [system/netlify/functions/sotce-net.mjs](../system/netlify/functions/sotce-net.mjs#L3258) (settings page)
- Backend: Same file, `/delete-account` endpoint (~line 6568)

**User Flow:**
1. User clicks "delete account" button in settings
2. Browser `confirm()` dialog appears (single confirmation)
3. Account deletion proceeds

**What Gets Deleted:**

| Data | Action | Notes |
|------|--------|-------|
| Subscription | ✅ Cancelled | Via Stripe API |
| Chat Messages | ⚠️ Anonymized | Text cleared, user field blanked |
| Handle (Redis) | ✅ Deleted | `@handles` and `userIDs` cache |
| Handle (MongoDB) | ⚠️ Conditional | See cross-platform logic below |
| Auth0 Account | ✅ Deleted | `deleteUser(sub, "sotce")` |
| Sotce Pages | N/A | Admin-only content, not user data |

**Cross-Platform Handle Logic:**
- If user has an **Aesthetic Computer account** with the same email AND verified email:
  - Handle ownership transfers to the AC account
  - Handle is NOT deleted, just re-keyed
- If no AC account exists:
  - Handle is fully deleted

---

## Issues & Concerns

### 1. ✅ Sotce Net Pages - Not a Concern
**Severity:** N/A

Sotce Net pages are **admin-only content** (written by Sotce for subscribers to read). Users cannot write their own pages, so there's no user-generated content to delete. The current behavior is correct.

### 2. ⚠️ Inconsistent Confirmation UX
**Severity:** MEDIUM

| Platform | Confirmation Method |
|----------|---------------------|
| Aesthetic Computer | 3 button taps + loading animation |
| Sotce Net | Single browser `confirm()` dialog |

**Recommendation:** Standardize on a more robust confirmation (like AC's approach) or at least require typing "DELETE" to confirm.

### 3. ⚠️ Chat Messages Anonymized, Not Deleted
**Severity:** LOW

Both platforms clear chat message content but leave the records in place with empty `text` and `user` fields. This is actually good for:
- Preserving conversation flow for other users
- Audit trail integrity

However, the user should be informed that message metadata (timestamps) may remain.

### 4. ⚠️ Cross-Platform Handle Transfer Not Explained
**Severity:** MEDIUM

Users aren't told that:
- Deleting one account may transfer their handle to the other platform
- If they have both accounts, deleting one doesn't affect their handle
- Handles are "shared" between platforms

**Recommendation:** Add explanation in the deletion UI.

### 5. ⚠️ Logs Intentionally Preserved
**Severity:** LOW

The AC deletion code has a comment explaining logs are kept for debugging partially deleted accounts. This is reasonable but should be disclosed.

### 6. ❓ ATProto Moods Deletion
**Severity:** UNKNOWN

The code calls `deleteAtprotoAccount()` but there are TODO items in `at/` files suggesting moods on ATProto may need additional deletion logic.

---

## Recommendations

### Immediate (Before Next Release)

1. **Sotce Net: Add page deletion or anonymization**
   ```javascript
   // In /delete-account handler, add:
   await database.db.collection("sotce-pages")
     .updateMany({ user: sub }, { $set: { user: "", words: "[deleted]" } });
   ```

2. **Update privacy policies** ✅ (Done in this session)
   - AC: Added note about what deletion removes
   - Sotce: Added note about deletion from settings

### Short-Term

3. **Improve Sotce Net deletion UX**
   - Add multi-step confirmation like AC
   - Or require typing "DELETE" to confirm

4. **Add deletion preview**
   - Before deletion, show user what will be removed
   - List: files, paintings, chat history, handle, etc.

5. **Document handle transfer behavior**
   - In deletion flow, explain: "Your @handle will be transferred to your [other platform] account if you have one"

### Long-Term

6. **Consider unified deletion**
   - Option to delete both AC and Sotce Net accounts at once
   - Or warn user if they have accounts on both platforms

7. **Data export before deletion**
   - Let users download their data before deleting
   - Paintings, pages, chat history export

8. **Soft delete with grace period**
   - Mark account as "pending deletion" for 30 days
   - Allow recovery during grace period
   - Then hard delete

---

## Files Involved

### Aesthetic Computer Deletion
- [system/public/aesthetic.computer/disks/delete-erase-and-forget-me.mjs](../system/public/aesthetic.computer/disks/delete-erase-and-forget-me.mjs) - UI
- [system/netlify/functions/delete-erase-and-forget-me.mjs](../system/netlify/functions/delete-erase-and-forget-me.mjs) - Backend
- [system/backend/authorization.mjs](../system/backend/authorization.mjs) - `deleteUser()` function
- [system/backend/at.mjs](../system/backend/at.mjs) - `deleteAtprotoAccount()` function

### Sotce Net Deletion
- [system/netlify/functions/sotce-net.mjs](../system/netlify/functions/sotce-net.mjs) - Both UI (embedded) and backend
  - Settings UI: ~line 3258
  - Delete endpoint: ~line 6568

### Shared Infrastructure
- [system/backend/kv.mjs](../system/backend/kv.mjs) - Redis handle cache
- [system/backend/database.mjs](../system/backend/database.mjs) - MongoDB connection

---

## Summary Table

| Feature | Aesthetic Computer | Sotce Net |
|---------|-------------------|-----------|
| Trigger | `delete-erase-and-forget-me` command | Settings button |
| Confirmation | 3 taps | Browser confirm() |
| Files deleted | ✅ S3 bucket | N/A |
| Paintings deleted | ✅ | N/A |
| Pages deleted | N/A | ❌ NOT deleted |
| Moods deleted | ✅ | N/A |
| Chat anonymized | ✅ | ✅ |
| Subscription cancelled | N/A | ✅ |
| Handle deleted | ⚠️ Conditional | ⚠️ Conditional |
| ATProto deleted | ✅ | N/A |
| Auth0 deleted | ✅ | ✅ |
| Cross-platform aware | ✅ | ✅ |
