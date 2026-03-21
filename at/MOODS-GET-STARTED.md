# 🦋 Moods ATProto Integration - Getting Started

**Created:** October 14, 2025  
**Status:** Ready for implementation

---

## 📖 Quick Start

This integration adds ATProto support to Aesthetic Computer's moods system, enabling moods to be stored on both MongoDB (primary) and ATProto PDS (federated).

### What You Need to Know

1. **MongoDB stays primary** - All existing moods remain, no migration risk
2. **Dual-write pattern** - New moods go to both MongoDB + ATProto
3. **Graceful degradation** - If ATProto fails, MongoDB still works
4. **Backfill supported** - Historical moods can be migrated
5. **Delete syncs** - Deleting account removes moods from both

---

## 📁 Files Overview

```
/at/
├── MOODS-ATPROTO-INTEGRATION-PLAN.md   ⭐ Complete implementation guide
├── MOODS-INTEGRATION-SUMMARY.md         📋 Quick reference
├── MOODS-INTEGRATION-CHECKLIST.md       ✅ Track progress
├── GET-STARTED.md                       👈 You are here
├── lexicons/computer/aesthetic/
│   └── mood.json                        🦋 ATProto schema
└── scripts/
    └── moods-integration-commands.sh    🔧 Quick commands
```

### Where to Start

1. **Planning:** Read `MOODS-INTEGRATION-SUMMARY.md` (5 min read)
2. **Deep Dive:** Read `MOODS-ATPROTO-INTEGRATION-PLAN.md` (30 min)
3. **Tracking:** Use `MOODS-INTEGRATION-CHECKLIST.md` during implementation

---

## 🎯 Implementation Path

### Step 1: Review Current System
```bash
# Check existing moods in MongoDB
node scripts/audit-mood-atproto-sync.mjs 10

# See current mood endpoint
curl https://aesthetic.computer/api/mood/all | jq '.moods[:3]'
```

### Step 2: Deploy Lexicon
```bash
# Create deployment script (Phase 1)
# See MOODS-ATPROTO-INTEGRATION-PLAN.md Section: Phase 1
node scripts/create-mood-lexicon.mjs --deploy
```

### Step 3: Implement Backend
```bash
# Create helper (Phase 2.1)
# File: /system/backend/mood-atproto.mjs
# See MOODS-ATPROTO-INTEGRATION-PLAN.md Section: Phase 2.1

# Update mood function (Phase 2.2)
# File: /system/netlify/functions/mood.mjs
# See MOODS-ATPROTO-INTEGRATION-PLAN.md Section: Phase 2.2
```

### Step 4: Test Integration
```bash
# Create test mood
curl -X POST http://localhost:8888/api/mood \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -d '{"mood": "testing atproto!"}'

# Verify in MongoDB
node scripts/audit-mood-atproto-sync.mjs @testuser

# Check ATProto PDS
# (Commands in plan document)
```

### Step 5: Backfill Historical
```bash
# Dry run
node scripts/migrate-moods-to-atproto.mjs

# Execute
node scripts/migrate-moods-to-atproto.mjs --execute
```

### Step 6: Enable Deletion Sync
```bash
# Update delete function (Phase 3)
# File: /system/netlify/functions/delete-erase-and-forget-me.mjs
# See MOODS-ATPROTO-INTEGRATION-PLAN.md Section: Phase 3
```

---

## 🧪 Testing Strategy

### Manual Testing
```bash
# 1. Create mood with ATProto user
curl -X POST https://aesthetic.computer/api/mood \
  -H "Authorization: Bearer TOKEN" \
  -d '{"mood": "happy!"}'

# 2. Check MongoDB has atproto.uri
mongosh --eval 'db.moods.findOne({user: "auth0|123..."})'

# 3. Verify on PDS
node scripts/audit-mood-atproto-sync.mjs @handle
```

### Automated Testing
See `MOODS-ATPROTO-INTEGRATION-PLAN.md` Section: Phase 5 - Testing Checklist

---

## 📊 Schema Changes

### MongoDB Moods Collection

**Before:**
```javascript
{
  _id: ObjectId("abc123"),
  user: "auth0|123...",
  mood: "feeling good",
  when: ISODate("2025-10-14"),
  deleted: false
}
```

**After:**
```javascript
{
  _id: ObjectId("abc123"),
  user: "auth0|123...",
  mood: "feeling good",
  when: ISODate("2025-10-14"),
  deleted: false,
  atproto: {                    // ← NEW
    uri: "at://did:plc:abc.../computer.aesthetic.mood/3k...",
    cid: "bafyrei...",
    created: ISODate("2025-10-14"),
    synced: true
  }
}
```

### ATProto Record

```javascript
{
  $type: "computer.aesthetic.mood",
  text: "feeling good",
  createdAt: "2025-10-14T12:00:00Z",
  isPrivate: false,
  mongoId: "abc123"  // Back-reference
}
```

---

## 🔧 Key Scripts to Create

During implementation, you'll create these scripts:

### Phase 1: Lexicon
- `scripts/create-mood-lexicon.mjs` - Deploy lexicon to PDS

### Phase 2: Backend
- `system/backend/mood-atproto.mjs` - Helper functions

### Phase 4: Migration
- `scripts/migrate-moods-to-atproto.mjs` - Backfill historical moods

### Phase 5: Auditing
- `scripts/audit-mood-atproto-sync.mjs` - Verify sync status

See full code samples in `MOODS-ATPROTO-INTEGRATION-PLAN.md`

---

## 🚨 Important Considerations

### What Works Without ATProto
✅ Creating moods (MongoDB only)  
✅ Reading moods  
✅ Deleting moods  
✅ All existing functionality

### What Requires ATProto
🦋 Federating moods to ATProto network  
🦋 Cross-PDS mood discovery  
🦋 ATProto-native mood apps

### Rollback Safety
- MongoDB is primary, ATProto is secondary
- Can disable ATProto sync anytime without breaking moods
- Can remove `atproto` field from MongoDB if needed
- ATProto records can be deleted separately

---

## 📞 Need Help?

### Documentation
- Full plan: `MOODS-ATPROTO-INTEGRATION-PLAN.md`
- Quick ref: `MOODS-INTEGRATION-SUMMARY.md`
- Checklist: `MOODS-INTEGRATION-CHECKLIST.md`

### Commands
```bash
# Show all quick commands
./scripts/moods-integration-commands.sh
```

### Common Issues
See `MOODS-ATPROTO-INTEGRATION-PLAN.md` Section: Edge Cases & Considerations

---

## ✅ Definition of Done

Project is complete when:

- ✅ Lexicon deployed to PDS
- ✅ New moods auto-create ATProto records
- ✅ Historical moods backfilled
- ✅ Delete syncs to both MongoDB + ATProto
- ✅ Audit shows 100% sync rate
- ✅ Tests passing
- ✅ Documentation complete
- ✅ Monitoring in place

---

## 🚀 Ready to Start?

1. Read `MOODS-INTEGRATION-SUMMARY.md` (5 min)
2. Open `MOODS-INTEGRATION-CHECKLIST.md` to track progress
3. Follow `MOODS-ATPROTO-INTEGRATION-PLAN.md` step-by-step
4. Use `scripts/moods-integration-commands.sh` for quick reference

**Estimated Time:** 3-4 weeks  
**Complexity:** Medium  
**Risk:** Low (MongoDB stays primary)

---

**Let's build it! 🦋**
