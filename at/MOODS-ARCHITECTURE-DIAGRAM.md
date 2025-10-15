# Moods ATProto Integration - Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                   Aesthetic Computer User                            │
│                        (via prompt/api)                              │
└───────────────────────────┬─────────────────────────────────────────┘
                            │
                            ↓
┌─────────────────────────────────────────────────────────────────────┐
│               POST /api/mood { mood: "happy!" }                      │
│                   (mood.mjs Netlify Function)                        │
└───────────────────────────┬─────────────────────────────────────────┘
                            │
                ┌───────────┴───────────┐
                ↓                       ↓
┌───────────────────────────┐  ┌─────────────────────────┐
│      MongoDB (PRIMARY)     │  │  ATProto PDS (SECONDARY) │
│   aesthetic.moods          │  │  at.aesthetic.computer   │
│                            │  │                          │
│  {                         │  │  {                       │
│    user: "auth0|123",      │  │    $type: "computer.    │
│    mood: "happy!",         │  │      aesthetic.mood",   │
│    when: Date,             │  │    text: "happy!",      │
│    atproto: {        ←─────┼──┼──  createdAt: ISO,     │
│      uri: "at://...", ←────┼──┼──  mongoId: "abc"      │
│      cid: "bafy...",       │  │  }                       │
│      synced: true          │  │                          │
│    }                       │  │  URI: at://did:plc:abc/  │
│  }                         │  │    computer.aesthetic.   │
│                            │  │    mood/3k...            │
└────────────┬───────────────┘  └──────────┬──────────────┘
             │                             │
             │  ┌──────────────────────────┘
             │  │
             ↓  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                    Read Operations (GET)                             │
│   • /api/mood/all - List all moods (MongoDB primary)                │
│   • /api/mood/@handle - Get user's latest (MongoDB)                 │
│   • ATProto firehose - Federated discovery (ATProto)                │
└─────────────────────────────────────────────────────────────────────┘


═════════════════════════════════════════════════════════════════════

                        DELETION FLOW

┌─────────────────────────────────────────────────────────────────────┐
│            POST /api/delete-erase-and-forget-me                      │
│         (delete-erase-and-forget-me.mjs Function)                    │
└───────────────────────────┬─────────────────────────────────────────┘
                            │
                ┌───────────┴───────────┐
                ↓                       ↓
┌───────────────────────────┐  ┌─────────────────────────┐
│  MongoDB: DELETE moods     │  │  ATProto PDS: DELETE    │
│  where user = sub          │  │  all mood records       │
│                            │  │  via repo.deleteRecord  │
│  ✓ All moods removed       │  │  ✓ All moods removed    │
└────────────────────────────┘  └─────────────────────────┘


═════════════════════════════════════════════════════════════════════

                      MIGRATION/BACKFILL FLOW

┌─────────────────────────────────────────────────────────────────────┐
│         node scripts/migrate-moods-to-atproto.mjs --execute          │
└───────────────────────────┬─────────────────────────────────────────┘
                            │
                            ↓
                   ┌────────────────┐
                   │  For each user │
                   │  with ATProto  │
                   └────────┬───────┘
                            │
                            ↓
              ┌─────────────────────────┐
              │  Find moods without     │
              │  atproto.uri field      │
              └───────────┬─────────────┘
                          │
                          ↓
              ┌───────────────────────────┐
              │  For each mood:           │
              │  1. Create ATProto record │
              │  2. Get URI + CID         │
              │  3. Update MongoDB with   │
              │     atproto.uri           │
              │  4. Wait 100ms (rate      │
              │     limit)                │
              └───────────┬───────────────┘
                          │
                          ↓
              ┌───────────────────────────┐
              │  Report:                  │
              │  • Migrated: N moods      │
              │  • Failed: M moods        │
              │  • Total: X moods         │
              └───────────────────────────┘


═════════════════════════════════════════════════════════════════════

                        AUDIT/VERIFICATION FLOW

┌─────────────────────────────────────────────────────────────────────┐
│         node scripts/audit-mood-atproto-sync.mjs @handle             │
└───────────────────────────┬─────────────────────────────────────────┘
                            │
                ┌───────────┴───────────┐
                ↓                       ↓
┌───────────────────────────┐  ┌─────────────────────────┐
│  MongoDB Query:            │  │  ATProto PDS Query:     │
│  • Total moods             │  │  • listRecords()        │
│  • Moods with atproto.uri  │  │  • Count records        │
│  • Moods missing ATProto   │  │  • Sample records       │
└────────────┬───────────────┘  └──────────┬──────────────┘
             │                             │
             │  ┌──────────────────────────┘
             │  │
             ↓  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                    Compare & Report                                  │
│   ✓ MongoDB: 150 moods                                               │
│   ✓ ATProto: 150 records                                             │
│   ✓ Sync Rate: 100%                                                  │
│   ✓ All checks passed!                                               │
└─────────────────────────────────────────────────────────────────────┘


═════════════════════════════════════════════════════════════════════

                       DATA FLOW SUMMARY

  User Action → MongoDB (PRIMARY) → ATProto (SECONDARY)
                    ↓                      ↓
              Source of truth       Federated copy
              Fast queries          ATProto network
              Existing data         New discovery
              Soft deletes          Permanent records*

  * ATProto records can be deleted, but deletion is explicit via API


═════════════════════════════════════════════════════════════════════

                      LEXICON DEFINITION

  computer.aesthetic.mood {
    text: string (max 280 chars)        ← The mood content
    createdAt: datetime                 ← Timestamp
    intensity: int (1-10, optional)     ← Future feature
    context: string (optional)          ← Additional notes
    isPrivate: boolean (default false)  ← Federation control
    mongoId: string                     ← Back-reference
  }

  Collection namespace: computer.aesthetic.mood
  Record key: tid (timestamp-based ID)
  URI format: at://did:plc:abc.../computer.aesthetic.mood/3k...


═════════════════════════════════════════════════════════════════════

                     IMPLEMENTATION PHASES

  Week 1: ┌─────────────────────┐
          │ 1. Deploy Lexicon   │
          │ 2. Create Helpers   │
          │ 3. Test Dual-Write  │
          └─────────────────────┘

  Week 2: ┌─────────────────────┐
          │ 4. Delete Sync      │
          │ 5. Migration Script │
          │ 6. Test Migration   │
          └─────────────────────┘

  Week 3: ┌─────────────────────┐
          │ 7. Backfill All     │
          │ 8. Audit & Verify   │
          │ 9. Monitoring       │
          └─────────────────────┘

  Week 4: ┌─────────────────────┐
          │ 10. Documentation   │
          │ 11. Production      │
          │ 12. Complete ✅     │
          └─────────────────────┘
```
