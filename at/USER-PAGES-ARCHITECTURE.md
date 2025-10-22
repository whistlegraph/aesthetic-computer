# User Pages Architecture Diagram

## Request Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                        User's Browser                           │
│                                                                 │
│  URL: https://fifi.at.aesthetic.computer                       │
└─────────────────────────────────────────────────────────────────┘
                               │
                               │ HTTP GET /
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Caddy Reverse Proxy                          │
│                   (at.aesthetic.computer)                       │
│                                                                 │
│  Config for *.at.aesthetic.computer:                           │
│  ┌────────────────────────────────────────────────────────┐   │
│  │  GET /              → Serve user-page.html (static)    │   │
│  │  GET /xrpc/*        → Proxy to PDS:3000 (API)         │   │
│  │  GET /.well-known/* → Proxy to PDS:3000 (ATProto)     │   │
│  └────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
                               │
                               │ Returns user-page.html
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                   user-page.html (Client-Side)                  │
│                                                                 │
│  JavaScript extracts handle from URL:                          │
│  "fifi.at.aesthetic.computer" → "fifi.at.aesthetic.computer"   │
│                                                                 │
│  Then makes API calls...                                       │
└─────────────────────────────────────────────────────────────────┘
                               │
                               │ Multiple XRPC API calls
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                    ATProto PDS Server (Port 3000)               │
│                                                                 │
│  XRPC Endpoints:                                               │
│  ┌────────────────────────────────────────────────────────┐   │
│  │  /xrpc/com.atproto.identity.resolveHandle             │   │
│  │    Input: handle (fifi.at.aesthetic.computer)         │   │
│  │    Output: { did: "did:plc:..." }                     │   │
│  │                                                         │   │
│  │  /xrpc/com.atproto.repo.listRecords                   │   │
│  │    Input: repo (DID), collection                      │   │
│  │    Output: { records: [...], cursor: "..." }          │   │
│  └────────────────────────────────────────────────────────┘   │
│                                                                 │
│  Collections queried:                                          │
│    • computer.aesthetic.painting                               │
│    • computer.aesthetic.mood                                   │
│    • computer.aesthetic.piece                                  │
│    • computer.aesthetic.kidlisp                                │
└─────────────────────────────────────────────────────────────────┘
                               │
                               │ Returns JSON data
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                   user-page.html (Rendering)                    │
│                                                                 │
│  Displays:                                                     │
│  ┌────────────────────────────────────────────────────────┐   │
│  │  Header:                                                │   │
│  │    • Handle: @fifi.at.aesthetic.computer               │   │
│  │    • DID: did:plc:...                                  │   │
│  │    • Stats: 5 paintings, 12 moods, etc.               │   │
│  │                                                         │   │
│  │  Tabs:                                                  │   │
│  │    [All] [Paintings] [Moods] [Pieces] [KidLisp]       │   │
│  │                                                         │   │
│  │  Records Grid:                                          │   │
│  │    ┌──────────────────────────────────────────┐        │   │
│  │    │ 🎨 Painting                              │        │   │
│  │    │ [Image thumbnail]                        │        │   │
│  │    │ Oct 20, 2025 • #abc123                  │        │   │
│  │    │ View on pdsls.dev →                     │        │   │
│  │    └──────────────────────────────────────────┘        │   │
│  │    ┌──────────────────────────────────────────┐        │   │
│  │    │ 💭 Mood                                  │        │   │
│  │    │ "Feeling creative today!"                │        │   │
│  │    │ Oct 19, 2025                            │        │   │
│  │    │ View on pdsls.dev →                     │        │   │
│  │    └──────────────────────────────────────────┘        │   │
│  └────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

## Data Flow Sequence

```
1. User visits https://fifi.at.aesthetic.computer
   ↓
2. Caddy serves /var/www/at.aesthetic.computer/user.html
   ↓
3. Browser loads user-page.html
   ↓
4. JavaScript extracts "fifi" from subdomain
   ↓
5. API Call #1: Resolve handle to DID
   POST /xrpc/com.atproto.identity.resolveHandle
   → Returns: did:plc:xyz123...
   ↓
6. API Call #2: List paintings
   GET /xrpc/com.atproto.repo.listRecords
   ?repo=did:plc:xyz123...&collection=computer.aesthetic.painting
   → Returns: { records: [...], cursor: null }
   ↓
7. API Call #3: List moods
   GET /xrpc/com.atproto.repo.listRecords
   ?repo=did:plc:xyz123...&collection=computer.aesthetic.mood
   → Returns: { records: [...], cursor: null }
   ↓
8. API Call #4, #5: List pieces, kidlisp (parallel)
   ↓
9. Render all records in tabbed interface
   ↓
10. User can:
    - Switch tabs to filter by type
    - Click records to view on pdsls.dev
    - See images loaded from blob storage
```

## File Structure on Server

```
/var/www/at.aesthetic.computer/
├── index.html    ← Landing page (at.aesthetic.computer)
└── user.html     ← User page (*.at.aesthetic.computer)

/pds/
└── Caddyfile     ← Reverse proxy configuration
```

## Key Benefits

✅ **Decentralized** - No aesthetic.computer backend dependency  
✅ **Fast** - Static file served from CDN  
✅ **Transparent** - All data from public ATProto APIs  
✅ **Portable** - Users own their data via ATProto  
✅ **Scalable** - No server-side processing  

## Comparison with Main Landing Page

| Feature | at.aesthetic.computer | [handle].at.aesthetic.computer |
|---------|----------------------|--------------------------------|
| Purpose | Project overview | User-specific data |
| Content | All users' paintings/moods | Single user's records |
| Data Source | aesthetic.computer APIs | ATProto APIs only |
| Authentication | None | None (future: optional) |
| Caching | Static + API calls | Static + API calls |

## Future: Authenticated Features

```
User logs in (ATProto session)
   ↓
Verify ownership of handle
   ↓
Show admin controls:
   • Delete records
   • Edit metadata
   • Privacy settings
   • Export data
```

This will be implemented in Phase 1 of enhancements.

---

**See:** [USER-PAGES.md](USER-PAGES.md) for full documentation
