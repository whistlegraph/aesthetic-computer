# User-Specific AT Protocol Subdomain Pages

## Summary

Implemented custom user pages for AT Protocol subdomains (e.g., `https://fifi.at.aesthetic.computer`) that display all of a user's ATProto records in a beautiful, tabular interface.

**Status:** ✅ Ready for deployment  
**Date:** 2025-10-20

## What Was Built

### 1. User Page (`user-page.html`)
- **Purpose:** Display all ATProto records for a specific user
- **Technology:** Pure client-side HTML/CSS/JavaScript
- **Features:**
  - Automatically extracts handle from subdomain
  - Resolves handle to DID via ATProto API
  - Fetches and displays all record types:
    - `computer.aesthetic.painting` (with thumbnails)
    - `computer.aesthetic.mood` (text posts)
    - `computer.aesthetic.piece` (code pieces)
    - `computer.aesthetic.kidlisp` (programs)
  - Tabbed interface for filtering by type
  - Card-based layout with hover effects
  - Dark mode support
  - Links to pdsls.dev for record inspection

### 2. Deployment Script (`deploy-user-pages.fish`)
- Uploads both landing page and user page to PDS server
- Updates Caddy configuration to:
  - Serve landing page at `https://at.aesthetic.computer`
  - Serve user pages at `https://[handle].at.aesthetic.computer`
  - Proxy `/xrpc/*` to PDS for API access
  - Handle `.well-known` for ATProto protocol

### 3. Documentation
- **USER-PAGES.md** - Complete documentation including:
  - Architecture overview
  - Implementation details
  - Deployment instructions
  - Usage examples
  - Future enhancements
  - Troubleshooting guide

- **test-user-pages.html** - Testing interface for:
  - Manual handle input testing
  - Direct API testing
  - Deployment verification

## Key Design Decisions

### Independent Architecture
✅ Uses **ONLY** ATProto/at.aesthetic.computer APIs  
✅ No dependencies on aesthetic.computer backend  
✅ Pure client-side - works even if aesthetic.computer is down  

### Direct API Access
- `com.atproto.identity.resolveHandle` - Convert handle to DID
- `com.atproto.repo.listRecords` - Fetch all records for collections
- All requests go directly to `https://at.aesthetic.computer/xrpc/*`

### Caddy Reverse Proxy Configuration
```
*.at.aesthetic.computer
├─ GET / → user-page.html (static file)
├─ GET /xrpc/* → PDS:3000 (API proxy)
└─ GET /.well-known/* → PDS:3000 (ATProto protocol)
```

## Example URLs

- Main landing: `https://at.aesthetic.computer`
- User pages:
  - `https://fifi.at.aesthetic.computer`
  - `https://jeffrey.at.aesthetic.computer`
  - `https://[any-handle].at.aesthetic.computer`

## Deployment

### To Deploy:
```fish
cd /workspaces/aesthetic-computer/at
./deploy-user-pages.fish
```

### Prerequisites:
- SSH access to PDS server (root@138.197.35.160)
- SSH key at `~/.ssh/aesthetic_pds`
- Caddy running in Docker

### What It Does:
1. Uploads `landing-page.html` → `/var/www/at.aesthetic.computer/index.html`
2. Uploads `user-page.html` → `/var/www/at.aesthetic.computer/user.html`
3. Updates `/pds/Caddyfile` with new configuration
4. Reloads Caddy to apply changes

## Files Created/Modified

### New Files:
- `/at/user-page.html` - User-specific subdomain page
- `/at/deploy-user-pages.fish` - Deployment script
- `/at/USER-PAGES.md` - Full documentation
- `/at/test-user-pages.html` - Testing interface
- `/at/USER-PAGES-SUMMARY.md` - This summary

### Modified Files:
- None (all new functionality)

## Technical Highlights

### Client-Side Record Fetching
```javascript
// 1. Extract handle from subdomain
const handle = hostname.match(/^([^.]+)\.at\.aesthetic\.computer$/)[1];

// 2. Resolve to DID
const { did } = await fetch(`/xrpc/com.atproto.identity.resolveHandle?handle=${handle}`);

// 3. List all records
const { records } = await fetch(`/xrpc/com.atproto.repo.listRecords?repo=${did}&collection=...`);
```

### Pagination Support
- Automatically handles cursors for large record sets
- Fetches all records across multiple pages
- Displays in chronological order (newest first)

### Performance
- Static HTML file (fast first load)
- Parallel fetching of multiple collections
- Lazy image loading from blob storage
- No server-side processing required

## Future Enhancements

### Phase 1 (Next Steps)
- [ ] Add authentication detection
- [ ] Allow users to manage their own records
- [ ] Privacy controls (hide/show records)

### Phase 2
- [ ] Custom themes per user
- [ ] Profile customization
- [ ] Featured/pinned records
- [ ] Social graph visualization

### Phase 3
- [ ] Search within user records
- [ ] Date range filtering
- [ ] Export functionality
- [ ] Embed pieces/kidlisp programs

## Testing

### Local Testing:
```fish
# Open test interface
open /workspaces/aesthetic-computer/at/test-user-pages.html

# Test API calls
# Enter handle: jeffrey.at.aesthetic.computer
# Click: Test Resolve Handle
# Click: Test List Records
```

### Production Testing:
```bash
# After deployment, test:
curl -I https://fifi.at.aesthetic.computer
curl -I https://jeffrey.at.aesthetic.computer

# Verify API access:
curl https://fifi.at.aesthetic.computer/xrpc/_health
```

## Impact

### For Users
- ✅ Personal data dashboard at their own subdomain
- ✅ No login required to view public records
- ✅ Direct access to raw ATProto data
- ✅ Easy sharing of their work

### For the Platform
- ✅ Decentralized data access
- ✅ No backend load for viewing
- ✅ Transparent data storage
- ✅ ATProto-native experience

### For Development
- ✅ Clean separation from aesthetic.computer backend
- ✅ Pure ATProto implementation
- ✅ Easy to extend and customize
- ✅ Educational example of ATProto usage

## Questions & Answers

**Q: Why client-side only?**  
A: Independence, simplicity, transparency, and scalability. Static files can be cached by CDN.

**Q: What if a handle doesn't exist?**  
A: The page will show an error message. The PDS API returns 400 for invalid handles.

**Q: Can users edit their records?**  
A: Not yet - that's Phase 1 of future enhancements. Currently read-only.

**Q: Does this work for guest paintings?**  
A: No, only for users with ATProto accounts and handles.

**Q: What about privacy?**  
A: Currently all records are public. Privacy controls are planned for Phase 1.

## Related Work

- Landing page deployment: `/at/pds/scripts/deploy-landing-page.fish`
- ATProto documentation: `/at/README.md`
- PDS administration: `/at/ADMIN.md`
- Lexicon schemas: `/at/lexicons/`

## Contact

For questions or issues:
- Jeffrey Scudder (@jeffrey.at.aesthetic.computer)
- Aesthetic Computer Team

---

**Ready for production deployment! 🚀**
