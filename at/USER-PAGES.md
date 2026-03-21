# User Pages for AT Protocol Subdomains

## Overview

Custom user pages are now available on AT Protocol subdomains! Each user gets their own subdomain that displays all their ATProto records in a beautiful, tabular interface.

**Example:** `https://fifi.at.aesthetic.computer` shows all records for the `fifi` handle.

## Features

✨ **User-Specific Data**
- Automatically extracts handle from subdomain (e.g., `fifi` from `fifi.at.aesthetic.computer`)
- Resolves handle to DID using ATProto XRPC API
- Displays all records for that user

📊 **Record Types Supported**
- `computer.aesthetic.painting` - User's paintings with thumbnails
- `computer.aesthetic.mood` - User's mood posts
- `computer.aesthetic.piece` - Code pieces
- `computer.aesthetic.kidlisp` - KidLisp programs

🔌 **Independent Architecture**
- Uses ONLY ATProto/at.aesthetic.computer APIs
- No dependencies on aesthetic.computer backend servers
- Pure client-side JavaScript
- Fetches data directly from PDS via XRPC

🎨 **User Interface**
- Tabbed navigation by record type
- Card-based layout with hover effects
- Dark mode support
- Responsive design
- Links to pdsls.dev for record inspection

## Architecture

```
User Browser
    ↓
fifi.at.aesthetic.computer/
    ↓
Caddy Web Server
    ├─ GET / → Serves user-page.html (static)
    ├─ GET /xrpc/* → Proxies to PDS:3000 (API access)
    └─ GET /.well-known/* → Proxies to PDS:3000 (ATProto)
    
user-page.html (Client-side JS)
    ↓
ATProto XRPC APIs
    ├─ com.atproto.identity.resolveHandle → Get DID from handle
    └─ com.atproto.repo.listRecords → List all records for collections
```

## Implementation Details

### File Structure
```
/at/
├── user-page.html              # User-specific page template
├── landing-page.html           # Main at.aesthetic.computer page
└── deploy-user-pages.fish      # Deployment script
```

### Caddy Configuration

The Caddy reverse proxy is configured to:

1. **Main domain** (`at.aesthetic.computer`):
   - Serve `landing-page.html` at root
   - Proxy everything else to PDS

2. **Wildcard subdomains** (`*.at.aesthetic.computer`):
   - Serve `user-page.html` at root
   - Proxy `/xrpc/*` to PDS for API access
   - Proxy `/.well-known/*` for ATProto protocol

### Client-Side Logic

The `user-page.html` does:

1. **Extract handle from subdomain**
   ```javascript
   // fifi.at.aesthetic.computer → fifi.at.aesthetic.computer
   const hostname = window.location.hostname;
   const handle = hostname.match(/^([^.]+)\.at\.aesthetic\.computer$/)[1] + '.at.aesthetic.computer';
   ```

2. **Resolve DID from handle**
   ```javascript
   const response = await fetch(
     `${PDS_URL}/xrpc/com.atproto.identity.resolveHandle?handle=${handle}`
   );
   const { did } = await response.json();
   ```

3. **List records for each collection**
   ```javascript
   const response = await fetch(
     `${PDS_URL}/xrpc/com.atproto.repo.listRecords?repo=${did}&collection=computer.aesthetic.painting`
   );
   ```

4. **Render in tabbed interface**
   - All records in one view
   - Separate tabs for each collection type
   - Card-based layout with metadata

## Deployment

### Prerequisites
- SSH access to PDS server (root@138.197.35.160)
- SSH key at `~/.ssh/aesthetic_pds`
- Caddy running in Docker container

### Deploy

```fish
cd /workspaces/aesthetic-computer/at
./deploy-user-pages.fish
```

This will:
1. Upload `landing-page.html` and `user-page.html` to server
2. Update Caddy configuration
3. Reload Caddy to apply changes

### Verify Deployment

```fish
# Test main landing page
curl -I https://at.aesthetic.computer

# Test user page (replace with actual handle)
curl -I https://fifi.at.aesthetic.computer

# Test API access through subdomain
curl https://fifi.at.aesthetic.computer/xrpc/_health
```

## Usage Examples

### View All Records
```
https://jeffrey.at.aesthetic.computer
```

Shows all records for @jeffrey.at.aesthetic.computer user

### Direct API Access

The subdomains also proxy XRPC requests:

```bash
# Resolve handle
curl "https://fifi.at.aesthetic.computer/xrpc/com.atproto.identity.resolveHandle?handle=fifi.at.aesthetic.computer"

# List paintings
curl "https://fifi.at.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=did:plc:xxx&collection=computer.aesthetic.painting"
```

## Future Enhancements

🔐 **Authentication & Administration**
- Detect if user is logged in (via ATProto session)
- Allow authenticated users to manage their records
- Delete, edit, or hide records
- Privacy controls

🎨 **Customization**
- User-configurable themes
- Custom bio/description
- Profile pictures
- Featured/pinned records

📱 **Enhanced Features**
- Search within user's records
- Filter by date range
- Export records as JSON
- Share individual records

🔗 **Integration**
- Link to aesthetic.computer paintings
- Embed pieces/kidlisp programs
- Show related records
- Social graph (followers/following)

## Technical Notes

### Why Client-Side Only?

The decision to use only ATProto APIs (no aesthetic.computer backend) provides:

1. **Independence** - Works even if aesthetic.computer is down
2. **Simplicity** - No server-side logic needed
3. **Transparency** - Users can inspect network requests
4. **Scalability** - Static files cached by CDN
5. **Privacy** - No additional tracking/logging

### Performance

- Initial page load: ~500ms
- Record fetching: Depends on record count
  - Pagination: 100 records per request
  - Multiple collections fetched in parallel
- Images loaded lazily from blob storage

### Browser Compatibility

- Modern browsers (Chrome, Firefox, Safari, Edge)
- ES6+ JavaScript required
- Fetch API required
- No polyfills included

## Troubleshooting

### "Invalid subdomain format" error
- Ensure subdomain matches pattern: `handle.at.aesthetic.computer`
- Check that handle exists in PDS

### "Failed to resolve handle" error
- Handle may not exist
- PDS may be down
- Check PDS health: `https://at.aesthetic.computer/xrpc/_health`

### No records showing
- User may not have any records yet
- Check specific collection on pdsls.dev
- Verify DID is correct

### Images not loading
- Blob CIDs may be invalid
- Check blob storage availability
- Verify image uploaded correctly

## Related Documentation

- [ATProto XRPC API Reference](https://atproto.com/specs/xrpc)
- [Lexicon Schemas](../lexicons/)
- [Landing Page Deployment](./pds/scripts/deploy-landing-page.fish)
- [PDS Administration](./ADMIN.md)

## Contact

For issues or feature requests, contact:
- Jeffrey Scudder (@jeffrey.at.aesthetic.computer)
- Aesthetic Computer Team

---

**Last Updated:** 2025-10-20
**Status:** ✅ Deployed and Active
