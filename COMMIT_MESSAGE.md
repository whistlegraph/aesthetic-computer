# Add handle-based filtering with autocomplete to ATProto landing page

## Landing Page Enhancements
- Added handle autocomplete system with debounced search (300ms)
- Implemented client-side painting filtering with optimized fetching (10x results)
- Implemented server-side mood filtering via API parameter
- Added pdsls.dev links for ATProto record exploration
- Updated painting URLs to use `painting~@handle/slug` or `#code` format
- Added Enter key support for filter inputs
- Added defensive error handling for malformed handle data

## API Updates
- **handles.mjs** (NEW): Handle autocomplete endpoint with search parameter
  - Returns array of handles matching search query
  - Supports tenant filtering (aesthetic vs sotce)
  - Includes type checking to filter non-string handles
  
- **tv.mjs** (UPDATED): Added `code` field to painting projections
  - Enables #code display in painting thumbnails
  - Already deployed and working in production

## Key Features
1. **Handle Autocomplete**:
   - Dropdown with top 10 matching handles
   - Debounced search to avoid excessive API calls
   - Works for both paintings and moods galleries

2. **Smart Filtering**:
   - Paintings: Fetch 10x results (min 200) when filtering to ensure matches
   - Moods: Server-side filtering via `/api/mood/all?for=handle`
   - Normalizes handles (@fifi → fifi) for matching

3. **ATProto Integration**:
   - pdsls.dev links for record exploration (no URL encoding)
   - Painting URLs: `painting~@handle/slug` (priority 1) or `#code` (priority 2)
   - Blob URLs for thumbnails

## Files Changed
### Core Functionality
- `at/landing-page.html` - Landing page with galleries and filtering
- `system/netlify/functions/handles.mjs` - NEW: Autocomplete API
- `system/netlify/functions/tv.mjs` - Added code field

### Future Enhancement
The handles autocomplete system can be extended to `prompt.mjs` for system-wide
handle completion in commands (e.g., when typing @username).

## Testing
- Handle filtering works for both paintings and moods
- Autocomplete dropdown appears on typing
- pdsls.dev links open correctly
- Painting URLs format correctly with handles
- Console logging added for debugging

## Notes
- `/api/handles` endpoint created but needs Netlify deployment
- Currently returns raw MongoDB response in production (harmless)
- Mood migration still running (978 users being processed)
