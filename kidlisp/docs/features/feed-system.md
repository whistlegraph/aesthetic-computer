# KidLisp Feed Feature (`$.mjs`)

## Overview
The KidLisp Feed is a new piece that displays a live, scrolling feed of recently cached KidLisp codes, similar to the moods feed. Users can discover new code snippets and see what the community is creating.

## Usage

### Accessing the Feed
- **Via Prompt**: Type `$` in the AC prompt
- **Via URL**: Navigate to `/$/` 
- **With Limit**: Use `$/30` to show 30 recent codes (default: 30, max: 100)
- **Scaled**: Use `$:2` for 2x scale display

### Display Format
```
Recent KidLisp Codes

$tezz    (wipe "blue") (ink "red") (bo...    @digitpain    47 hits
$hello   (ink "yellow") (write "hello...    @alice        12 hits  
$spiral  (repeat 100 (forward 5) (r...       @bob          203 hits
```

## API Endpoint

### New Parameter: `recent`
Extends the existing `/api/store-kidlisp` endpoint:

```
GET /api/store-kidlisp?recent=true&limit=30
```

#### Parameters
- `recent=true` - Activates recent codes mode
- `limit=N` - Number of codes to return (default: 50, max: 100)

#### Response Format
```json
{
  "recent": [
    {
      "code": "tezz",
      "source": "(wipe \"blue\") (ink \"red\") (box 10 10 50 50)",
      "preview": "(wipe \"blue\") (ink \"red\") (bo...",
      "when": "2025-08-29T10:30:00Z",
      "hits": 47,
      "user": "auth0|abc123",
      "handle": "@digitpain"
    }
  ],
  "count": 25,
  "limit": 30
}
```

## Technical Implementation

### Database Query
- Uses MongoDB aggregation pipeline with `$lookup` to join handles
- Sorts by `when` field descending (most recent first)
- Projects relevant fields and creates handle display names
- Limits results to prevent excessive queries

### Display Components
- **Code Name**: `$code` in cyan/blue
- **Source Preview**: Truncated to ~40 characters in white/gray  
- **Handle**: User handle in purple (like moods)
- **Hit Count**: Popularity indicator in green
- **Scrolling**: Continuous vertical scroll like moods feed

### Features
- **Handle Integration**: Shows `@username` for registered users, "anon" for anonymous
- **Source Truncation**: Long code gets truncated with "..." 
- **Hit Counter**: Shows popularity/usage of each code
- **Responsive Overflow**: Text oscillates horizontally when too long
- **Scalable Display**: Supports 1x and 2x scale modes

## Files Modified/Created

### Modified
- `/system/netlify/functions/store-kidlisp.mjs`
  - Added `recent` parameter handling in GET method
  - Added aggregation pipeline for handle joining
  - Added source preview generation

### Created
- `/system/public/aesthetic.computer/disks/$.mjs`
  - Main feed display piece
  - Follows moods.mjs pattern for scrolling and rendering
  - Handles user interaction and display scaling

## Future Enhancements
- Click interaction to copy codes or navigate to them
- Relative timestamps ("2h ago", "1d ago")
- Filtering by popularity or specific users
- Real-time updates via polling
- Search functionality within the feed

## Database Schema
Leverages existing `kidlisp` collection structure:
- `code`: Short nanoid identifier
- `source`: Full KidLisp source code
- `when`: Creation timestamp
- `hits`: Usage/popularity counter
- `user`: Optional user ID for handle lookup
- Joins with `@handles` collection for display names
