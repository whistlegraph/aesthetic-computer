# User Pages - API Examples

Real examples of ATProto API responses used by the user pages.

## 1. Resolve Handle to DID

### Request
```
GET https://at.aesthetic.computer/xrpc/com.atproto.identity.resolveHandle?handle=fifi.at.aesthetic.computer
```

### Response
```json
{
  "did": "did:plc:xyz123abc456def789ghi012jkl345"
}
```

## 2. List Painting Records

### Request
```
GET https://at.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=did:plc:xyz123...&collection=computer.aesthetic.painting&limit=100
```

### Response
```json
{
  "records": [
    {
      "uri": "at://did:plc:xyz123.../computer.aesthetic.painting/3jzx7k2m4n5",
      "cid": "bafyreib...",
      "value": {
        "$type": "computer.aesthetic.painting",
        "slug": "2023.10.20.15.30.45",
        "code": "abc123",
        "createdAt": "2023-10-20T15:30:45.123Z",
        "image": {
          "$type": "blob",
          "ref": {
            "$link": "bafkreiabcd..."
          },
          "mimeType": "image/png",
          "size": 45678
        },
        "ref": "65f3d2c1a8b9e4f5a6b7c8d9"
      }
    },
    {
      "uri": "at://did:plc:xyz123.../computer.aesthetic.painting/2hwy6j1l3m4",
      "cid": "bafyreic...",
      "value": {
        "$type": "computer.aesthetic.painting",
        "slug": "2023.10.19.12.15.30",
        "code": "def456",
        "createdAt": "2023-10-19T12:15:30.456Z",
        "image": {
          "$type": "blob",
          "ref": {
            "$link": "bafkreixyz..."
          },
          "mimeType": "image/png",
          "size": 38901
        },
        "ref": "65f3d2c1a8b9e4f5a6b7c8d8"
      }
    }
  ],
  "cursor": "3jzx7k2m4n5"
}
```

## 3. List Mood Records

### Request
```
GET https://at.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=did:plc:xyz123...&collection=computer.aesthetic.mood&limit=100
```

### Response
```json
{
  "records": [
    {
      "uri": "at://did:plc:xyz123.../computer.aesthetic.mood/5pzn8r3q4t6",
      "cid": "bafyreid...",
      "value": {
        "$type": "computer.aesthetic.mood",
        "mood": "Feeling creative today! Just finished a new piece.",
        "when": "2023-10-20T14:25:15.789Z",
        "ref": "65f3d2c1a8b9e4f5a6b7c8d7"
      }
    },
    {
      "uri": "at://did:plc:xyz123.../computer.aesthetic.mood/4nyi7q2p3s5",
      "cid": "bafyreie...",
      "value": {
        "$type": "computer.aesthetic.mood",
        "mood": "Working on something experimental...",
        "when": "2023-10-19T09:45:30.123Z",
        "ref": "65f3d2c1a8b9e4f5a6b7c8d6"
      }
    }
  ],
  "cursor": null
}
```

## 4. Get Blob (Image)

### Request
```
GET https://at.aesthetic.computer/xrpc/com.atproto.sync.getBlob?did=did:plc:xyz123...&cid=bafkreiabcd...
```

### Response
Binary image data (PNG)

## Record URI Structure

ATProto URIs follow this pattern:
```
at://[DID]/[COLLECTION]/[RKEY]
```

Examples:
- `at://did:plc:xyz123.../computer.aesthetic.painting/3jzx7k2m4n5`
- `at://did:plc:xyz123.../computer.aesthetic.mood/5pzn8r3q4t6`

Components:
- **DID**: Decentralized identifier for the user
- **Collection**: Lexicon name (e.g., `computer.aesthetic.painting`)
- **RKEY**: Record key (unique ID within collection)

## Field Descriptions

### Painting Record Fields

| Field | Type | Description |
|-------|------|-------------|
| `$type` | string | Always `computer.aesthetic.painting` |
| `slug` | string | Timestamp-based painting ID |
| `code` | string | Short alphanumeric code (e.g., `abc123`) |
| `createdAt` | string | ISO 8601 timestamp |
| `image` | blob | Reference to image data |
| `ref` | string | MongoDB ObjectId reference |

### Mood Record Fields

| Field | Type | Description |
|-------|------|-------------|
| `$type` | string | Always `computer.aesthetic.mood` |
| `mood` | string | The mood text content |
| `when` | string | ISO 8601 timestamp |
| `ref` | string | MongoDB ObjectId reference |

### Blob Fields

| Field | Type | Description |
|-------|------|-------------|
| `$type` | string | Always `blob` |
| `ref.$link` | string | CID (Content Identifier) |
| `mimeType` | string | MIME type (e.g., `image/png`) |
| `size` | number | Size in bytes |

## Error Responses

### Handle Not Found
```json
{
  "error": "InvalidRequest",
  "message": "Unable to resolve handle: fifi.at.aesthetic.computer"
}
```
Status: 400

### Collection Not Found
```json
{
  "error": "InvalidRequest",
  "message": "Collection not found: computer.aesthetic.painting"
}
```
Status: 400

### DID Not Found
```json
{
  "error": "InvalidRequest",
  "message": "Could not find repo: did:plc:xyz123..."
}
```
Status: 400

## Pagination

When there are more than 100 records, use the `cursor`:

### Request with Cursor
```
GET https://at.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=did:plc:xyz123...&collection=computer.aesthetic.painting&limit=100&cursor=3jzx7k2m4n5
```

### Response
```json
{
  "records": [
    // Next 100 records...
  ],
  "cursor": "2gwx6i1k2m3"  // Or null if no more records
}
```

## JavaScript Examples

### Fetch Handle DID
```javascript
const PDS_URL = 'https://at.aesthetic.computer';

async function resolveDID(handle) {
  const url = `${PDS_URL}/xrpc/com.atproto.identity.resolveHandle?handle=${encodeURIComponent(handle)}`;
  const response = await fetch(url);
  const data = await response.json();
  return data.did;
}

// Usage
const did = await resolveDID('fifi.at.aesthetic.computer');
console.log(did); // did:plc:xyz123...
```

### List All Records (with Pagination)
```javascript
async function listRecords(did, collection) {
  const records = [];
  let cursor = undefined;

  do {
    const url = new URL(`${PDS_URL}/xrpc/com.atproto.repo.listRecords`);
    url.searchParams.append('repo', did);
    url.searchParams.append('collection', collection);
    url.searchParams.append('limit', 100);
    if (cursor) url.searchParams.append('cursor', cursor);

    const response = await fetch(url);
    const data = await response.json();
    
    records.push(...data.records);
    cursor = data.cursor;
  } while (cursor);

  return records;
}

// Usage
const paintings = await listRecords(did, 'computer.aesthetic.painting');
console.log(`Found ${paintings.length} paintings`);
```

### Get Image URL
```javascript
function getImageUrl(did, blobCid) {
  return `${PDS_URL}/xrpc/com.atproto.sync.getBlob?did=${did}&cid=${blobCid}`;
}

// Usage
const painting = records[0];
const did = painting.uri.split('/')[2];
const cid = painting.value.image.ref.$link;
const imageUrl = getImageUrl(did, cid);

// Use in img tag
document.querySelector('img').src = imageUrl;
```

### Extract Record Key (RKEY)
```javascript
function getRkey(uri) {
  return uri.split('/').pop();
}

// Usage
const uri = 'at://did:plc:xyz123.../computer.aesthetic.painting/3jzx7k2m4n5';
const rkey = getRkey(uri); // '3jzx7k2m4n5'
```

## Testing with cURL

```bash
# 1. Resolve handle
curl "https://at.aesthetic.computer/xrpc/com.atproto.identity.resolveHandle?handle=fifi.at.aesthetic.computer"

# 2. List paintings (replace DID)
curl "https://at.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=did:plc:xyz123...&collection=computer.aesthetic.painting&limit=5"

# 3. Get image (replace DID and CID)
curl "https://at.aesthetic.computer/xrpc/com.atproto.sync.getBlob?did=did:plc:xyz123...&cid=bafkreiabcd..." --output image.png

# 4. List moods
curl "https://at.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=did:plc:xyz123...&collection=computer.aesthetic.mood&limit=5"
```

## Rate Limiting

The PDS does not currently enforce strict rate limits, but:
- Recommended: Max 100 requests per minute per client
- Pagination: Use reasonable limits (100-500)
- Caching: Cache resolved DIDs client-side

## CORS

The PDS allows CORS requests from any origin:
```
Access-Control-Allow-Origin: *
```

This enables the user pages to work purely client-side.

## Security Notes

1. **No Authentication Required** - All data is public
2. **Read-Only** - XRPC endpoints used are read-only
3. **No Personal Data** - Only public ATProto records
4. **Client-Side Only** - No server-side secrets exposed

## Related Documentation

- [ATProto Lexicon Reference](https://atproto.com/specs/lexicon)
- [XRPC Specification](https://atproto.com/specs/xrpc)
- [User Pages Architecture](USER-PAGES-ARCHITECTURE.md)
- [User Pages Documentation](USER-PAGES.md)

---

**Last Updated:** 2025-10-20
