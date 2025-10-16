# Paintings ATProto Migration Plan

## Current State

### MongoDB Paintings Collection
- **Total paintings:** 3,903
- **With user attribution:** 2,839 (73%)
- **Anonymous:** 1,064 (27%)

### Painting Record Structure
```json
{
  "_id": "6519da7dcb8ff23906174fc7",
  "slug": "2023.10.01.15.41.08.676",
  "user": "auth0|63effeeb2a7d55f8098d62f9",
  "when": "2023-10-01T20:45:49.608Z",
  "bucket": "user-aesthetic-computer",
  "code": "k3d"
}
```

### Storage Details
- **Backend:** Digital Ocean Spaces (S3-compatible)
- **Bucket:** `user-aesthetic-computer` (for user paintings)
- **Bucket:** `art-aesthetic-computer` (for anonymous paintings)
- **Access:** Paintings stored as image files (PNG/JPEG)
- **URL Pattern:** Likely `https://[bucket].s3.region.digitaloceanspaces.com/[slug].[ext]`

## ATProto Lexicon Design

### Option 1: Store Paintings as Blobs + Records
```typescript
{
  lexicon: 1,
  id: "computer.aesthetic.painting",
  defs: {
    main: {
      type: "record",
      key: "tid",
      record: {
        type: "object",
        required: ["image", "createdAt"],
        properties: {
          image: {
            type: "blob",
            accept: ["image/png", "image/jpeg", "image/webp"],
            maxSize: 10000000 // 10MB
          },
          title: {
            type: "string",
            maxLength: 100
          },
          slug: {
            type: "string", // Original aesthetic.computer slug
            maxLength: 50
          },
          code: {
            type: "string", // Short code (k3d, hxO, etc)
            maxLength: 10
          },
          createdAt: {
            type: "string",
            format: "datetime"
          },
          ref: {
            type: "string", // MongoDB _id for bidirectional link
            maxLength: 24
          }
        }
      }
    }
  }
}
```

### Option 2: External Reference (Keep in Digital Ocean)
```typescript
{
  lexicon: 1,
  id: "computer.aesthetic.painting",
  defs: {
    main: {
      type: "record",
      key: "tid",
      record: {
        type: "object",
        required: ["url", "createdAt"],
        properties: {
          url: {
            type: "string",
            format: "uri"
          },
          thumbnail: {
            type: "blob", // Small preview blob
            accept: ["image/png", "image/jpeg"],
            maxSize: 500000 // 500KB
          },
          slug: {
            type: "string",
            maxLength: 50
          },
          code: {
            type: "string",
            maxLength: 10
          },
          createdAt: {
            type: "string",
            format: "datetime"
          },
          ref: {
            type: "string",
            maxLength: 24
          }
        }
      }
    }
  }
}
```

## Migration Strategy

### Recommended Approach: **Option 2 (External Reference)**

**Pros:**
- Paintings already in Digital Ocean (no need to move data)
- Smaller ATProto records (just metadata + thumbnail)
- Faster migration
- Lower storage costs on PDS
- Can still link to full-res paintings

**Cons:**
- Paintings not fully "on ATProto"
- Dependent on Digital Ocean availability
- URLs might break if DO structure changes

### Migration Steps

1. **Create Lexicon**
   - Define `computer.aesthetic.painting` schema
   - Test with sample painting

2. **Download & Generate Thumbnails**
   - Fetch full painting from Digital Ocean
   - Generate 300x300px thumbnail
   - Store thumbnail as blob in ATProto

3. **Create ATProto Records**
   - For each painting in MongoDB with user attribution
   - Create ATProto record with metadata + thumbnail
   - Store rkey back in MongoDB (like moods)

4. **Bidirectional Links**
   - MongoDB: Add `atproto.rkey` field
   - ATProto: Add `ref` field pointing to MongoDB `_id`

5. **Handle Users Without ATProto**
   - Skip the 6 blocked users (same as moods)
   - Only migrate paintings for 978 users with ATProto accounts

## Implementation Plan

### Scripts Needed

1. `create-painting-lexicon.mjs` - Define and register the lexicon
2. `generate-painting-thumbnails.mjs` - Download + create thumbnails
3. `migrate-user-paintings.mjs` - Main migration script
4. `compare-painting-records.mjs` - Verification tool

### Estimated Numbers

- **Users with ATProto:** 978
- **Paintings to migrate:** ~2,839 (only user-attributed)
- **Top user:** @ac25namuc with 384 paintings
- **Estimated time:** ~1-2 hours (with thumbnail generation)

## Questions to Answer

1. Do we want full paintings in ATProto or just metadata + thumbnails?
2. Should we migrate anonymous paintings (1,064) or only user paintings?
3. What size thumbnail is appropriate?
4. Should we batch this differently than moods (paintings are much larger)?

## Next Steps

Choose approach and I can start implementing the lexicon and migration scripts!
