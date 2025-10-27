# Lexicon Publication Plan

## Overview
Publish the `computer.aesthetic.*` lexicon schemas to ATProto according to the official specification at https://atproto.com/specs/lexicon#lexicon-publication-and-resolution

## Current State

### Lexicon Schemas (5 total)
Located in `/at/lexicons/computer/aesthetic/`:
- `painting.json` - Digital paintings with thumbnails and recordings
- `mood.json` - User mood/status updates
- `piece.json` - Interactive piece metadata
- `kidlisp.json` - KidLisp code artifacts
- `tape.json` - Video/audio tape recordings

### Authority & Identity
- **Authority Domain**: `aesthetic.computer`
- **Reversed Authority**: `computer.aesthetic`
- **DID**: `did:plc:tliuubv7lyv2uiknsjbf4ppw` (art.at.aesthetic.computer)
- **PDS URL**: `https://at.aesthetic.computer`

## ATProto Lexicon Publication Requirements

### 1. DNS TXT Record Setup
According to the spec, create a DNS TXT record at:
```
_lexicon.aesthetic.computer
```

**Value**:
```
did=did:plc:tliuubv7lyv2uiknsjbf4ppw
```

**Purpose**: Links the authority domain to the DID hosting the lexicon schemas

### 2. Schema Record Publication
For each lexicon, publish a `com.atproto.lexicon.schema` record to the repository.

**Record Structure**:
```json
{
  "$type": "com.atproto.lexicon.schema",
  "lexicon": 1,
  "id": "computer.aesthetic.painting",  // NSID
  "defs": {
    // schema definitions from painting.json
  }
}
```

**Record Key**: Must be the NSID itself (e.g., `computer.aesthetic.painting`)

**AT-URI Format**:
```
at://did:plc:tliuubv7lyv2uiknsjbf4ppw/com.atproto.lexicon.schema/computer.aesthetic.painting
```

### 3. Authority at "Group" Level
All NSIDs with the same authority (`computer.aesthetic.*`) are published to a single repository (our PDS at at.aesthetic.computer).

## Implementation Steps

### Phase 1: DNS Configuration
**Tool**: Cloudflare DNS (credentials in vault)

1. Access Cloudflare DNS for aesthetic.computer domain
2. Create TXT record:
   - **Name**: `_lexicon.aesthetic.computer`
   - **Value**: `did=did:plc:tliuubv7lyv2uiknsjbf4ppw`
   - **TTL**: Auto (or 300 for testing)
3. Verify DNS propagation:
   ```fish
   dig TXT _lexicon.aesthetic.computer +short
   ```

### Phase 2: Create Publication Script
**File**: `/at/scripts/publish-lexicons.mjs`

**Features**:
- Read all lexicon JSON files from `/at/lexicons/computer/aesthetic/`
- For each lexicon:
  - Load schema definitions
  - Create `com.atproto.lexicon.schema` record
  - Use NSID as record key
  - Publish to PDS repository
- Support dry-run mode
- Provide detailed logging

**Dependencies**:
- `@atproto/api` (already in package.json)
- Use existing `.env` credentials (BSKY_IDENTIFIER, BSKY_APP_PASSWORD)

### Phase 3: Publish Schemas
1. Run script in dry-run mode to verify
2. Publish all 5 lexicons:
   - `computer.aesthetic.painting`
   - `computer.aesthetic.mood`
   - `computer.aesthetic.piece`
   - `computer.aesthetic.kidlisp`
   - `computer.aesthetic.tape`
3. Verify records exist in repository

### Phase 4: Verification
1. **DNS Check**: Verify TXT record resolves correctly
2. **DID Document Check**: Verify PDS service endpoint
3. **Record Retrieval**: Query each schema record via AT-URI
4. **Lexicon Resolution**: Test full resolution chain:
   - NSID → reversed authority → DNS lookup → DID → PDS → record

## Script Pseudocode

```javascript
import { BskyAgent } from '@atproto/api';
import { readdir, readFile } from 'fs/promises';
import path from 'path';

async function publishLexicons(dryRun = true) {
  // 1. Initialize agent
  const agent = new BskyAgent({ service: process.env.PDS_URL });
  await agent.login({
    identifier: process.env.BSKY_IDENTIFIER,
    password: process.env.BSKY_APP_PASSWORD
  });

  // 2. Read lexicon files
  const lexiconsDir = './lexicons/computer/aesthetic';
  const files = await readdir(lexiconsDir);
  
  for (const file of files.filter(f => f.endsWith('.json'))) {
    const schemaPath = path.join(lexiconsDir, file);
    const schema = JSON.parse(await readFile(schemaPath, 'utf-8'));
    
    // 3. Create schema record
    const record = {
      $type: 'com.atproto.lexicon.schema',
      lexicon: schema.lexicon,
      id: schema.id,  // NSID
      defs: schema.defs
    };
    
    // 4. Publish with NSID as rkey
    const rkey = schema.id;
    
    if (dryRun) {
      console.log(`[DRY RUN] Would publish ${schema.id}`);
      console.log(JSON.stringify(record, null, 2));
    } else {
      await agent.api.com.atproto.repo.putRecord({
        repo: agent.session.did,
        collection: 'com.atproto.lexicon.schema',
        rkey: rkey,
        record: record
      });
      console.log(`✓ Published ${schema.id}`);
    }
  }
}
```

## Testing Strategy

### 1. Dry Run
```fish
node /workspaces/aesthetic-computer/at/scripts/publish-lexicons.mjs --dry-run
```

### 2. Live Publication
```fish
node /workspaces/aesthetic-computer/at/scripts/publish-lexicons.mjs
```

### 3. Verification Queries
```fish
# Query a specific schema
node -e "
import('@atproto/api').then(({BskyAgent}) => {
  const agent = new BskyAgent({service: 'https://at.aesthetic.computer'});
  agent.api.com.atproto.repo.getRecord({
    repo: 'did:plc:tliuubv7lyv2uiknsjbf4ppw',
    collection: 'com.atproto.lexicon.schema',
    rkey: 'computer.aesthetic.painting'
  }).then(res => console.log(JSON.stringify(res.data, null, 2)));
});
"
```

## Success Criteria

- [ ] DNS TXT record at `_lexicon.aesthetic.computer` returns DID
- [ ] All 5 lexicon schemas published as `com.atproto.lexicon.schema` records
- [ ] Each record key matches its NSID
- [ ] Records retrievable via AT-URI
- [ ] Schema validation passes (lexicon: 1, id, defs all present)
- [ ] Records visible in PDS repository

## References

- [ATProto Lexicon Spec](https://atproto.com/specs/lexicon#lexicon-publication-and-resolution)
- [com.atproto.lexicon.schema](https://atproto.com/lexicons/com-atproto-lexicon#comatprotolexiconschema)
- Existing sync scripts: `/system/backend/sync-atproto.mjs`, `/at/share-latest-painting.mjs`

## Notes

- Authority is at "group" level, so all `computer.aesthetic.*` NSIDs go to one repo
- DNS record links the authority domain to the DID
- Record keys MUST be the NSID (not TIDs like other records)
- Schema records are special system records in the `com.atproto.lexicon` namespace
- Once published, external apps can discover and use these schemas via DNS + DID resolution
