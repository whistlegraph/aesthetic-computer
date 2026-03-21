# DP1 Provenance Block Update

## Summary

This update adds **provenance blocks** to all KidLisp playlists according to the [DP-1 specification](https://github.com/display-protocol/dp1/blob/main/docs/spec.md#6--provenance-provenance).

## Changes Made

### 1. Updated Playlist Generation Scripts

All playlist generation scripts now include offchain provenance blocks:

- ✅ `create-top-kidlisp-playlist.mjs` - Top 100 KidLisp pieces
- ✅ `create-kidlisp-colors-playlist.mjs` - CSS color exploration
- ✅ `create-kidlisp-chords-playlist.mjs` - Musical chords

**Provenance Block Format:**
```javascript
provenance: {
  type: 'offChainURI',
  contract: {
    uri: 'https://aesthetic.computer/$code',
  }
}
```

### 2. Created Regeneration Script

**File:** `regenerate-kidlisp-playlists.mjs`

This script automates the complete regeneration process:

1. Finds and deletes existing KidLisp channel
2. Finds and deletes all existing KidLisp playlists
3. Regenerates all playlists with provenance blocks
4. Creates a new channel with all the updated playlists

**Usage:**
```bash
cd /workspaces/aesthetic-computer
FEED_API_SECRET=your_secret node feed/regenerate-kidlisp-playlists.mjs
```

## Why Provenance Blocks Matter

According to the DP-1 spec, provenance blocks:

- Link rendered assets to their source of truth (on-chain or off-chain)
- Provide verifiable documentation of artwork origins
- Support future tokenization and rights management
- Enable proper attribution in display contexts

## Provenance Types

The DP-1 spec supports three types:

1. **`onChain`** - Explicit contract (ERC-721, FA2, etc.)
2. **`seriesRegistry`** - Parent collection in SeriesRegistry
3. **`offChainURI`** - URI points to canonical source ✅ (used here)

For KidLisp pieces, we use **offChainURI** since they're not yet tokenized but have canonical URLs.

## Next Steps

### To Regenerate Your Playlists:

```bash
# Set your Feed API secret
export FEED_API_SECRET="your_actual_secret_here"

# Run the regeneration script
cd /workspaces/aesthetic-computer
node feed/regenerate-kidlisp-playlists.mjs
```

### Future Tokenization

When KidLisp pieces are minted as tokens, update the provenance to:

```javascript
provenance: {
  type: 'onChain',
  contract: {
    chain: 'tezos',
    standard: 'fa2',
    address: 'KT1...',  // Your FA2 contract address
    tokenId: '42',       // Token ID
    uri: 'ipfs://...',   // Token metadata URI
  }
}
```

## Verification

After regeneration, verify playlists include provenance:

```bash
# Get playlist and check for provenance field
curl https://feed.aesthetic.computer/api/v1/playlists/{id} | jq '.items[0].provenance'
```

Expected output:
```json
{
  "type": "offChainURI",
  "contract": {
    "uri": "https://aesthetic.computer/$code"
  }
}
```

## Files Modified

- ✅ `feed/create-top-kidlisp-playlist.mjs`
- ✅ `feed/create-kidlisp-colors-playlist.mjs`
- ✅ `feed/create-kidlisp-chords-playlist.mjs`
- ✨ `feed/regenerate-kidlisp-playlists.mjs` (new)
- ✨ `feed/DP1-PROVENANCE-UPDATE.md` (this file)

## References

- [DP-1 Provenance Spec](https://github.com/display-protocol/dp1/blob/main/docs/spec.md#6--provenance-provenance)
- [DP-1 Full Specification](https://github.com/display-protocol/dp1/blob/main/docs/spec.md)
- [KidLisp FA2 Contract Spec](../reports/kidlisp-fa2-contract-spec.md)
