# ATProto Experiments - Quick Start

## What We Just Set Up

Created `/at` directory with ATProto exploration tools for Aesthetic Computer.

## Current State of @aesthetic.computer on Bluesky

```
Handle:       aesthetic.computer
DID:          did:plc:k3k3wknzkcnekbnyde4dbatz
Followers:    0
Following:    2
Posts:        1
```

**Existing post:** "ty the invite 🦋" (from October 20, 2024)

## Ready-to-Use Tools

### 1. Query Profile
```bash
cd /workspaces/aesthetic-computer/at
node query-profile.mjs aesthetic.computer
```

Fetches profile info, follower counts, and recent posts for any handle.

### 2. Query Posts
```bash
node query-posts.mjs aesthetic.computer --limit=10
```

Fetches detailed post information including text, embeds, engagement.

### 3. Explore Lexicons
```bash
node explore-lexicons.mjs
```

Shows all standard Bluesky lexicons + proposed AC custom lexicons.

### 4. Post to Bluesky (Requires Setup)

**First, create app password:**
1. Go to https://bsky.app/settings/app-passwords
2. Create new app password
3. Copy `.env.example` to `.env`
4. Add credentials:
   ```env
   BSKY_IDENTIFIER=aesthetic.computer
   BSKY_APP_PASSWORD=your-app-password-here
   ```

**Then post:**
```bash
node post-to-bluesky.mjs "Hello from Aesthetic Computer! 🎨"
node post-to-bluesky.mjs "New painting!" --image=painting.png
```

## Intermediate Path: Official Account Strategy

Instead of building a full PDS immediately, you can:

1. **Post paintings to @aesthetic.computer Bluesky account**
   - User shares painting in AC
   - AC posts to official account: "New painting by @jeffrey: 'Abstract Dreams' 🎨"
   - Image attached to post
   - Links back to AC

2. **Test federation before infrastructure commitment**
   - See how AC content looks on Bluesky
   - Learn ATProto SDK
   - Build following
   - Zero infrastructure cost

3. **Later: Migrate to full PDS**
   - When ready, give each user their own identity
   - `jeffrey.aesthetic.computer`
   - Full data portability
   - Custom lexicons

## Integration Example

```javascript
// In AC's painting share handler
import { BskyAgent } from '@atproto/api'

async function sharePaintingToBluesky(painting, user) {
  const agent = new BskyAgent({ service: 'https://bsky.social' })
  
  await agent.login({
    identifier: process.env.BSKY_IDENTIFIER,
    password: process.env.BSKY_APP_PASSWORD
  })

  // Upload image
  const imageData = await fetch(painting.url).then(r => r.arrayBuffer())
  const { data } = await agent.uploadBlob(new Uint8Array(imageData), {
    encoding: 'image/png'
  })

  // Post
  await agent.post({
    text: `New painting by @${user.handle}: "${painting.title}"\n\n🎨 aesthetic.computer/${painting.slug}`,
    embed: {
      $type: 'app.bsky.embed.images',
      images: [{
        image: data.blob,
        alt: painting.description || painting.title
      }]
    }
  })
}
```

## Next Steps

1. ✅ **Done:** Set up `/at` directory with tools
2. ✅ **Done:** Query existing @aesthetic.computer profile
3. 🔜 **Next:** Get app password and test posting
4. 🔜 **Next:** Integrate "Share to Bluesky" button in AC
5. 🔜 **Future:** Design custom lexicons (`computer.aesthetic.*`)
6. 🔜 **Future:** Deploy own PDS for per-user identities

## Resources

- **ATProto Docs:** https://atproto.com
- **Bluesky API:** https://docs.bsky.app
- **ATProto SDK:** https://github.com/bluesky-social/atproto/tree/main/packages/api
- **Your Roadmap:** `/workspaces/aesthetic-computer/plans/atproto-pds-roadmap.md`
- **Data Architecture:** `/workspaces/aesthetic-computer/plans/atproto-data-architecture.md`

## Benefits of This Approach

- ✅ **Learn by doing** - experiment with real ATProto APIs
- ✅ **Instant visibility** - use existing Bluesky account
- ✅ **No infrastructure** - no servers to manage
- ✅ **Test federation** - see how AC fits into ATProto
- ✅ **Build following** - grow @aesthetic.computer presence
- ✅ **Easy integration** - just API calls, no auth changes

When you're ready for full PDS with per-user identities, the roadmap has all the details!
