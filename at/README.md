# ATProto Experiments & Tools

This directory contains experiments, tools, and explorations of the ATProto (Authenticated Transfer Protocol) network, including Bluesky.

## Purpose

Before committing to running a full PDS (Personal Data Server), this space lets us:

- 🔍 **Query** existing ATProto data (like @aesthetic.computer on Bluesky)
- 🧪 **Experiment** with ATProto SDKs and APIs
- 🛠️ **Build** introspective terminal tools
- 📊 **Analyze** the network and its capabilities
- 🚀 **Test** posting and federation features

## Getting Started

```bash
cd /workspaces/aesthetic-computer/at
npm install
```

## Available Tools

### `query-profile.mjs`
Query information about any ATProto handle or DID.

```bash
node query-profile.mjs aesthetic.computer
```

### `query-posts.mjs`
Fetch recent posts from a handle.

```bash
node query-posts.mjs aesthetic.computer --limit 10
```

### `post-to-bluesky.mjs`
Post to the official @aesthetic.computer Bluesky account.

```bash
# Requires BSKY_IDENTIFIER and BSKY_APP_PASSWORD env vars
node post-to-bluesky.mjs "Hello from Aesthetic Computer! 🎨"
```

### `explore-lexicons.mjs`
Explore ATProto lexicons and schemas.

```bash
node explore-lexicons.mjs
```

### `firehose-monitor.mjs`
Monitor the ATProto firehose in real-time.

```bash
node firehose-monitor.mjs
```

### `resolve-did.mjs`
Resolve DIDs to see PDS endpoints and DID documents.

```bash
node resolve-did.mjs aesthetic.computer
node resolve-did.mjs did:plc:k3k3wknzkcnekbnyde4dbatz
```

### `test-all.mjs`
Run all tests to verify setup.

```bash
node test-all.mjs
```

## Environment Variables

Create a `.env` file in this directory:

```env
# For posting to Bluesky (optional)
BSKY_IDENTIFIER=aesthetic.computer
BSKY_APP_PASSWORD=your-app-password-here

# For querying (uses public API by default)
BSKY_SERVICE=https://bsky.social
```

**To create an app password:**
1. Go to https://bsky.app/settings/app-passwords
2. Create new app password
3. Copy it to `.env`

## Key Dependencies

- **@atproto/api** - Main ATProto SDK for Bluesky interactions
- **@atproto/xrpc** - Low-level XRPC client
- **@atproto/lexicon** - Lexicon schema tools
- **@atproto/identity** - DID resolution and handle verification
- **@atproto/sync** - Firehose subscription client

## Architecture Notes

### ATProto != Bluesky

- **ATProto** is the protocol (like ActivityPub or SMTP)
- **Bluesky** is one implementation/network using ATProto
- You can run your own PDS and still federate with Bluesky
- Custom lexicons are first-class citizens

### Current State: Using Bluesky's PDS

The @aesthetic.computer handle currently uses:
- **PDS:** `https://bsky.social` (Bluesky's official PDS)
- **DID:** Will be resolved when queried
- **Handle:** `aesthetic.computer` (custom domain)

### Future State: AC's Own PDS

Eventually, AC could run:
- **PDS:** `https://pds.aesthetic.computer`
- **DID:** AC-controlled DIDs for all users
- **Handles:** `user.aesthetic.computer`
- **Lexicons:** Custom `computer.aesthetic.*` schemas

## Learning Resources

- [ATProto Documentation](https://atproto.com)
- [Bluesky API Docs](https://docs.bsky.app)
- [ATProto GitHub](https://github.com/bluesky-social/atproto)
- [Lexicon Specs](https://atproto.com/specs/lexicon)

## Examples

### Simple Query

```javascript
import { BskyAgent } from '@atproto/api'

const agent = new BskyAgent({ service: 'https://bsky.social' })
const profile = await agent.getProfile({ actor: 'aesthetic.computer' })

console.log(profile.data.displayName)
console.log(profile.data.followersCount)
```

### Post with Image

```javascript
import { BskyAgent } from '@atproto/api'
import fs from 'fs'

const agent = new BskyAgent({ service: 'https://bsky.social' })
await agent.login({
  identifier: process.env.BSKY_IDENTIFIER,
  password: process.env.BSKY_APP_PASSWORD
})

const imageData = fs.readFileSync('./painting.png')
const { data } = await agent.uploadBlob(imageData, { encoding: 'image/png' })

await agent.post({
  text: 'Check out this painting! 🎨',
  embed: {
    $type: 'app.bsky.embed.images',
    images: [{ image: data.blob, alt: 'A beautiful painting' }]
  }
})
```

## Next Steps

1. ✅ Explore existing @aesthetic.computer data
2. ✅ Test posting from AC to Bluesky
3. ✅ Understand lexicons and schemas
4. 🔜 Design custom `computer.aesthetic.*` lexicons
5. 🔜 Decide on PDS deployment strategy
6. 🔜 Build migration path from Auth0 to ATProto

---

**Status:** 🧪 Experimental - exploring before committing to infrastructure
