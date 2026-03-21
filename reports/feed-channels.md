# Feed Broadcast Plan

Channels and playlists for feed.aesthetic.computer.

---

## Channels

### 1. KidLisp (live)

User-authored generative programs ranked by popularity.

| Playlist | Type | Description |
|----------|------|-------------|
| Top 100 | dynamic | Most-hit $xxx pieces, all authors |
| Top @jeffrey | dynamic | Most-hit by @jeffrey |
| Top @fifi | dynamic | Most-hit by @fifi |
| Colors | static | 151 CSS color names (valid one-line KidLisp) |

Source: MongoDB `kidlisp` collection. Updates daily 06:00 UTC.

### 2. Paintings (new)

User-created art from AC's drawing tools.

| Playlist | Type | Description |
|----------|------|-------------|
| Recent Paintings | dynamic | Latest 100 paintings |
| Top @jeffrey Paintings | dynamic | Jeffrey's art |
| Top @fifi Paintings | dynamic | Fifi's art |

Source: MongoDB `paintings` collection.
Item URL: `https://aesthetic.computer/painting~{code}`
CDN: `art-aesthetic-computer.sfo3.digitaloceanspaces.com`

### 3. Mugs (new)

Print-on-demand ceramic mugs with spinning mockup previews.

| Playlist | Type | Description |
|----------|------|-------------|
| Recent Mugs | dynamic | Latest mugs across all colors |
| @jeffrey's Mugs | dynamic | Jeffrey's mugs |

Source: MongoDB `products` collection.
Item URL: `https://aesthetic.computer/mug~+{code}`
Each mug has a 4-frame animated WebP mockup (front/right/front/left).

### 4. Clocks (new)

The `clock` piece is a musical instrument + time display. Users save
clock melodies with short codes. Thousands of user-created clocks exist.

| Playlist | Type | Description |
|----------|------|-------------|
| Top Clocks | dynamic | Most-hit saved clocks |
| Recent Clocks | dynamic | Newly created clocks |
| @jeffrey's Clocks | dynamic | Jeffrey's clock melodies |

Source: `/api/store-clock` (or MongoDB collection behind it).
Item URL: `https://aesthetic.computer/clock~*{code}`

Clock melodies use notepat notation: `ceg` = C major chord,
`^cdefg` = struck piano notes, `{sine}cde` = sine wave melody.
Each clock shows time + plays its melody on loop.

### 5. Moods (new)

Text status updates from users. Short, personal, poetic.

| Playlist | Type | Description |
|----------|------|-------------|
| Moods of the Day | dynamic | Today's moods from all users |
| @jeffrey's Moods | dynamic | Jeffrey's thoughts |

Source: MongoDB `moods` collection.
Item URL: needs a display piece (e.g., `aesthetic.computer/mood-display~@handle~rkey`)
or render as text items if DP-1 supports it.

API: `/api/mood/moods-of-the-day?list=true`

### 6. Chats (new)

Live oscillating display between `chat` and `laer-klokken` pieces --
community conversation in real-time.

| Playlist | Type | Description |
|----------|------|-------------|
| Chat Rooms | static | Alternating: `chat`, `laer-klokken` |

`chat` = live multiplayer chat with message history, syntax highlighting
`laer-klokken` = "learn the clock" educational chat in warm terracotta theme

These are live interactive pieces, not static content. The playlist
would cycle between them for ambient community display.

### 7. Instruments (new)

Curated parameterized sequences using JavaScript pieces for TV.

| Playlist | Type | Description |
|----------|------|-------------|
| Chords | static | 31 Western chords via `clock` notepat |
| Scales | static | Major/minor/modal scales via `clock` |
| Drum Patterns | static | Common rhythms via `beat` or `3x3` |

Chords moves here from KidLisp channel (it's not actually KidLisp).

### 8. Tapes (new)

Session recordings and replays.

| Playlist | Type | Description |
|----------|------|-------------|
| Recent Tapes | dynamic | Latest recordings |
| Popular Tapes | dynamic | Most watched |

Source: MongoDB `tapes` collection.
Already has TV API: `/api/tv/tapes`
CDN: `at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/tapes/{code}.mp4`

---

## Status

All 8 channels are **live** as of 2026.02.20. Updated daily at 06:00 UTC
by `silo-update-feed.mjs` on silo via `feed-update.timer`.

| Channel | Playlists | Items | Status |
|---------|-----------|-------|--------|
| KidLisp | 4 | ~400 | Live (Colors static, Chords moved to Instruments) |
| Paintings | 3 | ~300 | Live |
| Mugs | 1 | ~50 | Live |
| Clocks | 2 | ~200 | Live |
| Moods | 2 | ~150 | Live |
| Chats | 1 | 2 | Live (static) |
| Instruments | 1 | 31 | Live (Chords, static) |
| Tapes | 1 | ~50 | Live |

---

## Implementation

### Updater Architecture

Generalize `silo-update-top100.mjs` into `silo-update-feed.mjs` with
a config-driven approach:

```javascript
const CHANNELS = [
  {
    slug: 'kidlisp',
    title: 'KidLisp',
    collection: 'kidlisp',
    playlists: [
      { slug: 'top-100', handle: null, sort: 'hits', limit: 100 },
      { slug: 'top-jeffrey', handle: 'jeffrey', sort: 'hits', limit: 100 },
      { slug: 'top-fifi', handle: 'fifi', sort: 'hits', limit: 100 },
    ],
    staticPlaylists: ['colors-id'],
    itemUrl: (doc) => `https://device.kidlisp.com/$${doc.code}`,
  },
  {
    slug: 'paintings',
    title: 'Paintings',
    collection: 'paintings',
    playlists: [
      { slug: 'recent', handle: null, sort: 'when', limit: 100 },
      { slug: 'top-jeffrey', handle: 'jeffrey', sort: 'when', limit: 100 },
      { slug: 'top-fifi', handle: 'fifi', sort: 'when', limit: 100 },
    ],
    itemUrl: (doc) => `https://aesthetic.computer/painting~${doc.code}`,
  },
  // ... mugs, clocks, tapes, moods
];
```

Each channel follows the same lifecycle:
1. Query MongoDB collection
2. Join `@handles` for attribution
3. Build DP-1 playlist items
4. Create/replace playlists via feed API
5. Update channel
6. Delete old playlists

### Update Schedule

| Channel | Frequency | Why |
|---------|-----------|-----|
| KidLisp | Daily 06:00 UTC | High activity |
| Paintings | Daily 06:00 UTC | Moderate activity |
| Clocks | Daily 06:00 UTC | Moderate activity |
| Mugs | Weekly | Low volume |
| Tapes | Daily 06:00 UTC | Moderate activity |
| Moods | Every 6 hours | Time-sensitive ("of the day") |
| Chats | Never (static) | Live pieces, no regeneration |
| Instruments | Never (static) | Curated, no regeneration |

### Landing Page

The landing page at feed.aesthetic.computer already reads from the API
dynamically -- new channels will appear automatically as they're created.
Update subtitle from "playlists and channels for KidLisp" to something
broader once we have multiple channels.

---

## Summary

8 channels, ~25 playlists, covering all of AC's user-generated content:

```
feed.aesthetic.computer
  KidLisp     [Top 100, Top @jeffrey, Top @fifi, Colors]
  Paintings   [Recent, Top @jeffrey, Top @fifi]
  Mugs        [Recent, @jeffrey's]
  Clocks      [Top, Recent, @jeffrey's]
  Moods       [Of the Day, @jeffrey's]
  Chats       [chat, laer-klokken]
  Instruments [Chords, Scales, Drums]
  Tapes       [Recent, Popular]
```

---

*Generated 2026.02.20*
