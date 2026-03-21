# Feed Score

This score defines automated tasks for the feed broadcast system on
`feed.aesthetic.computer` (SQLite on silo).

## Philosophy

The feed is a living thing. Dynamic playlists reflect what people
are actually watching and making — they should stay fresh. Static
playlists (Colors, Chords, Chats) are fixed and don't change.
Channels are the containers that hold everything together.

Signal: playlists that accurately reflect current activity.
Noise: updating when nothing has changed.

## Architecture

```
feed.aesthetic.computer (Caddy HTTPS + landing page)
  -> localhost:8787 (dp1-feed, SQLite)

8 channels, ~15 playlists:

  KidLisp       [Top 100, Top @jeffrey, Top @fifi, Colors]
  Paintings     [Recent, Top @jeffrey, Top @fifi]
  Mugs          [Recent]
  Clocks        [Top, Recent]
  Moods         [Recent, @jeffrey]
  Chats         [chat + laer-klokken]  (static)
  Instruments   [Chords]               (static)
  Tapes         [Recent]
```

Backend: silo.aesthetic.computer, systemd service `dp1-feed`
Storage: `/opt/dp1-feed/data/dp1-feed.db` (SQLite, WAL mode)
State: `/opt/feed-updater/feed-state.json` (tracks channel/playlist IDs)
Update script: `feed/silo-update-feed.mjs` (runs on silo)
Landing page: `/opt/dp1-feed/landing-page.html` (served by Caddy at `/`)

## Current Tasks

### Daily (automated via systemd timer)

- **Refresh all channels**: Query MongoDB for each channel's collections,
  regenerate dynamic playlists, update channels, clean up old playlists.
  Static playlists (Colors, Chords, Chats) are preserved via state file.
  Script: `/opt/feed-updater/silo-update-feed.mjs` on silo
  Timer: `feed-update.timer` on silo (runs daily at 06:00 UTC)

### On Demand (run manually)

- **Update one channel**: `node silo-update-feed.mjs kidlisp`
- **Update specific channels**: `node silo-update-feed.mjs paintings clocks`
- **Full rebuild**: Delete everything and regenerate from scratch
  Script: `fish feed/build-feed.fish` (KidLisp channel only)

### Future Ideas

- Weekly "Rising" playlist: pieces gaining hits fastest this week
- "New This Week" playlist: most recent KidLisp pieces
- Seasonal rotation: swap out channel playlist order by season
- Scales playlist for Instruments channel
- Drum Patterns playlist for Instruments channel

## Sacred Ground

- Never delete channels without rebuilding them immediately
- Never modify `dp1-feed/` submodule files from this score
- The API secret lives in vault only — never hardcode it
- Colors and Chords playlists are static — don't regenerate them
  (wastes IDs and breaks external references)
- The `feed-state.json` file is the source of truth for channel/playlist mapping

## Data Sources

| Channel | MongoDB Collection | Sort | Key Fields |
|---------|-------------------|------|------------|
| KidLisp | `kidlisp` | hits desc | code, hits, user |
| Paintings | `paintings` | when desc | code, user, nuked |
| Mugs | `products` | createdAt desc | code, variant, preview |
| Clocks | `clocks` | hits desc / when desc | code, source, hits |
| Moods | `moods` | when desc | mood, handle, deleted |
| Tapes | `tapes` | when desc | code, slug, nuked |

## IDs

```
Feed URL: https://feed.aesthetic.computer/api/v1
Silo IP:  64.23.151.169
```
