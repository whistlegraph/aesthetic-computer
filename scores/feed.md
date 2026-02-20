# Feed Score

This score defines automated tasks for the KidLisp feed system on
`feed.aesthetic.computer` (SQLite on silo).

## Philosophy

The feed is a living thing. The Top 100 playlist reflects what people
are actually watching and making — it should stay fresh. Colors and
Chords are fixed palettes that don't change. The channel is the
container that holds everything together.

Signal: a playlist that accurately reflects current activity.
Noise: updating when nothing has changed.

## Architecture

```
feed.aesthetic.computer (Caddy HTTPS)
  -> localhost:8787 (dp1-feed, SQLite)
     -> /api/v1/channels/156c4235-...  (KidLisp channel)
        -> Top 100 playlist      (dynamic, daily)
        -> Top @jeffrey playlist (dynamic, daily)
        -> Top @fifi playlist    (dynamic, daily)
        -> Colors playlist       (static)
        -> Chords playlist       (static)
```

Backend: silo.aesthetic.computer, systemd service `dp1-feed`
Storage: `/opt/dp1-feed/data/dp1-feed.db` (SQLite, WAL mode)
Build script: `fish feed/build-feed.fish`
Update script: `feed/silo-update-top100.mjs` (runs on silo)

## Current Tasks

### Daily (automated via systemd timer)

- **Refresh dynamic playlists**: Query MongoDB for the most-hit KidLisp
  pieces (overall and per-author), regenerate Top 100, Top @jeffrey,
  and Top @fifi playlists, update the channel.
  Colors and Chords are static and left untouched.
  Script: `/opt/feed-updater/update-top100.mjs` on silo
  Timer: `feed-update-top100.timer` on silo (runs daily at 06:00 UTC)

### On Demand (run manually)

- **Full rebuild**: Delete everything and regenerate all playlists
  plus the channel from scratch.
  Script: `fish feed/build-feed.fish`

- **Add new playlist**: Create a new playlist generator script in
  `feed/`, add it to `build-kidlisp-feed.mjs`, rebuild.

### Future Ideas

- Weekly "Rising" playlist: pieces gaining hits fastest this week
- "New This Week" playlist: most recent KidLisp pieces
- Seasonal rotation: swap out the channel's playlist order by season

## Sacred Ground

- Never delete the KidLisp channel without rebuilding it immediately
- Never modify `dp1-feed/` submodule files from this score
- The API secret lives in vault only — never hardcode it
- Colors and Chords playlists are static — don't regenerate them
  on the daily schedule (wastes IDs and breaks external references)

## IDs

```
Channel:  156c4235-4b24-4001-bec9-61ce0ac7c25e
Feed URL: https://feed.aesthetic.computer/api/v1
Silo IP:  64.23.151.169
```
