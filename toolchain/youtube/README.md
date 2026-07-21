# YouTube posting pipeline

Scripted uploads to the aesthetic.computer YouTube channel
(signed in as **mail@aesthetic.computer**). Zero npm dependencies —
`yt.mjs` talks to the YouTube Data API v3 directly over `fetch`.

## One-time setup

Credentials live in the **vault**, never in this repo:

```
aesthetic-computer-vault/youtube/client.json   OAuth client (Desktop app)
aesthetic-computer-vault/youtube/token.json    saved refresh token
```

1. **Enable the API.** [console.cloud.google.com](https://console.cloud.google.com)
   → project `aesthetic-computer` → APIs & Services → Library →
   enable **YouTube Data API v3**.
2. **Make an OAuth client.** APIs & Services → Credentials →
   Create credentials → OAuth client ID → Application type
   **Desktop app**. Download the JSON, save it as
   `aesthetic-computer-vault/youtube/client.json`.
   - If the consent screen isn't configured yet, set it up as
     **External**, add `mail@aesthetic.computer` as a test user
     (the app can stay in "testing" — test-user refresh tokens
     issued to a Desktop client don't expire on the 7-day clock).
3. **Authorize.**

   ```bash
   node toolchain/youtube/yt.mjs auth
   ```

   A browser opens — sign in as **mail@aesthetic.computer**, approve.
   The refresh token is written to the vault. Done once; uploads are
   fully scripted afterward.

## Commands

```bash
# confirm which channel is wired up
node toolchain/youtube/yt.mjs whoami

# upload (defaults to PRIVATE so you review in YouTube Studio first)
node toolchain/youtube/yt.mjs upload <video.mp4> \
  --title "..." \
  --description-file <path.txt> \
  --tags trance,waltz,aesthetic-computer \
  --privacy private \
  --category 10 \
  --thumbnail <image.jpg>
```

Flags: `--title` (required), `--description` / `--description-file`,
`--tags a,b,c`, `--privacy private|unlisted|public` (default
`private`), `--category` (default `10` = Music), `--thumbnail`,
`--playlist <id>`, `--language <code>`, `--made-for-kids`.

Every upload drops a `<video>.youtube.json` receipt next to the file
with the resulting `videoId`, watch URL, and the metadata that was
sent.

## Other channels (whistlegraph)

Every command takes `--as <channel>`, which swaps in
`aesthetic-computer-vault/youtube/<channel>-token.json`. The OAuth
client is shared; each channel just needs its own one-time consent:

```bash
node toolchain/youtube/yt.mjs auth --as whistlegraph
node toolchain/youtube/yt.mjs whoami --as whistlegraph
node toolchain/youtube/yt.mjs upload video.mp4 --as whistlegraph --title "..."
```

Use `--login-hint <email>` on `auth` when several Google accounts are
present in the browser and the token must belong to one specific account.

## Playlists and resumable batches

Create or reuse a playlist by exact title (safe to rerun):

```bash
node toolchain/youtube/yt.mjs playlist-ensure --as <channel> \
  --title "Playlist title" --description-file description.txt \
  --privacy unlisted
```

For a set of uploads, put the channel, playlist, defaults, and a `videos[]`
array in a JSON manifest, then run:

```bash
node toolchain/youtube/batch.mjs path/to/manifest.json
node toolchain/youtube/batch.mjs path/to/manifest.json --dry-run
```

The batch runner uses the same per-channel OAuth token as `yt.mjs`. Each
successful video leaves its usual `.youtube.json` receipt; rerunning skips
matching receipts, reuses the playlist, and resumes at the first unfinished
video. This is useful when the YouTube API quota resets between upload days.

### Unlisted uploads before a YouTube API audit

YouTube forces uploads from unaudited API projects created after July 28, 2020
to Private, even if `videos.insert` asks for Unlisted. While an audit is
pending, `studio-batch.mjs` drives an already signed-in, remote-debugging
Chrome profile through YouTube Studio instead. It applies the same manifest
metadata, playlist, not-made-for-kids setting, and Unlisted visibility, and
writes the same resumable receipts:

```bash
open -na "Google Chrome" --args \
  --user-data-dir="$HOME/.local/share/fuser-youtube-chrome" \
  --remote-debugging-port=9444 https://studio.youtube.com

node toolchain/youtube/studio-batch.mjs path/to/manifest.json --port=9444
node toolchain/youtube/studio-batch.mjs path/to/manifest.json --start=4 --limit=1
```

Keep the dedicated profile private: Studio automation uses its signed-in
browser session. The batch is safe to resume because a receipt is written only
after Studio accepts the Unlisted publish action.

During consent, Google's account chooser lists the channel identities —
pick **whistlegraph**. If the whistlegraph channel lives under a
*different* Google account (not a brand channel of
mail@aesthetic.computer), first add that account as a test user on the
OAuth consent screen (GCP project `aesthetic-computer` → APIs &
Services → OAuth consent screen), then sign in as it.

## Notes

- **Private-first.** The default privacy is `private` — uploads land
  as drafts you flip to public yourself in YouTube Studio.
- **Quota.** `videos.insert` currently allows 100 calls/day and costs one unit
  in the Video Uploads quota bucket.
- **Music visualizers** come from `pop/dance/bin/cover-video.mjs`
  rendered at `--size 1920x1080`. See `pop/RELEASES.md` for per-track
  build recipes.
