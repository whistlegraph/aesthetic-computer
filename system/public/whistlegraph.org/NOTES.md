# whistlegraph.org — open notes

Deferred items from @minanimals (Alex Freundlich) via iMessage, 2026-07-10.
Data edits + copy/loop changes from that thread are already applied to `graphs.json` / `index.html`.

## Whistlegraph Desk

`/desk` is the private Whistlegraph Desk. It uses the existing Aesthetic Computer
Auth0 application and authorizes the immutable Auth0 subjects for @jeffrey and
@minanimals on the server; matching a display handle is never enough. Live edits
are stored as a MongoDB curation overlay and applied on top of the generated
`graphs.json` / `posts.json`, so the scraper remains reproducible and an edit can
be restored to generated data at any time.

The Auth0 application needs these production entries before first deployment:

- Allowed callback URLs: `https://whistlegraph.org`,
  `https://www.whistlegraph.org`
- Allowed logout URLs: the same two URLs
- Allowed web origins: `https://whistlegraph.org`,
  `https://www.whistlegraph.org`

Auth0 returns to the bare origin; Caddy recognizes its `state` parameter, serves
the Desk callback there, and the client replaces the address with `/desk` after
the transaction completes. Ordinary visits to `/` continue to show the public site.

The server allowlist defaults to the two current Auth0 subjects. Override it with
the comma-separated `WHISTLEGRAPH_ADMIN_SUBS` environment variable when identities
change; do not put email addresses or handles in that variable.

### Codex publishing

The Desk's **Codex tools** panel installs the repository's `plugins/whistlegraph`
plugin. Its MCP creates a clean worktree from `origin/main`, restricts writes to
`system/public/whistlegraph.org/**`, validates the patch, creates an attributed
review-branch commit, and fast-forwards `main` only when the reviewed base is still
current. The plugin then POSTs the exact commit SHA to the Auth0-protected
`?action=deploy` endpoint. Production re-checks that SHA against `origin/main`
before `lith/webhook.sh` changes the live checkout. Neither the MCP nor the browser
receives `DEPLOY_SECRET` or the deployment vault.

The publisher requires the Tangled knot as the checkout's `origin`; GitHub remains
the public marketplace/mirror used to install the plugin.

## Bigger feature — needs a data model (2026-07-11)
- **Per-page source-video list.** On each whistlegraph's detail page, list *every*
  TikTok video feeding its aggregate data, each with its own view count — so we can
  audit that the numbers (and which takes belong to which composition) are accurate.
  `graphs.json` currently only stores aggregate `views` + a `perf` count, not the
  individual source URLs/views. Needs a `sources:[{url,views,date}]` array per graph
  (populated from the TikTok archive / Notion) plus a render block in `showDetail`.
  This will also make the folds above self-documenting.

## Data-accuracy pass (Alex, 2026-07-11) — resolve during the source-video intake
- **Renamed codes break CDN assets.** Archive assets are keyed by the *original*
  recovered code (`index/<code>.jpg|.mp4`). Any future code rename must add an
  `asset:"<oldcode>"` field (see `bowm`→lvme, `more`→wtm) or the thumbnail/video 404s.
- **Suspicious counts.** `2la` "Certain Personality" shows 10 videos — Alex thinks
  that's dubious. The per-page source list is the way to verify what's actually
  filed under each whistlegraph (he suspects some videos are mis-filed).
- **Double posts.** When unprivating everything, decide whether duplicate re-posts
  count toward a whistlegraph's video total or get de-duped. (Alex's open question.)
- **Missed attributions.** Alex says a few attribution requests from the past couple
  days didn't take — needs a final comb-through against his list (get specifics).
- Expect a **large manual component** to the final accuracy check.

## "Where to find our merch?" section (Alex/jeffrey, 2026-07-11)
- Don't funnel merch to one storefront — being spread across URLs and stocked in
  multiple shops (incl. museum / art-museum gift shops) is a feature. Add a
  **"Where to find our merch?"** section that *lists the outlets*: Sex Magazine
  (the zine), the Whistlegraph shop, shop.aesthetic.computer's whistlegraph section
  (TBD), plus any brick-and-mortar / museum gift shops that carry it. A directory,
  not a checkout.
- **Fan-made / user-made merch.** Some whistlegraph merch was made by users & fans —
  worth surfacing (a "made by the community" strand of the directory). Evidence lives in
  the **Instagram platter history** (see `/social`) and possibly **Notion**. Task:
  source that evidence (screenshots / posts / links) — good subagent job, can lean on
  the just-repurchased Notion seat + the IG archive.

## To design / decide
- **Left-column "cover" colors.** Hand-select the swatch color on the left of each
  title (most archive rows currently share `#b44887`). Alex: pull colors from each
  composition's main video thumbnail. For the featured top 10, consider pulling
  preview thumbnails / "covers" from the Feral File performance videos.
- **Underpainting versions.** Link the two versions of *Underpainting* on the
  Whistlegraph YouTube channel from the `undr` detail page. (Needs a `versions`
  array of embeddable sources — YouTube can't drop straight into the `<video>` tag,
  so this needs either mp4 mirrors or a YouTube-embed branch in `showDetail`.)
- **Archive combing.** Cross-check the full list against the old Notion + website
  archive; fold duplicates, decide what to keep vs. ditch, and pull in any linkable
  extras (lectures, livestreams, old blogs) not already in "Lectures, shows & press."
  Notion seat was repurchased 2026-07-10 to crawl this.
- **CV line-items.** *The Longest Whistlegraph Ever (So Far)* and *Underpainting*
  live mainly as bottom-of-page CV items, but Alex wants them to still carry a
  `[code]` and count toward the ultimate total even if they're out of the main scroll.
  (`long` and `undr` codes already exist — confirm they're counted the way he means.)

## Needs input from Alex
- **Kunstverein Hamburg link.** He asked to add "Kunstverein Hamburg" on the Feral
  File line (Elsewhere) but the URL didn't come through in iMessage ("have the link
  be:" arrived empty). Placeholder currently points at the existing Kunstverein
  Hamburg recital video (YouTube `YXUUCkqv2LY`) — swap in the intended link.
- **"More Than Enough" duplicate.** `wtm` was renamed to *More Than Enough* `[more]`
  by Alex Freundlich per his note. A separate `enuf` *More Than Enough* (by
  Whistlegraph) still exists. Confirm whether these are the same composition and
  should fold, or stay distinct.
