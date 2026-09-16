# Mime

Discussion on AC media. The frontend is `/mime/` and the `https://mime.ac/`
apex; the API is `/api/mime`. `www.mime.ac` redirects to the apex.
The main feed is one responsive column of media cards, with contained media,
author-colored handles and avatars, comment controls, and the first comment preview.
Letterboxing uses a slowly drifting, author-tinted checkerboard; reduced motion
keeps it still. It loads
12 posts at a time and preserves the current card and offset across thread visits
and orientation changes. Videos play muted
inline and pause offscreen. A load-more button supports manual loading and
retries. The feed has no category bar, file composer, or footer. Each post shows its full
MIME type and a bookmark ribbon. Saved favorites live in browser storage as post
IDs; the Saved view rechecks current public visibility and supports removal.
Failed media previews show an explicit fallback link. ZIP tapes remain downloads;
MP4 tapes autoplay muted while visible and pause offscreen.
`/mimechan/` preserves old hash links through a redirect, and `/api/mimechan`
remains an alias. Existing uploaded files and replies stay in `mimechan`.

## Media identity

Public `paintings`, `tapes`, `pieces`, and `kidlisp` records act as opening posts.
Their thread keys are `<kind>_<Mongo ObjectId>`. Codes, handles, filenames, and
tape conversions can change without changing the discussion. Media without
short codes can be addressed by ObjectId.

Reads combine native uploads with the current source records. No backfill or
upload hook is required, and source files are not copied. `mime-media-threads`
stores only reply counts and bump times, starting with the first reply.

Records marked nuked, deleted, hidden, private, draft, or with a non-public
visibility are excluded. Legacy records without a visibility field follow the
existing public AC feeds. Private upload drafts live in a separate collection
and are never queried. Removed media threads and their reply attachments return
404; restoring the original record restores its discussion.

## API

- `GET /api/mime` — boards and recent opening posts, with up to two text-only
  `preview` entries (`name`, `text`) per commented post; `page=0` by default.
- `GET /api/mime?board=image/png&page=0` — 12 threads, three recent replies each.
- `GET /api/mime?media=painting&code=abc` — resolve a public media code to its thread.
- `GET /api/mime?thread=painting_<id>` — original media and replies.
- `GET /api/mime?file=painting_<id>` — resolve current source bytes or storage URL.
- `GET /api/mime?me=1` — verified handle for the supplied bearer token.
- `POST /api/mime` — `{ parent, text, name, file }`. A null parent requires a file;
  a reply requires text and rejects files. Opening-post files use
  `{ name, type, data }` with base64 bytes and an 8 MiB limit. Replies keep their
  parent's board. The first 300 replies bump a thread.

Source metadata comes from AC records, not request-supplied URLs or attribution.
Signed-in posts store the verified AC account and display its current handle.
The composer reuses AC’s same-origin Auth0 session (SDK keys use `::`
separators) or its encoded `session-aesthetic` host session. Both are verified
by the API; invalid sessions cannot fall back to posting anonymously. Guest names remain unverified. Existing comment
attachments remain readable, but new comments are text-only.
Program source is displayed as text. Native HTML previews remain sandboxed.
Painting and tape metadata endpoints expose `discussion` URLs for other clients.

Direct entry: `/mime/#/media/painting/abc` (also `tape`, `piece`, or `kidlisp`).
The painting viewer includes a Comment button. Other media can be discussed from
the main feed or a direct entry URL.
Entering `mime` in the prompt opens the feed.
The corner word returns to `/prompt`.

Legacy piece records without an extension retain the existing JavaScript
default. Both upload paths now preserve the extension for future records.
Rate limiting and moderation tools for anonymous replies remain follow-up work.

## Validation

Use a disposable loopback MongoDB; the tests refuse remote hosts and create
their own temporary database:

```sh
MIME_TEST_MONGO_URI=mongodb://127.0.0.1:27017 node --test system/backend/tests/mime.test.mjs
```

Tests cover automatic discovery without writes, attribution, concurrent first
replies, attachment rejection, verified authorship, source visibility, storage URLs, legacy uploads,
pagination, and identity across tape conversion.

## Auth0 domains

MIME uses the existing AC SPA client (`LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt`)
through `hi.aesthetic.computer`. Keep all existing application URL entries;
append these exact values in the Auth0 application settings:

- Allowed Callback URLs: `https://mime.ac/`, `https://aesthetic.computer/mime/`
- Allowed Logout URLs: `https://mime.ac/`, `https://aesthetic.computer/mime/`
- Allowed Web Origins and Allowed Origins (CORS): `https://mime.ac`

The SDK handles authorization-code callbacks and validates OAuth state. The
return hash and an in-progress comment survive the login redirect. Account
handles are still resolved by the API from the verified access token. Browser
storage is origin-scoped, so MIME offers sign-in on the new domain; it does
not copy tokens between domains. The wordmark returns to AC's prompt.

## Name and history

The name refers to MIME media types (`image/png`, `audio/ogg`, `text/plain`).
[Content-Type predates MIME](https://www.rfc-editor.org/rfc/rfc1049): RFC 1049
proposed it for structured Internet messages in March 1988. Nathaniel Borenstein
and Ned Freed's [RFC 1341](https://www.rfc-editor.org/rfc/rfc1341), June 1992,
extended mail to carry multiple parts, richer character sets, images, audio,
and other data. The familiar [RFC 2045](https://www.rfc-editor.org/rfc/rfc2045)
is a November 1996 revision, not MIME's starting date.

An [early HTTP working draft](https://www.w3.org/History/1995/WWW/Paper/http-spec.html)
uses MIME representations and discusses replies and separate annotation stores.
That is a conceptual precedent for discussion attached to media, not a claim
that MIME itself specifies comment threads.

In this repository, `7f0af85c25` (September 2, 2026) introduced Mimechan:
each uploaded file's MIME type selected its board. `0c27619279` (September 15)
made MIME a discussion layer over existing public AC media. The media records
provide the subjects; comments attach to their stable identities.

Each extruded letter floats, changes lighting, and gently transforms on its own
timing; reduced-motion preferences disable these animations. The `.ac` suffix
uses YWFT Processing. The wordmark borrows its proportions and slab serifs from early
workstation typography. [Andrew Messages screenshots from 1994](https://www.cs.cmu.edu/~AUIS/ljdocs/mail/welcome.html)
show bitmap controls, serif correspondence, and embedded images in one mail
reader. This is a new AC mark, not a reproduction of an official MIME logo.
[Borenstein's reconstructed March 1992 demo](https://www.guppylake.com/nsb/mime.html)
provides another reference: a photograph and audio carried within a message.

## Tab and share assets

The browser title cycles between Unicode lettering styles every 1.25 seconds,
following Oskiewar's tab rhythm. It returns to plain `mime.ac` when hidden or
when reduced motion is enabled. The HTML retains a plain title for crawlers.
SVG/PNG favicons, an Apple touch icon, and a 1200×630 JPEG share card live in
`system/public/mime/`; Caddy serves their paths directly on `mime.ac`.
Open Graph and Twitter metadata use absolute public image URLs.

Rebuild the assets from the inline wordmark with
`node system/scripts/build-mime-brand.mjs` (set `CHROME_PATH` on hosts whose
Chromium executable is elsewhere). The build uses local bundled fonts.

## Feed engagement metadata

The feed uses native document scrolling, including gestures over the margins and
header. The sticky header shrinks and centers the logo after scrolling; reduced
motion disables that transition. The account/profile control stays bottom-right,
with bottom padding to keep the last content reachable. Visibility measurements
exclude the sticky header.

The continuous scrolling feed measures post visibility while the document is
visible and the browser window has focus. The post with the largest visible area
is focused; distance to the viewport center breaks ties. DOM attributes expose
`data-focused-post` on the feed and `data-visible-ratio` / `data-focused` on cards.

`POST /api/mime?engagement=1` receives cumulative per-post counters under a random,
in-memory page-visit ID. Mongo's `mime-engagement` collection uses that ID plus
the post code as its key; `$max` makes retries and out-of-order delivery idempotent.
Only existing public roots qualify. The collector sends no account identifiers,
comment contents, URLs, cookies, or persistent device identifiers. Request fields
are allowlisted; batches have at most 24 posts and durations cap at 24 hours per
post/visit. The existing post/reply collections remain unchanged.

Thread responses expose aggregate `metadata.engagement` (version 1):

- `visibleMs`: any part of the card visible.
- `partialMs` / `majorityMs`: below 50% / at least 50% of the card visible.
- `focusedMs`: time as the foreground feed's dominant card.
- `weightedVisibleMs`: duration multiplied by visible fraction.
- `maxVisiblePermille`: greatest fraction visible, from 0 to 1000.
- `impressions`: page visits with at least one second of majority visibility.
- `commentOpens` / `originalOpens`: clicks on the feed's comment/original actions.

Timers sample each second and on scrolling, with a two-second maximum elapsed
sample to exclude suspended-browser gaps. Counters flush every 15 seconds and on
navigation/backgrounding; failures retry while the page remains open. Delivery
on page exit is best effort. These are client-reported exposure estimates, not
measured gaze, unique people, or fraud-resistant metrics. Multiple posts may accrue
visible time simultaneously; only one accrues focused time. Thread reading time is
not included. The feed interleaves paintings, tapes, KidLisp, pieces, and native uploads,
newest first within each kind. These counters do not change ranking.
