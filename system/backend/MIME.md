# Mime

Discussion on AC media. The frontend is `/mime/` and the `https://mime.ac/`
apex; the API is `/api/mime`. `www.mime.ac` redirects to the apex.
The main feed snaps to one viewport per media item without cropping, following
the tape viewer (`disks/tv.mjs`). It loads 12 posts at a time and preserves the
current item across thread visits and orientation changes. Videos play muted
inline and pause offscreen. A load-more button supports manual loading and
retries. The feed has no category bar, file composer, or footer.
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

- `GET /api/mime` — boards and recent opening posts; `page=0` by default.
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
