# Mime

Discussion on AC media. The frontend is `/mime/`; the API is `/api/mime`.
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
- `POST /api/mime` — `{ parent, text, name, file }`. A null parent requires a file;
  a reply requires text or a file. Files use `{ name, type, data }` with base64
  bytes and an 8 MiB limit. Replies keep their parent's board regardless of the
  attachment's type. The first 300 replies bump a thread.

Source metadata comes from AC records, not request-supplied URLs or attribution.
Names on manually submitted posts remain unverified, as on the original board.
Program source is displayed as text. Native HTML previews remain sandboxed.
Painting and tape metadata endpoints expose `discussion` URLs for other clients.

Direct entry: `/mime/#/media/painting/abc` (also `tape`, `piece`, or `kidlisp`).
The painting viewer includes a Discuss button. Other media can be discussed from
their MIME board or direct entry URL.

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
replies, mixed attachments, source visibility, storage URLs, legacy uploads,
pagination, and identity across tape conversion.
