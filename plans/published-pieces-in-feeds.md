# Published pieces never reach the feeds

A decision to make, not a bug to fix. Written 2026-09-16 alongside the KidLisp
projection (`system/backend/kidlisp-projection.mjs`), which fixed the *other*
half of "why don't recent pieces show up on mime".

## What happens today

`publish` — from Easel (`easel/src/publish.mjs`) or the web prompt
(`bios.mjs:21213`) — does three things and no more:

1. `GET /presigned-upload-url/<ext>/piece-<slug>.<ext>/user`
2. `PUT` the source into the user's DO Spaces bucket
3. re-fetch `/media/@handle/piece/<slug>.<ext>` to verify the bytes

`system/netlify/functions/presigned-url.js` never opens a database connection —
no `connect`, no `collection`, anywhere in the file. So a published piece leaves
a bucket object and nothing else.

Everything that lists pieces reads the Mongo `pieces` collection
(`backend/mime-media.mjs`, `MEDIA_KINDS.piece`), and the only writer of that
collection is `/api/store-piece` — which `publish.mjs`'s own header says it
deliberately is not:

> it is not the anonymous `/api/store-piece` short-code path.

The result, live: the entire `pieces` collection is about a dozen anonymous
short-code records, most of them `mcp_test_*` fixtures, newest 2026-08-13. Zero
handle-owned publishes, ever. `publish` has never been visible anywhere but the
piece's own URL.

This is not an Easel bug. Easel mirrors the web prompt exactly, and the gap is
in the path both share.

## Why it isn't a one-line fix

The two halves of the system disagree about what a piece *is*.

|                | `publish`                        | `/api/store-piece`              |
| -------------- | -------------------------------- | ------------------------------- |
| addressed by   | `@handle/slug`, chosen by author | short code, generated           |
| source of truth| the bucket object                | the Mongo row                   |
| renaming       | republish under a new slug       | impossible, code is the identity|
| dedup          | none; republish overwrites       | `hash` unique index             |
| revisions      | overwrite in place               | new code per distinct source    |

`store-piece`'s schema assumes the generated short code *is* the identity: both
`code` and `hash` carry unique indexes, and `slug` for an authenticated caller
is a derived path (`sub/YYYY/MM/DD/code`), not the author's chosen name. A
published piece has no code, has a mutable author-chosen slug, and is expected
to change in place when republished. Writing published pieces into `pieces` as
it stands would either mint codes nobody asked for, or break the uniqueness the
existing rows depend on.

So the real question is not "should publish write a row" but **what a published
piece's stable identity is** — and that answer is what MIME needs, because a
thread key has to outlive a rename.

## Options

### A. Publish writes a `pieces` row keyed by `(user, slug)`

Give `pieces` a second identity shape: authored pieces keyed by owner plus
slug, with `code` absent and the `hash` index made partial so overwrite-in-place
does not collide. Publish becomes a two-step (grant, then record), and the
recorded row is what the feeds list.

- Feeds, search, profile counts and `discussion` URLs all work for the first
  time.
- MIME gets a durable thread key: the row's `_id` survives a republish, so
  comments stay attached across revisions — which is the behavior a
  discussion layer wants.
- Costs: a schema migration on a collection with live unique indexes; publish
  gains a failure mode where the bucket has the bytes but the row is missing
  (needs the same idempotent-upsert treatment the KidLisp projection got);
  `pieces-search` and `piece-metadata` need to handle code-less rows.

### B. Publish routes through `store-piece`

Make publish a `store-piece` caller, so every piece gets a code.

- Much smaller; one identity shape; dedup and revisions already solved.
- But it changes what publishing *means*: `@handle/slug` stops being the
  identity, republishing a tweak mints a new code and a new thread, and the
  author's chosen name becomes decoration. That is a product decision, and
  probably the wrong one for a piece someone iterates on.

### C. Index the bucket instead

Leave publish alone; have a job list the user bucket and project rows the way
`kidlisp-projection.mjs` does for Datomic.

- No change to the write path at all, and it retroactively surfaces every
  piece ever published.
- But it is a poller with no event to hang off, and the bucket carries no
  author metadata beyond the path, so `when` is the object's mtime and a
  deleted piece lingers until the next sweep.

## Recommendation

**A**, with the projection pattern already proven this week: publish writes an
identity row through an idempotent upsert keyed on `(user, slug)`, and a
backfill walks the user buckets once to catch the existing publishes. It is the
only option that keeps `@handle/slug` as the identity — which is the thing that
makes publishing feel different from getting a short code — and the only one
that gives MIME a thread key that survives a republish.

The prerequisite is the schema call: making `hash` a partial index and allowing
code-less rows in `pieces`. That wants @jeffrey's sign-off before any of it is
written, because it is a migration on a live collection.

## Also worth knowing

`renderRunnable` in `system/public/mime/index.html` already addresses a piece by
its bare code and will address `@handle/slug` with no change beyond the prefix
table — the frontend is not a blocker for any of the three options.
