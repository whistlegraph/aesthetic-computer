# Turns off the phone

Whistlegraph turns run on servers, not in the app. The phone submits a
request and goes away; the piece is ready when it comes back. This is the
infrastructure for that, written before the first line of it, so the slices
below can be built and judged one at a time.

## Why

A turn today runs inside the app's WebView: the phone calls the model,
streams the code, paints it in its own preview, runs the source checks and
the picture review, repairs once, then commits. That is 40–80 seconds of
foreground time. iOS suspends the WebView the moment the app is
backgrounded, so a turn interrupted by a lock screen, a phone call or a
swipe to another app is lost (it survives only as a journal checkpoint).
Every query must be able to succeed whether or not the phone is watching.

## Shape

```
phone ──POST /api/whistlegraph-turn──▶ lith (API) ──▶ Mongo: walkieware-turns (queue)
  ▲                                                        │ claim / heartbeat / complete
  │ thread socket (ledger revision)                        ▼
  │ push: "your piece is ready"                     turn workers (N containers)
  │                                                   ├─ prompt + model loop (easel-inference)
  └──────────────────────────────────────────────────┤─ source checks (edit-contract)
                                                      ├─ paint check: graph.mjs in a worker thread
                                                      ├─ picture review (visual-review)
                                                      └─ commit → ledger (store.save) + broadcast
```

Four parts, each already partly built:

1. **API on lith.** `POST /api/whistlegraph-turn` enqueues `{code, request,
   drawing, baseVersion, baseHash, model}` for the signed-in owner and answers
   with a job id. `GET ?id=` reads a job; `GET ?code=` lists the thread's open
   jobs. Billing happens here exactly as it does for the phone today: the
   free allowance, then a paid hold sized the way easel-inference sizes it.
   One active turn per thread; a second submit on the same thread queues
   behind it or replaces it (the phone chooses).
2. **Queue in Mongo.** Collection `walkieware-turns`: `{_id, owner, threadID,
   code, request, baseVersion, baseHash, model, status: queued|running|done|
   failed, createdAt, claimedBy, claimedAt, heartbeatAt, finishedAt, result:
   {versionID|draft, notes, error}, receipt}`. A worker claims with an atomic
   `findOneAndUpdate` on `status: queued`, writes a lease (`heartbeatAt`), and
   a claim older than the lease is claimable again. Jobs are idempotent by
   `requestID`: a re-run after a worker death cannot commit the same version
   twice because `store.save` checks `revision`.
3. **Workers.** A stateless Node process that loops: claim → run the turn →
   commit → repeat. It imports the same modules the app's engine imports
   (`aesel/src/edit-contract.mjs` for the checks and the repair loop,
   `attempt-receipt.mjs` for receipts, `visual-review.mjs` for the review,
   `drawing-input.mjs` for the chalk image) so there is one turn, not two.
   Nothing in the worker knows about the phone.
4. **Return path.** The worker commits the version to the thread ledger
   through `mongoWhistlegraphStore.save` and tells lith, which broadcasts the
   new revision on the thread socket (`lith/whistlegraph-socket.mjs` already
   has the rooms). The phone's `WhistlegraphThread` client already merges
   remote revisions on reconnect. If the app is closed, the device registry
   from build 116 sends a push and the piece opens on tap.

## Painting without a browser

The checks need pixels: did the candidate paint, is the picture what was
asked for. The app gets them from the AC runtime in a WebView. The worker
does not need a browser for this. `graph.mjs` is a software rasterizer, and
`kidlisp/conformance/pixel-worker.mjs` already runs it under Node in a
`worker_threads` isolate, one isolate per render, with console warnings
captured as diagnostics. The worker renders a candidate the same way: a
fresh isolate, the piece's `boot`/`paint` driven for a few frames against a
`graph.mjs` buffer at the phone's pixel size, the buffer hashed and encoded
as PNG evidence for the review. A candidate that throws, loops, or never
touches the buffer fails the paint check in under a second, with no Chrome
process anywhere.

What that isolate cannot do: audio, `net`, UI, and the few disk APIs that
reach for `document`. The worker shims those to no-ops the way the
conformance worker does. Pieces that depend on them still paint; what they
cannot do headlessly, they do on the phone when the version arrives.

Built first, 2026-10-10: a pool of warm headless Chrome tabs
(`lith/whistlegraph-render.mjs`) loading
`https://aesthetic.computer/wipe?…preview=walkieware`, the runtime the phone
paints with. A tab comes up in ~2 s; a render is ~3 s including four frames
over 2.2 s; the same tab rasterizes chalk. A throw before the first paint
means no painted event (`unverified-render`); a throw after it arrives as a
warn-level console line with a stack, which the worker reads as a runtime
error, as the phone does. The graph.mjs isolate above is the lighter path and
can replace the pool per piece later without touching callers.

## Scale

What a turn costs a worker:

| Phase | Time | Bound by |
|---|---|---|
| Model stream (Sonnet, 16k output) | 10–40 s | network, idle CPU |
| Source checks | <100 ms | CPU |
| Paint check, isolate | 0.3–1 s | CPU, one core |
| Picture review | 5–15 s | network |
| Repair (at most once) | same again | |
| Commit + broadcast | <100 ms | Mongo |

A turn is mostly waiting. One worker process holds many turns at once; the
CPU-bound part is a second or two per candidate. Budget per worker:

- `TURN_CONCURRENCY` model streams in flight (start at 24).
- `RENDER_CONCURRENCY` renderers at once (Chrome tabs now, isolates later;
  start at 2 per 4 GB); renders queue inside the worker.
- Memory: ~150 MB per warm Chrome tab, ~5 MB per idle turn.

So **100 people iterating at once** is roughly 100 turns in flight, 100 × (2–4
renders of ~1 s) = a few hundred render-seconds per minute. Two workers on a
4-core box clear that with room; a 16-core poorslice clears it alone. The
first worker goes on poorslice (16 GB, node already installed, no sudo
needed). The second goes wherever the first is not.

Workers are interchangeable, so scaling is replicas, not bigger boxes:

- Nothing lives in a worker between jobs. State is Mongo (queue, ledger,
  wallets) and the thread socket on lith.
- A worker that dies mid-turn loses nothing but time: the lease expires,
  another worker claims the job, the hold is still open, the journal
  checkpoint in the job row lets it resume from the last painted candidate.
- Backpressure is explicit: one running turn per thread; a global cap on
  queued jobs per owner (3); queue depth is the autoscale signal.
- Priority is first-come; a user's own second request waits behind their
  first, never behind everyone else's.

## Virtualization

The worker ships as one container image (`lith/whistlegraph-worker.Dockerfile`,
built from the repo root): `node:22-slim` with Chromium, the repo's
`aesel/src`, `apple/whistlegraph/Resources/Web` engine modules, and
`system/public/aesthetic.computer/lib` for the rasterizer. No Chrome in the
default image; a second tag adds chromium for the fallback pool. Config is
environment only: `MONGODB_CONNECTION_STRING`, `MONGODB_NAME`,
`OPENROUTER_API_KEY` (or the lith-side inference route with a worker token),
`TURN_CONCURRENCY`, `RENDER_CONCURRENCY`, `WORKER_NAME`.

Where it runs is a deployment choice, not a code one:

- **poorslice**, a plain `node` process under systemd-user or a `launchd`
  agent, pulling from the knot. First.
- **DigitalOcean droplet(s)** running the image, one per 4 cores. Second,
  when the first is busy more than idle.
- **Jamsocket**, which already hosts the session server from this repo and
  starts a container per session on demand. It fits a per-thread worker
  model (one container per active piece, asleep when idle) if the queue model
  ever feels too coarse. Not first; the queue is simpler and the billing is
  already per request.

The same image runs on a laptop for development against a local Mongo.

## Failure modes, decided now

- **Model 429/5xx**: backoff and retry inside the job up to a deadline; the
  hold stays; the job reports `waiting on the model` to the phone.
- **Reviewer unavailable**: keep the painted candidate, mark it unreviewed
  (already the rule in the app).
- **Repair breaks the picture**: keep the last painted candidate with a note
  (already the rule in the app).
- **Nothing paints**: the job fails with the diagnostics; the last streamed
  code is saved as a draft on the thread, keepable from the phone.
- **Phone offline when done**: ledger has the version; push is queued; the
  thread client merges on the next open.
- **Worker gone**: lease expiry, re-claim, resume from the job's checkpoint.
- **Same request twice**: `requestID` is unique per job; the second is
  rejected at enqueue.

## Phone, after

The app keeps chalk, typing, talk, the preview, Keep/Discard and Try again.
It stops running turns. `ask` becomes: build the request exactly as today
(words, chalk image, sound), submit it, show the row as "working on the knot",
and let the preview follow the ledger. Try again resubmits the same request.
Background, lock, switch apps: nothing changes. The in-flight journal stays
as the fallback until the worker has carried real traffic for a week, then
it goes.

## Slices

1. **Queue + API.** `system/backend/whistlegraph-turns.mjs` (enqueue, claim,
   heartbeat, complete, fail, read, listOpen, reap) with a fake-collection
   test; `system/netlify/functions/whistlegraph-turn.mjs`. Acceptance: a
   job round-trips through curl; a stale lease is reclaimable; a duplicate
   requestID is refused.
2. **Worker, model loop only.** `lith/whistlegraph-worker.mjs` claims jobs,
   runs prompt + model + source checks with the engine's modules, commits to
   the ledger, no pixels yet (every version marked unreviewed). Acceptance:
   a curl-submitted request becomes a version on the thread; the phone shows
   it after reopen.
3. **Paint check in an isolate.** `apple/whistlegraph/worker/render.mjs`
   around `graph.mjs`; PNG evidence; the review wired through it; repair
   once; drafts and notes as in the app. Acceptance: the same request yields
   the same decision on the worker as on the phone for ten saved pieces.
4. **Return path.** Socket broadcast from the worker through lith; push via
   the device registry. Acceptance: version appears on a backgrounded phone
   within two seconds of commit; a closed app gets the push.
5. **Phone switch.** `ask` submits instead of running; "working on the knot"
   row; Try again resubmits. Behind a flag per account, jeffrey first.
6. **Second worker and the container image.** Replicas on a droplet; queue
   depth dashboard on the admin page; the browser fallback pool.

Slices 1–2 are server-only and safe to land any time. Nothing on the phone
changes until slice 5.
