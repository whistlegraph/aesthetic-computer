# The Last Week of Boots

*2026-07-14 · 2,314 boots, 2026-07-07 → 2026-07-14, localhost excluded*

A read of the `boots` telemetry collection, which has been collecting faithfully
for months and has never once been looked at. Reproduce with:

```
node --env-file=aesthetic-computer-vault/.devcontainer/envs/devcontainer.env \
  toolchain/boots/report.mjs --days 7
```

## Summary

| | count | share |
|---|---|---|
| boots | 2,314 | |
| succeeded | 1,814 | 78.4% |
| errored | 36 | **1.6%** |
| never finished | 464 | **20.1%** |

The headline number looks alarming and mostly is not. The 1.6% error rate is
real and has a single cause. The 20% "never finished" is, for the most part, the
telemetry measuring the wrong thing. Both are fixable, and the second one has to
be fixed *before* any of this can be trusted enough to put on a menu bar.

---

## Finding 1 — "Never finished" was mostly an instrumentation artifact *(FIXED)*

A boot was recorded `success` only when `bootTelemetry.complete()` ran, and
`complete()` had exactly one caller: `hideBootLog()`, the function that dismisses
the boot-canvas overlay. That overlay is dismissed on the `piece-paint-ready`
message, which `disk.mjs` sends only once a piece's *custom* paint takes over
from the default:

```js
if (paint !== defaults.paint && !window.acPieceReady) {
  send({ type: "piece-paint-ready" });
}
```

So the success signal did not mean *the boot worked*. It meant *a piece's custom
paint rendered and hid the overlay*. Two large groups of perfectly healthy boots
never sent it:

- **Default-paint pieces** — the guard's first clause is false, so
  `piece-paint-ready` never fires.
- **Slow-initialising pieces (KidLisp)** — `defaults.paint` runs *first* while
  the evaluator loads, which sets `window.acPieceReady`; by the time the custom
  paint takes over, the guard's second clause is false and the signal is
  suppressed. The comment directly above the gate names this exact case.

Both fell through to an unreliable 500 ms fallback and were recorded, forever, as
`status: "started"`.

The data agreed. Abandonment by piece:

| piece | never finished |
|---|---|
| `/kidlisp` | **73.2%** |
| `/starfield` | 44.2% |
| `/notepat` | 21.9% |
| `/prompt` | 21.5% |
| `/` | 12.4% |
| `/laer-klokken` | **6.9%** |

Users do not behave ten times differently on `/kidlisp` than on `/laer-klokken`.
And the abandoned boots did not die during load — of `/kidlisp`'s 35
never-finished, only 2 died in the first 0.5 s; **25 lived 1–2 s and then went
silent**, i.e. they loaded fully (boot events flushed at 1–2 s) and simply never
had their completion recorded. Everything else points the same way: if this were
real abandonment it would track device and network, and it does not —

- mobile 21.3% vs desktop 19.4% — **flat**
- 4G 26.6% vs 3G 16.7% — **backwards**
- embedded in an iframe **55.7%** vs top-level 17.0% — a different code path, not a different kind of person

**The fix (shipped in this change):** boot success is now decoupled from the
overlay UI. `boot.mjs` exposes an idempotent `window.acBOOT_SUCCESS`, and
`bios.mjs` calls it on `disk-loaded-and-booted` — the moment the disk has loaded
and run its boot lifecycle, which fires for *every* piece regardless of paint,
and sits `outside` disk.mjs's boot try/catch so a piece-level `boot()` exception
still counts the platform boot as successful. `hideBootLog()` keeps calling it as
a safety net; the only way it never fires is a genuine module-import failure —
which is exactly the case that *should* stay uncounted.

**Predicted effect, verifiable in the data one day after deploy:** `/kidlisp`
success climbs from ~22% (10 of 46) toward ~85% as the ~33 boots that reached
`disk-loaded-and-booted` flip from never-finished to success; the overall
never-finished rate collapses from 20% toward the true bounce rate. The residual
— boots that die in the first 500 ms — is the part worth keeping as a real
signal, now that the artifact is gone.

---

## Finding 2 — Two errors, not one: a dead incident and a real retry bug *(FIXED)*

The 7-day snapshot made the errors look like one undifferentiated "module fetch
failed" pile. Widening to 45 days and bucketing by *type over time* splits them
cleanly, and the split is the whole point.

**(a) A transient stale-cache incident that already self-resolved.** The single
most common error over 45 days —
`The requested module './melody-parser.mjs' does not provide an export named
'buildMelodyState'`, 195 hits — is not a network failure. It is a
service-worker cache-coherency break: an old cached `melody-parser.mjs`
(pre-`buildMelodyState`) loaded against a newer importer. It spiked from 06-11 to
06-20, peaking at 47/day, then **dropped to zero on 06-21 and has stayed zero for
3.5 weeks.** The current source is consistent (`melody-parser.mjs` exports
`buildMelodyState`; nothing imports it anymore), and the fix that ended it —
`lib/` going network-first in the service worker (commit 1cef5d4, 06-10) — is
already deployed. This was an incident, not a standing bug. My earlier "one bug,
97% of errors" read caught its tail and over-generalized.

**(b) The real standing bug: the retry never retried.** What remains, steady at
~5/day for the last 3.5 weeks, is the genuine transient — a dynamic `import()`
whose fetch didn't complete, on `bios.mjs` and `lib/parse.mjs`:

| error (browser) | matched retry before? |
|---|---|
| `Failed to fetch dynamically imported module` (Chrome) | yes, but no-op — see below |
| `Importing a module script failed.` (Safari) | **no — never retried** |
| `error loading dynamically imported module` (Firefox) | **no — never retried** |

Both modules were *already* wrapped in `importWithRetry`, so why did they fail?
Two defects in that function, both now fixed:

1. **The browser caches a failed import.** Once `import("./bios.mjs?v=123")`
   rejects, every later `import()` of that exact specifier returns the same
   rejected promise without re-fetching (ES module spec). The retry loop
   re-imported an unchanged URL, so all three "retries" were instant no-ops. Fix:
   append a unique query on every attempt after the first, forcing a real
   re-request. (Simulated: same-URL retry fetches once and never recovers; unique
   -URL retry re-fetches each time and succeeds on the attempt that would.)
2. **Safari and Firefox never qualified for retry at all.** The
   `isNetworkError` gate only matched Chrome's wording, so Safari's "Importing a
   module script failed." and Firefox's "error loading…" — the entire
   `import script failed` bucket — threw on the first try. Fix: match all three
   phrasings. A real `SyntaxError` still fails fast (not retried).

**Effect, verifiable in the data:** the steady-state `bios.mjs` / `lib/parse.mjs`
fetch failures should now mostly recover on retry instead of becoming boot errors,
across all browsers. This does not touch the incident class (a), which is already
handled by network-first + cache versioning.

---

## Finding 3 — Three fields were lying the whole time *(all three FIXED)*

Anything built on these numbers so far was built on sand. All three are corrected
in this change; each was a few lines.

**Browser was wrong for Chrome and Safari — the two biggest.** `parseBrowser()`
had three alternations; the 2nd and 3rd captured only *one* group, but the code
read `m[1]` as the name and `m[2]` as the version. So:

| reported | actually |
|---|---|
| `150 undefined` (376) | Chrome 150 |
| `149 undefined` (363) | Chrome 149 |
| `26 undefined` (175) | Safari 26 |
| `17 undefined` (81) | Safari 17 |

Firefox, Edge and Samsung parsed fine (they sat in the first alternation, which
has two groups). The `m[1] === "Version"` branch could never be true. **Chrome is
the #1 browser by a wide margin and was invisible in every chart.** The parser is
now a `[name, regex]` table where every pattern captures the version in group 1;
8 representative UA strings (including the Chrome-carries-`Safari/` and
Edge/Opera/Samsung-carry-`Chrome/` overlaps) verified.

**Geography was 100% null — and the data was there the whole time.**
`boot-log.mjs` read the `x-nf-*` **Netlify** headers. Production is lith (Caddy +
Express) behind Cloudflare. Those headers do not exist, so every boot recorded
`country: null` and the report's country table was empty. Now it reads
`cf-ipcountry` / `cf-ipcity` / `cf-region` / `cf-connecting-ip`.

The galling part: Cloudflare's `add_visitor_location_headers` Managed Transform
is **already enabled** on the zone (verified against the API), so `cf-ipcountry`,
`cf-ipcity` and `cf-region` have been arriving on every single request this whole
time. Nothing in the repo read them. This was never an edge or a plan problem —
it is a migration leftover, the same species as shipping `netlify-cli` to a
production box that no longer runs Netlify.

**Signed-in was always 0%.** 0 of 2,314. `meta` was built once at module
evaluation and read `window.acUSER`, which is not assigned until much later,
after auth resolves. The object was frozen into a closure and reused for every
event, so `meta.user` was permanently `null`. Now `meta.user` is read at *send*
time. The server side needed a matching fix: `start` writes meta with
`$setOnInsert` and fires before auth resolves, so `boot-log.mjs` now lets any
later phase (`log`/`error`/`complete`) upgrade a null user to a real one without
rewriting the rest of meta.

---

## Finding 4 — `/kidlisp` is a dead-end route

Surfaced while investigating the above. `disks/kidlisp.mjs` — what loads at
`aesthetic.computer/kidlisp` and `prompt.ac/kidlisp` — is a 50-line placeholder
that draws a scrolling gray checkerboard and nothing else. No prompt, no input,
no way to enter code, no link onward. A visitor who types `/kidlisp` expecting to
*write* KidLisp lands on decoration.

The real entry points are elsewhere: **kidlisp.com**, a **`/$code`** short link,
or **inline KidLisp source** typed straight into the prompt. `/kidlisp` as a bare
route matches none of them and teaches nothing about them.

Once Finding 1's fix lands and `/kidlisp`'s success rate is honest, whatever
bounce remains on that path is real, and this is why. Options, roughly in order
of effort: redirect the bare route to kidlisp.com; or make the placeholder an
actual on-ramp (a prompt that accepts code, a one-line "type KidLisp or visit
kidlisp.com"); or leave it and accept it as decoration. A product call, noted
here so the telemetry and the UX are not confused for each other.

---

## What the week actually looked like

Traffic is real and steady — ~300/day, no growth trend, no incident spikes.

| day | boots | errors |
|---|---|---|
| 07-08 | 286 | 3 |
| 07-09 | 368 | 6 |
| 07-10 | 329 | 7 |
| 07-11 | 308 | 4 |
| 07-12 | 323 | 8 |
| 07-13 | 274 | 3 |
| 07-14 | 391 | 5 |

`/laer-klokken` is the quiet story: 593 boots, second only to the root, with the
*lowest* abandonment on the board (6.9%). Someone is using the Danish clock, a
lot, and it works. Top referrer is `jas.life` (91), then Google (49),
`hi.aesthetic.computer` (30), `kidlisp.com` (28). 37% mobile, 8% embedded.

---

## Actionable, in order

Done in this change (needs `fish lith/deploy.fish` — touches `system/public/**`
and `system/netlify/functions/**` — then a day of fresh data to confirm):

- ✅ **Success signal decoupled from the overlay UI** (Finding 1). The
  prerequisite for trusting every rate here. Watch `/kidlisp` climb from ~22% to
  ~85% success.
- ✅ **The three lying fields** (Finding 3): browser parse, Cloudflare geo
  headers, signed-in user (client + server).
- ✅ **The dynamic-import retry** (Finding 2b): `importWithRetry` now busts the
  URL on each attempt and retries across all three browsers. The big error class
  (2a) was already dead before this session.

Still open:

1. **Decide `/kidlisp`** (Finding 4). The only remaining item, and a product
   call, not an engineering one: redirect to kidlisp.com, turn the placeholder
   into an on-ramp, or accept it as decoration.

## On surfacing this in the menu bar

**Not yet — and possibly not ever as a live indicator.**

At a true error rate of 1.6%, roughly 5 errors a day, a menu-bar badge would be
green essentially always. That is the classic dead alert: a light you stop seeing
inside a week, that then fails to fire on the day it matters. And until the
Finding 1 fix lands in production it would be reporting a 20% failure rate that is
mostly not real, which is worse than no indicator at all.

The shape that fits this data is **anomaly, not status**. Nothing on screen while
things are normal; speak only on a change worth acting on — *"boot errors 3×
yesterday"*, *"a piece started failing on Safari"*. Given that this collection
went months without anyone reading it, the honest move is probably not a glyph at
all but a **weekly digest that arrives somewhere already looked at**, via
`/schedule`. A report you read once a week beats an indicator you stop seeing.

Revisit the live-indicator question after Finding 1 lands, when the success rate
means what it says.
