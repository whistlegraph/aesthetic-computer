# kidlisp → Datomic migration punchlist

Generated from a grep across the repo for `.collection('kidlisp')`. Each
entry is one place that reads or writes the Mongo `kidlisp` collection
and needs a decision: migrate to sidecar, keep on Mongo read-only, or
retire.

## ✅ Migrated in v1

- [`system/netlify/functions/store-kidlisp.mjs`](../../system/netlify/functions/store-kidlisp.mjs) → routes to
  [`store-kidlisp-datomic.mjs`](../../system/netlify/functions/store-kidlisp-datomic.mjs) behind `KIDLISP_DATOMIC=on`.
- [`system/backend/backfill-kidlisp-to-datomic.mjs`](../../system/backend/backfill-kidlisp-to-datomic.mjs) — one-shot
  Mongo → Datomic replay. Idempotent.
- [`silo/server.mjs`](../server.mjs) — `/api/datomic/*` proxy to the sidecar's admin
  surface. Silo dashboard still browses Mongo `kidlisp` for historical
  comparison during validation.

## 🟡 Production hot path — migrate before flipping `KIDLISP_DATOMIC=on` everywhere

These functions read/write the kidlisp collection directly. Each needs
to be rewritten to call the sidecar client in
`system/backend/kidlisp-sidecar.mjs`.

- [ ] `system/netlify/functions/kidlisp-list.mjs` — list endpoint.
- [ ] `system/netlify/functions/kidlisp-count.mjs` — count endpoint.
- [ ] `system/netlify/functions/kidlisp-keep.mjs` — keep write path.
- [ ] `system/netlify/functions/keep-prepare.mjs` — pre-mint prep writes.
- [ ] `system/netlify/functions/keep-prepare-background.mjs` — background
  writer.
- [ ] `system/netlify/functions/keep-confirm.mjs` — mint confirmation
  write (sets `kept` field).
- [ ] `system/netlify/functions/keep-mint.mjs` — mint execution write.
- [ ] `system/netlify/functions/keep-update.mjs` — rebake path.
- [ ] `system/netlify/functions/keep-update-confirm.mjs` — rebake
  confirmation write.
- [ ] `system/netlify/functions/tv.mjs` — reads for TV feed.
- [ ] `system/netlify/functions/test-tv-hits.mjs` — test endpoint reads.
- [ ] `system/netlify/functions/metrics.mjs` — analytics reads.
- [ ] `system/netlify/functions/atproto-user-stats.mjs` — per-user reads.
- [ ] `system/netlify/functions/index.mjs` — verify kidlisp reference.

## 🟠 Other services (non-Netlify, need own sidecar client)

- [ ] `oven/grabber.mjs` — screenshot/OG image gen reads kidlisp source.
      Oven has its own Mongo connection; give it a sidecar client too.
- [ ] `oven/kidlisp-mini/bundle.mjs` — bundle pipeline reads + writes
      `ipfsMedia`. Biggest side-effect surface.

## 🟢 Tezos tooling (operator-run CLIs; low urgency)

- [ ] `tezos/keep-cli.mjs`
- [ ] `tezos/ac-keeps.mjs`
- [ ] `tezos/find-my-kidlisp.mjs`

## 🟢 Feed worker (Cloudflare; runs off its own schedule)

- [ ] `feed/silo-update-top100.mjs`
- [ ] `feed/create-top-kidlisp-playlist.mjs`

## 🔵 Scripts / utilities / one-offs — likely retire after cutover

- `scripts/kidlisp-db-stats.mjs` — ad-hoc stats. Can be replaced by
  `GET /admin/stats` + `/api/datomic/stats`.
- `scripts/atproto/backfill-kidlisp-rkeys.mjs` — one-shot backfill,
  already ran. Keep for reference, no migration needed.
- `scripts/atproto/check-fifi-kidlisp.mjs` — ad-hoc debug.
- `scripts/atproto/check-kidlisp-atproto.mjs` — ad-hoc debug.
- `scripts/atproto/sync-fifi-kidlisp.mjs` — ad-hoc sync.
- `scripts/atproto/sync-jeffrey-kidlisp.mjs` — ad-hoc sync.
- `at/scripts/atproto/backfill-kidlisp-to-atproto.mjs` — one-shot.
- `utilities/keep-cleanup.mjs` — cleanup utility; rewrite against
  sidecar if still needed.

## Cutover sequence (when ready)

1. Bring Datomic + sidecar up on silo (see [README.md](README.md)).
2. Populate vault entries (already done in `aesthetic-computer-vault/kidlisp-datomic/`).
3. Run backfill from silo: `node system/backend/backfill-kidlisp-to-datomic.mjs`.
4. Verify counts match: `/api/datomic/stats` vs Mongo `kidlisp.countDocuments()`.
5. Set `KIDLISP_DATOMIC=on` in lith's `.env` and redeploy. `store-kidlisp.mjs`
   is now Datomic-authoritative for create/read/update-hit/mint/etc.
6. Work through the 🟡 hot-path list above. Each file becomes a small PR.
7. When 🟡 list is empty, Mongo `kidlisp` collection is frozen (read-only
   for the migrated paths; unused by new code).
8. After stable operation, work the 🟠 + 🟢 lists.
9. 🔵 list can be retired anytime — confirm no scheduled jobs invoke them.
