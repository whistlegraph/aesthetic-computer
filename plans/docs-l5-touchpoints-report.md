# Docs + L5 Touchpoints Report

Updated: 2026-02-26  
Scope window: `bddb38067` (L5 planning) -> `f15c1c3ce` (latest docs pass)

## Executive Summary

- L5 runtime + docs + subdomain surface shipped and iterated in production code paths.
- `/docs` is now split into explicit lanes (`MJS`, `L5`, `KidLisp`) plus added `Prompts` and `Pieces` lanes.
- Function-level docs now have structured pages with embedded iframe preview/run/reset controls.
- API docs coverage is near complete; prompt and piece docs are partially complete and now easier to browse.

## Touchpoints Matrix

### 1) L5 runtime and execution bridge

- `system/public/aesthetic.computer/lib/l5.mjs`
- `system/public/aesthetic.computer/boot.mjs`
- `system/public/aesthetic.computer/lib/disk.mjs`
- `system/public/aesthetic.computer/disks/l5-hello.lua`
- `system/public/aesthetic.computer/dep/wasmoon/*`

Key commits:
- `da230c6ab` feat(l5): add docs checklist, try page, and l5 routing
- `577cf8930` fix(l5): ship lua runtime assets and yellow/black/white l5 page
- `f544d02ee` fix: ... L5/boot updates

Status:
- L5 runtime path is wired and shippable.
- Lua assets are vendored and loaded from AC public runtime.

### 2) L5 web surface (`/l5` and subdomain)

- `system/public/l5.aesthetic.computer/index.html`
- `system/public/l5.aesthetic.computer/l5-logo-blob.png`
- `system/public/l5.aesthetic.computer/studies/l5lua-style-study.md`
- `studies/l5lua-org-style-study.md`

Key commits:
- `da230c6ab`, `577cf8930`, `36f8925ce`, `3ed77f5d9`

Status:
- Yellow/black/white visual direction implemented.
- Absolute `https://aesthetic.computer` URL strategy applied for subdomain reliability.

### 3) Routing/deploy touchpoints

- `system/netlify.toml`

Key commits:
- `da230c6ab` (L5 routing additions)
- `f544d02ee` (follow-up updates)

Status:
- L5 route handling added/updated in deploy config.

### 4) Docs backend and docs UX overhaul

- `system/netlify/functions/docs.js`
- `plans/docs-js-lua-overhaul-hitlist.md`

Key commits:
- `198111374` (hitlist baseline)
- `38a63ae37` (lane split + structured pages)
- `5d3357fde`, `889b17196`, `f70de5b98` (major API coverage passes)
- `66188a069` (lane visual separation polish)
- `ce318aa3b` (shared description source for prompts/pieces)
- `dc22a7816` (direct preview routes for prompt/piece docs)
- `220d2c085` (prompt/piece lanes + browse index routes)
- `f15c1c3ce` (top prompt command docs completion pass)
- `53ec884ab` (hitlist refresh with current metrics)

Status:
- Docs now support structured function pages + live preview embedding.
- API lanes are complete except for 3 in-progress L5 status pages.

### 5) Prompt command integration touchpoints

- `system/public/aesthetic.computer/disks/prompt.mjs`
- `system/netlify/functions/docs.js` (prompt docs registry usage)

Key commits:
- `da230c6ab`
- `3919eb26f`
- `ce318aa3b`
- `f15c1c3ce`

Status:
- Prompt docs now consume shared descriptions and have better command-level coverage.

### 6) External/manual infra touchpoints (not git-tracked)

- Cloudflare DNS/domain entries for `l5.aesthetic.computer`
- `prompt.ac` zone redirect change
- Netlify dashboard domain wiring

Status:
- Manual operations by operator; not represented directly in git diff.
- Recommend dashboard-level verification as part of release checklist.

## Current Coverage Snapshot (`/docs.json`)

Captured: 2026-02-26

- API total: `310` (`307 done`, `3 in-progress`, `0 planned`)
- MJS lane total: `264` (`264 done`, `0 in-progress`, `0 planned`)
- L5 lane total: `50` (`47 done`, `3 in-progress`, `0 planned`)
- KidLisp lane total: `2` (`2 done`)
- Prompts total: `129` (`58 done`, `71 planned`)
- Pieces total: `186` (`23 done`, `163 planned`)

## Recent Commit Timeline (selected, in order)

1. `da230c6ab` feat(l5): add docs checklist, try page, and l5 routing
2. `577cf8930` fix(l5): ship lua runtime assets and yellow/black/white l5 page
3. `f544d02ee` fix: ... L5/boot updates
4. `3ed77f5d9` docs(l5): add l5lua style study
5. `36f8925ce` fix(l5): use absolute aesthetic.computer urls on l5 page
6. `198111374` plans(docs): add unified mjs+l5+kidlisp overhaul hitlist
7. `38a63ae37` feat(docs): split docs lanes and add structured API pages
8. `5d3357fde` docs: expand mjs api coverage and add preview run/reset
9. `889b17196` docs: complete number/help api coverage and refresh hitlist
10. `f70de5b98` docs: complete structure/sound docs and expand l5 function api
11. `66188a069` docs: color-code api lanes for mjs, l5, and kidlisp
12. `ce318aa3b` docs: auto-fill prompt and piece descriptions from shared registry
13. `dc22a7816` docs: use direct piece and prompt preview routes in doc iframes
14. `53ec884ab` plans: refresh docs overhaul hitlist with current coverage and shipped work
15. `220d2c085` docs: add prompt/piece lanes and browse index routes
16. `f15c1c3ce` docs: complete top prompt command docs with examples and status

## Remaining Open Touchpoints

- L5 in-progress docs status pages still to finalize:
  - `l5:lifecycle`
  - `l5:graphics`
  - `l5:input`
- Prompt docs backlog: `38` visible commands still planned.
- Piece docs backlog: `163` entries still planned.
- Unified cross-site source adapter work (AC docs + learn.kidlisp.com) still pending.

## Verification Commands Used For This Report

- `git log --oneline -n 20`
- `git log --name-only --pretty=... --since=2026-02-25`
- `node` checks against `/docs.json` via `system/netlify/functions/docs.js` handler
