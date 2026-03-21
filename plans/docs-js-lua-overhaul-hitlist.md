# Docs Overhaul Hitlist: MJS vs L5 Split + Function-Level Examples

Date: 2026-02-26
Owner: AC docs project (long-running)
Primary source today: `system/netlify/functions/docs.js`

## Mission
Ship a docs system that:
- visually separates JavaScript piece API (`.mjs`) from Lua/L5 API,
- hooks directly into KidLisp docs data from `learn.kidlisp.com`,
- replaces stub-heavy function docs with real coverage,
- redesigns individual function pages,
- provides a runnable iframe example for each function entry,
- uses one shared source of truth for all platform docs surfaces.

## Baseline (Current State)
- Docs are currently assembled inline in one file: `system/netlify/functions/docs.js`.
- Landing navigation mixes many categories and only lightly distinguishes L5.
- KidLisp docs already exist on `learn.kidlisp.com` and should be treated as a first-class upstream source.
- Function pages use a simple template (`sig`, `desc`, optional `body`) with many placeholders.
- Historical baseline marker when this hitlist started:
  - `done: false` -> 527
  - `done: true` -> 63
- Current snapshot from `/docs.json` (2026-02-26):
  - API entries: `310 total` / `307 done` / `3 in-progress` / `0 planned`
  - L5 entries: `50 total` / `47 done` / `3 in-progress`
  - Prompt entries: `129 total` / `32 done` / `97 planned`
  - Piece entries: `186 total` / `23 done` / `163 planned`
- L5 section exists but is still checklist-forward and not yet parity-complete.

## Progress Update (2026-02-26)
- `/docs` landing now has three explicit lanes:
  - `MJS / AC Piece API`
  - `L5 / Lua API`
  - `KidLisp / Language API`
- `/docs` lane cards now have family-specific color treatment for faster visual separation.
- New route entries shipped:
  - `/docs/mjs:overview`
  - `/docs/kidlisp:overview`
  - `/docs/kidlisp:core`
- Individual docs page template now includes:
  - family/category/status breadcrumb,
  - signature + summary,
  - parameters table,
  - returns section,
  - examples section,
  - runtime notes,
  - detail body,
  - embedded live iframe preview with `Run` / `Reset` controls and open links.
- Core graphics docs filled with signatures/params/examples and preview metadata:
  - `line`, `point`, `box`, `wipe`, `ink`, `circle`,
  - `paste`, `stamp`, `pixel`, `plot`, `flood`,
  - `oval`, `poly`, `shape`, `resolution`, `write`.
- Structure docs pass completed:
  - `boot`, `paint`, `act`, `sim`, `beat`, `leave`, `meta`, `preview`, `icon`,
  - `brush`, `filter`, `curtain`, `background`, `api`, `DEBUG`.
- Sound docs pass completed:
  - `sound.time`, `sound.bpm`, `sound.freq`, `sound.microphone`, `sound.speaker`,
  - `sound.play`, `sound.synth`, `sound.bubble`, `sound.kill`.
- L5 function-level docs expanded from checklist-only to 50 entries, including core graphics, state globals, loop controls, and math helpers.
- Interaction docs pass completed:
  - `pen`, `pens`, `pen3d`, `event`.
- Network docs pass completed:
  - `net.signup`, `net.login`, `net.logout`, `net.parse`, `net.userRequest`,
  - `net.udp`, `net.socket`, `jump`, `load`, `preload`, `rewrite`, and related route/session fields.
- System docs pass started and expanded:
  - `reload`, `store.*`, `signal`, `sideload`, `meta`, `upload`, `encode`, `authorize`,
  - `hud.*`, `bgm.*`, `darkMode`, `gpu.*`, and additional runtime state fields.
- Number docs pass completed:
  - `num.*` math/color helpers, `num.p2.*`, `geo.*`, timing helpers (`delay`, `blink`),
  - and glMatrix bridge surfaces (`vec2/vec3/vec4/mat3/mat4/quat`).
- Help docs pass completed:
  - `choose`, `flip`, `repeat`, `every`, `any`, `anyIndex`, `anyKey`, `each`, `shuffleInPlace`,
  - `gizmo.Hourglass`, `gizmo.EllipsisTicker`.
- Prompt + piece docs now auto-fill missing descriptions from shared command registry:
  - source: `system/public/aesthetic.computer/lib/prompt-commands.mjs`
  - remaining empty prompt descriptions: `0`
  - remaining empty piece descriptions: `0`
- Doc iframe preview routing improved:
  - `/docs/pieces:*` now embeds `https://aesthetic.computer/<piece>`
  - `/docs/prompts:*` now embeds `https://aesthetic.computer/prompt~<command>`

## Success Criteria (Program-Level)
- Landing docs page has explicit top-level visual split:
  - `MJS / AC Piece API`
  - `L5 / Lua API`
  - `KidLisp / Language API`
- Every function page has:
  - real signature,
  - concise behavior description,
  - params/returns notes,
  - one runnable iframe example.
- Stub ratio drops from current baseline to agreed milestones.
- Docs data model supports future automation instead of manual string-heavy assembly.
- A single docs registry feeds:
  - `/docs` (AC platform docs),
  - `/l5` checklist and L5 function docs,
  - `learn.kidlisp.com` language reference surfaces.

## Hitlist

### P0: Information Architecture + Visual Separation
- [x] Create first-class sections on landing page for `MJS API` and `L5 API` with distinct panels.
- [x] Add a third first-class panel for `KidLisp API` linked to integrated entries.
- [x] Add clear visual language split (labels, badges, section headers, status counters).
- [x] Keep existing route compatibility (`/docs/<category>:<word>`), add explicit split entry links.
- [x] Add route-level quick links:
  - `/docs/mjs:overview` (new)
  - `/docs/l5:overview` (existing)
  - `/docs/kidlisp:overview` (new)

Acceptance:
- A user landing on `/docs` can immediately tell which API family they are browsing.
- L5 docs are not visually mixed into general MJS links.
- KidLisp docs are visible as a peer API family, not a side property.

### P0: Redesign Individual Function Page Template
- [x] Replace current minimal doc page with a structured template containing fixed sections:
  - signature,
  - summary,
  - parameters,
  - returns,
  - side effects,
  - runtime notes,
  - related functions,
  - runnable example iframe.
- [x] Add consistent status metadata rendering (`done`, `in-progress`, `planned`).
- [x] Add a slim breadcrumb line showing API family and category.

Acceptance:
- Any function page reads as a proper reference page, not a stub card.

### P0: Iframe Example System (Per Function)
- [x] Define a single example embed contract in docs metadata:
  - `example.type` (`mjs` | `l5`)
  - `example.code`
  - `example.entry` (piece or loader route)
  - `example.height`
- [x] Build one shared docs iframe renderer component/template.
- [ ] For MJS examples, run isolated AC preview route with provided snippet.
- [ ] For L5 examples, run through Lua pathway (`l5-reload` compatible runner).
- [x] Add `Run` and `Reset` controls per function example.

Acceptance:
- Every non-deprecated function entry can show one runnable example in-page.

### P1: Docs Data Model Refactor
- [ ] Move docs definitions out of one large inline object into modular source files:
  - `docs/api-mjs.*`
  - `docs/api-l5.*`
  - `docs/api-kidlisp.*` (or adapter-fed from learn.kidlisp source)
  - `docs/prompts.*`
  - `docs/pieces.*`
- [ ] Define a normalized schema per entry:
  - `id`, `family`, `category`, `sig`, `desc`, `params`, `returns`, `notes`, `example`, `status`.
- [ ] Keep `docs.json` output backward compatible during migration.
- [ ] Add source adapters so one registry can ingest:
  - AC-native docs modules,
  - KidLisp learn docs data,
  - L5 docs/checklist data.

Acceptance:
- Docs authoring no longer requires editing one monolithic file.
- Schema supports rendering richer pages without ad-hoc HTML bodies.
- Cross-site docs content stays synchronized from one registry build step.

### P1: Fill Missing MJS Docs (Highest-Use First)
- [ ] Build a prioritized function coverage queue by runtime usage frequency and beginner value.
- [x] Fill `Graphics` core first: `line`, `point`, `box`, `wipe`, `ink`, `circle`, `oval`, `poly`, `shape`.
- [x] Fill `Interaction`, `System`, and `Network` essentials next.
- [x] Replace empty signatures/descriptions with concrete behavior and examples (API lanes).

Acceptance milestones:
- Milestone A: 100 high-traffic MJS functions complete.
- Milestone B: 200 functions complete.
- Milestone C: remaining stubs triaged as `planned` or `deprecated` with explicit rationale.

### P1: L5 Documentation Maturity
- [x] Expand L5 function-level entries beyond checklist pages.
- [ ] Add explicit parity tables against AC implementations for each L5 API area.
- [ ] Mark unsupported APIs with alternatives and roadmap status.

Acceptance:
- L5 docs become function-reference usable, not only rollout-status docs.

### P1: KidLisp Docs Hook + Convergence
- [ ] Inventory `learn.kidlisp.com` docs identifiers and map them to unified schema fields.
- [ ] Build a KidLisp docs adapter (import/transform) into unified docs registry.
- [ ] Support canonical source attribution per entry (e.g. `source: kidlisp-learn`).
- [ ] Add cross-links between platform functions and KidLisp equivalents where relevant.
- [ ] Preserve `learn.kidlisp.com` UX while serving from shared docs data.

Acceptance:
- KidLisp docs content can be rendered from the same registry as MJS/L5 without duplication.
- Updating a shared entry propagates consistently across `/docs` and learn surfaces.

### P2: Quality Gates + Tooling
- [ ] Add docs lint script that fails on:
  - empty `sig` or `desc` for `done` entries,
  - missing example for non-deprecated functions,
  - broken internal links.
- [ ] Add schema validation in CI for docs source files.
- [ ] Add simple coverage report output (`done`, `in-progress`, `planned`, `missing-example`).

Acceptance:
- Regressions in docs quality are caught before deploy.

### P2: Navigation + Discoverability Improvements
- [ ] Add search/filter by family/category/status.
- [ ] Add “See also” graph links across related functions.
- [ ] Add stable per-function anchors and copy-link controls.

Acceptance:
- Users can discover related API quickly without scanning long lists.

## Execution Order (Recommended)
1. IA split and page template redesign.
2. Single-source-of-truth registry design + KidLisp adapter contract.
3. Example iframe system.
4. Data model modularization.
5. MJS fill pass (top 100).
6. L5 fill pass.
7. KidLisp convergence pass.
8. Tooling, lint, and search improvements.

## Tracking Cadence
- Weekly checkpoint in this file:
  - completed items,
  - functions documented count,
  - blockers,
  - next 10 functions queued.

## First Sprint Slice (Concrete)
- [x] Implement visual split on `/docs` landing (`MJS` vs `L5`).
- [x] Extend split to include `KidLisp` as third top-level lane.
- [x] Add stronger family-level visual style separation on lane cards.
- [x] Ship new function-page template with structured sections.
- [x] Ship iframe example support for at least 12 core graphics functions.
- [x] Fill complete docs for those 12 functions (MJS).
- [x] Expand L5 docs from checklist-first to function-level references.
- [x] Remove empty prompt/piece descriptions using shared registry + generated fallback text.
- [ ] Draft and validate unified schema + KidLisp adapter interface in code.

## Risks
- Tight coupling of docs rendering and data in a single function file can slow iteration.
- Example iframes may increase payload and complexity if not lazily loaded.
- Maintaining parity between runtime behavior and docs requires metadata discipline and linting.

## Notes
- This hitlist intentionally scopes the full docs overhaul as a program, not a single PR.
- Existing L5 checklist and `/l5` playground should remain source-of-truth references while function-level docs are expanded.
- `learn.kidlisp.com` should be integrated as a data source, not rewritten as an isolated docs island.
