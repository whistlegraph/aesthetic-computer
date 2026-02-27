# Feasibility Report: Shared "Try Page" Client Library + Multi-Language Docs Integration

Date: 2026-02-27
Scope: Reuse the `/l5` page pattern as a shared architecture for multiple creative coding languages (including early Processing API experiments)

## Executive Summary

This integration is feasible and aligns with existing AC architecture.

- High feasibility: shared try-page client runtime (iframe runner, editor wiring, language pack contract)
- High feasibility: docs-lane expansion for additional language families
- Medium feasibility: adding new lightweight languages via adapter modules
- Medium/Low feasibility: full Java Processing parity in-browser without a compatibility subset

Recommended direction:
1. Extract a shared try-page client library from `/l5`
2. Standardize a language adapter contract for live reload and docs metadata
3. Move docs language definitions out of monolithic `docs.js` into modular registries
4. Start Processing support as a scoped compatibility subset (not full parity)

## Current Architecture (Observed)

### 1) `/l5` try page already behaves like a proto-template

- `system/public/l5.aesthetic.computer/index.html`
  - Monaco setup + fallback editor
  - example picker + docs hover/completion metadata
  - iframe runner + queued reload flow
  - posts `{ type: "l5-reload", code, ext, liveName }` to AC runtime

### 2) Runtime bridge already supports language-aware live reload

- `system/public/aesthetic.computer/boot.mjs`
  - handles `l5-reload` and forwards to worker as `piece-reload`
- `system/public/aesthetic.computer/lib/disk.mjs`
  - `piece-reload` supports `language`, `ext`, `liveName`
  - `.mjs -> .lua -> .lisp` fetch fallback path already exists
  - detects and compiles Lua via `lib/l5.mjs`

### 3) Language lanes already exist in docs UX

- `system/netlify/functions/docs.js`
  - lane cards for `MJS`, `L5`, `KidLisp`, plus `Prompts`, `Pieces`
  - per-doc preview iframe with run/reset controls
  - L5 lane links to `/l5`

### 4) Routing infra already supports language subdomains and docs routing

- `system/netlify.toml`
  - `/l5`, `l5.aesthetic.computer`, `/docs/*`, `/docs.json` are wired

## Feasibility Assessment

## A) Shared Try Page Client Library

Feasibility: High

Reason:
- `/l5` and `kidlisp.com/pj.html` already share core patterns (iframe boot, readiness queue, postMessage bridge).
- Most variability is data/config, not architecture.

What can be shared immediately:
- iframe runner lifecycle
- message protocol wrappers
- editor bootstrapping + keybindings + debounce
- language metadata pack (examples, hover docs, completion tokens)

## B) Shared Language Docs Module

Feasibility: High

Reason:
- `docs.js` already implements lanes and an implied schema (`sig`, `desc`, `params`, `returns`, `examples`, `done`, `body`).
- Existing plans already call for modularization.

What should change:
- split language entries into per-language modules
- keep one registry output for `/docs` and try-page metadata
- add language-level status/parity metadata for roadmap clarity

## C) New Language Runtimes

Feasibility: Medium (depends on runtime model)

Reason:
- AC loader model is extensible (extension detection + adapter compile path).
- Lua path proves a third runtime can be integrated.

Constraint:
- each language must compile/bridge to AC lifecycle (`boot/paint/sim/act/leave`) and respect restricted API policy.

## D) Early Java Processing API Support

Feasibility: Medium for subset, Low for full parity

Reason:
- Full Java Processing requires JVM-like behavior and broad API surface.
- Browser-native full parity is expensive/heavy.

Pragmatic route:
- implement a Processing-compatible subset first (core draw/input/timing/state)
- explicitly scope unsupported APIs
- ship as "Processing v0 compatibility" not "full Processing"

## Recommended Target Architecture

## 1) Shared try-page client package

Create:
- `system/public/aesthetic.computer/lib/try/frame-runner.mjs`
- `system/public/aesthetic.computer/lib/try/editor-host.mjs`
- `system/public/aesthetic.computer/lib/try/protocol.mjs`
- `system/public/aesthetic.computer/lib/try/language-packs/*.mjs`

Core contract:
- `createTryRunner({ entryUrl, readyTypes, sendReload })`
- `initEditorHost({ languageId, initialCode, onRun, onChange, docsIndex, examples })`
- `languagePack` fields:
  - `id`, `label`, `ext`, `entryPiece`, `examples`, `docsMap`, `messageType`, `liveName`

## 2) Unified message contract

Add generic message:
- `type: "ac:try:reload"`
- payload: `{ source, language, ext, liveName }`

Compatibility:
- keep `l5-reload` and `kidlisp-reload` as legacy aliases

## 3) Docs registry modularization

Move from one monolith to modules:
- `system/netlify/functions/docs/registry/index.mjs`
- `.../registry/languages/l5.mjs`
- `.../registry/languages/kidlisp.mjs`
- `.../registry/languages/processing-v0.mjs`

Then:
- `docs.js` becomes renderer + router only
- try pages consume same language metadata source where possible

## 4) Runtime adapter pattern for languages

Standard adapter interface:
- `module(source) -> { boot, paint, sim, act, leave }`

Register per extension/language in `disk.mjs` loader map instead of ad hoc condition chains.

## Processing-Specific Integration Strategy

## Phase P0 (safe)
- Add docs lane + try page shell without runtime execution (read-only examples + roadmap).

## Phase P1 (v0 runtime)
- Support subset:
  - lifecycle: `setup/draw`
  - canvas: `size/background`
  - style: `fill/stroke/noFill/noStroke/strokeWeight`
  - shapes: `line/rect/ellipse/circle/triangle/quad`
  - loop/input globals: `frameCount`, mouse/key basics
- Use explicit unsupported list in docs.

## Phase P2 (expansion)
- add shape modes, text semantics, transform stack approximation, parity tests.

## Risks

- `docs.js` size/complexity will slow iteration until modularized.
- Monaco duplication across many try pages can hurt load performance.
- Message protocol drift (`l5-reload`, `kidlisp-reload`, future language types) without normalization.
- Claiming Processing parity too early will create support debt.

## Effort Estimate

- Shared try-page lib extraction: 3-5 days
- Docs modularization foundation: 4-7 days
- First non-Lua language lane + try page shell: 2-4 days
- Processing v0 runtime adapter: 1-2 weeks
- Processing parity hardening (beyond v0): multi-sprint

## Recommended Rollout

1. Extract shared try-page primitives from `/l5` (no feature changes).
2. Refactor `/l5` to consume shared primitives (prove no regressions).
3. Modularize docs registry and keep existing `/docs` routes stable.
4. Launch one additional language page using shared stack.
5. Start Processing as explicitly scoped v0 compatibility mode.

## Conclusion

A shared template/client-lib architecture for try pages is practical now and matches existing AC patterns. The blocking item is not architecture; it is scope management per language runtime. Processing support should be positioned as incremental compatibility, not full Java parity in v1.
