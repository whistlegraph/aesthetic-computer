# Score for Aesthetic.Computer & Pals

Working rules for this monorepo, maintained by [@jeffrey](https://prompt.ac/@jeffrey).
Read the relevant subsystem score before changing it. Keep implementation
catalogs, account setup, backlogs, and dated status in the linked references.

## Context Resolution Order

Interpret names and requests in this order unless the user explicitly points elsewhere:

1. **Our seat / local fleet** — machines, displays, prompt rocks, host tooling, and the immediate development environment. Names such as `panda`, `chicken`, `neo`, and `blueberry` mean machines first.
2. **Aesthetic Computer** — this monorepo, its pieces, services, and production system.
3. **Client context** — client infrastructure or projects only when the conversation makes that context clear; resolve their details through `aesthetic-computer-vault`.

Do not interpret a known machine name as an Aesthetic Computer piece until the seat/fleet meaning has been ruled out.

## Public, private, and client boundaries

- This repository holds public source and documentation. Keep client-specific
  instructions, correspondence, infrastructure details, credentials, and private
  data in `aesthetic-computer-vault`.
- Closed-source products live in the vault; commit their edits there. Keep
  shared public runtime code here. See [private products](docs/private-products.md).
- Keep client accounts, connections, credentials, and data separate from
  Aesthetic Computer and from other clients. Access to one does not authorize
  use for another.
- Before sharing reusable work publicly, remove private and client-specific
  material and confirm it is authorized for publication. Do not copy personal
  seat policy into client repositories.

## Editorial Output Score — Wordcrust + Slidecop

These rules apply to authored output across personal and client work.

### Wordcrust

**Wordcrust** is explanatory text accumulating around the thing: marginalia,
duplicated labels, decorative headings, restatements, throat-clearing, and
LLM-sounding description. Remove it before delivery.

- No marginalia by default. Do not add eyebrows, kickers, explanatory
  subheads, side notes, callout prose, or summaries unless they add information
  needed to use, verify, attribute, access, or safely interpret the artifact.
- One visible label per idea. Do not restate a title in a subtitle, body, badge,
  diagram label, or callout.
- Prefer the artifact, evidence, or action over prose about it. If deleting a
  text element changes neither meaning nor action, delete it.
- Preserve necessary facts, source attribution, accessibility text, safety
  constraints, and the user's voice. Concision is not vagueness.

### Slidecop

A slide passes **Slidecop** only when it works at conference-room distance.

- One claim per slide.
- No marginalia.
- Essential type is large, high-contrast, and readable in a 10%-scale preview.
- Evidence dominates the frame. Select and crop screenshots to the exact useful
  state; never shrink a whole interface into an unreadable postcard.
- Inspect the rendered slide or video, not only its source. Fail any slide whose
  claim or evidence requires zooming.

### Design proposal routing

A proposal with multiple directions must make the choice visible before it is
explained.

- Shared foundations are not directions. Name the common entry, then show the
  point where the routes diverge.
- Each direction owns a distinct visual grammar: density, image scale or ratio,
  typography, primary action, and explicit exclusions. If two routes cannot be
  distinguished in silent thumbnail view, collapse or redesign them.
- Put one route map before the options and repeat a miniature on every routed
  page. Highlight the current path; keep inactive paths visible for orientation.
- State each route as a narrow contract: intended use, governing visual rule,
  and what it must never absorb from the other routes.
- Inspect the map and the differences at 10% scale. The active route and the
  visual character of each option must remain legible without reading body copy.

## Working in the repository

- Resolve the active environment through [ENVIRONMENT.md](ENVIRONMENT.md).
  Use the local shell for an ordinary terminal session; Emacs instructions
  apply only in Emacs.
- Keep private session content, messages, files, secrets, and raw tool payloads
  out of analytics by default. New local-tool analytics require an explicit,
  minimized, opt-in event schema. Read the
  [analytics score](toolchain/analytics/SCORE.md) before analytics work.
- Use `prox_send` for peer messages and `prox_receive` for replies;
  `prox_artifact_ready` announces artifacts. Never communicate by typing into
  another agent's terminal. A queued message is not a read acknowledgement.
- Use the guarded `swift`, `ffmpeg`, and `ffprobe` commands on PATH so builds
  and renders respect the host's resource budget. See the
  [macOS score](toolchain/macos/SCORE.md).
- `compushloy` means commit, push, deploy from the intended branch, and verify
  production serves that revision. For lith, use `ac-deploy`, which deploys
  pushed `main`; `ac-deploy --verify` checks the served revision.
- Arena client and server physics must match. Commits on `main` touching the
  arena deployment paths or `shared/` can trigger paired deployment. Read the
  [deployment details](docs/development-reference.md#architecture) first;
  `AC_NO_AUTO_DEPLOY=1` skips that hook.

## Pull requests

Lead with the concrete problem and resulting behavior. Small PRs should be
terse. Include relevant validation and known limits. Recovery work should
identify the earlier PRs it depends on and the safety rules it preserves.
Use a diagram only when it makes the change easier to review.

## Where to work

| Task | Read first |
| --- | --- |
| Use Aesthetic Computer | [User guide](USER-GUIDE.md) |
| Create or publish a piece | [Write a Piece](WRITE-A-PIECE.md), [piece conventions](docs/development-reference.md#adding-a-piece), [API](CLAUDE.md) |
| Runtime, services, logs, chat | [Development reference](docs/development-reference.md), [lith](lith/README.md) |
| KidLisp | [Language source and docs](kidlisp/) |
| Native OS and hardware | [Native score](fedac/native/SCORE.md), [hardware probing](docs/development-reference.md#hardware-probing-on-ac-native) |
| Fleet and host tooling | [macOS score](toolchain/macos/SCORE.md), [Slab](slab/README.md) |
| Papers and print | [Papers score](papers/SCORE.md), [voice](papers/VOICE.md), [quality checks](papers/QUALITY-RUBRIC.md) |
| Analytics | [Analytics score](toolchain/analytics/SCORE.md) |
| Tezos check-ins | [Status checks](tezos/STATUS-CHECKS.md) — HEN dailies, Keeps, wallets, and braincell payments |
| Ant agents | [Mindset and rules](ants/mindset-and-rules.md) |

`/papers` names the scholarly publishing workflow. Consult its score, Platter,
prior papers, and source evidence; default to the archival/arXiv lane unless
the user requests another format. Inspect the rendered output before delivery.

## Development

Run commands from the repository root:

```sh
npm run site                 # local web development
npm test                     # test runner
npm run test:kidlisp:direct   # KidLisp tests without the file watcher
npm run new <slug> "description"
```

[package.json](package.json) is the command source of truth. Run checks suited
to the change; subsystem scores specify additional validation.

The canonical repository is [Tangled](https://tangled.org/aesthetic.computer/core).
GitHub is also mirrored; see [mirror operations](lith/mirror/README.md).
Project introduction and support links live in [README.md](README.md).
