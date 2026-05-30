# KidLisp Directory Structure

This is the navigation map for everything KidLisp-related in the monorepo. For the **runtime registry, spec pointers, and conformance corpus**, see [`SCORE.md`](SCORE.md).

## 🏠 The KidLisp House (this directory)

`kidlisp/` is the *house* — spec, docs, corpus, tooling, registry. It is **not** where the reference evaluator lives (see §"Reference Implementation" below).

```
kidlisp/
├── SCORE.md                      # 🎯 Runtime registry + spec pointers + conformance corpus
├── STRUCTURE.md                  # 🗺️ This file
├── README.md                     # 📖 Language overview & user-facing intro
├── COMPLETE_API_MAP.md           # 📋 All 118 built-ins, grouped
├── CHAOS-MODE.md                 # 🌀 Chaos-mode notes
├── PERFORMANCE-ANALYSIS.md       # ⚡ Perf benchmarks
├── CITATION.cff                  # 📚 Citation metadata
├── docs/
│   ├── core/
│   │   ├── kidlisp-decree.md         # → latest-stable pointer
│   │   ├── kidlisp-decree-26.md      # 📜 Decree '26 (current stable spec)
│   │   └── language-reference.md     # Syntax + constructs
│   ├── features/                     # Per-feature deep dives (embed, fade, suck, transforms…)
│   ├── functions/                    # Per-function references
│   ├── implementation/               # Implementation notes (not normative)
│   ├── integration/                  # External integrations (Feral File, etc.)
│   └── reports/                      # Technical analyses
├── dictionary/                       # Token dictionary work
├── examples/                         # Working KidLisp pieces
├── reports/                          # Architecture / comparison reports
└── tools/                            # Source-fetching + API-summary scripts (Node)
```

## 🏗️ Reference Implementation (lives outside this directory)

The canonical evaluator is **not** under `kidlisp/`. It lives where the web runtime needs it:

| Path | What it is |
|---|---|
| `system/public/aesthetic.computer/lib/kidlisp.mjs` | Reference evaluator (118 built-ins, frozen path — load-bearing for deployment) |
| `system/netlify/functions/store-kidlisp.mjs` | Source storage / `$code` resolution / hit-count API |
| `system/public/aesthetic.computer/disks/*.lisp` | Built-in KidLisp pieces shipped with AC |

Don't move these. The Decree (`docs/core/kidlisp-decree-26.md`) is the abstract spec; `kidlisp.mjs` is the behavioral definition.

## 🛰️ Sibling Implementations (other runtimes in the monorepo)

Sibling directories at the monorepo root, each implementing or supporting KidLisp on a different host:

| Path | Purpose |
|---|---|
| `fedac/native/cl/kidlisp-*.lisp` | Common Lisp port (AC Native OS, runs on bare-metal ThinkPads) |
| `slab/menuband/` | Swift port for Menuband (planned — see SCORE.md) |
| `kidlisp-wasm/` | WASM compiler |
| `kidlisp-playdate/` | Panic Playdate runtime (C) |
| `kidlisp-gameboy/` | Game Boy ROM toolchain (GBDK + asm) |
| `kidlisp-n64/` | Nintendo 64 exploration (bare-metal asm) |
| `kidlisp-cli/` | Public `kidlisp` CLI |
| `kidlisp-sidecar/` | Clojure service |
| `kidlisp-tools/` | Probe + source-tree utilities (separate from `kidlisp/tools/` — see SCORE.md §6) |
| `kidlisp-knowledge/` | LLM-oriented knowledge base aggregator |
| `kidlisp.com/` | Landing page |
| `vscode-extension/kidlisp-syntax.ts` | Editor syntax highlighting |

For status, claimed conformance level, and per-runtime notes: see [`SCORE.md`](SCORE.md) §3.

## 🎯 Quick Navigation

- **What is KidLisp?** → [`README.md`](README.md)
- **Adding a new runtime / checking conformance** → [`SCORE.md`](SCORE.md)
- **Reading the spec** → [`docs/core/kidlisp-decree-26.md`](docs/core/kidlisp-decree-26.md)
- **Function reference** → [`COMPLETE_API_MAP.md`](COMPLETE_API_MAP.md) or [`docs/functions/`](docs/functions/)
- **Working examples** → [`examples/`](examples/)
- **Dev tooling** → [`tools/`](tools/) (this dir) or `kidlisp-tools/` (sibling)
