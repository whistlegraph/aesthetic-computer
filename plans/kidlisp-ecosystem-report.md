# KidLisp Ecosystem Report

**Generated:** November 25, 2025  
**Repository:** aesthetic-computer (whistlegraph/aesthetic-computer)

---

## Executive Summary

KidLisp is a minimal Lisp dialect designed for creating generative art and interactive experiences within the Aesthetic Computer platform. This report provides a comprehensive overview of the KidLisp ecosystem, including its architecture, tooling, user content metrics, and related projects.

### Key Highlights

- **Core Interpreter:** 13,458 lines of JavaScript (`kidlisp.mjs`)
- **API Surface:** 118+ built-in functions across 12 categories
- **User Content:** 10,543 stored KidLisp programs in MongoDB
- **Registered Users:** 8,555 attributed programs, 1,988 anonymous
- **Most Popular Program:** `$bop` with 4,647 views
- **Extended Platforms:** GameBoy ROMs, N64, Tezos blockchain integration

---

## 1. Repository Structure

KidLisp spans multiple directories across the aesthetic-computer repository:

### Core Implementation
| Path | Purpose |
|------|---------|
| `system/public/aesthetic.computer/lib/kidlisp.mjs` | Main evaluator (13,458 lines) |
| `system/public/aesthetic.computer/lib/kidlisp-keep.mjs` | Keep/persistence module |
| `system/netlify/functions/store-kidlisp.mjs` | Backend storage API (541 lines) |
| `system/netlify/functions/tv.mjs` | TV/Feed data API |
| `system/backend/media-atproto.mjs` | ATProto sync for KidLisp |

### Documentation & Knowledge
| Path | Purpose |
|------|---------|
| `kidlisp/` | Main documentation hub |
| `kidlisp/COMPLETE_API_MAP.md` | Full API reference (262 lines) |
| `kidlisp/STRUCTURE.md` | Directory navigation guide |
| `kidlisp/docs/` | Detailed documentation (core, features, implementation) |
| `kidlisp/reports/` | Technical analysis reports |
| `kidlisp/tools/` | Development utilities (`api-summary.mjs`, `source-tree.mjs`) |
| `kidlisp-knowledge/` | LLM training knowledge base |

### Web Presence
| Path | Purpose |
|------|---------|
| `kidlisp.com/` | Landing page source |
| `system/public/kidlisp.com/index.html` | Deployed website (4,159 lines) |

### Extended Platforms
| Path | Purpose |
|------|---------|
| `kidlisp-gameboy/` | GameBoy ROM compiler |
| `kidlisp-n64/` | N64 bare-metal experiments |
| `kidlisp-tools/` | CLI source analysis tools |

---

## 2. Architecture

### 2.1 Core Evaluator (`kidlisp.mjs`)

The KidLisp interpreter is a tree-walking evaluator implemented in JavaScript:

```
┌─────────────────────────────────────────────────────────────┐
│                     KidLisp Runtime                         │
├─────────────────────────────────────────────────────────────┤
│  Parser                                                     │
│  └── Tokenizer → S-Expression AST → Tree Walker            │
├─────────────────────────────────────────────────────────────┤
│  Global Environment                                         │
│  └── 118+ built-in functions                               │
│  └── Variable bindings (def)                               │
│  └── User-defined functions (later)                        │
├─────────────────────────────────────────────────────────────┤
│  Graphics Pipeline                                          │
│  └── 2D primitives (box, circle, line, tri)                │
│  └── Transformations (zoom, scroll, spin, blur, suck)      │
│  └── 3D rendering (cube, form, trans)                      │
├─────────────────────────────────────────────────────────────┤
│  Audio System                                               │
│  └── mic, amplitude, melody, overtone                      │
├─────────────────────────────────────────────────────────────┤
│  Embedding System                                           │
│  └── ($code) → Load/execute cached pieces                  │
│  └── Multi-layer composition                               │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 API Categories (118+ Functions)

| Category | Count | Examples |
|----------|-------|----------|
| Language & Control | 7 | `def`, `later`, `if`, `once`, `not`, `now`, `die` |
| Math & Numbers | 14 | `+`, `-`, `*`, `/`, `sin`, `cos`, `random`, `wiggle` |
| Graphics - Basic | 10 | `wipe`, `ink`, `line`, `box`, `circle`, `tri`, `plot` |
| Images & Media | 7 | `paste`, `stamp`, `painting`, `write`, `len` |
| Transformations | 11 | `scroll`, `zoom`, `suck`, `spin`, `blur`, `contrast` |
| Audio & Sound | 6 | `mic`, `amplitude`, `melody`, `overtone` |
| 3D Graphics | 8 | `cube`, `form`, `trans`, `cubespin`, `cubepos` |
| Camera Control | 8 | `camrot`, `camspin`, `camrotx/y/z` |
| System Properties | 9 | `width`, `height`, `frame`, `clock`, `fps`, `resolution` |
| Colors & Effects | 19+ | All CSS colors + `rainbow`, `zebra`, `fade`, `coat` |
| Utility | 12+ | `tap`, `draw`, `hop`, `choose`, `repeat`, `bunch` |
| Advanced | 5 | `embed`, `bake`, `jump`, `mask`, `unmask` |

### 2.3 Backend Storage

**Netlify Function:** `store-kidlisp.mjs`

Features:
- SHA-256 content hashing for deduplication
- Intelligent short code generation (3-letter codes like `$bop`, `$cow`)
- User attribution via Auth0 JWT
- Hit counting and access tracking
- ATProto sync for decentralized storage
- Optional Tezos blockchain minting (feature-flagged)

**MongoDB Indexes:**
- `code` (unique) - Short code lookup
- `hash` (unique) - Content deduplication  
- `when` - Chronological queries
- `user` (sparse) - User-specific queries

---

## 3. MongoDB Content Analysis

**Database:** `aesthetic` (MongoDB Atlas)  
**Collection:** `kidlisp`

### 3.1 Overall Statistics

| Metric | Value |
|--------|-------|
| Total Documents | 10,543 |
| With User Attribution | 8,555 (81.1%) |
| Anonymous | 1,988 (18.9%) |
| Average Hits | 4.75 |
| Maximum Hits | 4,647 |

### 3.2 Top KidLisp Programs by Hits

| Code | Hits | Source Preview |
|------|------|----------------|
| `$bop` | 4,647 | `purple, ink, line, blur 5` |
| `$pie` | 2,785 | `(fps 24) (0.25s (wipe (... red yellow blue))) (ink...` |
| `$cow` | 1,221 | `($39i 0 0 w h 128) ($r2f 0 0 w h 128) (contrast 1...` |
| `$roz` | 1,062 | `fade:red-blue-black-blue-red ink (? rainbow white...` |
| `$39i` | 813 | `black (0.1s (ink (? black white) 32) (circle ? ? 3...` |

### 3.3 Recent Activity (Sample)

| Code | Hits | User | Pattern |
|------|------|------|---------|
| `$sqq` | 8 | anon | Line scrolling with rainbow |
| `$8p9` | 1 | anon | Gradient fades with spin |
| `$hhn` | 1 | anon | Red ink, line, blur |
| `$ad6` | 2 | anon | Lightsteelblue-pink gradient |

### 3.4 Related Collections

| Collection | Documents | Purpose |
|------------|-----------|---------|
| `paintings` | 4,167 | User-created paintings |
| `@handles` | 2,676 | User handle mappings |
| `moods` | 2,805 | User mood messages |
| `tapes` | - | Video recordings |

---

## 4. Web Platform: kidlisp.com

**Domain:** kidlisp.com (redirects to aesthetic.computer/kidlisp.com/)

### 4.1 Features

- **Responsive Design:** Light/dark mode with CSS variables
- **Multi-language Support:** English, Spanish, Chinese, Danish
- **Live Editor:** Real-time syntax highlighting and execution
- **Touch-friendly:** Mobile wizard mode for easy code building
- **Connectivity Indicator:** Shows connection status to AC backend

### 4.2 Architecture

```
kidlisp.com Domain
     │
     └──→ Netlify Redirect ──→ aesthetic.computer/kidlisp.com/
                                    │
                                    └──→ index.html (4,159 lines)
                                           │
                                           ├── Inline CSS (themes, layout)
                                           ├── Live Editor Component
                                           ├── Language Switching (i18n)
                                           └── Imports kidlisp.mjs for tokenization
```

### 4.3 Netlify Configuration

```toml
# Redirects in system/netlify.toml
[[redirects]]
from = "https://kidlisp.com/*"
to = "https://aesthetic.computer/kidlisp.com/:splat"

# SPA fallback
[[redirects]]  
from = "/kidlisp.com/*"
to = "/kidlisp.com/index.html"
```

---

## 5. Extended Platforms

### 5.1 KidLisp GameBoy (`kidlisp-gameboy/`)

**Status:** Active Development ✅

Compiles KidLisp to actual GameBoy ROMs using GBDK (C) and hUGEDriver (audio).

**Supported Commands:**
- `wipe <color>` - Clear screen
- `ink <color>` - Set color (black/white)
- `line x1 y1 x2 y2` - Bresenham line drawing
- `melody "notes"` - Play note sequences

**Build Pipeline:**
```
.lisp source → kidlisp-to-gb.mjs → C code → GBDK → .gb ROM
```

**Example:**
```kidlisp
; Grid pattern with melody
wipe black
ink white
line 10 10 150 134
melody "ceg"
```

### 5.2 KidLisp N64 (`kidlisp-n64/`)

**Status:** Experimental 🧪

Bare-metal N64 development explorations:
- `asm/` - Assembly experiments
- `bare-metal.s` - Low-level code
- `sdk-hello/` - SDK examples

### 5.3 Tezos Blockchain Integration (`tezos/kidlisp/`)

**Status:** Feature-flagged (disabled by default)

Mints KidLisp programs as NFT tokens on Tezos blockchain:
- FA2 smart contract (`keeps_fa2_*.py`)
- IPFS metadata upload
- Ghostnet testnet support
- Wallet integration scripts

**Integration Point:** `store-kidlisp.mjs` calls `integrateWithKidLispCache()` when `TEZOS_ENABLED=true`

---

## 6. ATProto Integration

KidLisp content syncs to ATProto (Bluesky protocol) via `media-atproto.mjs`:

**Lexicon:** `computer.aesthetic.kidlisp`

**Features:**
- Automatic sync on creation
- User credential management
- PDS endpoint: `at.aesthetic.computer`
- Record creation with rkey tracking

---

## 7. Development Tools

### 7.1 CLI Tools (`kidlisp/tools/`)

| Tool | Purpose |
|------|---------|
| `api-summary.mjs` | Generates categorized API documentation |
| `source-tree.mjs` | Analyzes embedded layer structure |
| `get-source.mjs` | Fetches KidLisp source by code |

### 7.2 Analysis Scripts (`scripts/`)

| Script | Purpose |
|--------|---------|
| `kidlisp-db-stats.mjs` | MongoDB collection analysis |

### 7.3 External Tools (`kidlisp-tools/`)

| Tool | Purpose |
|------|---------|
| `get-source.mjs` | Source fetcher |
| `source-tree.fish` | Shell utility for source tree |

---

## 8. Documentation Ecosystem

### 8.1 Documentation Hierarchy

```
kidlisp/
├── README.md                 # Main entry point
├── COMPLETE_API_MAP.md       # Full API reference  
├── STRUCTURE.md              # Navigation guide
├── docs/
│   ├── core/                 # Language fundamentals
│   ├── features/             # Feature deep-dives
│   ├── implementation/       # Technical guides
│   ├── integration/          # External systems
│   └── reports/              # Analysis documents
└── reports/
    └── tinylisp/             # TinyLisp comparison research
```

### 8.2 Knowledge Base (`kidlisp-knowledge/`)

Structured for LLM training:
- `/core/` - Getting started, language reference
- `/features/` - Embedding, feed, transformations
- `/implementation/` - Execution system, refactors
- `/reports/` - Feral File integration, contracts

### 8.3 Plans Directory

Related planning documents in `/plans/`:
- `kidlisp-singleton-refactor.md`
- `kidlisp-embedding-fix.md`
- `kidlisp-live-parameter-editing.md`
- `recursive-embedded-kidlisp.md`
- `implementation-unified-kidlisp.md`
- `TODO-kidlisp-fixes.md`
- And 15+ more KidLisp-related plans

---

## 9. Observations & Recommendations

### 9.1 Strengths

1. **Comprehensive API:** 118+ functions provide rich creative possibilities
2. **Strong User Adoption:** 10,543 programs with healthy engagement
3. **Good Documentation:** Structured docs and knowledge base
4. **Multi-platform Vision:** GameBoy, N64, blockchain show ambition
5. **Decentralized Strategy:** ATProto integration for data sovereignty

### 9.2 Areas for Improvement

1. **Anonymous Content:** 19% of content lacks attribution - consider guest accounts
2. **Hit Distribution:** Long tail - most content has low engagement
3. **GameBoy Compiler:** Limited to basic commands - expand API parity
4. **Tezos Integration:** Currently disabled - needs stability work
5. **N64 Work:** Very experimental - needs roadmap

### 9.3 Technical Debt Indicators

- Multiple `kidlisp.mjs` copies in `.netlify/functions-serve/` (build artifacts)
- Empty `kidlisp-keep.mjs` in functions (needs implementation or removal)
- Some duplicated documentation across directories

### 9.4 Metrics to Track

| Metric | Current | Recommendation |
|--------|---------|----------------|
| Programs Created/Day | Unknown | Add analytics |
| Active Users/Week | Unknown | Add tracking |
| Avg Session Duration | Unknown | Add telemetry |
| Error Rate | Unknown | Add Sentry/logging |

---

## 10. Conclusion

KidLisp is a mature, well-documented creative coding language with strong user adoption (10,543+ programs). The core interpreter is substantial (13K+ lines) with a rich API surface. The ecosystem extends ambitiously to retro gaming platforms and blockchain, though these extensions need further development.

The MongoDB data shows healthy content creation with viral hits (`$bop` at 4,647 views) and consistent anonymous usage indicating easy onboarding. The ATProto integration positions KidLisp for decentralized content ownership.

**Next Steps:**
1. Complete Tezos integration testing
2. Expand GameBoy compiler API
3. Add usage analytics
4. Consider guest account system for anonymous users
5. Continue knowledge base development for LLM training

---

*Report generated from repository analysis and MongoDB inspection.*
