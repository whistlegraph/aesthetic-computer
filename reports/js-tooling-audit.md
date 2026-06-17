# JS/TS Code-Intelligence Tooling Audit — 2026-06-10

Machine: MacBook (Apple Silicon), **8 GB RAM** — every recommendation below was
filtered through "one process at a time, low resident memory."

Target repos:
- `/Users/jas/aesthetic-computer` — large JS/.mjs runtime, no TS (~8,600 .mjs files, 75 MB in `system/public/aesthetic.computer`)
- `/Users/jas/Developer/fuser` — TypeScript 5.8.3 pnpm/turbo monorepo, 22 packages, ~5,900 .ts/.tsx files

## 1. Inventory (before → after)

| Tool | Before | After | Notes |
|---|---|---|---|
| ripgrep (rg) | 15.1.0 | — | already best-in-class baseline |
| ugrep | "missing" on PATH | — | **Claude Code embeds it**: the agent-shell `grep` function execs the `claude` binary as `ARGV0=ugrep` (ugrep 7.5.0). So plain `grep` in agent shells is already smart. |
| fd | missing | **10.4.2** | installed (brew) |
| fzf | missing | **0.73.1** | installed (brew) — interactive, marginal for agents |
| ast-grep (`ast-grep` / `sg`) | missing | **0.43.0** | installed (brew); binary is `ast-grep` (no `sg` symlink — `sg` is a Unix builtin name) |
| comby | missing | not installed | superseded by ast-grep (not AST-aware, weak query logic) |
| semgrep | missing | not installed | Python, slow startup, heavier RSS; overkill vs ast-grep for grokking |
| universal-ctags | only BSD `/usr/bin/ctags` | **6.2.1** (brew, with `readtags`) | TS/TSX parser maintained upstream |
| tree-sitter CLI | missing | not installed | ast-grep covers the use cases |
| jq | 1.x present | — | fine |
| gron | missing | not installed | jq suffices |
| node / tsc | v22.22.2 / TS 6.0.3 global; fuser pins TS 5.8.3 | — | |
| deno | 2.7.14 | — | |
| bun | missing | not installed | no current need |
| typescript-language-server | 5.1.3 (global) | — | used by the Claude Code typescript-lsp plugin |
| vtsls | missing | **0.3.0** (npm -g `@vtsls/language-server`) | installed as an option; see proposals |
| **tsgo** (`@typescript/native-preview`) | missing | **7.0.0-dev.20260610.1** (npm -g) | the headline install |
| oxlint | missing | **1.69.0** (brew) | instant whole-package sanity linting |
| biome | missing | not installed | oxlint is ~2x faster for lint-only; fuser keeps eslint as canonical |
| eslint | repo-local only (fuser `node_modules/.bin/eslint`) | — | canonical but slow |
| difftastic (difft) | missing | **0.69.0** (brew) | semantic diffs |
| scip-typescript / scip | missing | **not installed — deliberately** | docs recommend `--max-old-space-size=16000`; a non-starter on 8 GB. ctags is the cheap index instead. |
| Claude Code | 2.1.170 | — | LSP tool is built-in (shipped ~2.0.74, no env flag needed) |
| Claude plugins | `typescript-lsp@claude-plugins-official` + `swift-lsp` installed & **enabled** | — | `.in_use` markers show live sessions using it |
| MCP servers (`~/.claude.json`) | asana, chrome-devtools ×3, sketchup | — | no code-intel MCP servers |

## 2. Research findings

### Structural/AST search → **ast-grep wins**
- Rust, multi-core, "tens of thousands of files in seconds"; semgrep is slow as a CLI; comby is not AST-aware and can't express logic (and/not/inside) — [ast-grep tool comparison](https://ast-grep.github.io/advanced/tool-comparison.html), [TS catalog](https://ast-grep.github.io/catalog/typescript/).
- Measured here: 49 MB peak RSS scanning the whole AC public tree. Perfect 8 GB citizen.
- Gotcha found in testing: `--lang ts` does **not** match `.tsx` files. Omit `--lang` and ast-grep infers per-file — that's the right default idiom.

### Fast typechecking → **tsgo is real and a 7–40x win**
- Native Go port of tsc, nightly `@typescript/native-preview`, becomes TypeScript 7; `--build`/project-references/`--incremental` ported; ~10x typical speedups, lower memory ([npm](https://www.npmjs.com/package/@typescript/native-preview), [typescript-go repo](https://github.com/microsoft/typescript-go), [announcement](https://devblogs.microsoft.com/typescript/announcing-typescript-native-previews/), [Dec 2025 progress](https://devblogs.microsoft.com/typescript/progress-on-typescript-7-december-2025/), [early-2026 release target](https://www.infoworld.com/article/4100582/microsoft-steers-native-port-of-typescript-to-early-2026-release.html)).
- **Verified locally** (numbers in §3): 5.35 s vs 37.2 s on the same fuser package, similar RSS. The 8 GB swap-fest tsc typecheck becomes a coffee-sip.
- **Blocker found:** tsgo (TS7) removed `baseUrl` → `error TS5102` and the check aborts. Most fuser packages (incl. `core`, `flow-types`, `flow`) set `baseUrl: "."`. Eight packages don't and check cleanly today. Fix is mechanical: replace `baseUrl` with `"paths": {"*": ["./*"]}` (or `"@/*": ["./*"]` where used) — that's a fuser-repo change to propose, not made here.

### LSP for agents → **already wired; vtsls is the upgrade path**
- Claude Code has built-in LSP support (changelog ~v2.0.74; no `ENABLE_LSP_TOOL` needed on 2.1.170) and this machine already has `typescript-lsp@claude-plugins-official` installed/enabled, backed by global `typescript-language-server` 5.1.3 ([plugin page](https://claude.com/plugins/typescript-lsp), [Scott Spence](https://scottspence.com/posts/enable-lsp-in-claude-code), [field report](https://alirezarezvani.medium.com/i-set-up-claude-code-lsp-for-a-200-file-typescript-codebase-heres-what-actually-happened-1dd84e85187d)).
- vtsls (wraps the actual VS Code TS extension) is faster/more correct on big monorepos and is now Zed's default TS server ([vtsls](https://github.com/yioneko/vtsls), [helix issue](https://github.com/helix-editor/helix/issues/13032), [zed issue](https://github.com/zed-industries/zed/issues/18698)). Caveat for 8 GB: tsserver-family servers grow with project size (VS Code defaults the cap to 3 GB) — on fuser, prefer opening one package at a time, and lean on rg/ast-grep/ctags for repo-wide questions instead of repo-wide LSP.
- Serena MCP ([oraios/serena](https://github.com/oraios/serena)) layers symbol-level navigation/edit tools over LSP; popular with Claude Code in 2026. On this machine it would run *another* Python process + a tsserver — redundant with the built-in LSP plugin. Verdict: skip unless doing heavy cross-file refactors, and never alongside a typecheck.

### Symbol indexing → **universal-ctags yes, scip no**
- universal-ctags has a maintained TS parser ([ctags repo](https://github.com/universal-ctags/ctags)); measured: full fuser index in 16.7 s / 36 MB RSS, lookups via `readtags` in <1 s / 1.4 MB.
- scip-typescript runs the real TS compiler to index; upstream guidance includes `node --max-old-space-size=16000` and `--no-global-caches` to avoid OOM ([scip-typescript](https://github.com/sourcegraph/scip-typescript), [SCIP announcement](https://sourcegraph.com/blog/announcing-scip)). Wrong tool for 8 GB; precision delta over ctags+LSP isn't worth it here.

### Fast linting as comprehension → **oxlint**
- oxlint 50–100x faster than eslint, ~2x faster than biome; ~300 rules ([oxc benchmarks](https://oxc.rs/docs/guide/benchmarks), [bench-linter](https://github.com/oxc-project/bench-linter), [LogRocket](https://blog.logrocket.com/retire-eslint-migrate-oxlint/), [PkgPulse 2026 comparison](https://www.pkgpulse.com/guides/biome-vs-eslint-vs-oxlint-2026)). Use as a zero-config "is this package internally sane / what's dead code here" probe; the repo's eslint stays canonical for CI.
- biome skipped: its formatter would fight prettier (AC) and fuser's eslint config.

### New-in-2025/26 misc
- **difftastic** — tree-sitter-based semantic diffs; installed, verified (§3). Memory scales with file size: 212 MB on the 16k-line `bios.mjs`, trivial on normal files. Use per-file, not `git difftool --dir-diff` on huge trees.
- **Repomix/codebase-map context packers** — built for stuffing cloud-model contexts; for a local agent with rg/ast-grep/LSP they just burn tokens. Skip.
- **GritQL** — overlaps ast-grep with a smaller community and stalled momentum; ast-grep's YAML rules + relational constraints cover the same ground. Skip.

## 3. Benchmarks (wall clock + `/usr/bin/time -l` max RSS)

| Query | Tool / scope | Time | Peak RSS |
|---|---|---|---|
| All `setTimeout($FN, $MS)` (311 hits) | ast-grep, whole AC public tree | 6.5 s | 49 MB |
| `setTimeout` with delay > 5000 (inline YAML rule, 5 hits) | ast-grep, AC `lib/` | ~1 s | <50 MB |
| Callers of `requestAppPreviewReload` | ast-grep, fuser `packages/flow/src` | 1.9 s | 16 MB |
| Build full symbol index | ctags, fuser `packages/` (22 pkgs) | 16.7 s | 36 MB (5.9 MB tags file) |
| Build full symbol index | ctags, AC public tree | 10.0 s | 36 MB (23 MB tags file) |
| "Where is `executionData` defined?" | readtags on fuser index | 0.8 s | 1.4 MB → `packages/core/src/sanitizeUtils.ts` `interface SanitizableNodeDataShape` |
| Typecheck `packages/app-node-protocol` | **tsgo** `--noEmit -p` | **5.4 s** | 119 MB |
| Typecheck `packages/app-node-protocol` | tsc 5.8.3 `--noEmit -p` | **37.2 s** | 125 MB |
| Typecheck `packages/flow-types` | tsc 5.8.3 | **256.8 s** (pulls Next.js types; machine contended) | 257 MB RSS / 880 MB footprint |
| Typecheck `packages/flow-types` | tsgo | 6.4 s — **but aborts on `baseUrl` (TS5102)**, so not a completed check | 306 MB |
| Lint AC `disks/` (~1,000 pieces) | oxlint | 2.6 s | 47 MB |
| Lint fuser `packages/core/src` | oxlint | 1.4 s | 12 MB |
| Semantic diff of dirty `bios.mjs` (16k lines) | difft | 9.1 s | 212 MB |
| File find `-e mjs` pattern | fd, whole AC repo | 1.0 s | 13 MB |
| tsgo error-detection sanity | bad `/tmp/bad.ts` | instant | exit 1, both TS2322 caught |

Honest pairing: tsgo vs tsc on the *same clean package* = **6.9x faster, same memory**. The flow-types row shows the worst-case tsc behavior the 8 GB machine suffers (4+ min, ~900 MB footprint for a 41-file package, because the shared `nextjs.json` config drags in the Next type universe).

## 4. Playbook — how agents should grok TS/JS on this machine

**Golden rule: one heavy process at a time.** rg/fd/ast-grep/readtags/oxlint are all "free" (<50 MB). tsgo, difft-on-huge-files, and any tsserver are the heavies — never overlap them with ffmpeg/Chromium/whisper or each other.

### Structural search (ast-grep)
```bash
# Callers of a function (omit --lang: it auto-infers per file, and
# --lang ts does NOT match .tsx — easy silent miss)
ast-grep run -p 'requestAppPreviewReload($$$ARGS)' packages/flow/src

# Pattern with metavariable capture
ast-grep run -p 'setTimeout($FN, $MS)' system/public/aesthetic.computer/lib

# One-shot rule with constraints (e.g. numeric delay >= 5000)
ast-grep scan --inline-rules '
id: long-settimeout
language: js
rule: { pattern: "setTimeout($FN, $MS)" }
constraints:
  MS: { regex: "^([5-9][0-9]{3}|[0-9]{5,})$" }
' system/public/aesthetic.computer/lib

# Relational logic: awaits inside a loop
ast-grep scan --inline-rules '
id: await-in-loop
language: ts
rule:
  pattern: "await $X"
  inside: { kind: for_statement, stopBy: end }
' packages/core/src

# Rewrite preview (no file changes without --update-all / -U)
ast-grep run -p 'console.log($$$A)' -r 'console.debug($$$A)' lib/
```

### Single-package typecheck (the big workflow change)
```bash
# fuser: tsgo on a package WITHOUT baseUrl — 5s instead of 37s
tsgo --noEmit -p /Users/jas/Developer/fuser/packages/app-node-protocol

# baseUrl-free today: app-node, app-node-protocol, canvas-runtime,
# code-gen, flow-agents, flow-chats, harness, node-enrichment
# Packages WITH baseUrl (core, flow, flow-types, …) abort with TS5102
# until baseUrl → paths is landed in fuser; fall back to repo tsc:
cd /Users/jas/Developer/fuser && node_modules/.bin/tsc --noEmit -p packages/<pkg>

# Single-file sanity (no tsconfig needed):
tsgo --noEmit path/to/file.ts
```
Never run tsgo/tsc `--build` across the whole fuser monorepo casually; per-package only, one at a time.

### Symbol lookup (ctags index — rebuild is cheap, do it per session)
```bash
# Index (fuser: ~17s; AC: ~10s)
ctags -R --languages=TypeScript,JavaScript \
  --exclude=node_modules --exclude=dist --exclude=.next --exclude=.turbo \
  -f /tmp/fuser.tags /Users/jas/Developer/fuser/packages
ctags -R --languages=JavaScript --exclude=node_modules \
  -f /tmp/ac.tags /Users/jas/aesthetic-computer/system/public/aesthetic.computer

# Exact symbol → definition site + kind + owning interface/class
readtags -t /tmp/fuser.tags -e executionData
# Prefix search
readtags -t /tmp/fuser.tags -e -p execution
```

### Whole-package sanity sweep
```bash
oxlint packages/core/src          # fuser, ~1.4s — unused vars/dead code map
oxlint system/public/aesthetic.computer/disks   # AC, ~2.6s
# eslint stays canonical for fuser CI; oxlint is the comprehension probe.
```

### Semantic diffs
```bash
difft --display inline /tmp/old.mjs current.mjs   # ignores formatting noise
GIT_EXTERNAL_DIFF=difft git -C <repo> diff -- path/file.ts   # ad hoc, per-file
# Caution: ~212 MB RSS on 16k-line files (bios.mjs/disk.mjs scale) — fine
# solo, don't run while a typecheck is live.
```

### Fast file finding
```bash
fd -e mjs kidlisp /Users/jas/aesthetic-computer     # respects .gitignore
fd --type f -e tsx AppNode /Users/jas/Developer/fuser/packages
```

### LSP (already on)
The built-in Claude Code LSP tool + `typescript-lsp` plugin already serve
go-to-definition / find-references / hover for `.ts/.tsx/.js/.mjs` in both
repos. Use it for "what is this symbol exactly" questions; use rg/ast-grep/
ctags for repo-wide enumeration (cheaper than forcing tsserver to load the
whole monorepo into an 8 GB machine).

## 5. Proposed Claude Code / repo config changes (NOT applied)

1. **Switch the TS language server backing to vtsls** (installed: `@vtsls/language-server` 0.3.0). Either via a community vtsls plugin (e.g. the Piebald `claude-code-lsps` marketplace) or when the official plugin grows a server option. Faster/more correct completions on the fuser monorepo; keep typescript-language-server as fallback. Human applies — plugin config untouched.
2. **fuser repo: replace `baseUrl` with `paths`** in `packages/*/tsconfig.json` (`"paths": {"*": ["./*"]}`; keep existing `@/*` mappings) so tsgo can check every package. This is also the TS7 migration path — needed eventually regardless. Coordinate with the agent working in fuser-genrecovery before touching shared config.
3. **Optional fuser devDependency**: `@typescript/native-preview` pinned in the repo so `pnpm tsgo --noEmit -p packages/x` matches the repo TS version drift-free (global nightly works fine meanwhile).
4. **Permissions allowlist** (project `.claude/settings.json`, via `/fewer-permission-prompts`): pre-allow `ast-grep run*`, `ast-grep scan*`, `readtags*`, `fd*`, `oxlint*`, `difft*`, `tsgo --noEmit*` as read-only-safe Bash patterns to cut prompt friction.
5. **Do not add** Serena MCP or scip-typescript on this machine (memory rationale in §2). Revisit Serena if a 16 GB+ machine appears.
6. **Tags freshness convention**: indexes live in `/tmp` (wiped on reboot) — agents should rebuild rather than trust a stale index; 10–17 s is cheap.
