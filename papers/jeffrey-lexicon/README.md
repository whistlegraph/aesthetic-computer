# Jeffrey Lexicon

A frequency-attributed dictionary of every word **Jeffrey Alan Scudder** has used in first-hand textual or transcribed material across the Aesthetic Computer corpus. A sub-platter within the [papers platter](../SCORE.md), parallel to [jeffrey-platter](../jeffrey-platter/) (canonical photographic self) and the `jeffrey-pvc` voice clone (canonical audio self).

**Live dashboard:** https://papers.aesthetic.computer/platter/jeffrey-lexicon/ — search box, top tokens, neologism candidates, spoken-only / written-only splits. Built by [`bin/render-report.mjs`](bin/render-report.mjs); the HTML lives at [`system/public/papers.aesthetic.computer/platter/jeffrey-lexicon/index.html`](../../system/public/papers.aesthetic.computer/platter/jeffrey-lexicon/index.html) and is also dropped to `~/Desktop/jeffrey-lexicon-report.html` on each render.

> The lexicon is the textual analogue of the photo index. Where `jeffrey-platter/manifest.json` answers "what does jeffrey look like?", `jeffrey-lexicon/dictionary.json` answers "what words does jeffrey actually use, where did each come from, and how often?"

This is **first-hand only**. Anything generative — gpt-image-2 prompts, Claude co-authored commits, AI-rewritten paper paragraphs, machine-translated body text — is excluded by source filter. The provenance discipline is the artifact's reason to exist: a poisoned dictionary is no dictionary.

---

## 1. What counts as a first-hand source

A source qualifies if jeffrey personally typed, spoke, or wrote it, and the path back to him is auditable.

| Source class | Path | Filter |
|---|---|---|
| Commit messages | `git log --author=` matches jeffrey identities | Drop `Co-Authored-By: Claude` blocks |
| Code comments | `git blame` author == jeffrey, comment-line tokens only (`//`, `/*`, `;`, `#`, `--`) | Skip vendored / `node_modules` / minified |
| AC piece source | `system/public/aesthetic.computer/disks/*.{mjs,lisp}` | String literals + comments authored by jeffrey |
| Lectures | `papers/lectures/*.{vtt,md}` | Speaker-tagged jeffrey lines only |
| Recap audio | `recap/out/**/subs.json` | Whisper segments of jeffrey speaking |
| arXiv papers | `papers/arxiv-*/*.tex` where author == jeffrey alone | LaTeX-stripped body; drop AI-rewritten sections (tag) |
| CV | `papers/cv/cv.tex` | Body text, no field labels |
| Repo planning | `TODO.txt`, `schedule.txt`, `honeydo.txt` | Whole file |
| IG captions | `portraits/jeffrey/ig-archive/whistlegraph/*.txt` | Captions only, drop hashtags+mentions to a side bucket |
| Vault email | `aesthetic-computer-vault/<mail>/sent/` | Sent-by-jeffrey only; never enters the public artifact |
| Handwriting | TBD: scanned-notebooks OCR pass | Confidence ≥ 0.85 per token |

**Excluded by rule:**
- gpt-image prompts, gpt-4o vision descriptions, jeffrey-described.jsonl
- Commits authored by Oven, Claude, or any non-jeffrey contributor
- Auto-generated logs, build outputs, lock files, package metadata
- Translation outputs (Danish/Spanish/Chinese/Japanese paper variants)
- Paper sections marked `% ai-pass` in the .tex source (convention TBD)
- jas.life closed-source material

---

## 2. Schema

`dictionary.json` is the build output. One entry per token (lemmatized? — see §4 open questions).

```json
{
  "kidlisp": {
    "count": 1247,
    "by_source": {
      "commits": 512,
      "comments": 88,
      "lectures": 14,
      "papers": 421,
      "ig_captions": 23,
      "recap_audio": 7,
      "ac_pieces": 182
    },
    "first_seen": {
      "iso": "2024-08-14T03:17:22Z",
      "ref": "commits/9f3a1b2",
      "context": "rename lisp -> kidlisp"
    },
    "last_seen": {
      "iso": "2026-05-08T11:04:51Z",
      "ref": "lectures/K8xCBPW1rmo",
      "context": "...so kidlisp is what fits on a card..."
    },
    "sample_contexts": [
      {"src": "papers/arxiv-kidlisp/kidlisp.tex", "line": 142, "snippet": "..."},
      {"src": "lectures/K8xCBPW1rmo.vtt", "ts": "00:14:32", "snippet": "..."}
    ],
    "provenance": "first-hand",
    "neologism": true
  }
}
```

Every entry carries the source breakdown so derivative views are cheap:

- **Neologisms** — words first introduced by jeffrey (`kidlisp`, `whistlegraph`, `notepat`, `platter`, `platterization`, `aestheticants`, ...). Detected by absence in a baseline corpus + `first_seen.ref` being authored by jeffrey.
- **Vocabulary drift by year** — bucket counts by `first_seen` year.
- **Spoken vs written** — `lectures + recap_audio` vs everything else. Tells us whether a word lives in jeffrey's mouth or only on his keyboard.
- **Code-only words** — `comments + ac_pieces` only. Names that never escaped the source tree.

---

## 3. Build pipeline

Each source class gets its own extractor under [`bin/extractors/`](bin/). The driver [`bin/build-lexicon.mjs`](bin/build-lexicon.mjs) fans out, merges, and writes `dictionary.json`.

```
manifest.json (source roots + provenance rules)
  → bin/extractors/<class>.mjs (one extractor per source class)
    → per-source token streams with {token, src, ref, ts, context}
      → bin/build-lexicon.mjs (merge, count, attribute)
        → dictionary.json
        → views/ (neologisms.json, by-year.json, spoken-vs-written.json, code-only.json)
```

Cache discipline (mirrors `recap/cli.mjs` and `papers/cli.mjs`): each extractor caches by `(source-path, content-hash)` so reruns cost ~zero.

```bash
node papers/jeffrey-lexicon/bin/build-lexicon.mjs --all
node papers/jeffrey-lexicon/bin/build-lexicon.mjs --source commits
node papers/jeffrey-lexicon/bin/build-lexicon.mjs --since 2026-01-01
node papers/jeffrey-lexicon/bin/build-lexicon.mjs --views      # rebuild derivative views only
```

---

## 4. Open questions (decide before first full build)

- **Lemmatization** — collapse `paint / paints / painted / painting` to one entry, or keep separate? Argument for keep: the morphological choice is part of voice. Argument for collapse: frequencies are more interpretable.
- **Casing** — preserve original case (so `KidLisp` ≠ `kidlisp`)? Or fold? Probably fold but track a `forms: { "KidLisp": 412, "kidlisp": 835 }` side field.
- **Tokenizer for code identifiers** — split `paintBucket` into `paint` + `bucket`? Keep as one? Both views are useful.
- **Vault inclusion in the public artifact** — vault email content should *contribute to counts* but never appear in `sample_contexts` for anything jas.life-sourced. Two-tier output: `dictionary-public.json` (no vault snippets) vs `dictionary-full.json` (vault-included, gitignored).
- **AI-pass tagging in `.tex`** — convention for marking AI-rewritten paragraphs so the extractor can skip them. Proposal: `% ai-pass-begin` / `% ai-pass-end` block markers.
- **OCR pipeline for handwriting** — Tesseract? Apple Vision? confidence threshold for inclusion?

---

## 5. Roadmap

- [ ] **Phase 1 — commits + comments** — easiest, high yield, ~13k jeffrey commits + git-blame on `disks/`. Covers ~70% of the textual surface.
- [ ] **Phase 2 — lectures + recap audio** — pull from existing VTT and `subs.json`. The "spoken jeffrey" axis turns on here.
- [ ] **Phase 3 — papers + CV** — LaTeX-strip with `detex` or `pandoc -t plain`, drop `% ai-pass-*` blocks.
- [ ] **Phase 4 — IG captions + planning files** — straightforward.
- [ ] **Phase 5 — vault email** — sent-only, two-tier output.
- [ ] **Phase 6 — handwriting OCR** — last because the corpus is unknown.
- [ ] **Public dashboard** — `papers.aesthetic.computer/platter/lexicon/` mirroring the shape of `platter/jeffrey/` (search, sort, filter by source class, click-through to context).
- [ ] **Cross-reference back to [jeffrey-platter](../jeffrey-platter/)** — caption text from photo metadata also feeds the lexicon (where present).

---

## How to extend this index

1. New source class? Add a row to §1, an entry under `sources` in [manifest.json](manifest.json), and an extractor at `bin/extractors/<class>.mjs`.
2. Mark anything generative as `provenance: "ai"` in the manifest and the extractor will skip it. Never silently include.
3. When a build runs, append a row to a `BUILDLOG.md` here noting source counts + total tokens — drift in those numbers is the early-warning signal for filter regressions.
