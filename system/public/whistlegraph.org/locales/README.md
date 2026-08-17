# whistlegraph.org locales

The chrome of whistlegraph.org speaks twelve languages; the works do not.
`index.html` carries a small inline i18n layer (no build step, no library) and
these files feed it.

## The pattern

- **`en.json` is the canonical key registry.** Every other locale mirrors its
  keys exactly. The same dictionary is inlined in `index.html` as `EN` so a
  failed locale fetch can never blank the chrome — **if you add or change a key,
  update both `en.json` and the inline `EN` object.**
- **Keys are stable slugs**, flat, grouped by prefix (`appearance.*`,
  `search.*`, `sort.*`, `kind.*`, `detail.*`, `posts.*`, `post.*`, `legacy.*`,
  `cat.*`, `elsewhere.*`, `footer.*`, `alt.*`). The prefix is the comment.
- **Resolution order:** `?lang=` param → `localStorage["whistlegraph-lang"]` →
  first `navigator.languages` primary-subtag match → `en`. Only the active
  locale file is fetched. Choosing a language in the Appearance dialog persists
  it and strips any `?lang=` from the URL.
- **Static DOM** is marked with `data-i18n="key"` (textContent),
  `data-i18n-html="key"` (innerHTML, for values that carry markup),
  `data-i18n-attrs="attr:key,attr:key"` (placeholders, aria-labels, titles),
  and optional `data-i18n-vars='{"date":"…"}'` for slotted static text.
- **Dynamic JS strings** route through `t(key, vars)` — `{var}` slots are
  substituted verbatim — and `tp(key, n, vars)` for counts.
- Switching languages re-renders in place (list, cloud, detail, metas, title);
  no reload.

## The plural mini-format

A value holds its plural forms split on `|`:

    "posts.count": "{n} post|{n} posts"          (en — one|other)
    "posts.count": "{n} пост|{n} поста|{n} постов"  (ru — one|few|many)
    "posts.count": "{n} 件の投稿"                  (ja — single form, no pipe)

`tp` picks the form with a per-language rule in `PLURALS` (index.html): Russian
gets the one/few/many rule, French and Hindi treat 0–1 as singular, everything
else defaults to `n===1`. Single-form languages (ja, zh, ko, and where Danish/
Hindi/Tagalog nouns don't inflect) just write one form — the index clamps to
the last form, so a missing pipe is always safe. A rule, not a library.

## What stays English

Bibliography, not interface: work **titles**, **author names**, venue/press
**citation lines** (the Appearances/Exhibitions/… list items, the zine
caption), platform names (TikTok, YouTube), and the word **Whistlegraph**
itself — never translated, never transliterated. `Plot:` tags stay as-is (a
technical term for pen-plots). The language `<label>` in the Appearance dialog
is deliberately trilingual ("Language / 语言 / 言語") so anyone stranded in the
wrong tongue can find the way out; leave it be.

## Adding a language

1. Copy `en.json` to `<code>.json` (ISO 639-1) and translate the values,
   keeping every `{var}` slot, `<a>`/`<code>` tag, and `&nbsp;` intact.
2. Add the code to `LANGS` in index.html (value = the endonym) and, if the
   plural rule isn't one/other, a rule in `PLURALS`. Map the `<html lang>`
   value in `HTML_LANG` if it differs from the code (like `zh` → `zh-Hans`).
3. Add an `<option>` with the endonym to the `#langSelect` in the Appearance
   dialog.
4. All twelve current languages are LTR; an RTL locale would also need
   `dir` handling in `applyStatic()` and a CSS audit.

## Follow-ups

- `tv.html` is not localized yet; it can adopt the same layer + these files.
- The fold "…" aria-label ("Show N more") is stamped once at load and keeps
  its load-time language until the next visit; cosmetic, aria-only.
