# Opinions

Machine-generated essays connecting external ideas to Aesthetic Computer's design philosophy. Rendered natively at `aesthetic.computer/opinion` using pixel text.

## Adding an Opinion

1. Create a `.md` file in this directory with YAML frontmatter:

```markdown
---
title: "Your Title Here"
author: Claude Opus 4.6
date: 2026-02-18
source: https://example.com/original-article
---

Your opinion body in markdown...
```

2. Add an entry to `index.json`:

```json
{
  "slug": "your-file-name-without-extension",
  "title": "Your Title Here",
  "author": "Claude Opus 4.6",
  "date": "2026-02-18"
}
```

3. Visit `aesthetic.computer/opinion your-file-name` to see it.

## Frontmatter Fields

| Field | Required | Description |
|-------|----------|-------------|
| `title` | yes | Display title of the opinion |
| `author` | yes | LLM model name (e.g. `Claude Opus 4.6`) or human author |
| `date` | yes | Publication date (`YYYY-MM-DD`) |
| `source` | no | URL of the original essay/article being responded to |

## Supported Markdown

The renderer handles a subset of markdown suited to long-form reading:

- `# H1`, `## H2`, `### H3` headings
- Paragraphs (plain text with word wrapping)
- `> Blockquotes` (with vertical bar)
- `- List items` (bulleted)
- `---` horizontal rules
- `**bold**` and `*italic*` (rendered as color changes — no font variants)
- `[link text](url)` (stripped to text in body; source URL is clickable in header)

## Rendering

Opinions render with AC's native pixel fonts:
- Titles and H1/H2 use `font_1` (6x10px monospace)
- Body text, H3, metadata use `MatrixChunky8` (4x8px compact)
- Dark/light mode adapts automatically
- Scrollable via mouse wheel, touch drag, arrow keys, pgup/pgdn

## Authoring Symlink

The repo root `/opinion` is a symlink to `system/public/opinion/` — you can author from either location.

## Future

- Post opinions to `news.aesthetic.computer` for wider distribution
- RSS feed generation from `index.json`
- Auto-generate `index.json` from frontmatter scan
