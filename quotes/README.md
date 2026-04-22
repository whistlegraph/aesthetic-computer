# Quotes

Vendor quotes issued under **Aesthetic Inc.** Source of truth for what was billed, to whom, and for what scope. Each quote lives in its own directory named `<quote-id>-<slug>/` with a `quote.tex` source and built `quote.pdf`.

## Quote ID

Format: `AE-<YYYY>-<NNN>` — `AE` for Aesthetic (Inc.), four-digit year, three-digit sequence resetting each year. Set once per quote via `\newcommand{\quoteid}{...}` at the top of `quote.tex`.

## Ledger

| Quote ID | Date | Recipient | Scope | Amount | Status |
|----------|------|-----------|-------|--------|--------|
| [AE-2026-001](AE-2026-001-ucla-social-software/quote.pdf) | 2026-04-22 | UCLA Design Media Arts — Social Software (Jinwoo Park; program co-directed by Casey Reas and Lauren Lee McCarthy) | Technical documentation of Social Software runtime/language/tools, analysis of software in production across UCLA DMA, survey of open-source and proprietary stacks; hosted at [sosoft.arts.ucla.edu](https://sosoft.arts.ucla.edu/) | $1,200.00 | Issued |

## Building

Each quote builds independently with tectonic:

```bash
cd quotes/AE-2026-001-ucla-social-software
tectonic -X compile quote.tex
```

Paths in `quote.tex` are relative:
- Fonts: `../../system/public/type/webfonts/` (YWFT Processing)
- Logo: `../../papers/arxiv-ac/figures/pals.pdf`

## Starting a new quote

1. Pick the next sequence number (look at the ledger, increment).
2. `cp -R AE-YYYY-NNN-<prev-slug>/ AE-YYYY-NNN+1-<new-slug>/` (or start from a blank fork of an existing one).
3. Update `\newcommand{\quoteid}{...}` in `quote.tex`.
4. Update recipient, subject, scope, amount.
5. Build, commit, add a row to the ledger above.
