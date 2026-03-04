# Papers · Aesthetic Computer

## Papers

| Paper | Format | PDF | Source |
|-------|--------|-----|--------|
| Aesthetic Computer '26 | arXiv (LaTeX, 5pp) | `arxiv-ac/ac.pdf` | `arxiv-ac/ac.tex` |
| Aesthetic Computer '26 | JOSS (Markdown, 2pp) | `joss-ac/paper.pdf` | `joss-ac/paper.md` |
| KidLisp '26 | arXiv (LaTeX, 6pp) | `arxiv-kidlisp/kidlisp.pdf` | `arxiv-kidlisp/kidlisp.tex` |
| KidLisp '26 | JOSS (Markdown, 3pp) | `joss-kidlisp/paper.pdf` | `joss-kidlisp/paper.md` |

## Building

```bash
# arXiv papers (xelatex + bibtex, 3 passes)
cd papers/arxiv-ac && xelatex ac.tex && bibtex ac && xelatex ac.tex && xelatex ac.tex
cd papers/arxiv-kidlisp && xelatex kidlisp.tex && bibtex kidlisp && xelatex kidlisp.tex && xelatex kidlisp.tex

# JOSS papers (pandoc)
cd papers/joss-ac && pandoc paper.md --citeproc --pdf-engine=xelatex -o paper.pdf
cd papers/joss-kidlisp && pandoc paper.md --citeproc --pdf-engine=xelatex -o paper.pdf
```

## Formats

- **arXiv**: Full academic papers with two-column layout, AC custom fonts (YWFT Processing), syntax-highlighted code examples, development history tables, adoption metrics. Intended for arXiv preprint submission.
- **JOSS**: Condensed papers for Journal of Open Source Software submission. Markdown with LaTeX header-includes for syntax highlighting. JOSS reviews focus on software quality, documentation, and community impact.

## Subdomain

Published at `papers.aesthetic.computer` and `papers.prompt.ac`.

## Author

Jeffrey Alan Scudder — ORCID: [0009-0007-4460-4913](https://orcid.org/0009-0007-4460-4913)
