# KidLisp Reference Cards: LaTeX → SVG Pipeline

## Overview

Replace the current DOM-based card rendering with a proper typesetting pipeline:
**LaTeX → PDF → SVG** (or **LaTeX → DVI → SVG**)

This gives us professional typography, proper text flow, and pixel-perfect cards.

## Why LaTeX?

- **Proper typesetting** - Text automatically flows, hyphenates, and fits
- **Code blocks** - `listings` or `minted` packages handle syntax highlighting
- **Consistent output** - Same card looks identical everywhere
- **Card aspect ratio** - Easy to define exact dimensions
- **Mathematical precision** - If we ever need formulas

## Pipeline Options

### Option A: Server-side (Build time)
```
LaTeX source → pdflatex → PDF → pdf2svg → SVG files
```
- Generate SVGs at build time
- Store in `/system/public/kidlisp.com/cards/`
- Cards are just `<img src="cards/drawing-01.svg">`
- **Pros**: Fast runtime, no dependencies for users
- **Cons**: Need to rebuild when cards change

### Option B: Emacs-based (Dev time via MCP)
```
Card data → Emacs org-mode → LaTeX export → PDF → SVG
```
- Use Emacs MCP to generate cards interactively
- Org-mode templates for card content
- Export pipeline via `ox-latex`
- **Pros**: Interactive editing, live preview
- **Cons**: More complex setup

### Option C: Hybrid (Recommended)
- **Dev time**: Edit cards in Emacs org-mode, preview with LaTeX
- **Build time**: Generate final SVGs via CLI
- **Runtime**: Display pre-rendered SVGs

## Required Dependencies (Dockerfile)

```dockerfile
# LaTeX (texlive-small or texlive-full)
RUN dnf install -y \
    texlive-scheme-small \
    texlive-listings \
    texlive-xcolor \
    texlive-geometry \
    texlive-standalone \
    texlive-preview \
    texlive-fontspec \
    texlive-collection-fontsrecommended

# PDF to SVG conversion
RUN dnf install -y \
    pdf2svg \
    inkscape  # alternative: more control over SVG output

# Optional: dvisvgm for DVI→SVG (sharper, no PDF intermediate)
RUN dnf install -y texlive-dvisvgm
```

## Card Template (LaTeX)

```latex
\documentclass[tikz,border=0pt]{standalone}
\usepackage[utf8]{inputenc}
\usepackage{fontspec}
\usepackage{xcolor}
\usepackage{listings}
\usepackage{geometry}

% Card dimensions (playing card ratio 2.5:3.5)
\newcommand{\cardwidth}{2.5in}
\newcommand{\cardheight}{3.5in}

% Colors
\definecolor{cardbg}{HTML}{FFFFC0}
\definecolor{codebg}{HTML}{1F1B16}
\definecolor{codetext}{HTML}{F4E9D7}

% Code style
\lstset{
  basicstyle=\ttfamily\small,
  backgroundcolor=\color{codebg},
  frame=single,
  breaklines=true,
}

\begin{document}
\begin{tikzpicture}
  % Card background
  \fill[cardbg, rounded corners=8pt] (0,0) rectangle (\cardwidth, \cardheight);
  
  % Title
  \node[anchor=north, font=\Large\bfseries] at (1.25in, 3.3in) {Drawing};
  
  % Card title
  \node[anchor=north west, font=\large\bfseries] at (0.15in, 3.0in) {line};
  
  % Description
  \node[anchor=north west, text width=2.2in, font=\normalsize] at (0.15in, 2.7in) {
    Draw a line from one point to another.
  };
  
  % Code block
  \node[anchor=north west, text width=2.2in] at (0.15in, 2.0in) {
    \begin{lstlisting}
(line 0 0 width height)
    \end{lstlisting}
  };
\end{tikzpicture}
\end{document}
```

## Directory Structure

```
kidlisp.com/
├── cards/
│   ├── src/               # LaTeX source files
│   │   ├── template.tex   # Base card template
│   │   ├── drawing/
│   │   │   ├── line.tex
│   │   │   ├── box.tex
│   │   │   └── ...
│   │   └── colors/
│   │       ├── ink.tex
│   │       └── ...
│   ├── svg/               # Generated SVG files (git-tracked or built)
│   │   ├── drawing-line.svg
│   │   ├── drawing-box.svg
│   │   └── ...
│   └── build.sh           # Script to regenerate all cards
```

## Build Script (`cards/build.sh`)

```bash
#!/bin/bash
# Generate SVG cards from LaTeX sources

CARD_SRC="src"
CARD_OUT="svg"

mkdir -p "$CARD_OUT"

for tex in $(find "$CARD_SRC" -name "*.tex" -not -name "template.tex"); do
  name=$(basename "$tex" .tex)
  category=$(basename $(dirname "$tex"))
  
  echo "Building ${category}-${name}..."
  
  # Compile to PDF
  pdflatex -output-directory=/tmp "$tex"
  
  # Convert to SVG
  pdf2svg "/tmp/${name}.pdf" "${CARD_OUT}/${category}-${name}.svg"
done

echo "Done! Generated $(ls -1 $CARD_OUT/*.svg | wc -l) cards."
```

## Emacs Integration

### Org-mode Card Template
```org
#+TITLE: KidLisp Card: line
#+CATEGORY: Drawing
#+LATEX_CLASS: kidlisp-card

* line

Draw a line from one point to another.

#+begin_src lisp
(line 0 0 width height)
#+end_src
```

### Emacs Functions (add to init or artery)
```elisp
(defun kidlisp-export-card ()
  "Export current org buffer as KidLisp card SVG."
  (interactive)
  (org-latex-export-to-pdf)
  (let ((pdf (concat (file-name-sans-extension buffer-file-name) ".pdf"))
        (svg (concat (file-name-sans-extension buffer-file-name) ".svg")))
    (shell-command (format "pdf2svg %s %s" pdf svg))
    (message "Card exported to %s" svg)))

(defun kidlisp-preview-card ()
  "Preview current card in browser."
  (interactive)
  (kidlisp-export-card)
  (browse-url (concat "file://" (file-name-sans-extension buffer-file-name) ".svg")))
```

## Runtime Changes (index.html)

Replace the complex DOM card building with simple SVG loading:

```javascript
const buildCard = (cardData) => {
  const frame = document.createElement('div');
  frame.className = 'book-frame';
  
  const img = document.createElement('img');
  img.src = `/cards/svg/${cardData.category}-${cardData.id}.svg`;
  img.alt = cardData.title;
  img.className = 'card-svg';
  
  frame.appendChild(img);
  return frame;
};
```

```css
.card-svg {
  width: 100%;
  height: 100%;
  object-fit: contain;
}
```

## Migration Steps

1. **Add LaTeX to Dockerfile** - Install texlive + pdf2svg
2. **Create card templates** - Design base LaTeX template
3. **Convert existing cards** - Transform JSON card data → LaTeX files
4. **Build pipeline** - Create build script for SVG generation
5. **Update runtime** - Simplify JS to load SVGs instead of building DOM
6. **Emacs tooling** - Add org-mode workflow for editing cards

## Alternative: KaTeX for Web-native

If we want to stay browser-native but still get LaTeX quality:

```javascript
import katex from 'katex';

// Render math/code with KaTeX
katex.render("\\text{line}(x_1, y_1, x_2, y_2)", element);
```

But this doesn't solve the layout/overflow problem - just the typography.

## Decision Points

1. **Build-time vs runtime?** → Build-time (SVGs) recommended
2. **pdf2svg vs dvisvgm?** → dvisvgm is sharper, try both
3. **Track SVGs in git?** → Yes for simplicity, or build in CI
4. **Card data source?** → Org-mode files or JSON → LaTeX compiler

---

## Next Steps

1. [x] Test LaTeX installation in devcontainer
2. [x] Create first card template
3. [x] Generate sample SVG
4. [x] Verify it displays correctly
5. [x] Build full pipeline
6. [ ] Wire up SVG cards to kidlisp.com UI
7. [ ] Add file watcher for live rebuild

---

## Current Status

**Pipeline is working!** 

LaTeX packages installed:
- texlive-scheme-basic, texlive-standalone, texlive-listings, texlive-xcolor, etc.
- pdf2svg for PDF→SVG conversion

Files created:
- `cards/src/*.tex` - LaTeX source files
- `cards/svg/*.svg` - Generated SVG cards  
- `cards/org/*.org` - Org-mode source (optional)
- `cards/build.sh` - Build script
- `cards/kidlisp-cards.el` - Emacs integration

Sample cards built:
- drawing-line.svg
- drawing-box.svg
- colors-ink.svg
- colors-wipe.svg

---

## Emacs Integration (Artery TUI)

Load the card editing tools in Emacs:

```elisp
;; In *scratch* or init.el
(load-file "/workspaces/aesthetic-computer/system/public/kidlisp.com/cards/kidlisp-cards.el")
```

Or via Emacs MCP:
```
mcp_emacs_execute_emacs_lisp: (load-file "/workspaces/aesthetic-computer/system/public/kidlisp.com/cards/kidlisp-cards.el")
```

### Keybindings (in org-mode)
- `C-c C-b` - Build current card (org → tex → svg)
- `C-c C-p` - Preview SVG in browser

### Functions
- `M-x kidlisp-new-card` - Create a new card
- `M-x kidlisp-build-card` - Build current org file
- `M-x kidlisp-preview-svg` - Open SVG in viewer

---

## Live Preview Workflow

1. Open card org file in Emacs (via fishy or artery)
2. Edit the card content
3. Press `C-c C-b` to build
4. SVG is regenerated and preview refreshes via CDP

For manual refresh:
```bash
cd /workspaces/aesthetic-computer/system/public/kidlisp.com/cards
./build.sh drawing-line  # Build specific card
./build.sh               # Build all cards
```
