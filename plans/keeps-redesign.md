# Keeps Page Redesign — kidlisp.com/keeps

**Date:** 2026.02.06  
**Status:** In Progress

## Overview

Redesign `keeps.html` to replace the plain `| keeps` header with an illustrated SVG castle hero showing KidLisp `$codes` streaming out of it (like treasure from a keep/castle). Upgrade the prose section with colored-dot Aesthetic.Computer hyperlinks (from `give.aesthetic.computer` style), and add `/at` page design patterns: syntax-highlighted code previews with webp thumbnails, and improved modal popover behavior.

---

## 1. SVG Castle Illustration (Hero)

Replace the current `.keeps-about` text block with a full-width illustrated header featuring:

- **Inline SVG castle/keep** — a stylized pixel-art-meets-minimal castle in the AC pink palette
- **$codes streaming out** — animated `$code` labels (`$39j`, `$a1b`, `$zyx` etc.) floating/rising from the castle turrets, color-coded per the KidLisp rainbow palette
- Castle sits centered above the prose; the "keeps" metaphor is visual, not textual
- Light/dark mode aware using CSS variables

## 2. Colored-Dot Aesthetic Computer Links (from give)

Every "Aesthetic.Computer" hyperlink in the prose gets the `.logo-dot` treatment from `give.aesthetic.computer/index.html`:

```html
<a href="https://aesthetic.computer">Aesthetic<span class="ac-dot">.</span>Computer</a>
```

With `.ac-dot { color: var(--ac-purple); }` — the pink/cyan dot between "Aesthetic" and "Computer".

## 3. /at Page Design Patterns

Borrow from `at/user-page.html`:

- **Syntax-highlighted source previews** on cards — colored line numbers (6-color cycle), scrolling `source-preview` boxes
- **WebP thumbnail previews** — each card gets an `<img>` from `oven.aesthetic.computer/grab/webp/` for visual preview of the $code
- **Modal popover** — clicking a card opens a full modal with:
  - Large iframe preview (from aesthetic.computer/$code)  
  - Syntax-highlighted source sidebar
  - Action buttons (Edit, HTML, Keep)
  - Close on Escape, backdrop click, or ✕ button
  - Smooth fade/scale-in animation

## 4. Card Enhancements

- Add webp preview image to each card (thumbnail from oven)
- Syntax highlight the source code snippet with the rainbow line-number palette
- Modal click-through from card → full preview popover

## 5. Implementation Checklist

- [x] Write plan
- [ ] Add inline SVG castle with animated $codes
- [ ] Add `.ac-dot` colored-dot link style
- [ ] Update prose with colored-dot links
- [ ] Add webp preview images to cards
- [ ] Add syntax-highlighted source with colored line numbers
- [ ] Improve modal with fade/scale animation from /at pages
- [ ] Test light/dark mode
