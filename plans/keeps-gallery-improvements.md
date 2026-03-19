# Keeps Gallery Improvements Plan

## Overview
Four changes to `keep.kidlisp.com` for performance and usability:
1. Static PNG thumbnails (not animated WebP)
2. Paginated gallery with page controls (not infinite scroll)
3. IntersectionObserver that loads/unloads images based on visibility
4. Permalinks resolve for ALL pieces (kept + unkept)

---

## 1. Static PNG Thumbnails

**Why:** Animated WebP thumbnails are heavy (decode + memory for animation frames). Static PNGs load faster and are lighter for galleries.

### File: `system/public/kidlisp.com/keeps.html`

**Line 3605** — `createRow()` thumbnail URL:
```
// BEFORE:
const webpUrl = `https://oven.aesthetic.computer/grab/webp/80/60/$${entry.code}?duration=1800&fps=8&quality=70&density=1&nowait=true&source=gallery`;

// AFTER:
const thumbUrl = `https://oven.aesthetic.computer/grab/png/80/60/$${entry.code}?density=1&nowait=true&source=gallery`;
```

**Line 3647** — `createCard()` thumbnail URL:
```
// BEFORE:
const webpUrl = `https://oven.aesthetic.computer/grab/webp/280/210/$${entry.code}?duration=1800&fps=8&quality=70&density=1&nowait=true&source=gallery`;

// AFTER:
const thumbUrl = `https://oven.aesthetic.computer/grab/png/280/210/$${entry.code}?density=1&nowait=true&source=gallery`;
```

**Line 4642** — `ovenPreviewUrlForPiece()` (modal preview):
```
// BEFORE:
return `https://oven.aesthetic.computer/grab/webp/${size}/${size}/$${cleanPiece}?duration=2200&fps=8&quality=80&density=2&nowait=true&source=keep`;

// AFTER:
return `https://oven.aesthetic.computer/grab/png/${size}/${size}/$${cleanPiece}?density=2&nowait=true&source=keep`;
```

Also rename all `webpUrl` variables to `thumbUrl` at lines 3605, 3627, 3647, 3685.

---

## 2. Paginated Gallery (Replace Infinite Scroll)

**Why:** Infinite scroll makes it hard to find specific pages, wastes resources rendering off-screen cards, and has no stable URL state.

### File: `system/public/kidlisp.com/keeps.html`

**Lines 3234-3237** — Add page tracking variables:
```js
// BEFORE:
const PAGE_SIZE = 20;
let renderedCount = 0;
let isLoadingMore = false;

// AFTER:
const PAGE_SIZE = 24;  // nice grid (4x6 or 3x8)
let currentPage = 0;
let totalPages = 0;
```

**Lines 3695-3722** — `renderGrid()`: calculate total pages, render current page:
```js
function renderGrid() {
  grid.innerHTML = '';
  grid.classList.toggle('list-view', activeTab === 'kept');

  if (filteredCodes.length === 0) {
    // ... existing empty message ...
    paginationEl.style.display = 'none';
    return;
  }

  totalPages = Math.ceil(filteredCodes.length / PAGE_SIZE);
  if (currentPage >= totalPages) currentPage = totalPages - 1;
  if (currentPage < 0) currentPage = 0;

  renderPage();
  renderPaginationControls();

  if (activeTab === 'kept') {
    ensureTokenOwners().then(updateRowOwners);
  }
}
```

**Lines 3724-3761** — Replace `renderMoreCards()` with `renderPage()`:
```js
function renderPage() {
  grid.innerHTML = '';
  const start = currentPage * PAGE_SIZE;
  const end = Math.min(start + PAGE_SIZE, filteredCodes.length);
  const frag = document.createDocumentFragment();

  for (let i = start; i < end; i++) {
    frag.appendChild(activeTab === 'kept' ? createRow(filteredCodes[i]) : createCard(filteredCodes[i]));
  }

  grid.appendChild(frag);
  // Scroll grid into view on page change
  grid.scrollIntoView({ behavior: 'smooth', block: 'start' });
}
```

**New function** — `renderPaginationControls()` (insert after renderPage):
```js
function renderPaginationControls() {
  const paginationEl = document.getElementById('keeps-pagination');
  if (!paginationEl) return;

  if (totalPages <= 1) {
    paginationEl.style.display = 'none';
    return;
  }

  paginationEl.style.display = 'flex';
  paginationEl.innerHTML = `
    <button class="page-btn" ${currentPage === 0 ? 'disabled' : ''} onclick="goToPage(${currentPage - 1})">← Prev</button>
    <span class="page-info">Page ${currentPage + 1} of ${totalPages}</span>
    <button class="page-btn" ${currentPage === totalPages - 1 ? 'disabled' : ''} onclick="goToPage(${currentPage + 1})">Next →</button>
  `;
}

function goToPage(page) {
  if (page < 0 || page >= totalPages) return;
  currentPage = page;
  renderPage();
  renderPaginationControls();
}
```

**HTML** — Add pagination element after the grid (near line 2977):
```html
<div class="keeps-grid" id="keeps-grid"></div>
<div class="keeps-pagination" id="keeps-pagination"></div>
```

**CSS** — Add pagination styles:
```css
.keeps-pagination {
  display: none;
  justify-content: center;
  align-items: center;
  gap: 16px;
  padding: 20px;
  font-family: var(--font-mono);
  font-size: 14px;
}
.keeps-pagination .page-btn {
  background: var(--bg-tertiary);
  border: 1px solid var(--border-color);
  color: var(--text-primary);
  padding: 8px 16px;
  border-radius: 6px;
  cursor: pointer;
  font-family: var(--font-mono);
  font-size: 13px;
}
.keeps-pagination .page-btn:hover:not(:disabled) {
  background: var(--ac-purple);
  color: white;
}
.keeps-pagination .page-btn:disabled {
  opacity: 0.3;
  cursor: default;
}
.keeps-pagination .page-info {
  color: var(--text-tertiary);
}
```

**Search integration** — In `applyFilters()` (line 3766), reset page on filter change:
```js
// Add at end of applyFilters():
currentPage = 0;
```

---

## 3. IntersectionObserver Visibility Load/Unload

**Why:** Even with pagination (24 items), we should only decode images that are actually visible. When scrolled away, free the image memory. On scroll back, re-load from browser cache (instant).

### File: `system/public/kidlisp.com/keeps.html`

**Lines 3239-3252** — Replace current one-shot lazy loader with bidirectional observer:
```js
// Visibility-based image loading — load when visible, unload when not
const visibilityObserver = new IntersectionObserver((entries) => {
  for (const entry of entries) {
    const img = entry.target;
    if (entry.isIntersecting) {
      // Load: set src from data-src (browser cache makes re-loads instant)
      if (img.dataset.src && !img.src) {
        img.src = img.dataset.src;
      }
    } else {
      // Unload: free decoded image memory when off-screen
      if (img.src) {
        img.removeAttribute('src');
      }
    }
  }
}, { rootMargin: '200px' });

function observeImage(img) { visibilityObserver.observe(img); }
```

**Lines 3627-3629** — `createRow()`: rename `lazyLoadImage` → `observeImage`
**Lines 3685-3686** — `createCard()`: rename `lazyLoadImage` → `observeImage`

Note: With pagination (24 items per page), this is a nice-to-have but still useful for partial-scroll states and mobile where fewer cards are visible.

---

## 4. Permalinks for Unkept Pieces

**Why:** Currently `keep.kidlisp.com/$code` only resolves kept tokens (via Objkt query). Unkept pieces should also resolve — the store-kidlisp cache has them all.

### File: `system/public/kidlisp.com/keeps.html`

**Lines 4024-4090** — `openKeepDetailByCode()`: Add fallback to store-kidlisp API when Objkt returns no token:

After the existing Objkt query block (line 4070 `if (tokens.length === 0)`), instead of showing "Token not found", fetch from the store-kidlisp index:

```js
if (tokens.length === 0) {
  // Not a kept token — try store-kidlisp cache for unkept pieces
  try {
    const storeRes = await fetch(`/api/store-kidlisp?code=${encodeURIComponent(code)}`);
    if (storeRes.ok) {
      const piece = await storeRes.json();
      if (piece) {
        // Show unkept piece detail with run/edit links
        const acUrl = `https://aesthetic.computer/$${code}`;
        const klUrl = `https://kidlisp.com/$${code}`;
        const thumbUrl = `https://oven.aesthetic.computer/grab/png/320/320/$${code}?density=2&nowait=true&source=keep`;
        detailContent.innerHTML = `
          <div class="keep-detail-thumb"><img src="${thumbUrl}" alt="$${code}" /></div>
          <div class="keep-detail-body">
            <div class="keep-detail-row">
              <span class="keep-detail-status unlisted">Not Yet Kept</span>
            </div>
            <div class="keep-detail-row"><span class="keep-detail-label">Author</span><span class="keep-detail-value">${escapeHtml(piece.handle || 'anon')}</span></div>
            <div class="keep-detail-row"><span class="keep-detail-label">Hits</span><span class="keep-detail-value">${(piece.hits || 0).toLocaleString()}</span></div>
          </div>
          <div class="keep-detail-actions">
            <a class="keep-detail-btn primary" href="${acUrl}" target="_blank" rel="noopener">Run Piece ↗</a>
            <a class="keep-detail-btn secondary" href="${klUrl}" target="_blank" rel="noopener">Edit in KidLisp ↗</a>
          </div>`;
        return;
      }
    }
  } catch (e) {
    console.error('[keep-detail] store-kidlisp fallback error:', e);
  }
  detailContent.innerHTML = '<div class="keep-detail-loading">Piece not found.</div>';
  return;
}
```

### File: `system/netlify/edge-functions/keeps-social.js`

**Lines 118-123** — `fetchTokenData()`: When Objkt returns no token, provide a fallback description for social crawlers so unkept piece links still get good OG tags:

```js
// After: if (tokens.length === 0) return null;
// Change to: if (tokens.length === 0) return { token: null, listing: null, unkept: true };
```

**Lines 126-134** — `buildDescription()`: Handle unkept case:
```js
function buildDescription(tokenData) {
  if (!tokenData) return 'KidLisp generative art on Aesthetic.Computer.';
  if (tokenData.unkept) return 'KidLisp generative art — run it live on Aesthetic.Computer.';
  // ... existing kept logic ...
}
```

The OG image URL (`oven.aesthetic.computer/preview/1200x630/$code.png`) already works for all pieces regardless of kept status, so no change needed there.

---

## Files Changed

| File | Changes |
|------|---------|
| `system/public/kidlisp.com/keeps.html` | PNG thumbnails, pagination, visibility observer, unkept permalinks |
| `system/netlify/edge-functions/keeps-social.js` | Unkept piece fallback for social crawlers |

## Testing

1. Load `keep.kidlisp.com` — verify PNG thumbnails load (check Network tab for `.png` requests)
2. Verify pagination: prev/next buttons, page counter, search resets to page 1
3. Verify images unload when scrolled away (inspect `<img>` elements — `src` should clear)
4. Visit `keep.kidlisp.com/$cow` (kept) — should show keep detail with price
5. Visit `keep.kidlisp.com/$someunkeptcode` (unkept) — should show piece detail with run/edit links
6. Test social crawler with: `curl -A "Twitterbot" https://keep.kidlisp.com/$cow`
