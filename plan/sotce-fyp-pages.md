# SOTCE.NET FYP-Style Page Navigation

## Status: ✅ IMPLEMENTED

## Goal
Reimplement the diary page navigation with a TikTok/FYP-like swipe experience while keeping the existing page styling (4:5 aspect ratio, centered, not fullscreen).

## Current State
- Pages have `scroll-snap-align: start` but multiple pages can be visible
- IntersectionObserver with 239 entries was causing 3+ second blocking on garden open
- Attempted virtualization was janky

## Desired Behavior
1. **One page visible at a time** - centered in viewport
2. **Swipe/drag navigation** - vertical swipe to go prev/next
3. **Snap behavior** - pages snap to center, not top
4. **Only 3 pages in DOM** - prev, current, next for performance
5. **Keep existing page styling** - 4:5 aspect ratio, borders, etc.

## Implementation Plan

### Phase 1: CSS Changes

```css
#binding {
  /* Make binding the scroll container */
  height: calc(100vh - 100px); /* Minus header */
  overflow-y: scroll;
  scroll-snap-type: y mandatory; /* Strong snap */
  -webkit-overflow-scrolling: touch;
  overscroll-behavior: contain;
}

#garden div.page-wrapper {
  /* Each page takes full viewport height so only one shows */
  height: calc(100vh - 100px);
  display: flex;
  align-items: center; /* Center page vertically */
  justify-content: center;
  scroll-snap-align: center; /* Snap to center, not start */
  scroll-snap-stop: always; /* Must stop on each page */
}
```

### Phase 2: Virtualization Logic

```javascript
// State
let currentPageIndex = totalPages;
const pageElements = new Map(); // pageIndex -> DOM element

// Render exactly 3 pages around current
function updatePages(centerIndex) {
  const needed = [centerIndex - 1, centerIndex, centerIndex + 1]
    .filter(i => i >= 1 && i <= totalPages);
  
  // Remove pages not in needed set
  for (const [idx, el] of pageElements) {
    if (!needed.includes(idx)) {
      el.remove();
      pageElements.delete(idx);
    }
  }
  
  // Add missing pages in correct order
  for (const idx of needed) {
    if (!pageElements.has(idx)) {
      const wrapper = createPageWrapper(idx);
      insertInOrder(wrapper, idx);
      pageElements.set(idx, wrapper);
      loadPageContent(wrapper, idx);
    }
  }
  
  currentPageIndex = centerIndex;
  updatePath("/page/" + centerIndex);
}

// Detect which page is centered after scroll ends
let scrollEndTimer;
binding.addEventListener('scroll', () => {
  clearTimeout(scrollEndTimer);
  scrollEndTimer = setTimeout(() => {
    const centerY = binding.scrollTop + binding.clientHeight / 2;
    
    let closestIdx = currentPageIndex;
    let closestDist = Infinity;
    
    for (const [idx, el] of pageElements) {
      const elCenter = el.offsetTop + el.clientHeight / 2;
      const dist = Math.abs(elCenter - centerY);
      if (dist < closestDist) {
        closestDist = dist;
        closestIdx = idx;
      }
    }
    
    if (closestIdx !== currentPageIndex) {
      updatePages(closestIdx);
    }
  }, 150);
}, { passive: true });
```

### Phase 3: Initial Load

```javascript
// On garden open:
// 1. Create binding as scroll container
// 2. Call updatePages(startingPageIndex)
// 3. Scroll to center page immediately
```

### Key Differences from Previous Attempt

1. **`#binding` is the scroll container** - not `#wrapper`
2. **`scroll-snap-align: center`** - pages snap to center, not top
3. **`scroll-snap-stop: always`** - ensures one page at a time
4. **Page wrappers are full viewport height** - ensures only one visible
5. **Simpler scroll detection** - just check after scroll ends, no complex intersection logic

## Files to Modify

- `system/netlify/functions/sotce-net.mjs`
  - CSS for `#binding` and `.page-wrapper`
  - Replace virtualization JS with simpler approach

## Testing

1. Open sotce.net gate
2. Click gate to enter garden
3. Verify: transition is fast (no 3+ second delay)
4. Verify: one page centered at a time
5. Verify: swipe up/down navigates pages
6. Verify: pages snap to center
7. Verify: URL updates as you navigate
8. Verify: keyboard arrows work
