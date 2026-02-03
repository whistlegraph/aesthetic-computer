# Sotce Net Page Navigation & Lazy Loading

## Overview
Transform the main page feed into a TikTok-style swipeable experience with individual page routes, lazy loading, and chat linking.

## Page Numbering Convention
- **Diary pages**: `-1-`, `-2-`, `-3-` etc.
- **Question pages**: `*1*`, `*2*`, `*3*` etc.

## Features

### 1. Individual Page Routes
- [ ] Add routes like `/page/1`, `/page/2` for diary pages
- [ ] Add routes like `/ask/1`, `/ask/2` for question pages
- [ ] URL should update as user scrolls through content
- [ ] Refreshing page should load directly to that content

### 2. TikTok-Style Swipe Navigation
- [ ] Each page should be a distinct "slide" in the feed
- [ ] Swipe up/down (or scroll) to move between pages
- [ ] Snap scrolling to center each page
- [ ] Smooth transitions between pages
- [ ] Consider full-screen mode for each page

### 3. URL & Title Updates on Scroll
- [ ] Use IntersectionObserver to detect which page is in view
- [ ] Update `window.history.replaceState()` with current page route
- [ ] Update `document.title` to reflect current page (e.g., "Page 5 - Sotce Net")
- [ ] Handle both diary pages and question pages

### 4. Lazy Loading (Forward & Backward)
- [ ] Only render visible pages + buffer (e.g., 3 pages before/after)
- [ ] Load more pages as user scrolls toward edges
- [ ] Unload pages that are far from viewport to save memory
- [ ] Placeholder elements to maintain scroll position

### 5. API Endpoints for Pagination
- [ ] Modify `/sotce-net/pages` to support:
  - `?offset=N` - start from page N
  - `?limit=N` - return N pages
  - `?direction=forward|backward` - load direction
- [ ] Similar pagination for asks endpoint
- [ ] Return total count for progress indicators

### 6. Chat Linking
- [ ] Parse `-#-` in chat messages to link to diary page #
- [ ] Parse `*#*` in chat messages to link to question page #
- [ ] Clicking link scrolls to that page (or navigates if not loaded)
- [ ] Show preview tooltip on hover?

## Technical Implementation

### IntersectionObserver Setup
```javascript
const observer = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    if (entry.isIntersecting && entry.intersectionRatio > 0.5) {
      const pageNum = entry.target.dataset.pageNumber;
      const pageType = entry.target.dataset.pageType; // 'diary' or 'ask'
      updateRoute(pageType, pageNum);
      updateTitle(pageType, pageNum);
    }
  });
}, { threshold: 0.5 });
```

### Scroll Snap CSS
```css
#binding {
  scroll-snap-type: y mandatory;
  overflow-y: scroll;
}

.page-wrapper {
  scroll-snap-align: center;
  scroll-snap-stop: always;
}
```

### Virtual Scrolling Strategy
1. Keep track of `firstLoadedPage` and `lastLoadedPage`
2. When scrolling near top, prepend older pages
3. When scrolling near bottom, append newer pages
4. Remove pages outside the buffer window
5. Use placeholder divs to maintain scroll height

## Priority Order
1. ✅ Page numbering convention (*1* and -1-)
2. [ ] Individual page routes with scroll-to on load
3. [ ] IntersectionObserver for URL/title updates
4. [ ] Scroll snap for TikTok feel
5. [ ] Lazy loading with pagination API
6. [ ] Chat linking for page references

## Notes
- Current page rendering happens in `computePageLayout()` function
- Pages are stored in `subscription.pages` array
- Questions are stored in `sotce-asks` MongoDB collection
- Need to consider mobile vs desktop scroll behavior
