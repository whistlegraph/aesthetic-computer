# Solo Mode Feature Plan

**Goal:** Add a "solo" mode to aesthetic.computer that locks a piece in place, preventing navigation away via keyboard shortcuts, prompt HUD corner label taps, or any other escape mechanism.

## URL Patterns

### Option 1: Pipe Suffix (Quick Typing)
```
aesthetic.computer/notepat|
```
- Trailing `|` expands to `?solo=true` internally
- Fast to type, visually clean URL
- Processed in `index.mjs` (router) — redirect or rewrite to query param

### Option 2: Query Parameter
```
aesthetic.computer/notepat?solo
aesthetic.computer/notepat&solo
```
- Standard query param approach
- More explicit, easier to understand

**Recommendation:** Support both — `|` as syntactic sugar that expands to `?solo`.

---

## Implementation Locations

### 1. Router (system/netlify/functions/index.mjs)
- Detect trailing `|` in slug/path
- Strip the `|` and redirect to `?solo` version (302)
- Example: `/notepat|` → `/notepat?solo`

### 2. Boot (system/public/aesthetic.computer/boot.mjs)
- Add `'solo'` to `LEGITIMATE_PARAMS` array (~line 466)
- Parse `solo` param like `tv`, `device`, etc. (~line 1097+)
- Pass `solo: true` to `boot()` via resolution object
- Example pattern follows existing `tv`/`device` handling

### 3. Bios (system/public/aesthetic.computer/bios.mjs)
- Accept `solo` in resolution object (already receives `tv`, `device`, etc.)
- Store in `preservedParams` for refresh functionality (~line 758)
- Pass through to disk init message

### 4. Disk (system/public/aesthetic.computer/lib/disk.mjs)
**Primary implementation location**

#### A. Add SOLO_MODE flag (~line 632)
```javascript
let SOLO_MODE = false; // Whether running in solo mode (prevents navigating away from piece)
```

#### B. Set flag from init message (~line 9258)
```javascript
SOLO_MODE = content.resolution?.solo === true;
```

#### C. Disable keyboard navigation shortcuts (~line 10860+)
- Block `Escape` key handling when `SOLO_MODE` is true
- Block `Back to prompt` functionality
- Possibly block `Tab` for HUD toggle (or keep but read-only)

#### D. Disable prompt HUD corner label interactivity
- The corner label tap functionality is handled via `qr-corner-tap` message (~line 14216)
- In solo mode:
  - Don't register the `qr-corner` hitbox (skip `button:hitbox:add`)
  - Or don't handle the tap message
  - Skip sound effects on touch/tap in that area

#### E. Prevent `$commonApi.jump()` (~line 2686)
```javascript
jump: function jump(to, ahistorical = false, alias = false) {
  if (SOLO_MODE) {
    console.log("🔒 Jump blocked: solo mode active");
    return;
  }
  // ... existing code
}
```

---

## Behavior in Solo Mode

| Feature | Normal | Solo Mode |
|---------|--------|-----------|
| Escape key → prompt | ✅ Works | ❌ Blocked |
| Back navigation | ✅ Works | ❌ Blocked |
| Corner label tap | ✅ Opens prompt | ❌ No action, no sound |
| Tab (HUD toggle) | ✅ Toggles HUD | ❓ Optional: keep or disable |
| Shift (QR fullscreen) | ✅ Works | ❓ Optional: keep for sharing |
| `jump()` API | ✅ Works | ❌ Blocked |
| Page refresh | ✅ Works | ✅ Works (stays in solo) |

---

## Files to Modify

1. **system/netlify/functions/index.mjs** — Pipe suffix detection & redirect
2. **system/public/aesthetic.computer/boot.mjs** — Add `solo` param handling
3. **system/public/aesthetic.computer/bios.mjs** — Pass `solo` to disk
4. **system/public/aesthetic.computer/lib/disk.mjs** — Core blocking logic

---

## Testing Checklist

- [ ] `aesthetic.computer/notepat|` redirects to `aesthetic.computer/notepat?solo`
- [ ] `aesthetic.computer/notepat?solo` activates solo mode
- [ ] Escape key does nothing in solo mode
- [ ] Corner label tap does nothing (no sound, no navigation)
- [ ] Piece `jump()` calls are blocked
- [ ] Refresh preserves solo mode
- [ ] Normal browsing still works without `|` or `?solo`
- [ ] Embedded contexts (kidlisp.com) not affected

---

## Implementation Order

1. Add `solo` to `LEGITIMATE_PARAMS` in boot.mjs
2. Parse and pass `solo` through boot→bios→disk chain
3. Add `SOLO_MODE` flag and blocking logic in disk.mjs
4. Add `|` suffix handling in index.mjs router
5. Test all edge cases

---

## Notes

- Similar pattern to existing `TV_MODE` and `DEVICE_MODE` flags
- Solo mode is purely client-side (no server changes needed beyond router)
- Could extend to support "presentation mode" or "kiosk mode" variants later
