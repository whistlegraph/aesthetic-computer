# Multi-Font Chat System

**Date:** 2026.01.10  
**Status:** ✅ Implemented  
**Unique Factor:** Never seen in any chat system before — expressive typography as identity

## Vision

Let users choose their personal font for chat messages, creating a multilingual, expressive bandwidth where typography becomes part of communication style. Each message displays in the sender's chosen font.

## Available Fonts

| Font | Style | Best For |
|------|-------|----------|
| `font_1` | Default bitmap | English, basic Latin |
| `matrix` | Chunky 8x8 | Retro/cyberpunk vibes |
| `unifont` | Unicode coverage | Multilingual, emoji, CJK |

## Implementation Plan

### Phase 1: Data Model

**MongoDB `chat_messages` schema change:**
```js
{
  _id: ObjectId,
  handle: "username",
  text: "message content",
  font: "font_1",  // NEW: "font_1" | "matrix" | "unifont"
  timestamp: Date,
  room: "aesthetic" | "laer-klokken" | etc
}
```

- Default to `font_1` for backwards compatibility
- Store user preference in `users` collection too: `preferredChatFont`

### Phase 2: Scroll Geometry Refactor

**Challenge:** Different fonts have different line heights!

| Font | Approx Height | Line Spacing |
|------|---------------|--------------|
| font_1 | 6px | 8-10px |
| matrix | 8px | 10-12px |  
| unifont | 16px | 18-20px |

**Solution:**
- Calculate each message's height dynamically based on its font
- Store `calculatedHeight` per message in the scroll list
- Modify `scrollY` math to use cumulative heights instead of fixed `lineHeight * index`

```js
// Current (fixed height):
const messageY = scrollY + index * lineHeight;

// New (variable height):
let cumulativeY = scrollY;
messages.forEach((msg, i) => {
  const msgHeight = getFontLineHeight(msg.font);
  // render at cumulativeY
  cumulativeY += msgHeight + padding;
});
```

### Phase 3: Font Picker UI

**Location:** Below text input when opened (slide down animation)

**Design:**
```
┌──────────────────────────────────┐
│ [text input field]          [⏎] │
├──────────────────────────────────┤
│ 🔤 Choose your font:             │
│                                  │
│ ┌─────────┐ ┌─────────┐ ┌──────┐│
│ │ Aa Bb   │ │ ▄▀ ██   │ │ 你好 ││
│ │ font_1  │ │ matrix  │ │unifnt││
│ │   ✓     │ │         │ │      ││
│ └─────────┘ └─────────┘ └──────┘│
└──────────────────────────────────┘
```

- Show sample text in each font
- Checkmark on current selection
- Persist choice to user profile via API
- Access via button near input or keyboard shortcut

### Phase 4: Rendering Changes in `chat.mjs`

1. **Message paint loop:**
   - Get font from `msg.font || "font_1"`
   - Call appropriate `write()` with font parameter
   - Track actual rendered height for scroll calculations

2. **Word wrapping:**
   - Each font needs its own `measureText()` or char-width lookup
   - Unifont especially needs wider calculations

3. **Scroll bounds:**
   - `totalContentHeight` = sum of all message heights
   - Viewport culling needs to account for variable heights

### Phase 5: API Changes

**New endpoints:**
- `PATCH /api/user/font` — Update user's preferred font
- Existing chat POST: Accept optional `font` field (defaults to user pref)

**Chat message broadcast:**
- Include `font` field in socket messages

## File Changes

| File | Changes |
|------|---------|
| `disks/chat.mjs` | Font picker UI, variable-height scroll, per-message font rendering |
| `lib/type.mjs` | Maybe expose font choice in TextInput? |
| `nanos/api/chat.mjs` | Accept/store font field |
| `nanos/api/user.mjs` | Font preference endpoint |
| MongoDB | Add `font` field to messages, `preferredChatFont` to users |

## Open Questions

1. **Font loading:** Are all fonts already available or need lazy loading?
2. **Mixing in one message:** Allow inline font changes like `[matrix]text[/matrix]`? (v2)
3. **Performance:** Unifont is larger — any perf impact?
4. **Accessibility:** Should there be a "normalize all to one font" option for readers?

## Why This Is Cool

- **Linguistic identity** — CJK users can use unifont, retro lovers use matrix
- **Visual diversity** — Chat becomes a living typographic tapestry
- **Never been done** — Unique to Aesthetic Computer
- **Expressive** — Font choice is a form of self-expression

## Milestones

- [ ] Add `font` field to MongoDB schema
- [ ] Refactor scroll math for variable heights
- [ ] Implement font picker UI
- [ ] Test with all three fonts
- [ ] Deploy and document
