# Chat Paint Performance Report

**Date:** 2026-03-07
**File:** `system/public/aesthetic.computer/disks/chat.mjs`

## Overview

The `paint` function (lines 588-1500+) runs every frame. For ~20 visible
messages averaging 2 lines each, costs per frame are:

| Operation | Count/Frame | Severity |
|---|---|---|
| `write()` calls | **120** | Medium |
| `text.width()` calls | **~62** | HIGH |
| `parseMessageElements()` | **20** | HIGH |
| Shadow regex `.replace()` | **40** | Medium |
| `substring()` + concat (color coding) | **400+** | HIGH |
| `stripInlineColorCodes()` | **20** | HIGH (O(n^2)) |
| `timeAgo()` Date creations | **20** | Low |

## Critical Bottlenecks

### 1. `parseMessageElements()` called fresh every frame (line 803)

Runs 10+ regex patterns per message through `chat-highlighting.mjs`. For
KidLisp messages, also instantiates `new KidLisp()` + `tokenize()`. **Not
cached between frames** even though message content is static.

### 2. Color code string building (lines 851-942)

Per colored element per line: 2x `substring()` + concatenation. With ~5
elements/line: `5 x 2 x 2 lines x 20 msgs = 400 substring() calls`.

### 3. `text.width()` O(n) per call (disk.mjs lines 3983-4010)

For proportional fonts, iterates char-by-char. Called for: last-line width,
timestamp width, heart width, kidlisp background positioning.

### 4. `stripInlineColorCodes()` O(n^2) (lines 136-153)

For each `\`, calls `indexOf("\\", i+1)` scanning forward. Called once per
message last line.

### 5. Shadow pass doubles write() calls (lines 944-959)

Every line gets shadow `write()` + main `write()` + a regex `.replace()` to
darken embedded RGB codes.

## Optimizations (Priority Order)

### P0: Cache `parseMessageElements()` on message object

```js
// Layout phase:
message._parsedElements = parseMessageElements(message.fullMessage);
// Paint (line 803):
const parsedElements = message._parsedElements;
```

Eliminates 20 multi-regex parse calls/frame.

### P1: Cache color-coded + shadow lines

After building `colorCodedLine` and `shadowLine`, cache on message. Invalidate
only on edit or hover state change.

Eliminates ~400 substrings, ~40 regex replacements, ~200 concatenations/frame
when nothing is hovered.

### P2: Cache `text.width()` results on layout

Store `lastLineStrippedWidth`, `timestampWidth`, `heartWidth` on layout object.
~37 fewer width calls/frame.

### P3: Fix `stripInlineColorCodes()` to O(n)

```js
function stripInlineColorCodes(s) {
  if (!s || typeof s !== "string") return "";
  let out = "", i = 0;
  while (i < s.length) {
    if (s[i] === "\\") {
      i++;
      while (i < s.length && s[i] !== "\\") i++;
      i++;
    } else {
      out += s[i++];
    }
  }
  return out;
}
```

### P4: Throttle `timeAgo()` to 1s intervals

Cache result per message, recompute only when >1s elapsed.

### P5 (Architectural): Dirty-rect message rendering

Render messages to offscreen buffer, only re-render changed/hovered messages.
Brings steady-state cost to near-zero when chat is idle. Larger refactor.

## Key Locations

- Paint function: `disks/chat.mjs:588-1500`
- Message parsing: `lib/chat-highlighting.mjs:21-262`
- Text measurement: `lib/disk.mjs:3983-4233`
- `stripInlineColorCodes`: `disks/chat.mjs:136-153`
- `timeAgo`: `disks/chat.mjs:4170-4190`
- Shadow pass: `disks/chat.mjs:944-959`
- Color coding loop: `disks/chat.mjs:851-942`
