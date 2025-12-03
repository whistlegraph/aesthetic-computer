# JS Piece Bundle Debug Report

## Problem
When bundling a JavaScript `.mjs` piece (notepat), the system still tries to run it as KidLisp, showing:
```
❌ KidLisp Validation Failed: Missing 10 closing parentheses, Unmatched double quote
```

## Flags Set in Bundle HTML

The `generateJSPieceHTMLBundle()` sets these window flags:
```javascript
window.acPACK_MODE = true;
window.acSTARTING_PIECE = "notepat";
window.acPACK_PIECE = "notepat";
window.acPACK_DATE = "...";
window.acPACK_GIT = "...";
window.acPACK_COLOPHON = {
  piece: {
    name: 'notepat',
    isKidLisp: false  // <-- This should prevent KidLisp execution
  },
  build: { ... }
};
```

## Execution Path Analysis

### 1. Boot Phase (`boot.mjs`)

```javascript
// Line ~307-316
if (window.acSTARTING_PIECE === undefined) window.acSTARTING_PIECE = "prompt";

if (!window.acPACK_MODE && !window.acSPIDER) {
  sluggedUrl = slug(originalUrl) || window.acSTARTING_PIECE;
} else {
  sluggedUrl = window.acSTARTING_PIECE;  // Uses "notepat"
}

const pieceToLoad = sluggedUrl;  // "notepat"
const parsed = parse(pieceToLoad);
```

**Question**: What does `parse()` return for "notepat"? Does it check for `.lisp` extension?

### 2. Disk Loading (`disk.mjs`)

The `load()` function in disk.mjs handles piece loading. Key areas to check:

#### Line ~6050-6110: URL Construction
```javascript
if (getPackMode()) {
  // In OBJKT mode, use relative paths to load bundled pieces
  baseUrl = ".";
}
// ...
if (path.endsWith('.lisp')) {
  // KidLisp file
  fullUrl = "/" + resolvedPath + "#" + Date.now();
} else {
  // JavaScript file
  fullUrl = "../" + relativePath + ".mjs" + "#" + Date.now();
}
```

**Question**: Is `path` correctly set to NOT end in `.lisp`?

#### Line ~6145+: Module Loading
```javascript
// 🅱️ Load the piece.
```

Need to trace what happens after URL construction.

### 3. KidLisp Detection (`kidlisp.mjs`)

```javascript
// isKidlispSource() function - Line ~?
// This function detects if source code is KidLisp
```

**Question**: Is the source code being passed through `isKidlispSource()` somewhere?

## Files to Investigate

1. **`boot.mjs`** - Lines 290-350
   - How does `parse()` work?
   - What gets passed to `disk.load()`?

2. **`lib/parse.mjs`** - The parse function
   - Does it detect piece type?
   - Does it look at file extensions?

3. **`lib/disk.mjs`** - Lines 6000-6500
   - The `load()` function
   - How does it decide between JS and KidLisp?
   - Where does KidLisp validation get triggered?

4. **`lib/kidlisp.mjs`** - Lines 3880-3900
   - The validation error we're seeing
   - What triggers `KidLisp Validation Failed`?

## Hypothesis

The issue might be one of:

1. **Parse detection**: `parse()` might be detecting the piece name as KidLisp source
2. **Pack mode special case**: There might be code that assumes PACK_MODE = KidLisp
3. **Embedded source**: `acKIDLISP_SOURCE` or similar flag might be set somewhere
4. **VFS lookup**: The piece might be loading from wrong location

## Next Steps

1. Search for where `KidLisp Validation Failed` is triggered
2. Trace backwards to find what calls the validation
3. Check if there's a flag like `acKIDLISP_SOURCE` being set
4. Check the `parse()` function in `lib/parse.mjs`

---

## Investigation Results

(To be filled in as we trace the code)
