# Monaco KidLisp Highlighting Integration Guide

## Overview

The `monaco-kidlisp-highlighting.mjs` module provides shared syntax highlighting for Monaco editors across the kidlisp.com platform.

## Features

- ✅ Token-based syntax highlighting using kidlisp.mjs
- ✅ Rainbow text coloring
- ✅ Zebra text coloring
- ✅ Fade expression multi-color highlighting
- ✅ Compound colors for $codes, #codes, !codes
- ✅ Animated timing blinks (1s, 2s, etc.)
- ✅ Light mode high-contrast colors
- ✅ Dark mode text shadows for dark colors

## Usage in index.html

Replace the existing `applyColorNameDecorations` function with:

```javascript
import { setupKidLispHighlighting } from './js/monaco-kidlisp-highlighting.mjs';

// After Monaco editor is created:
const highlighter = setupKidLispHighlighting(editor, {
  enableTimingBlinks: true,
  lightModeHighContrast: isLightMode
});

// Start timing blinks animation (only when playing)
highlighter.startTimingBlinksLoop(() => isPlaying && !isPaused);
```

## Usage in device.html

Replace the custom `applyMonacoSyntaxHighlighting` function with:

```javascript
import { MonacoKidLispHighlighter } from './js/monaco-kidlisp-highlighting.mjs';

// Create highlighter instance
const highlighter = new MonacoKidLispHighlighter(deviceMonacoEditor, {
  enableTimingBlinks: true
});

// Apply decorations (call after each setValue or when source changes)
highlighter.applyDecorations();

// Optional: Start timing blinks loop
highlighter.startTimingBlinksLoop(() => true); // Always animate
```

## API Reference

### `MonacoKidLispHighlighter` class

Constructor:
```javascript
new MonacoKidLispHighlighter(editor, options)
```

Options:
- `enableTimingBlinks` (boolean): Enable timing token animations (default: true)
- `lightModeHighContrast` (boolean): Use high-contrast colors for light mode (auto-detected if not set)

Methods:
- `applyDecorations(forceUpdate = false)`: Apply syntax highlighting to current editor content
- `startTimingBlinksLoop(shouldAnimate)`: Start 60fps update loop for timing blinks
  - `shouldAnimate`: Function that returns true when timing should animate
- `stopTimingBlinksLoop()`: Stop the update loop
- `destroy()`: Clean up and remove all decorations

### `setupKidLispHighlighting` helper

Quick setup function that auto-updates on content changes:

```javascript
const highlighter = setupKidLispHighlighting(editor, options);
```

Returns a `MonacoKidLispHighlighter` instance.

## Migration Steps

### For device.html:

1. Add module import at the top of script:
```html
<script type="module">
import { MonacoKidLispHighlighter } from './js/monaco-kidlisp-highlighting.mjs';
```

2. Remove the old `applyMonacoSyntaxHighlighting` function (lines ~2770-2944)

3. Remove the `highlightKidlisp` HTML generation function (lines ~3226-3401)

4. Create highlighter after Monaco editor is initialized:
```javascript
let highlighter = null;
// After deviceMonacoEditor is created:
highlighter = new MonacoKidLispHighlighter(deviceMonacoEditor);
```

5. Replace calls to `applyMonacoSyntaxHighlighting(source, highlightedHtml)` with:
```javascript
highlighter.applyDecorations();
```

6. Add timing blinks loop (optional):
```javascript
highlighter.startTimingBlinksLoop(() => true);
```

### For index.html:

1. Add module import
2. Replace `applyColorNameDecorations` function with `setupKidLispHighlighting`
3. Update the continuous update loop to use `highlighter.startTimingBlinksLoop()`

## Benefits

- **DRY**: Single source of truth for syntax highlighting logic
- **Consistency**: Both editors use identical highlighting
- **Maintainable**: Changes in one place affect both
- **Simpler**: No HTML parsing needed - direct token-to-decoration mapping
- **Performance**: Optimized decoration updates
