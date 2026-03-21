# KidLisp Chaos Mode 🌀

**A playful fallback evaluator for invalid/random input**

## Status: ✅ Phase 1 & 2 Implemented

- **Detection**: `isChaoticSource()` function analyzes input for validity
- **Evaluation**: `evaluateChaos()` generates 6 visual modes from chaotic input
- **Integration**: Hooks into `module()` before parsing, routes to chaos evaluator

## Problem Statement

When the KidLisp editor receives scrambled or random text (e.g., during decryption animations, or when users experiment with random characters), the interpreter throws many confusing errors:

```
❌ Unknown KidLisp word: s7a
❌ Unknown KidLisp word: _f
❗ Invalid `<`. Wrong number of arguments.
... (repeated many times)
```

This is:
1. **Scary** for new users experimenting
2. **Noisy** in the console
3. **Missed opportunity** for creative play

## Solution: Chaos Mode

Instead of failing loudly, detect "chaotic" input and route it through a **Chaos Evaluator** that produces visual output from any input.

## Implementation

### Phase 1: Detection ✅

Located in `kidlisp.mjs` at module level:

```javascript
// Exported for testing and external use
export { isChaoticSource, KIDLISP_VOCABULARY, KIDLISP_FUNCTIONS, KIDLISP_COLORS };

function isChaoticSource(source) {
  // Returns: { isChaotic: boolean, confidence: number, reason: string, stats: object }
  // Detection criteria:
  // 1. Low word recognition ratio (< 30% known KidLisp words/colors)
  // 2. High special character ratio (> 50% non-alphanumeric)
  // 3. Severely unbalanced parentheses (ratio > 3:1)
  // 4. Very short invalid inputs (< 3 chars)
}
```

### Phase 2: Chaos Evaluator ✅

The `evaluateChaos(source, api)` method in the KidLisp class generates one of 6 visual modes:

| Mode | Name | Description |
|------|------|-------------|
| 0 | Rainbow Static | Each character becomes a colored pixel |
| 1 | Pulsing Circles | Concentric circles based on text length |
| 2 | Wave Interference | Animated sine wave pattern |
| 3 | DNA Helix | Two strands based on character values |
| 4 | Matrix Rain | Falling green columns |
| 5 | Kaleidoscope | Rotating symmetric pattern |

The mode is deterministically chosen from the source hash, so the same input always produces the same visual.

### Integration Point

In `module()` method, before calling `parse()`:

```javascript
const chaosResult = isChaoticSource(source);
if (chaosResult.isChaotic) {
  console.log(`🌀 Chaos mode detected (${Math.round(chaosResult.confidence * 100)}% confidence)`);
  this.chaosMode = { active: true, source, ...chaosResult };
  return [['__chaos__', source]];  // Special AST that triggers chaos evaluator
}
```

In `evaluate()` method, at the top:

```javascript
if (Array.isArray(parsed) && parsed[0]?.[0] === '__chaos__') {
  return this.evaluateChaos(parsed[0][1], api);
}
```

## Phase 3: Console Integration (TODO)

**Goal**: Turn chaotic input into visual output.

```javascript
// New module: kidlisp/chaos.mjs

export function evaluateChaos(source, api) {
  const { wipe, ink, line, box, write, screen } = api;
  
  // Use source as seed for deterministic randomness
  const seed = hashString(source);
  const rng = seededRandom(seed);
  
  // Extract visual properties from source
  const colors = extractColors(source);
  const shapes = extractShapes(source);
  const intensity = source.length;
  
  // Generate visual based on source characteristics
  wipe(colors.background);
  
  // Draw based on character distribution
  for (let i = 0; i < source.length; i++) {
    const char = source[i];
    const charCode = char.charCodeAt(0);
    
    // Map character to visual element
    const x = (charCode * 7 + i * 13) % screen.width;
    const y = (charCode * 11 + i * 17) % screen.height;
    const size = (charCode % 20) + 5;
    
    // Color from character
    const hue = (charCode * 137) % 360;
    ink(hslToRgb(hue, 80, 60));
    
    // Shape from character type
    if (/[a-z]/i.test(char)) {
      box(x, y, size, size);  // Letters = boxes
    } else if (/\d/.test(char)) {
      // Numbers = circles (if we have circle, else box)
      box(x - size/2, y - size/2, size, size);
    } else if (/[()[\]{}]/.test(char)) {
      // Brackets = lines connecting
      line(x, y, screen.width - x, screen.height - y);
    } else {
      // Special chars = scattered pixels
      for (let j = 0; j < 5; j++) {
        const px = x + (rng() * 20 - 10);
        const py = y + (rng() * 20 - 10);
        box(px, py, 2, 2);
      }
    }
  }
  
  // Optional: Display the source text itself
  ink(colors.foreground);
  write(source.slice(0, 50), 10, screen.height - 20);
  
  return { chaosMode: true, seed };
}

function hashString(str) {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    hash = ((hash << 5) - hash) + str.charCodeAt(i);
    hash |= 0;
  }
  return hash;
}

function seededRandom(seed) {
  return function() {
    seed = (seed * 1103515245 + 12345) & 0x7fffffff;
    return seed / 0x7fffffff;
  };
}
```

### Phase 3: Integration Points

#### 3.1 In `kidlisp.mjs` module function

```javascript
export function module(source, api) {
  // Early chaos detection
  if (isChaoticSource(source)) {
    console.log('🌀 Chaos mode activated');
    return {
      paint: () => evaluateChaos(source, api),
      sim: () => {},
      act: () => {},
      chaosMode: true
    };
  }
  
  // Normal parsing continues...
  const parsed = parse(source);
  // ...
}
```

#### 3.2 In `kidlisp.com/index.html`

```javascript
// Before sending to iframe, check for chaos
import { isChaoticSource } from './kidlisp/chaos.mjs';

// In updatePreview():
if (isChaoticSource(code)) {
  // Show chaos indicator in UI
  showChaosIndicator();
  // Don't suppress - let it run through chaos evaluator
}
```

#### 3.3 Console Output

Instead of error spam, show:
```
🌀 Chaos mode: interpreting "8dk6:i` b*|o s7a~g" as visual noise
```

### Phase 4: Chaos Visual Modes

Different chaos styles based on input characteristics:

| Input Pattern | Visual Style |
|--------------|--------------|
| Mostly letters | Text scatter / waterfall |
| Mostly numbers | Numeric grid / matrix rain |
| Heavy punctuation | Line art / connections |
| Mixed/random | Noise field / static |
| Repeating patterns | Tiles / fractals |

### Phase 5: Educational Hooks

When in chaos mode, occasionally surface hints:

```javascript
const CHAOS_HINTS = [
  "Try: ink red, box 50 50 100 100",
  "Tip: Commands are separated by commas",
  "Start simple: wipe blue",
  "Colors: red, blue, green, yellow, purple...",
];

// Show hint after 3 seconds of chaos
setTimeout(() => {
  if (stillInChaosMode) {
    showHint(CHAOS_HINTS[Math.floor(Math.random() * CHAOS_HINTS.length)]);
  }
}, 3000);
```

## API Surface

```javascript
// kidlisp.mjs exports
export { 
  isChaoticSource,   // (source: string) => boolean
  evaluateChaos,     // (source: string, api: API) => ChaosResult
  CHAOS_THRESHOLD,   // Configurable detection sensitivity
};

// Types
interface ChaosResult {
  chaosMode: true;
  seed: number;
  visualStyle: 'scatter' | 'matrix' | 'lines' | 'noise' | 'tiles';
}
```

## Benefits

1. **No scary errors** for beginners experimenting
2. **Always produces output** - encourages play
3. **Deterministic** - same input = same visual (good for sharing)
4. **Educational** - hints guide users toward valid syntax
5. **Fun** - turns mistakes into art
6. **Performance** - skips expensive parsing for obvious chaos

## Future Ideas

- **Chaos → Valid**: Offer to "fix" chaotic input into similar valid code
- **Chaos Gallery**: Save interesting chaos outputs
- **Chaos Remix**: Let users intentionally trigger chaos mode
- **Sound**: Chaos mode could also produce audio from input
- **Animation**: Chaos visuals could animate based on character sequence

## Implementation Order

1. ✅ `isChaoticSource()` detector - Implemented with confidence scoring
2. ✅ Basic `evaluateChaos()` visual generator - 6 visual modes
3. ✅ Integration in `kidlisp.mjs` module - Before parsing
4. ✅ Console message cleanup - Shows `🌀 Chaos mode` instead of errors
5. ✅ Multiple visual styles - Rainbow, circles, waves, DNA, matrix, kaleidoscope
6. 🔲 Educational hints (Phase 3)
7. 🔲 kidlisp.com UI indicator (Phase 3)
8. 🔲 Chaos gallery/sharing (Future)

---

*"Every input is valid. Some are just more chaotic than others."*
