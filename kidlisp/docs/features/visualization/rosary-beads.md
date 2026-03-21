# Rosary Beads: A Cyclic S-Expression Visualizer

KidLisp programs loop—they're living, breathing cycles. This visualization treats each expression as a **bead** on a string, with the program's loop forming a continuous rosary.

## The Core Insight

```
Traditional Lisp view (tree):       Rosary view (cycle):
                                    
     (do                                 ┌──────────────┐
      ├── wipe                           │              │
      ├── ink                            ▼              │
      ├── line                      ○ wipe             │
      └── scroll)                   │                  │
                                    ▼                  │
                                    ● ink              │
                                    │                  │
                                    ▼                  │
                                    ◆ line             │
                                    │                  │
                                    ▼                  │
                                    ◇ scroll ──────────┘
                                    
                                    (loops back to wipe)
```

Because KidLisp runs in an animation loop, the program is **always cycling**. The rosary makes this visible.

---

## Bead Types

| Shape | Meaning |
|-------|---------|
| ○ | State change (wipe, ink, def) |
| ● | Drawing command (line, box, circle, point) |
| ◆ | Effect (scroll, blur, zoom) |
| ◇ | Timing (1s, 0.5s, later) |
| ◈ | Condition/branch (if, ?) |
| ⬡ | Loop within (repeat) |

---

## Real Examples from @jeffrey

### Example 1: `$mtz` - Simple Chaos

```kidlisp
red, ink white, line 0 0 100 100, scroll -1 4, ink (? red yellow blue), point ? ?
```

**Rosary diagram:**

```
        ┌─────────────────────────────────────┐
        │                                     │
        ▼                                     │
   ┌─────────┐                                │
   │ ○ red   │  (wipe red)                    │
   └────┬────┘                                │
        │                                     │
        ▼                                     │
   ┌──────────┐                               │
   │ ○ ink    │  (set color to white)         │
   │  white   │                               │
   └────┬─────┘                               │
        │                                     │
        ▼                                     │
   ┌──────────────┐                           │
   │ ● line       │  (draw diagonal)          │
   │ 0,0 → 100,100│                           │
   └──────┬───────┘                           │
        │                                     │
        ▼                                     │
   ┌────────────┐                             │
   │ ◆ scroll   │  (shift pixels)             │
   │  -1, 4     │                             │
   └──────┬─────┘                             │
        │                                     │
        ▼                                     │
   ┌────────────┐                             │
   │ ◈ ink      │  (random color)             │
   │ ? r/y/b    │                             │
   └──────┬─────┘                             │
        │                                     │
        ▼                                     │
   ┌────────────┐                             │
   │ ● point    │  (random position)          │
   │  ?, ?      │                             │
   └──────┬─────┘                             │
        │                                     │
        └─────────────────────────────────────┘
                    LOOP
```

**Compact form:**
```
○ red → ○ ink:white → ● line → ◆ scroll → ◈ ink:? → ● point:?
   ↑                                                       │
   └───────────────────────────────────────────────────────┘
```

---

### Example 2: `$gx6` - Nested Loop (48 lines)

```kidlisp
(wipe "navy")
  (repeat 48
    (ink "cyan")
    (line (random width) 0 (random width) height))
  (ink "magenta")
  (circle (/ width 2) (/ height 2) 56)
```

**Rosary with sub-loop:**

```
        ┌─────────────────────────────────────────────┐
        │                                             │
        ▼                                             │
   ┌──────────┐                                       │
   │ ○ wipe   │                                       │
   │  navy    │                                       │
   └────┬─────┘                                       │
        │                                             │
        ▼                                             │
   ┌────────────────────────────┐                     │
   │ ⬡ repeat 48                │                     │
   │  ┌──────────────────────┐  │                     │
   │  │  ○ ink cyan          │  │                     │
   │  │        ↓             │  │                     │
   │  │  ● line random→random│←─┤  (inner loop ×48)   │
   │  └──────────────────────┘  │                     │
   └────────────┬───────────────┘                     │
                │                                     │
                ▼                                     │
   ┌────────────────┐                                 │
   │ ○ ink magenta  │                                 │
   └───────┬────────┘                                 │
           │                                          │
           ▼                                          │
   ┌──────────────────┐                               │
   │ ● circle         │                               │
   │  center, r=56    │                               │
   └────────┬─────────┘                               │
            │                                         │
            └─────────────────────────────────────────┘
                         LOOP
```

The **sub-rosary** inside `repeat 48` runs 48 times before continuing!

---

### Example 3: `$puf` - Timed Events

```kidlisp
(1s (coat fade:black-red-rainbow-red-black:frame 64)) (0.3s (zoom 0.5)) (scroll 10)
```

**Rosary with timing beads:**

```
        ┌────────────────────────────────────────┐
        │                                        │
        ▼                                        │
   ┌────────────────────────┐                    │
   │ ◇ 1s                   │                    │
   │   └── ◆ coat           │  (every 1 second) │
   │       fade gradient    │                    │
   └───────────┬────────────┘                    │
               │                                 │
               ▼                                 │
   ┌────────────────────────┐                    │
   │ ◇ 0.3s                 │                    │
   │   └── ◆ zoom 0.5       │  (every 0.3 sec)  │
   └───────────┬────────────┘                    │
               │                                 │
               ▼                                 │
   ┌────────────┐                                │
   │ ◆ scroll   │                                │
   │    10      │  (every frame)                 │
   └──────┬─────┘                                │
          │                                      │
          └──────────────────────────────────────┘
```

**Timed beads** are like **weighted beads**—they only "drop" when their timer fires.

---

### Example 4: `$abg` - Repeat with Random Points

```kidlisp
blue, ink red, repeat 32 (point ? ?), scroll 1, (1s (zoom 1.1))
```

```
○ blue → ○ ink:red → ⬡ repeat 32 [● point:?,?] → ◆ scroll:1 → ◇ 1s [◆ zoom:1.1]
   ↑                                                                        │
   └────────────────────────────────────────────────────────────────────────┘

Expanded:

        ┌───────────────────────────────────────────────────┐
        │                                                   │
        ▼                                                   │
   [ ○ blue ]                                               │
        │                                                   │
        ▼                                                   │
   [ ○ ink red ]                                            │
        │                                                   │
        ▼                                                   │
   ┌─────────────────┐                                      │
   │ ⬡ repeat 32     │                                      │
   │  ┌───────────┐  │                                      │
   │  │ ● point   │  │  ← inner loop: 32 random points     │
   │  │   ?, ?    │←─┤                                      │
   │  └───────────┘  │                                      │
   └────────┬────────┘                                      │
            │                                               │
            ▼                                               │
   [ ◆ scroll 1 ]                                           │
            │                                               │
            ▼                                               │
   ┌────────────────┐                                       │
   │ ◇ 1s           │                                       │
   │  └── ◆ zoom    │  (triggers every second)             │
   │       1.1      │                                       │
   └────────┬───────┘                                       │
            │                                               │
            └───────────────────────────────────────────────┘
```

---

## Visual Language

### Linear Flow (Comma-separated)
```
○ ─── ○ ─── ● ─── ◆ ─── ○
      beads on a string
```

### Loop (The Rosary Circle)
```
    ○ ─── ○ ─── ●
    │           │
    │   MAIN    │
    │   LOOP    │
    │           │
    ◆ ─── ◇ ─── ◆
```

### Nested Loop (Sub-rosary)
```
    ○ ─── ⬡─────────⬡ ─── ●
          │ ○ ── ● │
          │   ↑    │
          │   │    │
          └───┘────┘
         (sub-loop)
```

### Conditional Branch (Fork in the string)
```
    ○ ─── ◈ ─┬─── ● ───┬─── ◆
             │   true  │
             │         │
             └─── ○ ───┘
               false
```

---

## Animation Ideas

### 1. **Current Position Marker**
A glowing bead shows where execution currently is:
```
○ ─── ○ ─── ●✧ ─── ◆ ─── ○
             ↑
         (evaluating now)
```

### 2. **Flow Animation**
Beads pulse in sequence, showing data flowing through:
```
Frame 1: ○* ─── ○ ─── ● ─── ◆
Frame 2: ○ ─── ○* ─── ● ─── ◆
Frame 3: ○ ─── ○ ─── ●* ─── ◆
Frame 4: ○ ─── ○ ─── ● ─── ◆*
```

### 3. **Bead Size = Execution Time**
Slow operations have bigger beads:
```
○ ─── ● ─── ●●● ─── ◆
           (blur takes longer)
```

### 4. **Wire Thickness = Data Size**
Thick strings carry more data:
```
○ ═══ repeat 100 ═══ ●
○ ─── point ─── ◆
```

---

## Physical Metaphor

Imagine the program as a **prayer bead necklace**:

- Each **bead** is an expression
- The **string** is execution order
- **Loops** form circles within circles
- **Timing** makes some beads "heavier" (they fall slower)
- **Conditions** create forks (like a rosary's decades)

You could even **physically build** programs:
- 3D print bead shapes
- String them on wire
- Arrange loops as actual circles

---

## Implementation Notes

### Parsing to Beads

```javascript
function toBeads(expr) {
  if (isAtom(expr)) {
    return { type: 'atom', value: expr, shape: '○' };
  }
  
  const [head, ...args] = expr;
  
  // Classify by head
  const shapes = {
    'wipe': '○', 'ink': '○', 'def': '○',        // state
    'line': '●', 'box': '●', 'circle': '●',      // draw
    'point': '●', 'write': '●',
    'scroll': '◆', 'blur': '◆', 'zoom': '◆',    // effect
    '1s': '◇', '0.5s': '◇', 'later': '◇',       // timing
    'if': '◈', '?': '◈',                         // branch
    'repeat': '⬡', 'loop': '⬡'                   // loop
  };
  
  return {
    type: head,
    shape: shapes[head] || '○',
    args: args.map(toBeads),
    isLoop: head === 'repeat' || head === 'loop'
  };
}
```

### Rendering

Could be:
- **Canvas 2D** - Simple, fast
- **SVG** - Scalable, crisp
- **Three.js** - 3D rosary you can rotate
- **ASCII** - Terminal-friendly (like `st` output)

---

## Why Rosary?

The rosary is:
1. **Meditative** - Programs as mantras, repeated endlessly
2. **Tactile** - You can "feel" each expression
3. **Cyclical** - Honest about what programs actually do
4. **Beautiful** - Ancient visual form meets modern code

> "In the rosary, repetition is not tedium but rhythm."

KidLisp programs are rhythms—patterns that repeat, evolve, and flow. The rosary visualization honors that.

---

*December 2024*
