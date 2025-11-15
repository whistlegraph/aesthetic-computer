# ✅ Bresenham Line Drawing - SUCCESS!

## Working Implementation

Created **line-demo.gb** - a working GameBoy ROM that implements Bresenham's line drawing algorithm!

### Key Features
- **Pure Bresenham algorithm** adapted from graph.mjs JavaScript implementation
- **Multiple line demonstrations**: diagonal, horizontal, vertical, shallow slope, steep slope
- **Tile-based rendering**: Uses the same 1-pixel tile technique from true-x.asm
- **WRAM variables**: Stores line state in $C000-$C009 to avoid complex stack operations
- **gbasm compatible**: No unsupported instructions, works around `neg`, `or [hl]`, etc.

### Algorithm Breakdown

```z80
DrawLine(x0, y0, x1, y1):
    dx = abs(x1 - x0)
    dy = -abs(y1 - y0)
    sx = (x0 < x1) ? 1 : -1
    sy = (y0 < y1) ? 1 : -1
    err = dx + dy
    
    while true:
        PlotPixel(x0, y0)
        
        if x0 == x1 && y0 == y1:
            break
            
        e2 = 2 * err
        
        if e2 >= dy:
            err += dy
            x0 += sx
            
        if e2 <= dx:
            err += dx
            y0 += sy
```

### Critical Insights

1. **Independent Conditions**: The two `if` statements (`e2 >= dy` and `e2 <= dx`) are NOT mutually exclusive - both can execute in the same iteration. This was a key bug in earlier attempts.

2. **Negative dy**: Bresenham uses `dy = -abs(y1 - y0)` (negative!), which affects comparisons.

3. **WRAM Storage**: gbasm's limitations with complex expressions made WRAM the best choice:
   - $C000: x0
   - $C001: y0
   - $C002: x1
   - $C003: y1
   - $C004: dx
   - $C005: dy
   - $C006: sx
   - $C007: sy
   - $C008: err
   - $C009: e2 (temporary)

4. **No `neg` instruction**: GameBoy SM83 CPU doesn't have `neg`, so we use `cpl` + `inc a` to negate values.

5. **Signed comparisons**: GameBoy Z80 comparisons with signed numbers require careful thought about which register holds which value.

### Lines Drawn

The demo ROM draws 5 lines:

1. **Diagonal** (0,0) → (19,18): Top-left to bottom-right
2. **Horizontal** (0,5) → (19,5): Straight across
3. **Vertical** (10,0) → (10,18): Straight down
4. **Shallow slope** (0,16) → (19,18): More horizontal than vertical
5. **Steep slope** (20,0) → (22,18): More vertical than horizontal

### Usage

```bash
# Assemble
cd kidlisp-gameboy
npx gbasm test/line-demo.asm -o test/line-demo.gb

# Deploy
cp test/line-demo.gb ../system/public/aesthetic.computer/gb-emulator/

# Load in browser (with hot-reload)
gameboy~line-demo
```

### References

- **JavaScript Bresenham**: `/system/public/aesthetic.computer/lib/graph.mjs` lines 2826-2866
- **ChibiAkumas GameBoy Tutorial**: https://www.chibiakumas.com/z80/platform1.php#LessonP9
- **Z80 Drawing Routines**: https://learn.cemetech.net/index.php/Z80:Drawing_Routines
- **Pixel plotting technique**: `true-x.asm` (tile-based pixel setting)

### Next Steps

This working implementation can be:
1. **Integrated into KidLisp compiler**: Generate `(line x0 y0 x1 y1)` code
2. **Optimized**: Inline pixel plotting, use registers instead of WRAM where possible
3. **Extended**: Add clipping (bounds checking), colors, line styles
4. **Combined with other primitives**: `(box)`, `(circle)`, etc.

### Lessons Learned

- **Study working examples first**: After multiple failed attempts with trial-and-error, studying the JavaScript Bresenham and ChibiAkumas tutorials led to success
- **gbasm syntax is limited**: No `neg`, `or [hl]`, complex expressions - work around it
- **GameBoy SM83 ≠ Z80**: Missing many Z80 instructions, need to adapt
- **WRAM is your friend**: When registers get complex, use memory
- **Independent conditionals**: Bresenham's two `if` statements are BOTH checked, not if/else

---

**Status**: ✅ WORKING - line-demo.gb successfully draws 5 Bresenham lines
**Date**: 2025-01-22
**Author**: AI assisted development via GitHub Copilot
