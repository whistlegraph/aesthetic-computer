# GameBoy Arbitrary Line Drawing for KidLisp

The `true-x.gb` ROM demonstrates the **core pixel-setting technique** needed for arbitrary Bresenham lines.

## How It Works

### Current Implementation (true-x.asm)
- Creates 2 pre-made tiles with diagonal pixel patterns
- Places these tiles along diagonal paths in the tilemap
- Result: Clean 1-pixel X pattern

### For Arbitrary Lines (KidLisp compiler would generate)

To implement `(line x0 y0 x1 y1)`, the KidLisp compiler would generate assembly that:

1. **Allocates unique tiles** for each 8×8 screen region (360 tiles for 20×18 grid)
2. **Runs Bresenham algorithm** to calculate which pixels to set
3. **For each pixel** along the line:
   - Calculate which tile: `tile_x = x / 8`, `tile_y = y / 8`
   - Calculate pixel within tile: `px = x % 8`, `py = y % 8`
   - Calculate VRAM address: `$8000 + (tile_index * 16) + (py * 2)`
   - Calculate bit mask: `1 << (7 - px)`
   - Set the bit in both bytes (GameBoy uses 2 bytes per row)

## Example Assembly Pseudocode

```assembly
; Bresenham line drawing
DrawLine: ; B,C = x0,y0  D,E = x1,y1
  ; Calculate dx, dy, error
  ; For each step:
    call SetPixel  ; Plot current point
    ; Update x, y based on Bresenham error
    
SetPixel: ; B=x, C=y
  ; tile_index = (y/8) * 20 + (x/8)
  ; vram_addr = $8000 + (tile_index * 16) + ((y%8) * 2)
  ; bit_mask = $80 >> (x % 8)
  ; OR the bit into VRAM bytes
```

## Key Insight

GameBoy has **no framebuffer** - everything is tile-based. But by:
- Assigning each 8×8 region its own unique tile
- Directly modifying tile pixel data in VRAM
- Using bit operations to set individual pixels

We can achieve **arbitrary pixel-level drawing** just like a framebuffer!

## Performance Notes

- Each `SetPixel` call: ~50-100 CPU cycles
- A 100-pixel line: ~5000-10000 cycles
- At 4MHz CPU: Can draw many lines per frame

This is exactly how the KidLisp→GameBoy compiler would implement graphics primitives!
