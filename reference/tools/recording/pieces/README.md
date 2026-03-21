# Recording Pieces

This folder contains AC pieces that can be recorded using the orchestrator.

## Usage

Place your `.mjs` piece files here, then record them with:

```bash
# From the recording directory
node orchestrator.mjs piece-name duration-in-ms
```

## Example Pieces

### elcid-flyer.mjs
Event flyer piece with animated text and blur effects. Good for testing high-resolution rendering.

```bash
node orchestrator.mjs elcid-flyer 30000  # 30 second render
```

## Creating New Pieces

To add a new piece for recording:

1. Create a `.mjs` file in this folder
2. Export a `paint` function that takes `{ api }` parameter
3. Use AC's drawing API (wipe, ink, box, blur, etc.)
4. Run with: `node orchestrator.mjs your-piece-name duration`

## Tips

- Pieces should be self-contained and stateless when possible
- Use piece-level variables for animation state that needs to persist
- The orchestrator preserves state between frames automatically
- Higher resolutions work better for text and fine details