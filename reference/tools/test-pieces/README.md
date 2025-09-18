# Test Pieces for Headless AC

This directory contains test pieces organized by their intended rendering mode:

## 📁 bakes/
Test pieces designed for single-frame rendering with `bake.mjs`
- Static images
- Single moment captures
- High-quality still renders

## 📁 tapes/  
Test pieces designed for animation recording with `tape.mjs`
- Animated sequences
- Time-based compositions
- Multi-frame recordings

## Usage

From the scripts directory:

```bash
# Bake single frames
node bake.mjs test-simple.mjs        # Auto-finds in bakes/
node bake.mjs bakes/test-simple.mjs  # Explicit path

# Record animations  
node tape.mjs test-complex.mjs 2000 30        # Auto-finds in tapes/
node tape.mjs tapes/test-complex.mjs 2000 30  # Explicit path
```

The scripts will automatically check the appropriate folder first if you provide just a filename.