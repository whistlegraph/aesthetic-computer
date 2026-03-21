# KidLisp GameBoy - Quick Reference

## 🎮 Current Status
✅ Working Bresenham line drawing in `test/line-demo.asm`

## 🔨 Build & Run Workflow

### 1. Build a ROM
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy
fish build.fish line-demo
```

### 2. Load in Browser
```bash
ac-run gameboy~line-demo
```

### 3. Watch Mode (auto-rebuild on save)
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy
fish watch-build.fish line-demo
```
Edit `test/line-demo.asm`, save, and it rebuilds automatically.
Then reload with `ac-run gameboy~line-demo`

## 📁 Available ROMs

See all available test ROMs:
```bash
ls test/*.asm | sed 's/test\///' | sed 's/\.asm$//'
```

Popular ones:
- `line-demo` - Bresenham X pattern ⭐
- `ascii-scroll` - Scrolling text
- `bounce-ball` - Bouncing ball physics  
- `starfield` - Animated stars
- `typewriter` - Text effect

## 🧪 Development Loop

1. **Edit** assembly in `test/<rom-name>.asm`
2. **Build** with `fish build.fish <rom-name>`
3. **Run** with `ac-run gameboy~<rom-name>`
4. Repeat!

Or use watch mode to skip step 2.

## 📚 Docs
- `BRESENHAM-SUCCESS.md` - How line drawing works
- `PROGRESS.md` - Full development history  
- `WORKFLOW.md` - Detailed workflow guide
- `README.md` - Project overview
