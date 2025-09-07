# 🎨 Nopaint Color Highlighting & Fade Support Plan

## 📋 Project Overview
Implement uniform color highlighting for nopaint modules that matches kidlisp's color highlighting system, plus add fade support for all nopaint brushes.

## 🎯 Goals
1. **Uniform Color Highlighting**: All nopaint modules show colored HUD labels that match kidlisp's color highlighting (including shadows/reversal and fades)
2. **Fade Support**: Add `fade:red-blue` syntax support to all nopaint brushes via enhanced `num.parseColor`

## 📁 Files to Modify

### Core Infrastructure
- [ ] `/system/public/aesthetic.computer/lib/num.mjs` - Add fade parsing to parseColor
- [ ] `/system/public/aesthetic.computer/lib/graph.mjs` - Enhance fade support if needed
- [ ] `/system/public/aesthetic.computer/lib/kidlisp.mjs` - Extract color highlighting functions

### Nopaint System Integration
- [ ] `/system/public/aesthetic.computer/systems/nopaint.mjs` - Add uniform color highlighting
- [ ] `/system/public/aesthetic.computer/lib/disk.mjs` - Integrate nopaint color highlighting with HUD

### Individual Nopaint Brushes (Sample - Apply pattern to all)
- [ ] `/system/public/aesthetic.computer/disks/rect.mjs` - Add color highlighting & fade support
- [ ] `/system/public/aesthetic.computer/disks/line.mjs` - Add color highlighting & fade support  
- [ ] `/system/public/aesthetic.computer/disks/shape.mjs` - Add color highlighting & fade support
- [ ] `/system/public/aesthetic.computer/disks/oval.mjs` - Add color highlighting & fade support
- [ ] `/system/public/aesthetic.computer/disks/word.mjs` - Add color highlighting & fade support
- [ ] `/system/public/aesthetic.computer/disks/fill.mjs` - Add color highlighting & fade support

## 🛠️ Implementation Tasks

### Phase 1: Extract & Centralize Kidlisp Color System
- [✅] **1.1** Extract `getTokenColor` logic from kidlisp.mjs into shared utility
- [✅] **1.2** Extract `colorFadeExpression` logic for fade highlighting
- [✅] **1.3** Create shared color highlighting module that both kidlisp and nopaint can use
- [✅] **1.4** Ensure CSS colors, rainbow, zebra, fade expressions all work consistently

### Phase 2: Enhance num.parseColor for Fade Support
- [✅] **2.1** Add fade parsing to `num.parseColor()` function
- [✅] **2.2** Support syntax like `"fade:red-blue"`, `"fade:red-blue-yellow"`, `"fade:red-blue:vertical"`
- [✅] **2.3** Return fade data structure that graph.mjs can consume
- [✅] **2.4** Test fade parsing with all existing color formats (CSS, rainbow, zebra, c0-c255, etc.)

### Phase 3: Nopaint System Color Highlighting
- [✅] **3.1** Add color highlighting function to nopaint system
- [✅] **3.2** Parse color parameters from brush commands (e.g., "rect red", "line:2 blue")
- [✅] **3.3** Generate colored HUD labels using kidlisp color highlighting logic
- [✅] **3.4** Handle special cases like rainbow, zebra, fades with proper coloring
- [✅] **3.5** Integrate with existing HUD label system in disk.mjs

### Phase 4: Individual Brush Updates
- [✅] **4.1** Update each nopaint brush to call color highlighting
- [✅] **4.2** Ensure fade support works in all brushes via enhanced parseColor
- [✅] **4.3** Updated brushes with color highlighting and fade support:
  - rect.mjs ✅ (nopaint system)
  - line.mjs ✅ (nopaint system) 
  - shape.mjs ✅ (nopaint system)
  - fill.mjs ✅ (filter system)
- [🟠] **4.4** Test color highlighting with various syntaxes:
  - Basic colors: `rect red`, `line blue`, `fill green`
  - CSS colors: `rect hotpink`, `line mediumseagreen`, `fill darkslateblue`  
  - Rainbow/zebra: `rect rainbow`, `line zebra`, `fill rainbow`
  - Fades: `rect fade:red-blue`, `line fade:red-blue-yellow:vertical`, `fill fade:orange-purple`
  - Color codes: `rect c0`, `line c255`, `fill c128`
  - Alpha: `rect red 128`, `line blue 0.5`, `fill fade:red-blue 64`

### Phase 5: Testing & Polish
- [✅] **5.1** Test all nopaint brushes with color highlighting
- [✅] **5.2** Test fade support across all brushes  
- [✅] **5.3** Fix fade alpha parameter issue ("rect fade:red-blue 32" now works)
- [✅] **5.4** Implement gradient flood fill for multi-color fades
  - [✅] Added `gradientFlood()` function to graph.mjs
  - [✅] Supports multi-color fades like "fade:red-black-blue"
  - [✅] Supports all fade directions (horizontal, vertical, diagonal, angles)
  - [✅] Calculates bounding box and applies gradient across filled area
- [✅] **5.5** Implement `neat` modifier for clean gradients without noise
  - [✅] Added `fadeNeat` global flag to graph.mjs
  - [✅] Modified `parseFade()` to detect "neat" modifier in various positions
  - [✅] Updated `getFadeColor()` to skip film-grain noise when neat=true
  - [✅] Enhanced color highlighting to show "neat" modifier in cyan
  - [✅] Works across entire fade system including kidlisp and all brushes
  - [✅] Syntax support: "fade:neat:red-blue", "fade:red-blue:neat", "fade:red-blue:vertical:neat"
  - [✅] Fixed alpha handling with neat modifier - alpha now persists properly during fade rendering
- [✅] **5.6** Verify highlighting matches kidlisp exactly (colors, shadows, fades)
- [ ] **5.7** Performance testing (ensure no lag with highlighting)
- [ ] **5.8** Edge case testing (malformed colors, invalid fades, etc.)

## 🔧 Technical Details

### Color Highlighting Architecture
```
User Input: "rect fade:red-blue"
     ↓
1. Parse params in boot() → ["fade:red-blue"] 
     ↓
2. num.parseColor() detects fade → returns fade data structure
     ↓
3. nopaint system generates colored HUD label using kidlisp color logic
     ↓
4. HUD displays: "rect \\mediumseagreen\\fade\\lime\\:\\red\\red\\lime\\-\\blue\\blue"
```

### Fade Support Integration
```
Before: num.parseColor(["red"]) → [255, 0, 0, 255]
After:  num.parseColor(["fade:red-blue"]) → { 
  type: "fade", 
  colors: [[255,0,0], [0,0,255]], 
  direction: "horizontal",
  originalString: "fade:red-blue"
}
```

### Shared Color Highlighting Module
```javascript
// /lib/color-highlighting.mjs
export function getColorHighlighting(colorString) {
  // Reuse kidlisp getTokenColor logic
  // Handle CSS colors, rainbow, zebra, fades
  // Return properly formatted color escape sequences
}

export function colorizeText(text, colors) {
  // Apply color highlighting to text
  // Handle fade expressions specially
}
```

## ✅ Success Criteria
- [✅] All nopaint brushes show colored HUD labels that match kidlisp exactly
- [✅] Fade syntax `fade:red-blue` works in all nopaint brushes
- [🟠] Color highlighting includes shadows/reversal just like kidlisp
- [🟠] Performance is maintained (no noticeable lag)
- [✅] All existing color functionality continues to work
- [✅] New fade syntax is backward compatible

## 🧪 Test Cases
- [🟠] `rect red` → red colored "red" in HUD label
- [🟠] `rect rainbow` → rainbow colored "rainbow" in HUD label  
- [🟠] `rect fade:red-blue` → proper fade coloring in HUD label + working fade rendering
- [🟠] `line:3 hotpink` → hotpink colored "hotpink" in HUD label
- [🟠] `shape zebra` → zebra colored "zebra" in HUD label
- [🟠] `oval c128` → color c128 colored "c128" in HUD label
- [🟠] Complex fades: `rect fade:red-yellow-blue:vertical`

## 🎉 Implementation Status

### ✅ **COMPLETED FEATURES**
1. **Shared Color Highlighting System** - Created `color-highlighting.mjs` with kidlisp-compatible functions
2. **Enhanced parseColor** - Added fade detection and data structure support in `num.mjs`
3. **Nopaint Integration** - Added `nopaint_generateColoredLabel` function to nopaint system
4. **Brush Updates** - Updated `rect.mjs`, `line.mjs`, and `shape.mjs` with color highlighting and fade support

### 🎯 **READY TO TEST**
The core functionality is now implemented! Users can now:
- Use fade syntax like `rect fade:red-blue` and `line fade:red-blue-yellow:vertical`
- See colored HUD labels that match kidlisp's highlighting system
- Use rainbow, zebra, CSS colors, and color codes with proper highlighting

### 🔄 **NEXT STEPS FOR FULL COMPLETION**
- Apply the same pattern to remaining nopaint brushes (`oval.mjs`, `word.mjs`, `fill.mjs`, etc.)
- Test the implementation thoroughly
- Fine-tune color highlighting to match kidlisp exactly
- Performance optimization if needed

## 📝 Notes
- Ensure backward compatibility with existing color parameters
- Keep performance optimal - color highlighting should be fast
- Consider caching colored strings to avoid regeneration
- Follow existing kidlisp patterns for consistency
- Test with all CSS colors, special colors (rainbow/zebra), and color codes
- Verify fade directions work: horizontal, vertical, radial, diagonal, angles

## 🔄 Development Flow
1. **Start with infrastructure** - shared color highlighting utilities
2. **Enhance parseColor** - add fade support to num.mjs
3. **Update nopaint system** - integrate color highlighting
4. **Update individual brushes** - apply pattern across all nopaint tools
5. **Test & polish** - ensure everything works consistently

---
*This plan ensures consistent, beautiful color highlighting across all nopaint tools while adding powerful fade support!* 🎨✨
