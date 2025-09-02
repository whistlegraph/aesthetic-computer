# KidLisp Singleton Refactor

## 🎯 Objective
Refactor the KidLisp system from multiple instances with complex embedded layer management to a single global instance integrated at the core level, similar to other AC systems like `tape.` and synth.

## 🔍 Current Problems
- Multiple KidLisp instances causing frame collision
- Complex embedded layer cache management
- Buffer context switching issues
- Inconsistent state management across instances
- Over-engineered evaluation logic

## 🏗️ New Architecture

### Single Global Instance
```javascript
// disk.mjs
let globalKidLispInstance = null;

function initializeGlobalKidLisp(api) {
  if (!globalKidLispInstance) {
    globalKidLispInstance = new KidLisp();
    globalKidLispInstance.setAPI(api); // Always use main API context
  }
}
```

### Simplified kidlisp() Function
```javascript
// Simple function that hooks into global instance
function kidlisp(source, options = {}) {
  if (!globalKidLispInstance) return;
  
  const { x = 0, y = 0, width, height, alpha = 255 } = options;
  
  // Create buffer for this call
  const buffer = createBuffer(width || api.screen.width, height || api.screen.height);
  
  // Execute on isolated buffer
  globalKidLispInstance.executeOnBuffer(source, buffer);
  
  // Paste result to main screen
  if (api.paste) {
    api.paste(buffer, x, y, alpha);
  }
}
```

## 📋 Implementation Plan

### Phase 1: Core Infrastructure (30 min)
1. **Create global instance in disk.mjs**
   - Add globalKidLispInstance variable
   - Add initialization function
   - Hook into main paint/act loop

2. **Simplify KidLisp class**
   - Remove embedded layer management
   - Remove multiple instance logic
   - Add executeOnBuffer method

### Phase 2: Refactor API Integration (45 min)
3. **Update kidlisp() function**
   - Remove complex embedded layer creation
   - Use simple buffer operations
   - Direct paste to main screen

4. **Remove complex caching**
   - Remove embeddedLayerCache
   - Remove frame collision logic
   - Remove shouldLayerEvaluate complexity

### Phase 3: Integration & Testing (30 min)
5. **Update disk.mjs integration**
   - Initialize global instance in boot
   - Hook into paint function
   - Remove renderProgrammaticLayers complexity

6. **Test cases**
   - Test kidlisp-in-js piece
   - Verify $bop and other embedded pieces work
   - Ensure no performance regression

### Phase 4: Cleanup (15 min)
7. **Remove obsolete code**
   - Remove embedded layer system
   - Remove multiple instance management
   - Clean up debug logging

## 🎯 Expected Benefits

### Performance
- Single instance = less memory overhead
- No frame collision checks
- Simplified evaluation logic
- Direct buffer operations

### Maintainability  
- Consistent with AC architecture
- Single source of truth
- Simpler debugging
- Less complex state management

### Functionality
- More reliable kidlisp() calls
- Better buffer management
- Consistent API context
- Cleaner integration

## 🧪 Testing Strategy

### Test Cases
1. **Basic kidlisp() calls** - Simple wipe, ink, drawing
2. **Buffer positioning** - x, y, width, height parameters
3. **Alpha blending** - Transparency support
4. **Complex expressions** - Multi-command sequences
5. **Embedded pieces** - $bop, $cow compatibility
6. **Performance** - No frame rate degradation

### Regression Testing
- Ensure existing pieces still work
- Verify no visual artifacts
- Check memory usage
- Validate API compatibility

## 📊 Success Criteria

✅ kidlisp() function works reliably
✅ No frame collision issues  
✅ Simplified codebase
✅ Better performance
✅ Consistent with AC architecture
✅ All existing functionality preserved

## 🚀 Deployment

1. Implement in feature branch
2. Test thoroughly with existing pieces
3. Performance benchmark
4. Merge to main after validation

---

**Estimated Total Time:** 2 hours
**Priority:** High - Fixes critical rendering issues
**Impact:** High - Simplifies architecture significantly
