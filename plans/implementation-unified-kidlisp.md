# Implementation Plan: Unified KidLisp Execution

## Phase 1: Immediate Fix - Use Embedded Layer System

### Current Problem
```javascript
// Current broken approach in disk.mjs:
kidlisp() -> creates instance -> calls pieceObject.paint(api) directly
```

### Solution
```javascript  
// New unified approach:
kidlisp() -> creates embedded layer -> uses same pipeline as $code
```

## Implementation Steps

### 1. Create Embedded Layer API in KidLisp
Add a public method to create embedded layers programmatically:

```javascript
// In kidlisp.mjs:
createProgrammaticEmbeddedLayer(source, x, y, width, height, options = {}) {
  // Use same logic as $code parsing but for programmatic creation
  // Return layer ID for tracking
}
```

### 2. Modify disk.mjs kidlisp() Function
Replace direct execution with embedded layer creation:

```javascript
// In disk.mjs:
kidlisp: (x, y, width, height, source) => {
  // Get main KidLisp instance (if exists) or create shared one
  const mainKidLispInstance = getOrCreateMainKidLispInstance();
  
  // Create embedded layer using same pipeline as $code
  const layerId = mainKidLispInstance.createProgrammaticEmbeddedLayer(
    source, x, y, width, height, { isNestedInstance: true }
  );
  
  // Layer will be rendered automatically in next frame via renderEmbeddedLayers()
  return layerId;
}
```

### 3. Shared Instance Management
Instead of separate instances per buffer, use a shared system:

```javascript
// Global KidLisp instance for JavaScript API calls
let sharedKidLispInstance = null;

function getOrCreateMainKidLispInstance() {
  if (!sharedKidLispInstance) {
    sharedKidLispInstance = new lisp.KidLisp();
    sharedKidLispInstance.isEmbeddedContext = true;
  }
  return sharedKidLispInstance;
}
```

### 4. Frame Integration
Ensure the shared instance participates in main frame execution:

```javascript
// In the main paint loop, ensure shared instance renders its layers
if (sharedKidLispInstance) {
  sharedKidLispInstance.renderEmbeddedLayers(api);
}
```

## Benefits of This Approach

1. **Same Code Path**: JavaScript kidlisp() uses identical execution as $code
2. **No Divergence**: All features work consistently 
3. **Proper Buffering**: Uses same buffer management as embedded layers
4. **Frame Integration**: Automatic participation in main frame execution
5. **Effect Persistence**: Blur and other effects accumulate correctly

## Testing Strategy

Create test cases that verify:
- JavaScript kidlisp() executes wipe/ink/line commands
- Blur effects persist across frames 
- Buffer contexts work correctly
- Performance is comparable to embedded $code

## Rollback Plan

If unified approach has issues:
1. Keep current separate instance approach as fallback
2. Add feature flag to switch between approaches
3. Gradually migrate once stable

## Success Criteria

✅ All console logs show wipe/ink/line execution for left buffer  
✅ Blur effects work identically in JavaScript API and $code  
✅ No missing commands in any execution path  
✅ Performance parity with current embedded system  

This approach eliminates the dual code paths that cause the current inconsistency.
