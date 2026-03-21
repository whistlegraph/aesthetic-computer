# Unified KidLisp Execution Pipeline Plan

## Problem Analysis

The current system has **divergent code paths** for KidLisp execution:

### Working Path: Embedded $code (✅ Perfect)
- Uses embedded layer system in KidLisp 
- Proper frame-by-frame evaluation
- Blur effects accumulate correctly
- Uses `isNestedInstance = true` for immediate execution

### Broken Path: JavaScript kidlisp() API (❌ Inconsistent)
- Creates separate KidLisp instances via disk.mjs
- Different buffer management
- Different frame execution
- Missing commands (wipe/ink/line not being called)
- Only sees blur from right side, nothing from left side

## Root Cause

The `kidlisp()` function in disk.mjs creates **completely separate execution contexts** instead of using the same pipeline as embedded layers. This causes:

1. **Execution Divergence**: Different evaluation logic
2. **Buffer Context Issues**: Separate buffer management
3. **Frame Loop Disconnection**: Not properly integrated with main frame execution
4. **Command Loss**: Some commands don't execute (wipe/ink/line missing)

## Proposed Solution: Unified Execution Pipeline

### Phase 1: Standardize on Embedded Layer System

Make `kidlisp()` JavaScript API use the **same code path** as embedded $code:

```javascript
// Current (broken):
kidlisp(x, y, w, h, code) → creates separate instance

// Proposed (unified):
kidlisp(x, y, w, h, code) → creates embedded layer using same pipeline
```

### Phase 2: Enhanced Instancing System in disk.mjs

Create a proper KidLisp instance manager that:

1. **Reuses Execution Pipeline**: All KidLisp execution goes through same code path
2. **Unified Buffer Management**: Same buffer context switching for all instances  
3. **Frame Loop Integration**: All instances participate in main frame execution
4. **Context Preservation**: Proper API context passing

### Phase 3: API Unification

Ensure these all use identical execution:
- Direct KidLisp pieces (.kidlisp files)
- Embedded $code syntax within KidLisp
- JavaScript kidlisp() API calls

## Implementation Strategy

### 1. Modify disk.mjs kidlisp() Function

Instead of creating separate instances, make it create embedded layers:

```javascript
function kidlisp(x, y, w, h, source) {
  // Use the same embedded layer creation as $code
  // Ensure proper buffer context
  // Connect to main frame execution loop
}
```

### 2. Extract Embedded Layer Logic

Create reusable embedded layer creation functions:
- `createEmbeddedKidLispLayer(bounds, source, options)`
- `executeEmbeddedLayer(layer, api, frameData)`
- `compositeEmbeddedLayers(layers, mainApi)`

### 3. Unified Frame Execution

All KidLisp execution should:
- Use same `isNestedInstance` logic
- Share buffer context management  
- Participate in unified frame timing
- Use consistent API wrapping

### 4. Debug and Validation

- Add execution path tracing
- Ensure command parity (wipe/ink/line work everywhere)
- Validate frame-by-frame consistency
- Test all three execution modes

## Benefits

1. **No Divergent Behavior**: All KidLisp execution works identically
2. **Easier Maintenance**: Single code path to debug/optimize
3. **Feature Parity**: All features work in all contexts
4. **Better Performance**: Shared execution infrastructure
5. **Consistent API**: Predictable behavior across usage patterns

## Success Metrics

- [ ] JavaScript `kidlisp()` calls execute wipe/ink/line commands
- [ ] Blur effects work consistently in all execution modes
- [ ] Frame-by-frame execution parity between embedded and API calls
- [ ] No missing commands in any execution path
- [ ] Identical debugging output for equivalent code

## Implementation Priority

**High Priority**: This blocks embedded KidLisp usage in JavaScript pieces, which is a core feature for the aesthetic computer ecosystem.

The current divergence makes it unpredictable when KidLisp will work correctly, undermining confidence in the system.
