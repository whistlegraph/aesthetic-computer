# KidLisp Integration with Aesthetic Computer Painting API

## Executive Summary

This document outlines a comprehensive plan to integrate kidlisp support into the aesthetic computer's common painting API. The goal is to enable a new `kidlisp()` function that creates isolated kidlisp rendering contexts within normal JavaScript pieces, allowing for dynamic lisp-based graphics generation.

## Current Architecture Analysis

### 1. KidLisp System (`/lib/kidlisp.mjs`)

**Core Components:**
- `KidLisp` class: Main interpreter with environment management
- Global environment with painting commands (`ink`, `wipe`, `line`, `box`, etc.)
- Embedded layer system with buffer isolation
- Performance optimizations and caching
- Source code compilation and evaluation

**Key Features:**
- Multiple environment support with `getGlobalEnv()`
- Embedded buffer rendering via `createEmbeddedLayerFromSource()`
- API wrapping with buffer switching via `page()` function
- Color processing and graphic primitives
- Isolation through separate `kidlispInstance` per layer

### 2. Common Painting API (`/lib/disk.mjs`)

**Core Components:**
- `$commonApi`: Main API object with all system functions
- `$paintApi`: Core painting functions (`write`, `form`, etc.)
- `$paintApiUnwrapped`: Direct graphics functions (`ink`, `wipe`, `line`, etc.)
- `Painting` class: Buffer management with `api.painting()` factory

**Key Features:**
- `painting(width, height, callback)`: Creates isolated paint buffers
- `page(buffer)`: Switches active painting context
- API composition with buffer-specific wrappers
- Screen and buffer management through `$activePaintApi`

### 3. Piece Structure (e.g., `wipppps.mjs`)

**Interaction Pattern:**
```javascript
function paint({ wipe, ink, line, box, painting, page, paste, ... }) {
  // Pieces receive destructured painting functions
  // Functions operate on current active buffer
}
```

## Implementation Plan

### Phase 1: Core `kidlisp()` Function

#### 1.1 Function Signature
```javascript
kidlisp(x, y, width, height, source, options = {})
```

**Parameters:**
- `x, y`: Position to paste the kidlisp buffer
- `width, height`: Dimensions of the kidlisp rendering buffer
- `source`: KidLisp source code string
- `options`: Configuration object

#### 1.2 Integration Point
Add to `$paintApiUnwrapped` in `/lib/disk.mjs`:

```javascript
const $paintApiUnwrapped = {
  // ... existing functions
  kidlisp: function(x, y, width, height, source, options = {}) {
    return renderKidLispBuffer(x, y, width, height, source, options);
  }
};
```

### Phase 2: KidLisp Buffer Manager

#### 2.1 Buffer Creation and Isolation

```javascript
function renderKidLispBuffer(x, y, width, height, source, options = {}) {
  // 1. Create isolated kidlisp instance
  const kidlispInstance = new KidLisp();
  
  // 2. Create isolated painting buffer
  const kidlispBuffer = $activePaintApi.painting(width, height, (bufferApi) => {
    // 3. Create wrapped API for kidlisp
    const kidlispAPI = createKidLispAPIWrapper(bufferApi, options);
    
    // 4. Execute kidlisp code in isolated context
    try {
      const parsedCode = kidlispInstance.parse(source);
      kidlispInstance.evaluate(parsedCode, kidlispAPI, kidlispInstance.getGlobalEnv());
    } catch (error) {
      console.error('KidLisp execution error:', error);
      // Render error message in buffer
      bufferApi.wipe(64, 0, 0); // Dark red background
      bufferApi.ink(255, 255, 255).write('ERROR', 10, 10);
    }
  });
  
  // 5. Paste buffer to main canvas
  $activePaintApi.paste(kidlispBuffer, x, y);
  
  return kidlispBuffer; // Allow for further manipulation
}
```

#### 2.2 API Wrapper Creation

```javascript
function createKidLispAPIWrapper(bufferApi, options = {}) {
  const kidlispEnv = new KidLisp().getGlobalEnv();
  
  // Merge buffer API with kidlisp environment
  const wrappedAPI = {
    // Core graphics from buffer API
    ...bufferApi,
    
    // KidLisp-specific commands
    ...kidlispEnv,
    
    // Screen context (bounded to kidlisp buffer)
    screen: {
      width: bufferApi.screen?.width || 256,
      height: bufferApi.screen?.height || 256,
      pixels: bufferApi.screen?.pixels
    },
    
    // Restricted API (no system access)
    system: undefined,
    net: undefined,
    
    // Configurable options
    ...options.additionalAPI
  };
  
  return wrappedAPI;
}
```

### Phase 3: Enhanced Features

#### 3.1 Caching System
```javascript
const kidlispCache = new Map();

function getCachedKidLispResult(source, width, height) {
  const cacheKey = `${source}-${width}x${height}`;
  return kidlispCache.get(cacheKey);
}

function setCachedKidLispResult(source, width, height, buffer) {
  const cacheKey = `${source}-${width}x${height}`;
  kidlispCache.set(cacheKey, buffer);
}
```

#### 3.2 Animation Support
```javascript
kidlisp(x, y, width, height, source, { 
  frame: paintCount,      // Pass frame number for animations
  time: performance.now(), // Pass time for time-based effects
  animated: true          // Enable per-frame evaluation
});
```

#### 3.3 Error Handling and Debugging
```javascript
kidlisp(x, y, width, height, source, {
  debug: true,           // Show debug info
  errorMode: 'visual',   // 'visual', 'console', 'silent'
  fallback: '(wipe red)' // Fallback code on error
});
```

### Phase 4: Performance Optimizations

#### 4.1 Lazy Evaluation
- Only recompile kidlisp when source changes
- Cache parsed AST for repeated calls
- Implement dirty checking for animated content

#### 4.2 Buffer Pooling
```javascript
const bufferPool = [];

function getPooledBuffer(width, height) {
  const existing = bufferPool.find(b => b.width === width && b.height === height);
  if (existing) {
    bufferPool.splice(bufferPool.indexOf(existing), 1);
    // Clear buffer for reuse
    existing.api.wipe(0, 0, 0, 0);
    return existing;
  }
  return createNewBuffer(width, height);
}
```

#### 4.3 Selective Rendering
```javascript
kidlisp(x, y, width, height, source, {
  renderWhen: (frame) => frame % 10 === 0, // Only render every 10th frame
  staticCache: true // Cache static content
});
```

## Implementation Sequence

### Step 1: Basic Integration (Week 1)
1. Add basic `kidlisp()` function to `$paintApiUnwrapped`
2. Implement `renderKidLispBuffer()` with minimal features
3. Create `createKidLispAPIWrapper()` for API isolation
4. Test with simple kidlisp programs: `(wipe blue)`, `(ink red) (box 10 10 50 50)`

### Step 2: Buffer Management (Week 2)
1. Implement proper buffer isolation using existing `painting()` system
2. Add error handling and visual error reporting
3. Test buffer compositing and transparency
4. Validate API isolation (no system access from kidlisp context)

### Step 3: Advanced Features (Week 3)
1. Add caching system for performance
2. Implement animation support with frame/time parameters
3. Add debugging and error visualization options
4. Performance profiling and optimization

### Step 4: Integration Testing (Week 4)
1. Test with complex kidlisp programs
2. Validate integration with existing pieces like `wipppps.mjs`
3. Performance testing with multiple concurrent kidlisp buffers
4. Memory leak detection and cleanup

## Usage Examples

### Basic Usage
```javascript
// In any aesthetic computer piece
function paint({ kidlisp, ... }) {
  // Simple static kidlisp graphic
  kidlisp(100, 100, 64, 64, '(wipe blue) (ink yellow) (box 10 10 40 40)');
}
```

### Animation
```javascript
function paint({ kidlisp, paintCount, ... }) {
  // Animated kidlisp content
  kidlisp(0, 0, 256, 256, `
    (wipe black)
    (ink rainbow)
    (def t ${paintCount})
    (circle (+ 128 (* 50 (cos (* t 0.1)))) 128 20)
  `, { frame: paintCount });
}
```

### Multiple Buffers
```javascript
function paint({ kidlisp, ... }) {
  // Background layer
  kidlisp(0, 0, screen.width, screen.height, '(wipe "fade:blue-purple")');
  
  // Foreground elements
  kidlisp(50, 50, 100, 100, '(ink white) (circle 50 50 30)');
  kidlisp(200, 50, 100, 100, '(ink red) (box 0 0 100 100)');
}
```

## Technical Considerations

### Security
- KidLisp contexts must be isolated from system APIs
- No access to `net`, `system`, `store`, or other sensitive functions
- File system access restrictions

### Performance
- Buffer creation overhead mitigation through pooling
- Smart caching based on source code and parameters
- Frame-rate-aware rendering for animations

### Memory Management
- Automatic cleanup of unused buffers
- Garbage collection integration
- Memory usage monitoring and limits

### Compatibility
- Maintain backward compatibility with existing pieces
- No breaking changes to current painting API
- Progressive enhancement model

## TODO Progress Tracking

### Current Sprint: Basic Prototype Testing ✅ STARTED
- [x] Create kidlisp-in-js.mjs based on blank.mjs template  
- [x] Add KidLisp import and initialization
- [x] Implement basic kidlispBasic() prototype function
- [x] Add buffer creation with kidlisp evaluation
- [ ] Test the piece in browser and debug any issues
- [ ] Validate kidlisp parsing and evaluation works
- [ ] Test API isolation and error boundaries
- [ ] Confirm painting buffer integration works

### Next Sprint: Core Functionality
- [ ] Refine kidlisp() function signature and behavior
- [ ] Add proper error handling and visual feedback  
- [ ] Implement buffer caching for performance
- [ ] Test complex kidlisp programs and graphics
- [ ] Validate memory management and cleanup
- [ ] Performance profiling and optimization

### Future Sprints: Production Integration
- [ ] Integrate kidlisp() into common API (disk.mjs)
- [ ] Add to $paintApiUnwrapped for all pieces
- [ ] Documentation and examples
- [ ] Integration testing across multiple pieces
- [ ] Performance benchmarks and optimization

## Implementation Notes

### Phase Status
- **Phase 1: Basic Prototype** - ✅ IN PROGRESS 
- **Phase 2: API Integration** - ⏳ PENDING
- **Phase 3: Production Implementation** - ⏳ PLANNED

### Current Prototype: kidlisp-in-js.mjs
Created a test piece that:
- Imports KidLisp class from ../lib/kidlisp.mjs
- Initializes kidlisp instance in boot()
- Tests buffer creation with kidlisp evaluation
- Implements prototype kidlispBasic() function
- Includes error handling and visual feedback

Ready to test in browser and iterate on the implementation!

## Future Enhancements

1. **Interactive KidLisp**: Mouse/touch event forwarding to kidlisp buffers
2. **3D KidLisp**: Integration with form system for 3D graphics
3. **Network KidLisp**: Remote kidlisp code execution and sharing
4. **KidLisp Compiler**: AOT compilation for performance-critical applications
5. **Visual KidLisp Editor**: In-browser code editor with live preview

This integration would provide a powerful way to embed dynamic, programmable graphics within aesthetic computer pieces while maintaining isolation and performance.
