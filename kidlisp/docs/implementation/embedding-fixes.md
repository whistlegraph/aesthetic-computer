# KidLisp Embedding Fix Plan

## Problem Analysis

Based on the console logs and code investigation, I've identified several critical issues with the current KidLisp embedding system:

### 1. **API Parameter Mismatch in disk.mjs**
- **Issue**: `disk.mjs` calls `kidlispInstance.module(source, kidlispAPI)` but the `module()` method expects `(source, isLispFile)` as parameters
- **Root Cause**: The KidLisp `module()` method returns a piece object with `boot`, `paint`, and `sim` functions that receive the API, rather than accepting the API directly
- **Impact**: The kidlispAPI is being passed as `isLispFile` parameter, breaking the execution flow

### 2. **Async $code Fetching Not Handled Properly**
- **Issue**: When `$bop` is detected, it starts an async fetch but the current frame returns before the code is loaded
- **Root Cause**: The `module()` method in KidLisp tries to handle $codes by redirecting to navigation instead of waiting for async resolution
- **Impact**: $codes never execute, causing infinite "loading" state

### 3. **Buffer Context vs Piece Context Mismatch**
- **Issue**: KidLisp is designed to run as a full piece, but disk.mjs is trying to run it in a limited buffer context
- **Root Cause**: The embedded kidlisp() function creates a painting buffer but doesn't provide the full piece lifecycle (boot/paint/sim)
- **Impact**: KidLisp initialization and state management is broken

## Fix Strategy

### Phase 1: Immediate API Fix (disk.mjs)
1. **Fix the module() call**: Change `kidlispInstance.module(source, kidlispAPI)` to properly handle the returned piece object
2. **Create proper piece lifecycle**: Call boot, then paint with the API
3. **Handle API parameter correctly**: Pass `false` for `isLispFile` parameter

### Phase 2: Async $code Support 
1. **Implement proper async handling**: Modify the embedded KidLisp to wait for $code resolution
2. **Add loading states**: Show loading indicator while fetching cached codes
3. **Cache management**: Ensure proper cache invalidation and memory management

### Phase 3: Buffer Context Enhancement
1. **Minimal piece environment**: Create a lightweight piece environment for embedded contexts
2. **State isolation**: Ensure embedded instances don't interfere with main piece state
3. **Performance optimization**: Minimize overhead of embedded executions

## Implementation Order

1. **Fix API parameter issue** (immediate - will fix both inline KidLisp and start fixing $codes)
2. **Add async $code support** (high priority - will fix $code embeds)
3. **Enhance buffer context** (medium priority - performance and stability)
4. **Add comprehensive testing** (ongoing - prevent regressions)

## Files to Modify

1. `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs` - Fix API parameter issue
2. `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/kidlisp.mjs` - Async $code support
3. Test with `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/kidlisp-in-js.mjs`

## Expected Outcomes

After fixes:
- `(wipe blue)` should show blue background on left side
- `$bop` should load and execute cached code on right side (once it exists)
- No infinite console logging
- Proper error handling for missing cached codes
