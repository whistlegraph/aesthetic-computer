# KidLisp Fixes TODO List

## 🎯 Current Focus
◆ **Fix API parameter issue in disk.mjs** (Phase 1)

## 📋 Todo Items

### Phase 1: Immediate API Fix (disk.mjs)
- [ ] ◆ **Fix the module() call**: Change `kidlispInstance.module(source, kidlispAPI)` to properly handle the returned piece object
- [ ] **Create proper piece lifecycle**: Call boot, then paint with the API
- [ ] **Handle API parameter correctly**: Pass `false` for `isLispFile` parameter
- [ ] **Test basic inline KidLisp**: Verify `(wipe blue)` works on left side

### Phase 2: Async $code Support
- [ ] **Implement proper async handling**: Modify embedded KidLisp to wait for $code resolution
- [ ] **Add loading states**: Show loading indicator while fetching cached codes  
- [ ] **Cache management**: Ensure proper cache invalidation and memory management
- [ ] **Test $code execution**: Verify `$bop` loads and executes on right side

### Phase 3: Buffer Context Enhancement (Blur Fix)
- [ ] **Verify buffer variable scope**: Check if `graph.blur` accesses correct width/height/pixels during isolated buffer operations
- [ ] **Add buffer context logging**: Log actual width/height/pixels values that blur operates on
- [ ] **Compare execution contexts**: Analyze why embedded pieces work vs isolated buffer calls
- [ ] **Buffer-aware blur function**: Modify blur to accept buffer context parameters instead of using globals
- [ ] **Global variable synchronization**: Ensure global width/height/pixels are updated before blur calls
- [ ] **Test blur in isolated vs main contexts**: Verify visual effects appear correctly

### Phase 4: Testing & Validation
- [ ] **Comprehensive testing**: Test all KidLisp embedding scenarios
- [ ] **Performance testing**: Measure overhead of embedded executions
- [ ] **Error handling**: Ensure proper error handling for missing cached codes
- [ ] **Regression testing**: Verify existing functionality still works

## 🔍 Investigation Notes

### Current Status
- Local server is running
- Ready to receive console logs for debugging
- Both plans have been analyzed and consolidated

### Key Files to Monitor
- `/system/public/aesthetic.computer/lib/disk.mjs` - Lines around 3000-3150
- `/system/public/aesthetic.computer/lib/kidlisp.mjs` - Lines around 2978-3000  
- `/system/public/aesthetic.computer/lib/graph.mjs` - Blur function
- `/system/public/aesthetic.computer/disks/kidlisp-in-js.mjs` - Test case

### Expected Outcomes After Current Fix
- `(wipe blue)` should show blue background on left side
- No infinite console logging
- Proper foundation for $code async support
- Foundation for blur buffer context fixes

---

**Legend:**
- ◆ = Current focus item
- ✅ = Completed
- ❌ = Blocked/Failed
- 🔄 = In Progress
