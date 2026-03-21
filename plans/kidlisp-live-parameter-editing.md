# KidLisp Live Parameter Editing

## 🎯 Goal
Enable live editing of KidLisp variables (like framerate, line coordinates, colors) while the program is running, without restarting from frame 0.

## 💡 Concept
When you change a `def` value in the editor while the code is playing, update only that variable in the running instance instead of reloading the entire program.

## 🎬 User Experience

### Before (Current)
```lisp
(def speed 5)
(def color "red")
(line 0 0 100 100)
```
- User edits `speed 5` → `speed 10`
- Presses Enter/Play
- **Program restarts from frame 0** (frameCount resets, all state lost)

### After (Proposed)
```lisp
(def speed 5)
(def color "red")
(line 0 0 100 100)
```
- User edits `speed 5` → `speed 10`
- **Variable updates live**, program keeps running
- Frame count continues, timing state preserved
- Only the changed variable updates

## 🔧 Implementation Strategy

### Phase 1: Variable Hot-Swapping (Simplest)

Detect when only `def` statements have changed:

```javascript
// In kidlisp.mjs
updateVariable(name, value) {
  // Update the variable without restarting
  this.globalDef[name] = value;
  // Keep frameCount, timing state, etc.
}

// In kidlisp.com or boot.mjs
function handleCodeUpdate(newCode, oldCode) {
  const changedDefs = detectChangedDefs(newCode, oldCode);
  
  if (changedDefs.length > 0 && onlyDefsChanged(newCode, oldCode)) {
    // Hot-swap the variables
    changedDefs.forEach(({ name, value }) => {
      kidlispInstance.updateVariable(name, value);
    });
    // Don't reload, keep running!
  } else {
    // Structure changed, do full reload
    kidlispInstance.reload(newCode);
  }
}
```

### Phase 2: Smart Diff Detection

Compare AST to detect what changed:

```javascript
function detectChanges(oldAST, newAST) {
  return {
    defsChanged: [...],     // Variable definitions that changed
    codeChanged: false,     // Whether non-def code changed
    timingChanged: false,   // Whether timing expressions changed
  };
}
```

### Phase 3: Partial Recompilation (Advanced)

For more complex changes:
- Changed `def` → hot-swap variable
- Changed function body → recompile function, keep state
- Changed timing expression → update timing map, keep counters
- Changed drawing code → reparse that section

## 📊 What to Preserve

### Must Keep Running
- `frameCount` - Current frame number
- `lastSecondExecutions` - Timing state for `1s...`, `2s...` etc
- `sequenceCounters` - Sequence state for timing
- `onceExecuted` - Track of `once` blocks
- Embedded layer buffers
- `bakes` - Baked background layers

### Can Update
- `globalDef` - Variables defined with `(def ...)`
- Parsed AST if code structure changed
- Function definitions (`later` functions)

## 🎨 UI Considerations

### Visual Feedback
```
🔴 PLAYING (Frame 234)     ← Show it's running
✏️ Variable 'speed' updated (5 → 10)  ← Confirm change
```

### Keyboard Shortcuts
- `Cmd+Enter` - Smart reload (hot-swap if possible, full reload if needed)
- `Cmd+Shift+Enter` - Force full reload (restart from frame 0)

## 🏗️ Architecture

### kidlisp.mjs Changes
```javascript
class KidLisp {
  // New method
  hotUpdateVariable(name, value) {
    this.globalDef[name] = value;
    // Invalidate any cached evaluations that use this variable
    this.invalidateVariableCache(name);
  }
  
  // New method  
  canHotUpdate(newSource) {
    const newAST = this.parse(newSource);
    const diff = this.diffAST(this.ast, newAST);
    return diff.onlyDefsChanged;
  }
  
  // Enhanced reload
  reload(newSource, options = {}) {
    if (options.preserveState && this.canHotUpdate(newSource)) {
      // Hot update path
      this.hotUpdate(newSource);
    } else {
      // Full reload path (current behavior)
      this.fullReload(newSource);
    }
  }
}
```

### boot.mjs Changes
```javascript
// Enhance existing kidlisp-reload handler
if (event.data?.type === "kidlisp-reload") {
  const code = event.data.code;
  const preserveState = event.data.preserveState !== false;
  
  window.acSEND({
    type: "piece-reload",
    content: { 
      source: code,
      preserveState: preserveState  // NEW
    }
  });
}
```

### kidlisp.com Changes
```javascript
// Add option to preserve state
function sendCode(preserveState = true) {
  previewIframe.contentWindow.postMessage({
    type: 'kidlisp-reload',
    code: editor.getValue(),
    preserveState: preserveState  // NEW
  }, aestheticUrl);
}

// Keyboard shortcuts
editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter, () => {
  sendCode(true);  // Smart reload
});

editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.Enter, () => {
  sendCode(false);  // Force full reload
});
```

## 🧪 Test Cases

### Should Hot-Update
```lisp
; Change value
(def speed 5) → (def speed 10)

; Change color
(def color "red") → (def color "blue")

; Change multiple defs
(def x 10)
(def y 20)
```

### Should Full-Reload
```lisp
; Add new code
(line 0 0 100 100) → (line 0 0 100 100) (circle 50 50 25)

; Change timing
(1s (ink red)) → (2s (ink red))

; Change structure
(repeat 5 i (box i)) → (repeat 10 i (box i))
```

## 🚀 Benefits

1. **Faster iteration** - Tweak values without losing animation progress
2. **Better debugging** - See parameter effects immediately
3. **Live performance** - Adjust values during live coding performances
4. **Teaching tool** - Show immediate cause/effect of parameter changes

## 📝 Notes

- Start simple: only hot-swap `def` changes
- Can expand to timing expressions, function bodies later
- Keep full reload as fallback for complex changes
- Consider auto-detection vs manual mode selection

## 🎯 Success Criteria

- ✅ Can change `(def speed 5)` to `(def speed 10)` while running
- ✅ Frame count continues from current value
- ✅ Timing state (1s..., 2s...) preserved
- ✅ Visual feedback shows what was updated
- ✅ Fallback to full reload for structural changes
