# PaperWM Window Drag Conflict Analysis

## Problem Summary

AC Electron app's drag events on side pane tabs don't work in Wayland Fedora when PaperWM is enabled, even though they work on Mac, Windows, and when PaperWM is disabled. The window is set to PaperWM's "scratch layer" for always-on-top behavior.

## Root Cause

### AC Electron App Implementation

**File**: [ac-electron/renderer/flip-view.html:1383-1432](../ac-electron/renderer/flip-view.html#L1383-L1432)

```javascript
tab.addEventListener('mousedown', (e) => {
  // Tracks screen coordinates
  startX = e.screenX;
  startY = e.screenY;
  startWinX = window.screenX;
  startWinY = window.screenY;

  const onMouseMove = (e) => {
    const dx = e.screenX - startX;
    const dy = e.screenY - startY;
    // Sends move command to main process
    ipcRenderer.send('move-window', {
      x: startWinX + dx,
      y: startWinY + dy
    });
  };
});
```

**File**: [ac-electron/main.js:2020-2026](../ac-electron/main.js#L2020-L2026)

```javascript
ipcMain.on('move-window', (event, position) => {
  const senderWindow = BrowserWindow.fromWebContents(event.sender);
  if (senderWindow) {
    senderWindow.setPosition(Math.round(position.x), Math.round(position.y));
  }
});
```

### PaperWM Implementation

**File**: `/tmp/PaperWM/grab.js`

PaperWM intercepts ALL window move operations through its `MoveGrab` class:

```javascript
class MoveGrab {
  begin() {
    grabbed = true;
    global.display.end_grab_op?.(global.get_current_time());
    // PaperWM takes control of dragging
  }

  motion(_actor, event) {
    let [gx, gy] = global.get_pointer();
    // PaperWM's own positioning logic
    clone.x = gx - dx;
    clone.y = gy - dy;
  }
}
```

**File**: `/tmp/PaperWM/scratch.js:44-59`

Scratch windows (what AC uses for always-on-top) have special animation:

```javascript
export function easeScratch(metaWindow, targetX, targetY, params = {}) {
  // PaperWM animates scratch windows with its own easing
  Easer.addEase(metaWindow.get_compositor_private(), {
    x: targetX - dx,
    y: targetY - dy,
    time: Settings.prefs.animation_time,
    onComplete: () => {
      metaWindow.move_frame(true, targetX, targetY);
    },
  });
}
```

**File**: `/tmp/PaperWM/scratch.js:80-81`

```javascript
metaWindow.make_above();  // Always on top
metaWindow.stick();       // Show on all workspaces
```

## The Conflict

1. **AC Electron** calls `BrowserWindow.setPosition(x, y)` which internally calls X11/Wayland window positioning
2. **PaperWM** intercepts Wayland window move operations via GNOME Shell's Meta API
3. **PaperWM's grab system** (`grabbed = true`) blocks external position changes
4. **Scratch windows** get additional special handling that conflicts with programmatic moves

On Wayland, PaperWM has tighter control over window management compared to X11, Mac, or Windows.

## Solutions

### Option 1: Bypass PaperWM (Recommended)

Detect PaperWM and use native GNOME window hints:

```javascript
// In ac-electron/main.js
async function isPaperWMActive() {
  try {
    const result = execSync(
      'gdbus call --session --dest org.gnome.Shell --object-path /org/gnome/Shell --method org.gnome.Shell.Eval "global.display"',
      { encoding: 'utf8', timeout: 1000 }
    );
    // Check if PaperWM extension is in the output
    return result.includes('paperwm');
  } catch {
    return false;
  }
}

// Modify window creation for PaperWM compatibility
if (await isPaperWMActive()) {
  win.setSkipTaskbar(true);
  win.setAlwaysOnTop(true);
  // Don't use transparent + frameless which confuses PaperWM
  // Use native GNOME window hints instead
}
```

### Option 2: Native Drag Handle

Use Electron's built-in `-webkit-app-region` CSS for PaperWM compatibility:

```html
<!-- In flip-view.html -->
<style>
  .flip-tab {
    -webkit-app-region: drag;  /* Native drag handle */
  }

  /* Disable for interactive elements */
  button, input {
    -webkit-app-region: no-drag;
  }
</style>
```

Remove custom mousedown handlers for drag, let Electron handle it natively. This works with PaperWM because it uses the window's native drag affordances.

### Option 3: Detect and Disable Custom Drag

Check if running under PaperWM and disable custom drag logic:

```javascript
// In flip-view.html
const isPaperWM = await ipcRenderer.invoke('check-paperwm');

if (!isPaperWM) {
  // Only attach custom drag handlers if not PaperWM
  flipTabs.forEach(tab => {
    tab.addEventListener('mousedown', handleCustomDrag);
  });
} else {
  // Use native window dragging
  flipTabs.forEach(tab => {
    tab.style.webkitAppRegion = 'drag';
  });
}
```

### Option 4: Work With PaperWM's API

Instead of fighting PaperWM, use its positioning system:

```javascript
// Send D-Bus commands to PaperWM
async function movePaperWMWindow(metaWindow, x, y) {
  execSync(`gdbus call --session --dest org.gnome.Shell \
    --object-path /org/gnome/Shell \
    --method org.gnome.Shell.Eval \
    "imports.ui.main.extensionManager.lookup('paperwm@hedning:matrix.org').stateObj.scratch.easeScratch(${metaWindow}, ${x}, ${y})"`
  );
}
```

This is fragile but would work perfectly with PaperWM.

## Testing the Fix

1. **Detect PaperWM presence**:
   ```bash
   gnome-extensions list | grep paperwm
   ```

2. **Check if scratch mode interferes**:
   ```bash
   gdbus call --session --dest org.gnome.Shell \
     --object-path /org/gnome/Shell \
     --method org.gnome.Shell.Eval \
     "global.display.focus_window"
   ```

3. **Test native drag**:
   - Apply `-webkit-app-region: drag` to flip tabs
   - Verify dragging works
   - Check if it breaks flip functionality

## Recommended Approach

**Use Option 2 (Native Drag)** with a progressive enhancement:

1. Try `-webkit-app-region: drag` for the flip tabs
2. Keep flip click detection but remove custom move logic
3. Only use custom drag on platforms where native doesn't work

This is cleanest and works across all platforms including PaperWM.

## Further Investigation

Clone PaperWM settings to see if there's a config option:

```bash
dconf dump /org/gnome/shell/extensions/paperwm/
```

Look for:
- `scratch-window-behavior`
- `window-position-mode`
- Any drag/move related settings

The user might be able to whitelist the AC window or disable scratch positioning for specific windows.
