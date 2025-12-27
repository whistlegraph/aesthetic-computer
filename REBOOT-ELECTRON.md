# Electron Reboot Issue

## Problem
The Electron app runs on the **host machine** (Windows/Mac), not inside the devcontainer.
From inside the container, we cannot directly restart the host Electron process.

## Why `app.relaunch()` Didn't Work
- File watcher in Electron main process saw the marker file
- Called `app.relaunch()` + `app.quit()`
- App quit successfully, but `app.relaunch()` failed because:
  - Platform-specific issues (works better on macOS)
  - Depends on how Electron was originally launched
  - May need parent process/shell to respawn

## Solutions

### Option 1: Manual Restart (Current)
Just reopen the Electron app from your host machine.

### Option 2: Host-Side Wrapper Script
Create a wrapper that automatically restarts Electron when it exits:

**For macOS/Linux host:**
```bash
#!/bin/bash
# restart-electron.sh
cd /path/to/aesthetic-computer/ac-electron
while true; do
  npm start
  EXIT_CODE=$?
  if [ $EXIT_CODE -eq 42 ]; then
    echo "Restarting Electron..."
    sleep 1
  else
    echo "Electron exited with code $EXIT_CODE"
    break
  fi
done
```

**For Windows host (PowerShell):**
```powershell
# restart-electron.ps1
cd C:\path\to\aesthetic-computer\ac-electron
while ($true) {
    npm start
    if ($LASTEXITCODE -eq 42) {
        Write-Host "Restarting Electron..."
        Start-Sleep -Seconds 1
    } else {
        Write-Host "Electron exited with code $LASTEXITCODE"
        break
    }
}
```

### Option 3: Use Exit Code Signal
Modify the marker file approach to exit with code 42, then have a parent wrapper restart.

**Updated main.js:**
```javascript
if (fs.existsSync(REBOOT_MARKER)) {
  console.log('[main] Reboot requested');
  fs.unlinkSync(REBOOT_MARKER);
  app.exit(42); // Exit with special code for wrapper to catch
}
```

### Option 4: VS Code Task Integration
Since Electron is likely launched from VS Code, we could integrate with VS Code's task system to auto-restart.

## Recommended Approach
Use Option 2 (wrapper script) on the host machine for reliable auto-restart.

From container, the trigger still works:
```bash
node ac-electron/trigger-reboot.js
```

The wrapper on the host will detect the exit and restart automatically.
