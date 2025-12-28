# VS Code Container Attach Crash - Debugging Report

**Date:** December 27, 2025  
**Issue:** VS Code crashes/halts when running `start` script to attach to devcontainer

## Environment
- **VS Code:** 1.107.1 (994fd12f8d3a5aa16f17d42c041e5809167e845a)
- **devcontainer CLI:** 0.80.3
- **Container:** aesthetic (running)
- **Host OS:** Fedora Linux

## Symptoms
1. `CodeWindow: detected unresponsive` errors in main.log
2. `fileWatcher crashed with code 15 and reason 'killed'`
3. `renderer process gone (reason: crashed, code: 132)`
4. Lock file conflicts: `EEXIST: file already exists...vscode.lock`

## Root Causes Identified

### 1. Stale Lock Files
```
/home/me/.vscode-server/data/User/workspaceStorage/e316bf54877144e484c7d72e8e778826/vscode.lock
Content: {"pid":2356,"willReleaseAt":0}
```
When VS Code crashes, it doesn't clean up lock files. Subsequent connections fail trying to acquire the same lock.

### 2. Multiple VS Code Server Processes
Found multiple processes running simultaneously:
- fileWatcher processes using 55%+ and 79%+ CPU
- Multiple extensionHost processes
- Multiple vscode-remote-containers-server processes

This indicates VS Code is spawning new connections without cleaning up old ones.

### 3. Previous Copilot Chat Extension Bug
`github.copilot-chat-0.35.2` had a `navigator` global bug that crashed the extension host. This was mitigated by:
- Adding `remote.extensionKind: {"github.copilot-chat": ["ui"]}` to devcontainer.json
- Adding `settingsSync.ignoredExtensions: ["github.copilot-chat", "github.copilot"]`

### 4. `attached-container` vs `dev-container` URI Confusion
The `start` script has been oscillating between:
- `vscode-remote://attached-container+{hex}` - Attaches to existing container, ignores devcontainer.json
- `vscode-remote://dev-container+{hex}` - Uses devcontainer.json settings

The `attached-container` approach syncs extensions from host, including the buggy Copilot Chat.

## Proposed Solution

### Immediate Fix Script
Create a cleanup script to run before attaching:

```fish
function ac-cleanup-vscode-server
    echo "🧹 Cleaning up stale VS Code server state..."
    
    # Kill orphaned processes in container
    docker exec aesthetic sh -c "pkill -9 -f 'vscode-server' 2>/dev/null" || true
    
    # Remove stale lock files
    docker exec aesthetic sh -c "rm -f /home/me/.vscode-server/data/User/workspaceStorage/*/vscode.lock" 2>/dev/null || true
    
    # Clear extension host cache
    docker exec aesthetic sh -c "rm -rf /home/me/.vscode-server/data/logs/*" 2>/dev/null || true
    
    echo "✅ Cleanup complete"
end
```

### Updated `acd` Function
```fish
function acd
    set -l workspace ~/aesthetic-computer
    
    # Pull latest
    cd $workspace
    git pull
    
    # Cleanup before connecting
    ac-cleanup-vscode-server
    
    # Use dev-container approach (respects devcontainer.json)
    set -l container_id (pwd | tr -d '\n' | xxd -c 256 -p)
    
    # Start CDP port forwarder
    pkill -f "socat.*9224" 2>/dev/null
    socat TCP-LISTEN:9224,bind=0.0.0.0,fork,reuseaddr TCP:127.0.0.1:9222 &
    
    # Open VS Code with dev-container URI
    code --folder-uri "vscode-remote://dev-container+$container_id/workspaces/aesthetic-computer" \
         --remote-debugging-port=9222 \
         --remote-allow-origins="*"
    
    cd -
end
```

### devcontainer.json Settings to Prevent Extension Sync Issues
```jsonc
{
  "customizations": {
    "vscode": {
      "settings": {
        "settingsSync.ignoredExtensions": ["github.copilot-chat", "github.copilot"],
        "remote.extensionKind": {
          "github.copilot-chat": ["ui"],
          "github.copilot": ["ui"]
        },
        "extensions.autoUpdate": false,
        "git.autorefresh": false,  // Reduce fileWatcher load
        "files.watcherExclude": {
          "**/node_modules/**": true,
          "**/.git/objects/**": true
        }
      }
    }
  }
}
```

## Testing Steps

1. **Kill all VS Code processes on host:**
   ```bash
   pkill -9 code
   pkill -9 code-insiders
   ```

2. **Clean container VS Code state:**
   ```bash
   docker exec aesthetic sh -c "rm -rf /home/me/.vscode-server/data /home/me/.vscode-server/extensions/*"
   ```

3. **Restart container:**
   ```bash
   docker restart aesthetic
   ```

4. **Open via "Reopen in Container":**
   - Open folder in VS Code: `code ~/aesthetic-computer`
   - Cmd/Ctrl+Shift+P → "Dev Containers: Reopen in Container"

5. **If that works, test the `start` script**

## Files to Update
- [dotfiles/dot_config/fish/config.fish](../dotfiles/dot_config/fish/config.fish) - `acd` and `start` functions
- [.devcontainer/devcontainer.json](../.devcontainer/devcontainer.json) - Extension and watcher settings

## Related Commits
- `fix: remove orphaned feral-file submodules causing git hang`
- `fix: prevent copilot-chat from installing on remote`
- `fix: use dev-container URI instead of attached-container`
