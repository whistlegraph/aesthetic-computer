# Fish "Device or Resource Busy" Error Investigation

## The Error
```
error: Error when renaming file: Device or resource busy
```

This error appears sporadically when running fish commands in the devcontainer.

---

## Root Cause Analysis

### The Mechanism

Fish shell saves command history by:
1. Writing commands to a **temporary file** (e.g., `fish_history.tmp.XXXXXX`)
2. Using `rename()` syscall to atomically replace the old file

The error occurs when the `rename()` syscall fails because the **target file is busy** — meaning another process has it open or the underlying filesystem doesn't support atomic rename on bind-mounted files.

### Why It Happens in This Devcontainer

1. **Bind Mount for fish_history**
   ```jsonc
   // devcontainer.json line 24
   "source=${localWorkspaceFolder}/.devcontainer/fish_history,target=/home/me/.local/share/fish/fish_history,type=bind,consistency=cached"
   ```

2. **Mount Type: `fakeowner`**
   ```
   /run/host_mark/Users /home/me/.local/share/fish/fish_history fakeowner rw,nosuid,nodev,relatime,fakeowner 0 0
   ```
   
   The `fakeowner` filesystem is used by Docker Desktop on macOS/Windows to emulate Linux file ownership. It has limitations with atomic file operations.

3. **Cross-Filesystem Rename Limitation**
   - The temp file is created in `/home/me/.local/share/fish/` (container's native filesystem)
   - The target `fish_history` is on the bind-mounted `fakeowner` filesystem
   - Atomic rename across different filesystems fails

4. **Multiple Fish Processes**
   - VS Code's integrated terminal
   - The `🐟-fishy` buffer in Emacs
   - Any other fish shell opened
   - All compete to write history simultaneously

---

## Why It's Intermittent

The error only occurs when:
- A command finishes executing AND
- Fish attempts to save history AND  
- Another process has the history file open OR
- The fakeowner filesystem rejects the rename

Most of the time it succeeds because:
- Commands execute quickly
- History save completes before conflict
- The filesystem cooperates

---

## Current Mitigations in Place

### 1. In Dockerfile (line 257-264)
```dockerfile
# Ensure fish doesn't try to use universal variable daemon (causes permission issues)
ENV fish_color_host_remote=""
RUN mkdir -p /home/me/.config/fish/conf.d && \
    echo 'set -U fish_greeting ""' > /home/me/.config/fish/conf.d/greeting.fish
```

### 2. In entry.fish (line 139-141)
```fish
# Disable fish's universal variable file daemon (fishd) which causes permission issues
# We'll use fish_variables instead which is simpler and doesn't create temp files
set -U fish_greeting ""
```

### 3. Permission Fixes (entry.fish line 108-142)
```fish
function ensure_fish_config_permissions
    # ... aggressive chmod/chown fixes
end
```

---

## Potential Solutions

### Option 1: Stop Persisting Fish History (Simplest)
Remove the bind mount entirely — history won't survive container rebuilds but the error goes away.

```jsonc
// Remove this line from devcontainer.json:
"source=${localWorkspaceFolder}/.devcontainer/fish_history,target=/home/me/.local/share/fish/fish_history,type=bind,consistency=cached",
```

**Pros**: Error completely eliminated  
**Cons**: Lose command history between rebuilds

### Option 2: Use a Volume Instead of Bind Mount
```jsonc
"source=aesthetic-fish-history,target=/home/me/.local/share/fish,type=volume"
```

**Pros**: Better filesystem semantics, still persists  
**Cons**: History isolated per Docker volume, not in git repo

### Option 3: Disable Fish History Entirely
Add to `config.fish`:
```fish
set -g fish_history ""
```

**Pros**: No history writes at all  
**Cons**: No up-arrow history

### Option 4: Periodic Sync Instead of Direct Mount
- Let fish write to native filesystem
- Sync to `.devcontainer/fish_history` on container stop
- Restore on container start

This is complex but preserves history without the rename errors.

### Option 5: Suppress the Error (Cosmetic Fix)
Wrap fish invocations to hide stderr containing this specific error. Doesn't fix the underlying issue.

---

## Recommended Action

**Short-term**: Accept the error as cosmetic — it doesn't affect functionality, history still works most of the time.

**Medium-term**: Switch to **Option 2** (volume mount) if the error becomes annoying:
```jsonc
// Replace the fish_history bind mount with:
"source=aesthetic-fish-history,target=/home/me/.local/share/fish,type=volume"
```

**Long-term**: Consider **Option 4** (periodic sync) for the best of both worlds — works reliably AND preserves history in git.

---

## Files Involved

| File | Purpose |
|------|---------|
| `.devcontainer/devcontainer.json` | Defines the bind mount (line 24) |
| `.devcontainer/fish_history` | The persisted history file |
| `.devcontainer/entry.fish` | Container startup, permission fixes |
| `.devcontainer/config.fish` | Fish shell configuration |
| `.devcontainer/Dockerfile` | Base image, fish installation |

---

## Reproduction

1. Open VS Code with the devcontainer
2. Run commands in the integrated terminal (fish)
3. Simultaneously open another fish terminal or run commands from Emacs
4. Eventually, one of them will show the error when saving history

The error is **benign** — the command still executes, only the history save fails for that one invocation.

---

## Related Fish Issues

- https://github.com/fish-shell/fish-shell/issues/5658 — Atomic rename on network filesystems
- https://github.com/fish-shell/fish-shell/issues/3845 — History file locking
- https://github.com/microsoft/vscode-remote-release/issues/2347 — Bind mount limitations

---

*Last updated: December 19, 2025*
