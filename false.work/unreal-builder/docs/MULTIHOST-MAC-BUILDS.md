# Multi-Host Mac Build System

This document describes the multi-host Mac build system for SpiderLily, which allows building on either Jeffrey's MacBook or the Mac Mini build server.

## Quick Start

### Building on Jeffrey's MacBook (from its devcontainer)

```fish
# Default - builds on the local MacBook via host.docker.internal
./false.work/unreal-builder/scripts/remote-mac-build-multihost.fish

# Explicit macbook target
./false.work/unreal-builder/scripts/remote-mac-build-multihost.fish --target=macbook

# Skip P4 sync (use local files)
./false.work/unreal-builder/scripts/remote-mac-build-multihost.fish --no-sync
```

### Building on Mac Mini (from any devcontainer)

```fish
./false.work/unreal-builder/scripts/remote-mac-build-multihost.fish --target=macmini
```

## Host Configuration

| Target     | Host                    | P4 Workspace                        | Use Case                          |
|------------|-------------------------|-------------------------------------|-----------------------------------|
| `macbook`  | `host.docker.internal`  | `spiderlily_build_workspace_macbook`| When on Jeffrey's MacBook         |
| `macmini`  | `192.168.12.27`         | `spiderlily_build_workspace_macmini`| Remote builds, CI, scheduled      |

## Setup Requirements

### For Jeffrey's MacBook

1. **Perforce Workspace**: Create `spiderlily_build_workspace_macbook` mapping to `~/Perforce/spiderlily_build_workspace_macbook/`
   ```
   P4 Workspace: spiderlily_build_workspace_macbook
   Root: /Users/jas/Perforce/spiderlily_build_workspace_macbook
   View: //depot/SpiderLily/SL_main/... //spiderlily_build_workspace_macbook/SL_main/...
   ```

2. **SSH Access**: Ensure SSH is enabled on the MacBook
   - System Settings → General → Sharing → Remote Login (ON)

3. **Unreal Engine**: Install UE 5.6 at `/Users/Shared/Epic Games/UE_5.6`

4. **Credentials**: Set password in vault file
   ```bash
   # Edit: aesthetic-computer-vault/false.work/mac-builder-credentials.env
   MACBOOK_PASSWORD=your_macos_password
   ```

### For Mac Mini (existing setup)

Already configured - just ensure `MACMINI_PASSWORD` is set in the credentials file.

## Credentials File

Located at: `aesthetic-computer-vault/false.work/mac-builder-credentials.env`

```env
# Jeffrey's MacBook
MACBOOK_HOST=host.docker.internal
MACBOOK_USERNAME=jas
MACBOOK_PASSWORD=<set this>
MACBOOK_UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
MACBOOK_P4_WORKSPACE=spiderlily_build_workspace_macbook

# Mac Mini
MACMINI_HOST=192.168.12.27
MACMINI_USERNAME=jas
MACMINI_PASSWORD=<set this>
MACMINI_UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
MACMINI_P4_WORKSPACE=spiderlily_build_workspace_macmini
```

## Build Output

All builds produce:
- **Local ZIP**: `system/public/assets/false.work/spiderlily-mac-{version}.zip`
- **CDN URL**: `https://assets.aesthetic.computer/false.work/spiderlily-mac-{version}.zip`
- **Build Log**: `https://assets.aesthetic.computer/false.work/spiderlily-mac-{version}.txt`
- **Website**: Auto-updated at `https://builds.false.work`

## Comparison with Original Script

| Feature              | Original `remote-mac-build.fish` | New `remote-mac-build-multihost.fish` |
|----------------------|----------------------------------|---------------------------------------|
| Mac Mini support     | ✅ Only                          | ✅                                    |
| MacBook support      | ❌                               | ✅                                    |
| Target selection     | ❌ Hardcoded                     | ✅ `--target=` flag                   |
| P4 workspace config  | ❌ Hardcoded                     | ✅ Per-host                           |
| Skip sync flag       | ❌                               | ✅ `--no-sync`                        |

## Troubleshooting

### "Failed to connect to host.docker.internal"
- Make sure you're running from the devcontainer **on Jeffrey's MacBook**
- The `host.docker.internal` DNS only works inside Docker containers

### "Project not found"
- The P4 workspace needs to be synced first
- Create the Perforce workspace if it doesn't exist
- Run initial sync: `p4 -c spiderlily_build_workspace_macbook sync`

### "Unreal Engine not found"
- Install UE 5.6 via Epic Games Launcher
- Ensure it's at `/Users/Shared/Epic Games/UE_5.6`

### Permission denied (SSH)
- Enable Remote Login in macOS System Settings
- Check credentials file has correct password
