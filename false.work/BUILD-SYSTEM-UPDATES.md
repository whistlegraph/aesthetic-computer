# false.work Build System Updates

**Date:** November 7, 2025

## Summary

Simplified the builds.false.work page and created a local Windows build script for the false.work (SpiderLily) UE5 project.

## Changes Made

### 1. Simplified builds.false.work Page ✅

**Files Updated:**
- `false.work/builds.false.work/index.html`
- `system/public/builds.false.work/index.html`

**Changes:**
- Stripped down from verbose SpiderLily-specific status page to clean, minimal build distribution page
- Removed builder status indicators, detailed pipeline info, and UE5-specific documentation
- Kept simple platform sections (Windows, Mac, iOS) with placeholder text
- Cleaner terminal/matrix aesthetic (black background, green text)
- More generic and reusable for any build artifacts

### 2. Local Windows Build Script ✅

**New Files:**
- `windows/build-false-work.ps1` - PowerShell script to build on local Windows host
- `windows/build-false-work.fish` - Fish wrapper to call PowerShell from WSL2
- `windows/BUILD-README.md` - Documentation for the build process

**Features:**
- Builds SpiderLily UE5 project locally (not on GCP)
- Interactive prompts for Perforce sync and compression
- Configurable UE5 path, project location, build config
- Automatic path validation before build
- Optional ZIP compression using 7-Zip or built-in PowerShell
- Can be called directly from Windows or via Fish wrapper from WSL2

**Usage:**
```powershell
# From Windows PowerShell
cd \\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows
.\build-false-work.ps1

# Or from WSL2 (after container rebuild with interop)
cd /workspaces/aesthetic-computer/windows
./build-false-work.fish
```

### 3. Windows Interop Configuration ✅

**Files Updated:**
- `.devcontainer/devcontainer.json` - Added Windows interop volume mounts

**New Files:**
- `.devcontainer/scripts/enable-windows-interop.fish` - Helper script to enable Windows interop

**Changes:**
- Added volume mounts for `wsl.exe`, `wslpath`, and `/run/WSL` to devcontainer
- Created setup script to symlink PowerShell executables into container PATH
- Updated documentation to reflect that container rebuild is needed for interop

**Note:** The current container needs to be rebuilt for these changes to take effect. Until then, the PowerShell script should be run directly from Windows.

## File Structure

```
aesthetic-computer/
├── false.work/
│   └── builds.false.work/
│       └── index.html          # Simplified build page (source)
├── system/
│   └── public/
│       └── builds.false.work/
│           └── index.html      # Simplified build page (deployed)
├── windows/
│   ├── build-false-work.ps1    # Local Windows build script
│   ├── build-false-work.fish   # WSL2 wrapper
│   └── BUILD-README.md         # Build documentation
└── .devcontainer/
    ├── devcontainer.json       # Updated with Windows interop
    └── scripts/
        └── enable-windows-interop.fish  # Interop setup helper

```

## Next Steps

1. **To use the Windows build script now:**
   - Open PowerShell on Windows host
   - Navigate to `\\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows`
   - Edit `build-false-work.ps1` to set your UE5 and project paths
   - Run `.\build-false-work.ps1`

2. **To enable WSL2 interop (call PowerShell from container):**
   - Rebuild the dev container to apply the new mounts
   - Run `.devcontainer/scripts/enable-windows-interop.fish` after rebuild
   - Then use `./build-false-work.fish` from inside the container

3. **To deploy builds:**
   - Use the existing upload scripts in `false.work/unreal-builder/scripts/`
   - Or manually upload to DigitalOcean Spaces
   - Update the HTML pages with build links (currently just placeholders)

## Notes

- The builds.false.work page is now generic and ready for any build artifacts (not just SpiderLily)
- Both source (`false.work/`) and deployed (`system/public/`) versions are synchronized
- The Windows build script includes error handling and helpful prompts
- Container currently runs in Docker Desktop which has some isolation from Windows host
