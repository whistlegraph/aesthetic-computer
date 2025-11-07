# false.work Local Windows Build

This directory contains scripts for building the false.work (SpiderLily) project on a local Windows machine.

## Quick Start

### From Windows (Recommended)

1. Open PowerShell as Administrator
2. Navigate to this directory:
   ```powershell
   cd \\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows
   ```
3. Run the build script:
   ```powershell
   .\build-false-work.ps1
   ```

### Configuration

Edit `build-false-work.ps1` and update these paths to match your setup:

```powershell
$UE5Path = "C:\Program Files\Epic Games\UE_5.6"
$ProjectRoot = "D:\false.work\SpiderLily"
```

The script will:
- ✅ Verify UE5 and project paths exist
- 📥 Optionally sync from Perforce
- 🔨 Build the project using UnrealBuildTool
- 📦 Package for Windows (Win64)
- 🗜️ Optionally compress to ZIP

## Build Options

```powershell
# Development build (default)
.\build-false-work.ps1

# Shipping build
.\build-false-work.ps1 -Config Shipping

# Custom version
.\build-false-work.ps1 -Version "2025.11.07-release"
```

## Outputs

Builds are created in: `$ProjectRoot\Builds\{version}\`

Example:
```
D:\false.work\SpiderLily\Builds\2025.11.07-1432\
```

## Integration with builds.false.work

After building, you can manually upload builds to DigitalOcean Spaces using the upload script in `/false.work/unreal-builder/scripts/upload-to-spaces.sh` or use the GCP automation pipeline.

## Troubleshooting

**"UE5 not found"**
- Update `$UE5Path` to match your installation
- Common paths: `C:\Program Files\Epic Games\UE_5.6`, `C:\UE5`

**"Project not found"**
- Update `$ProjectRoot` to point to your SpiderLily project folder
- Ensure `SpiderLily.uproject` exists in that directory

**Build fails**
- Ensure Visual Studio 2022 is installed with C++ tools
- Check you have enough disk space (builds can be 10+ GB)
- Review UE build logs in the output

## From WSL2 / Dev Container

To enable calling PowerShell from inside the dev container, you need to restart the container with the updated configuration that includes Windows interop mounts.

After restarting the container with the new `.devcontainer/devcontainer.json` configuration, you should be able to use:

```fish
cd /workspaces/aesthetic-computer/windows
./build-false-work.fish
```

**Note:** The dev container currently runs in Docker Desktop on Windows, which provides some isolation from the host. For direct access, you can:

1. Run the PowerShell script directly from Windows (recommended for now)
2. Or rebuild the container after the devcontainer.json changes are applied

The Windows interop mounts have been added to `devcontainer.json` but require a container rebuild to take effect.
