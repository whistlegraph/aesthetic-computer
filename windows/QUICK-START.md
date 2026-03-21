# Quick Start: Build false.work on Windows

Since the dev container runs in Docker Desktop (isolated from Windows host), 
the easiest way to build is to run the PowerShell script directly from Windows.

## Step 1: Open PowerShell on Windows

Press `Win + X` and select "Windows PowerShell" or "Terminal"

## Step 2: Navigate to the script

```powershell
cd \\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows
```

Or if you're in a different WSL distro location, find it with:
```powershell
wsl --list --verbose
```

## Step 3: Edit the script paths (first time only)

Open the script to configure your paths:
```powershell
notepad build-false-work.ps1
```

Update these variables to match your setup:
- `$UE5Path` - Where UE5 is installed (default: `C:\Program Files\Epic Games\UE_5.6`)
- `$ProjectRoot` - Where your SpiderLily project is (default: `D:\false.work\SpiderLily`)

## Step 4: Run the build

```powershell
.\build-false-work.ps1
```

The script will:
1. ✓ Verify UE5 and project paths
2. Ask if you want to sync from Perforce (optional)
3. Build the Windows package (this takes 10-30 minutes)
4. Ask if you want to compress to ZIP (optional)

## Example Output

```
=========================================
false.work Local Windows Build
=========================================
Platform: Win64
Config: Development
Version: 2025.11.07-1432

✓ Found UE5: C:\Program Files\Epic Games\UE_5.6
✓ Found Project: D:\false.work\SpiderLily\SpiderLily.uproject

🔨 Building Win64 package...
   This may take 10-30 minutes depending on your hardware...
```

## Build Output Location

Builds will be in: `$ProjectRoot\Builds\{version}\`

Example: `D:\false.work\SpiderLily\Builds\2025.11.07-1432\`

---

**Alternative:** If you need to call this from the dev container, you would need to 
rebuild the container with the Windows interop configuration (requires container 
restart and may not work in all Docker Desktop setups).
