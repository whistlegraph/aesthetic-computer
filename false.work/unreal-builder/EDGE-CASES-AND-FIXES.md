# Edge Cases & Fixes: UE5 Build Automation

This document covers all the non-obvious issues encountered during Spider Lily build automation setup and their solutions.

## Table of Contents
1. [LiveCoding Hanging Issue](#1-livecoding-hanging-issue)
2. [Linux Cross-Compilation Toolchain](#2-linux-cross-compilation-toolchain)
3. [Windows Defender File Locking](#3-windows-defender-file-locking)
4. [MSVC Version Mismatch](#4-msvc-version-mismatch)
5. [Perforce Workspace Configuration](#5-perforce-workspace-configuration)
6. [PATH Not Refreshing After Install](#6-path-not-refreshing-after-install)
7. [DefaultEngine.ini Read-Only Issue](#7-defaultengineini-read-only-issue)
8. [Target.cs Property Names](#8-targetcs-property-names)

---

## 1. LiveCoding Hanging Issue

### Problem
UE5 builds hang indefinitely at:
```
LogLiveCoding: Display: Starting LiveCoding
LogLiveCoding: Display: Waiting for server
```

### Root Cause
LiveCoding is designed for interactive development and requires a GUI server. It's enabled by default for Development builds but blocks automated/headless builds.

### Failed Attempts
❌ `-NoLiveCoding` command-line flag → Ignored  
❌ `$env:UE_DISABLE_LIVE_CODING = "1"` → Ignored  
❌ `[LiveCoding] bEnabled=False` in `DefaultEngine.ini` → Ignored  
❌ `bWithLiveCoding = false` in Target.cs without proper settings → Build conflict error  
❌ `bUseLiveCoding = false` in Target.cs → Property doesn't exist  
❌ `bBuildWithLiveCodingConsole = false` in Target.cs → Property doesn't exist  

### ✅ Working Solution
Create/modify `Config/DefaultEditorPerProjectUserSettings.ini`:

```ini
[/Script/LiveCoding.LiveCodingSettings]
bEnabled=False
Startup=Manual
```

This is a **project-specific config file** that overrides LiveCoding settings for all builds.

### Implementation
```powershell
$ConfigFile = "C:\Perforce\Config\DefaultEditorPerProjectUserSettings.ini"

if (Test-Path $ConfigFile) {
    $content = Get-Content $ConfigFile -Raw
} else {
    $content = ""
}

if ($content -notmatch "\[/Script/LiveCoding.LiveCodingSettings\]") {
    $content += "`n[/Script/LiveCoding.LiveCodingSettings]`nbEnabled=False`nStartup=Manual`n"
} else {
    $content = $content -replace "bEnabled=True", "bEnabled=False"
}

attrib -r $ConfigFile 2>$null
Set-Content -Path $ConfigFile -Value $content -NoNewline
```

### Reference
Forum post that led to solution: https://forums.unrealengine.com/t/how-to-disable-editor-live-coding-via-cli-arguments/599808

---

## 2. Linux Cross-Compilation Toolchain

### Problem
```
Platform Linux is not a valid platform to build. Check that the SDK is installed properly.
```

### Root Cause
UE5.6 includes Linux **platform support code** but not the **cross-compilation toolchain** (clang for Linux). These are separate installations.

### Detection
```powershell
# Platform code exists (always included)
Test-Path "C:\Program Files\Epic Games\UE_5.6\Engine\Source\Programs\UnrealBuildTool\Platform\Linux"  # True

# Toolchain missing (needs manual install)
Test-Path "C:\Program Files\Epic Games\UE_5.6\Engine\Extras\ThirdPartyNotUE\SDKs"  # False
```

### ✅ Solution
Download and install the Linux cross-compile toolchain:

```powershell
$ToolchainURL = "https://cdn.unrealengine.com/CrossToolchain_Linux/v22_clang-16.0.6-centos7.exe"
$Installer = "C:\Temp\linux-toolchain.exe"

New-Item -ItemType Directory -Force -Path C:\Temp
Invoke-WebRequest -Uri $ToolchainURL -OutFile $Installer
Start-Process -FilePath $Installer -ArgumentList "/S" -Wait
```

**Size**: ~2.5 GB  
**Install Time**: ~5-10 minutes

### Add to Bootstrap
Should be added to `bootstrap-windows-minimal.ps1` as step [9/9] if Linux builds are needed.

---

## 3. Windows Defender File Locking

### Problem
```
Error: Access is denied when writing 'UnrealEditor-SpiderLily.dll'
```

### Root Cause
Windows Defender real-time scanning locks DLL files during compilation, preventing UnrealBuildTool from writing build outputs.

### ✅ Solution
Add Windows Defender exclusions for build directories:

```powershell
# Exclude build folders
Add-MpPreference -ExclusionPath "C:\Perforce"
Add-MpPreference -ExclusionPath "C:\Program Files\Epic Games\UE_5.6"

# Exclude build tools
Add-MpPreference -ExclusionProcess "UnrealBuildTool.exe"
Add-MpPreference -ExclusionProcess "link.exe"
```

### Why This Matters
Without exclusions:
- Builds fail randomly with "Access denied" errors
- DLL locking causes incomplete builds
- Build times increase significantly due to scanning overhead

---

## 4. MSVC Version Mismatch

### Problem
```
Visual Studio 2022 compiler version 14.44.35217 is not a preferred version. 
Please use the latest preferred version 14.38.33130
```

### Root Cause
UE5.6 is tested with specific MSVC versions. Using newer versions may work but can cause subtle compilation issues.

### ❌ Wrong Approach
```powershell
# This installs the latest MSVC (14.44+)
choco install visualstudio2022buildtools --package-parameters "--add Microsoft.VisualStudio.Workload.VCTools"
```

### ✅ Correct Approach
Install the **specific version** UE5.6 expects:

```powershell
choco install visualstudio2022buildtools --package-parameters `
    "--add Microsoft.VisualStudio.Component.VC.14.38.17.8.x86.x64"
```

### Version Mapping
- UE 5.6 → MSVC 14.38.33130 (VS 2022 17.8)
- UE 5.5 → MSVC 14.38.33130 (VS 2022 17.8)
- UE 5.4 → MSVC 14.38.33130 (VS 2022 17.8)

---

## 5. Perforce Workspace Configuration

### Problem
Syncing the entire depot (including all intermediate files) causes:
- Extremely long sync times (30+ minutes)
- Wasted disk space (100+ GB)
- Build conflicts with stale intermediate files

### ✅ Solution
Create a workspace with **smart exclusions**:

```bash
p4 client -o spiderlily_build_workspace > workspace.txt

# Edit workspace.txt to add exclusions:
View:
    //SpiderLily/SL_main/... //spiderlily_build_workspace/...
    -//SpiderLily/SL_main/Saved/... //spiderlily_build_workspace/Saved/...
    -//SpiderLily/SL_main/Intermediate/... //spiderlily_build_workspace/Intermediate/...
    -//SpiderLily/SL_main/DerivedDataCache/... //spiderlily_build_workspace/DerivedDataCache/...
    -//SpiderLily/SL_main/Binaries/... //spiderlily_build_workspace/Binaries/...
    -//SpiderLily/SL_main/.vs/... //spiderlily_build_workspace/.vs/...

p4 client -i < workspace.txt
```

### What to Exclude
- `Saved/` - Runtime generated content
- `Intermediate/` - Build artifacts (rebuilt locally)
- `DerivedDataCache/` - Shader cache (rebuilt locally)
- `Binaries/` - Compiled binaries (rebuilt locally)
- `.vs/` - Visual Studio temp files

### What to Include
- `Content/` - Game assets
- `Config/` - Project configuration
- `Source/` - C++ source code
- `Plugins/` - Third-party plugins
- `.uproject` file

---

## 6. PATH Not Refreshing After Install

### Problem
After installing tools via Chocolatey, commands aren't available:
```powershell
choco install git
git --version  # ERROR: 'git' is not recognized
```

### Root Cause
PowerShell sessions cache the PATH environment variable. Installing software updates the **machine-level** PATH, but the current session doesn't see it.

### ❌ Wrong Approach
```powershell
# This only updates the current session temporarily
$env:PATH += ";C:\Program Files\Git\cmd"
```

### ✅ Correct Approach
Reload PATH from the registry after each install:

```powershell
function Refresh-Path {
    $machinePath = [System.Environment]::GetEnvironmentVariable("Path", "Machine")
    $userPath = [System.Environment]::GetEnvironmentVariable("Path", "User")
    $env:PATH = "$machinePath;$userPath"
}

# Use after every installation
choco install git -y
Refresh-Path
git --version  # Now works!
```

### Implementation in Bootstrap
Call `Refresh-Path` after:
- Chocolatey installation
- Git installation
- Perforce CLI installation
- Any tool that modifies PATH

---

## 7. DefaultEngine.ini Read-Only Issue

### Problem
```
LogTemp: Warning: Can't make file 'DefaultEngine.ini' writable when updating DefaultEngine.ini!
LogFileManager: Error: Error deleting file 'C:/Perforce/Config/DefaultEngine.ini'.
```

### Root Cause
Files synced from Perforce are marked read-only by default. Some UE5 processes (like PreBuildSteps.bat) try to modify config files but fail on read-only files.

### ✅ Solution
Make config files writable before builds:

```powershell
attrib -r C:\Perforce\Config\*.ini
```

### Not a Critical Issue
This warning doesn't break builds, but causes noise in logs. The PreBuildSteps.bat is trying to update revision numbers in the config.

---

## 8. Target.cs Property Names

### Problem
Attempting to disable LiveCoding in Target.cs files using wrong property names:

```csharp
// These DON'T exist in UE5.6
bBuildWithLiveCodingConsole = false;  // ❌
bUseLiveCoding = false;                // ❌
```

### Root Cause
Property names changed between UE4 and UE5, and vary by UE version.

### ✅ Correct Property (UE5.6)
```csharp
public SpiderLilyTarget(TargetInfo Target) : base(Target)
{
    Type = TargetType.Game;
    DefaultBuildSettings = BuildSettingsVersion.V5;
    ExtraModuleNames.AddRange(new string[] { "SpiderLily" });
    
    // Correct property for UE5.6
    bWithLiveCoding = false;
    bOverrideBuildEnvironment = true;  // Required when changing LiveCoding
}
```

### How to Find Correct Property Names
```powershell
# Search UE5 source for valid properties
Get-ChildItem "C:\Program Files\Epic Games\UE_5.6\Engine\Source\Programs\UnrealBuildTool" `
    -Filter "*.cs" -Recurse | 
    Select-String -Pattern "LiveCoding"
```

### Important Notes
- `bOverrideBuildEnvironment = true` is **required** when modifying `bWithLiveCoding`
- Without it, you get: "SpiderLilyEditor modifies the values of properties... This is not allowed"
- Cannot use `BuildEnvironment = TargetBuildEnvironment.Unique` with installed engines

---

## Summary: Critical Fixes for Build Automation

### Must-Have Fixes
1. ✅ **LiveCoding config** (`DefaultEditorPerProjectUserSettings.ini`) - Prevents infinite hang
2. ✅ **Windows Defender exclusions** - Prevents access denied errors
3. ✅ **MSVC 14.38.x specific version** - Ensures compatibility
4. ✅ **Perforce workspace exclusions** - Saves time and disk space

### Optional but Recommended
5. ✅ **Linux toolchain** (if building for Linux)
6. ✅ **PATH refresh function** (makes bootstrap more reliable)
7. ✅ **Config file unlock** (reduces warning noise)

### Script Updates Needed

**bootstrap-windows-minimal.ps1:**
```powershell
# Add after UE5 installation:
# [9/9] Configure LiveCoding Disable
$ConfigFile = "C:\Perforce\Config\DefaultEditorPerProjectUserSettings.ini"
# ... (implementation from section 1)

# [10/10] Install Linux Toolchain (optional)
if ($EnableLinuxBuilds) {
    # ... (implementation from section 2)
}
```

**daily-build.ps1 / package-for-team.ps1:**
```powershell
# Add at start of script:
# Ensure config file exists
$ConfigFile = "C:\Perforce\Config\DefaultEditorPerProjectUserSettings.ini"
if (-not (Test-Path $ConfigFile)) {
    # Create it
    # ... (implementation from section 1)
}

# Make config files writable
attrib -r C:\Perforce\Config\*.ini
```

---

## Lessons Learned

### 1. UE5 Configuration Hierarchy
UE5 has multiple config levels (Engine → Project → User). For automated builds, **User-level configs** (`DefaultEditorPerProjectUserSettings.ini`) work best because they:
- Override project settings
- Don't require modifying the project repo
- Persist across builds

### 2. LiveCoding is Not for Automation
LiveCoding is an **interactive development feature**. It requires:
- A GUI environment
- User to start the LiveCoding server
- Hot-reload workflow

For headless/automated builds, it must be **completely disabled** via config, not just flags.

### 3. Cross-Platform Builds Need Explicit Toolchains
UE5 doesn't include cross-compilation toolchains by default. Each platform needs:
- **Windows → Windows**: MSVC (included)
- **Windows → Linux**: Linux cross-compiler (separate download)
- **Windows → Mac**: Not possible (needs macOS)
- **Windows → Android**: Android SDK/NDK (separate)

### 4. Perforce Sync Strategy Matters
Syncing everything wastes:
- **Time**: 30+ min sync vs 3-5 min with exclusions
- **Space**: 100+ GB vs 15-20 GB
- **Reliability**: Stale intermediate files cause build conflicts

### 5. Windows-Specific Build Quirks
Windows has unique challenges:
- Windows Defender aggressive file locking
- PATH caching in PowerShell sessions
- Read-only file attributes from Perforce
- MSVC version compatibility

---

## Testing Your Setup

Run this validation script to ensure all fixes are applied:

```powershell
# validation-script.ps1

Write-Host "=== UE5 Build Setup Validation ===" -ForegroundColor Cyan

# 1. Check LiveCoding config
$LiveCodingConfig = "C:\Perforce\Config\DefaultEditorPerProjectUserSettings.ini"
if (Test-Path $LiveCodingConfig) {
    if ((Get-Content $LiveCodingConfig -Raw) -match "bEnabled=False") {
        Write-Host "✅ LiveCoding disabled" -ForegroundColor Green
    } else {
        Write-Host "❌ LiveCoding config missing" -ForegroundColor Red
    }
} else {
    Write-Host "❌ LiveCoding config file missing" -ForegroundColor Red
}

# 2. Check Windows Defender exclusions
$exclusions = Get-MpPreference | Select-Object -ExpandProperty ExclusionPath
if ($exclusions -contains "C:\Perforce") {
    Write-Host "✅ Windows Defender exclusions configured" -ForegroundColor Green
} else {
    Write-Host "❌ Windows Defender exclusions missing" -ForegroundColor Red
}

# 3. Check MSVC version
$msvcPath = "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Tools\MSVC"
$versions = Get-ChildItem $msvcPath -Directory -ErrorAction SilentlyContinue
if ($versions | Where-Object { $_.Name -like "14.38.*" }) {
    Write-Host "✅ MSVC 14.38.x installed" -ForegroundColor Green
} else {
    Write-Host "⚠️  MSVC version may not be optimal" -ForegroundColor Yellow
}

# 4. Check Perforce workspace
$p4client = p4 client -o spiderlily_build_workspace 2>&1
if ($p4client -match "-//SpiderLily/SL_main/Intermediate/") {
    Write-Host "✅ Perforce workspace has exclusions" -ForegroundColor Green
} else {
    Write-Host "⚠️  Perforce workspace may sync unnecessary files" -ForegroundColor Yellow
}

# 5. Check Linux toolchain (if needed)
if (Test-Path "C:\Program Files\Epic Games\UE_5.6\Engine\Extras\ThirdPartyNotUE\SDKs") {
    Write-Host "✅ Linux cross-compile toolchain installed" -ForegroundColor Green
} else {
    Write-Host "ℹ️  Linux toolchain not installed (optional)" -ForegroundColor Cyan
}

Write-Host "`n=== Validation Complete ===" -ForegroundColor Cyan
```

---

## Quick Reference

| Issue | Fix Location | Time to Fix |
|-------|--------------|-------------|
| LiveCoding hang | `Config/DefaultEditorPerProjectUserSettings.ini` | 1 min |
| Access denied errors | Windows Defender exclusions | 2 min |
| MSVC version | Chocolatey install command | 10 min |
| Slow Perforce sync | Workspace View config | 5 min |
| PATH not working | Refresh-Path function | 1 min |
| Linux builds fail | Install toolchain | 10 min |
| Read-only configs | `attrib -r` command | 1 min |

---

**Last Updated**: October 19, 2025  
**UE Version**: 5.6  
**Platform**: Windows Server 2022  
**Project**: Spider Lily (false.work Studio)
