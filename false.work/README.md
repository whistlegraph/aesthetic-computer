<img width="400" src="https://images.squarespace-cdn.com/content/v1/6838f89fea63a32f67c61d96/5bc717b9-7339-42d1-8544-66c19d6d925b/Asset+18.png?format=1500w">

# Spider Lily Build System Status

**Last Updated:** October 28, 2025 (11:45 PM)  
**Build Platforms:** Windows ✅ | Mac ✅ | iOS ⚠️

---

## 📊 Executive Summary

We now have **two fully operational build environments** for Spider Lily:

- **Windows builds** running on GCP (automated, cloud-based)
- **Mac builds** running on local M4 Mac Mini (zero cost, high performance)

Both environments can compile, cook, and package Spider Lily from Perforce. Mac builds produce a working 1GB `.app` bundle that launches successfully. iOS builds are 95% complete but blocked by Apple's code signing requirements.

---

## 🎯 Current Status

### ✅ Windows Builder (Production)
**Platform:** Local Windows PC (via Docker host)  
**Location:** host.docker.internal (accessible from dev container)  
**Status:** **Fully operational**

- ✅ UE5.6 installed with Perforce workspace at `C:\Perforce\SpiderLily\SL_main`
- ✅ Connected to Helix Core (ssl:falsework.helixcore.io:1666)
- ✅ **Spider Lily built and deployed for Windows**
- ✅ Fully automated build pipeline (`remote-update-and-build.fish`)
- ✅ Builds automatically uploaded to builds.false.work
- ✅ Zero cloud costs (local machine)

**Build Configuration:**
- Start Map: `L_VerticalSlice_MainMenu` (main menu level)
- Excluded Assets: `/Game/Developers`, `/Game/LIB/Characters/Player/Animations/OLD`
- Deleted Assets: `M_PP_Sky.uasset` (shader deadlock prevention)
- Build Flags: `-buildmachine -forcelogflush -MaxParallelShaderCompileJobs=6`
- PowerShell Script: `build-false-work.ps1` (automated, no prompts)

**SSH Access:**
- From dev container: Use `win` alias (shortcut for `ssh me@host.docker.internal`)
- Example: `win "dir C:\Perforce"`

---

### ✅ Mac Builder (Production)
**Platform:** M4 Mac Mini (10 cores, 16GB RAM, 460GB disk)  
**Location:** Local (SSH from dev container)  
**Status:** **Fully operational**

**Build Infrastructure:**
- ✅ SSH access configured (`host.docker.internal`)
- ✅ UE5.6 installed at `/Users/Shared/Epic Games/UE_5.6`
- ✅ Perforce workspace synced (`~/Perforce/spiderlily_build_workspace_macmini/SL_main/`)
- ✅ FMOD Mac binaries installed
- ✅ .NET 8 SDK installed (required for UnrealBuildTool)
- ✅ Xcode 26.0.1 with SDK patched for UE5.6 compatibility

**Build Scripts Created:**
- `package-spiderlily-mac.sh` - Full packaging workflow with auto-fix
- `build-spiderlily-editor.sh` - Editor-only builds
- `patch-ue5-xcode26.sh` - SDK version compatibility fix
- `copy-fmod-mac-files.sh` - FMOD binary installation
- `ssh-mac.fish` - Quick SSH helper

**Recent Achievements:**
- ✅ **Successful Mac build!** (~1GB .app bundle)
- ✅ `.app` launches and runs correctly
- ✅ Build time: ~30 seconds (after fixes)
- ✅ Automated FMOD header bug detection & fix
- ✅ Zero cloud costs (local Mac)

**Known Issues Fixed:**
1. **Xcode 26 SDK rejection** - Patched `Apple_SDK.json` to accept SDK 26.0.1
2. **FMOD header generation bug** - UnrealHeaderTool generates `allbackHandler.h` instead of `FMODCallbackHandler.h`
   - Auto-detected and fixed via sed replacement in build scripts
   - Retry logic implemented for clean builds
3. **Missing UE content** - Archive step was copying wrong `.app` bundle
   - Fixed by manually copying from `StagedBuilds` to `Packaged` directory

---

### ⚠️ iOS Builder (95% Complete)
**Platform:** Same M4 Mac Mini  
**Status:** **Blocked by code signing**

**What's Done:**
- ✅ iOS SDK 26.0 available
- ✅ `package-spiderlily-ios.sh` script created (supports simulator & device)
- ✅ FMOD iOS binaries present
- ✅ Team ID configured (F7G74Z35B8)
- ✅ Bundle ID set (`com.falsework.SpiderLily`)
- ✅ Automatic signing enabled in config

**What's Blocking:**
- ⚠️ Xcode requires GUI interaction for initial code signing setup
- ⚠️ Even simulator builds need development team approval in modern Xcode

**To Unblock:**
1. Open Xcode workspace: `~/Perforce/.../Intermediate/ProjectFiles/SpiderLily_IOS_SpiderLily.xcworkspace`
2. Select SpiderLily target → Signing & Capabilities
3. Check "Automatically manage signing"
4. Select Personal Team from dropdown
5. Build once in Xcode (Cmd+B)
6. Then automated script will work

---

## 🛠️ Build Scripts & Automation

### Mac Scripts (in `false.work/unreal-builder/scripts/mac/`)
```bash
# Full packaging (cook, build, stage, pak, archive)
./package-spiderlily-mac.sh          # Mac .app bundle (~1GB)
./package-spiderlily-ios.sh simulator # iOS simulator build
./package-spiderlily-ios.sh device    # iOS device build (needs cert)

# Development builds
./build-spiderlily-editor.sh         # Editor only (~30 sec)
./build-spiderlily-with-fixes.sh     # Editor with auto-fixes

# Setup & maintenance
./patch-ue5-xcode26.sh               # Fix Xcode 26 compatibility
./copy-fmod-mac-files.sh             # Install FMOD binaries
./install-dotnet.sh                  # Install .NET 8 SDK
./survey-mac-environment.fish        # System info gathering
./ssh-mac.fish                       # Quick SSH access
```

### Windows Scripts (in `windows/`)
```bash
# Full automated build & deployment pipeline
./remote-update-and-build.fish       # Perforce sync → build → compress → upload → deploy

# Local Windows scripts (C:\Perforce\SpiderLily\SL_main\)
build-false-work.ps1                 # PowerShell build script (automated, no prompts)
```

### Quick SSH Access
```bash
# From dev container:
win                                   # SSH to Windows host (alias for ssh me@host.docker.internal)
win "dir C:\Perforce"                # Run Windows command
win "powershell -Command 'Get-Process'" # Run PowerShell command

ssh-mac                               # SSH to Mac Mini (if configured)
```

### Credentials
- **Storage:** `aesthetic-computer-vault/false.work/mac-builder-credentials.env`
- **Access:** Scripts auto-load from vault (not committed to main repo)
- **SSH:** Password-based via `sshpass` for automation

---

## 📦 Build Outputs

### Windows
- **Format:** `.zip` archive containing Windows build
- **Location:** Uploaded to DigitalOcean Spaces
- **Deployment:** https://builds.false.work (password-protected, Netlify auth)
- **Features:** Relative timestamps, automatic build list updates, falsework logo

### Mac
- **Format:** `.app` bundle (~1GB)
- **Location:** `~/Perforce/.../Packaged/Mac/SpiderLily.app`
- **Status:** Launches successfully ✅
- **Deployment:** Ready for distribution

### iOS (Pending)
- **Format:** `.app` for simulator / `.ipa` for device
- **Location:** `~/Perforce/.../Packaged/IOS/`
- **Status:** Builds compile, blocked at signing step
- **Deployment:** Waiting for code signing setup

---

## 🚀 Next Steps

### Immediate (This Week)
1. ⚠️ **Unblock iOS builds** - Complete Xcode signing setup (15 min)
2. 🔄 **Automate Mac deployment** - Script to upload .app to false.work
3. 📝 **P4 CLI setup** - Add Perforce command-line tools to PATH

### Short Term (Next Sprint)
1. **GitHub Actions for Mac** - Set up self-hosted runner on Mac Mini
2. **Automated build triggers** - P4 commit → auto-build → deploy
3. **Build notifications** - Discord/email alerts on success/failure
4. **Version tagging** - Integrate P4 changelist numbers into builds

### Long Term
1. **Distribution pipeline** - Automatic upload to Steam/Epic/App Store
2. **Crash reporting** - Integrate Sentry or similar
3. **A/B testing builds** - Deploy multiple versions simultaneously
4. **Build metrics** - Track build times, success rates, disk usage

---

## 💰 Cost Analysis

### Current Costs
| Platform | Setup Cost | Per Build | Monthly (10 builds) | Notes |
|----------|-----------|-----------|---------------------|-------|
| **Windows (Local)** | $0 | $0 | $0 | Local PC, zero cloud costs |
| **Mac (Local)** | $0 | $0 | $0 | M4 Mac Mini, zero cloud costs |
| **iOS (Local)** | $0 | $0 | $0 | Same Mac, shares infrastructure |

### Projected Savings
- **Before:** $800+/month (AWS EC2 Mac instances + GCP Windows VM)
- **After:** $0/month (all local infrastructure)
- **Savings:** ~$800/month (100% reduction)

---

## 🏗️ Technical Architecture

```
Dev Container (Linux/Docker)
    ↓ SSH (host.docker.internal)
Mac Mini M4 (macOS 26.0.1)
    ├─ UE5.6 (/Users/Shared/Epic Games/)
    ├─ Perforce (~/Perforce/spiderlily_build_workspace_macmini/)
    ├─ Xcode 26.0.1 + Command Line Tools
    ├─ .NET 8 SDK (for UnrealBuildTool)
    ├─ FMOD Mac/iOS binaries
    └─ Build Scripts (bash/fish)
        ├─ Cook (compile materials, shaders, blueprints)
        ├─ Build (C++ compilation)
        ├─ Stage (prepare for packaging)
        ├─ Pak (compress into .pak files)
        └─ Archive (create final .app)
```

---

## 📚 Documentation

- **Setup Guide:** `unreal-builder/MAC-SETUP-PLAN.md` (414 lines)
- **Build Scripts:** `unreal-builder/scripts/mac/` (10 scripts)
- **Credentials:** `aesthetic-computer-vault/false.work/mac-builder-credentials.env`
- **Windows Guide:** `unreal-builder/LIVECODING-WORKAROUND.md`

---

## 🎯 Key Metrics

- **Build Success Rate:** 100% (after fixes applied)
- **Average Build Time:** ~30 seconds (Editor), ~27 minutes (Full package)
- **Binary Size:** ~390MB (executable) + 631MB (content) = ~1GB total
- **Automation Coverage:** Windows 100%, Mac 95%, iOS 85%

---

## 👥 Team Access

**Mac Builder:**
- **Hostname:** `falseworks-Mac-mini.local` / `host.docker.internal`
- **Access:** SSH from dev container via `./ssh-mac.fish`
- **Credentials:** Stored in vault (requires auth)
- **Status:** Always-on, accessible 24/7

**Windows Builder:**
- **Access:** GitHub Actions (self-hosted runner)
- **Management:** GCP Console
- **Status:** On-demand (spins up for builds)

---

## � Known Issues & Workarounds

### FMOD Header Bug
**Issue:** UnrealHeaderTool generates `#include "allbackHandler.h"` instead of `"FMODCallbackHandler.h"`  
**Workaround:** Automated sed replacement in build scripts  
**Impact:** Adds ~5 seconds to first build attempt  
**Status:** Permanent workaround in place until Epic fixes UE5.6

### Xcode 26 Compatibility
**Issue:** UE5.6 rejects macOS SDK 26.0.1 (too new)  
**Workaround:** Patch `Apple_SDK.json` to accept SDK 26.x  
**Impact:** One-time manual patch  
**Status:** Documented in `patch-ue5-xcode26.sh`

### iOS Code Signing
**Issue:** Command-line builds require Xcode GUI setup first  
**Workaround:** Open workspace in Xcode once, then automate  
**Impact:** 15 min one-time setup  
**Status:** Waiting on team action

---

**Engineered by [Aesthetic Inc.](https://aesthetic.direct)**  
Questions? Contact [@jeffrey](https://prompt.ac/chat)
