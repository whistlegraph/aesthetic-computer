# Mac Unreal Builder Setup Plan

**Date:** October 28, 2025  
**Platform:** macOS with Unreal Engine 5.6 + Perforce  
**Goal:** Enable automated UE5 builds for Mac platform from dev container

## 🎯 Objectives

1. ✅ Mac has Unreal Engine installed
2. ✅ Mac has Perforce (P4) installed
3. 🔜 Enable SSH access from dev container to Mac
4. 🔜 Configure build automation scripts
5. 🔜 Set up GitHub Actions runner (optional)
6. 🔜 Test end-to-end build pipeline

---

## 📡 Step 1: Enable Remote Access to Mac Host

### From Mac Host (needs to be done once):

1. **Enable Remote Login (SSH):**
   ```bash
   # Via System Settings:
   # System Settings → General → Sharing → Remote Login (turn ON)
   
   # Or via command line:
   sudo systemsetup -setremotelogin on
   
   # Verify it's running:
   sudo launchctl list | grep ssh
   ```

2. **Check your username:**
   ```bash
   whoami
   echo $USER
   ```

3. **Test SSH locally on Mac:**
   ```bash
   ssh localhost "uname -a"
   ```

4. **Find Mac's IP addresses:**
   ```bash
   ifconfig | grep "inet " | grep -v 127.0.0.1
   ```

### From Dev Container (after SSH is enabled on Mac):

```bash
# Test connection via Docker's special hostname
ssh YOUR_MAC_USERNAME@host.docker.internal "uname -a"

# If that works, try to get Unreal Engine info
ssh YOUR_MAC_USERNAME@host.docker.internal "ls -la /Users/Shared/Epic\ Games/UE_5.* 2>/dev/null || ls -la ~/Library/Application\ Support/Epic/UnrealEngine/"

# Check Perforce installation
ssh YOUR_MAC_USERNAME@host.docker.internal "which p4 && p4 -V"
```

---

## 🏗️ Step 2: Survey Mac Environment

Once SSH access is established, we need to discover:

### Unreal Engine Installation
```bash
# Find UE5 installation paths
find /Users/Shared/Epic\ Games -name "UnrealEditor" -type f 2>/dev/null
find ~/Library/Application\ Support/Epic -name "UnrealEngine" 2>/dev/null

# Check installed versions
ls -la "/Users/Shared/Epic Games/" 2>/dev/null || ls -la ~/Applications/ | grep -i unreal

# Find UnrealBuildTool
find /Users/Shared -name "UnrealBuildTool" 2>/dev/null
```

### Perforce Configuration
```bash
# Check P4 config
p4 set

# Test connection to Helix Core
p4 -p ssl:falsework.helixcore.io:1666 info

# Check existing workspaces
p4 clients -u $(whoami)
```

### System Resources
```bash
# CPU info
sysctl -n machdep.cpu.brand_string
sysctl -n hw.ncpu
sysctl -n hw.physicalcpu

# RAM info
sysctl -n hw.memsize | awk '{print $0/1024/1024/1024 " GB"}'

# Disk space
df -h /

# macOS version
sw_vers
```

### Xcode & Build Tools
```bash
# Check Xcode installation
xcode-select -p

# Check Xcode version
xcodebuild -version

# Check available SDKs
xcodebuild -showsdks
```

---

## 🛠️ Step 3: Create Mac Build Scripts

### Directory Structure
```
false.work/unreal-builder/
├── scripts/
│   ├── mac/
│   │   ├── setup-mac-builder.sh       # Initial setup
│   │   ├── sync-perforce.sh           # P4 sync
│   │   ├── build-mac.sh               # UE5 build for Mac
│   │   ├── cook-mac.sh                # Cook content
│   │   ├── package-mac.sh             # Package .app
│   │   └── upload-artifacts.sh        # Upload to storage
│   ├── create-gcp-vm.sh               # Already exists (Windows)
│   └── ...
└── config/
    ├── mac-build-config.json          # Mac-specific config
    └── build-config.json              # Existing Windows config
```

### Key Scripts to Create

1. **setup-mac-builder.sh** - Configure Mac for automated builds
2. **build-mac.sh** - Run UnrealBuildTool for Mac target
3. **remote-build-trigger.sh** - Trigger builds from dev container
4. **deploy-mac-build.sh** - Deploy to false.work website

---

## 🔧 Step 4: Mac-Specific Considerations

### Differences from Windows Builder

| Aspect | Windows (Current) | Mac (New) |
|--------|------------------|-----------|
| **UE Editor** | `UnrealEditor-Cmd.exe` | `UnrealEditor-Cmd` |
| **Build Tool** | `UnrealBuildTool.exe` | `UnrealBuildTool` |
| **Path Style** | `C:\Perforce\...` | `/Users/.../Perforce/...` |
| **Shell** | PowerShell | bash/zsh/fish |
| **Package Output** | `.exe` installer | `.app` bundle + `.dmg` |
| **Code Signing** | Optional | **Required** for distribution |
| **Notarization** | N/A | **Required** for distribution |

### Mac Build Requirements
- Valid Apple Developer ID
- Code signing certificate
- Provisioning profiles
- Entitlements configuration
- Notarization workflow (for public distribution)

### LiveCoding Issue
The Windows builder had LiveCoding issues - check if Mac has similar problems:
```bash
# Test if -NoLiveCoding flag is needed on Mac too
```

---

## 🚀 Step 5: Build Workflow Design

### Option A: SSH Remote Execution (Simpler)
```bash
# From dev container, trigger build on Mac via SSH
ssh user@host.docker.internal "/path/to/build-mac.sh"
```

**Pros:** 
- Simple to set up
- No GitHub runner needed
- Direct control from dev container

**Cons:**
- Mac must be powered on and accessible
- No GitHub Actions integration
- Manual artifact retrieval

### Option B: GitHub Actions Runner on Mac (Production)
```yaml
# .github/workflows/ue5-build-mac.yml
name: UE5 Build (Mac)
on:
  push:
    branches: [main]
  workflow_dispatch:

jobs:
  build:
    runs-on: self-hosted
    labels: [mac, ue5, perforce]
    steps:
      - name: Sync Perforce
      - name: Build Mac
      - name: Cook Content
      - name: Package .app
      - name: Upload Artifacts
```

**Pros:**
- Full CI/CD integration
- Artifact management
- Build history & logs
- Parallel builds with Windows

**Cons:**
- More setup required
- Mac must run GitHub Actions runner service

---

## 📋 Step 6: Testing Checklist

### Phase 1: Connectivity
- [ ] SSH enabled on Mac
- [ ] Can SSH from dev container to Mac
- [ ] Can execute commands remotely
- [ ] Can transfer files (scp/rsync)

### Phase 2: Environment Discovery
- [ ] Located UE5 installation path
- [ ] Verified Perforce connection
- [ ] Checked Xcode installation
- [ ] Verified disk space (need ~200GB free)
- [ ] Confirmed system specs meet requirements

### Phase 3: Script Development
- [ ] Create P4 sync script
- [ ] Create build script
- [ ] Create cook script
- [ ] Create package script
- [ ] Test each script individually

### Phase 4: Integration Testing
- [ ] Run full build pipeline
- [ ] Verify .app bundle created
- [ ] Test .app launches
- [ ] Verify artifacts upload
- [ ] Test deployment to false.work

### Phase 5: Automation
- [ ] Set up GitHub Actions runner (optional)
- [ ] Configure automated triggers
- [ ] Set up notifications
- [ ] Document the process

---

## 🔐 Security Considerations

1. **SSH Keys:** Use key-based auth instead of passwords
   ```bash
   # From dev container
   ssh-keygen -t ed25519 -C "devcontainer-to-mac"
   ssh-copy-id user@host.docker.internal
   ```

2. **Perforce Credentials:** Store securely
   - Use P4TICKETS for authentication
   - Don't commit credentials to git
   - Use environment variables or secrets

3. **Code Signing:** Secure the signing identity
   - Keep certificates in Keychain
   - Use security command-line tools
   - Consider CI-specific signing identity

---

## 💰 Cost Comparison

### Current: Windows on GCP
- **Setup:** ~$28
- **Per rebuild:** ~$2-3
- **Monthly (10 builds):** ~$30-40

### Proposed: Mac Host (already owned)
- **Setup:** $0 (Mac already purchased)
- **Per rebuild:** $0 (electricity negligible)
- **Monthly:** $0
- **Limitation:** Mac must be powered on and accessible

### Alternative: Mac on Cloud
- **AWS EC2 Mac (mac2.metal):** ~$1.08/hour (~$800/month if 24/7)
- **Not recommended** for intermittent builds

---

## 🎯 Immediate Next Steps

1. ✅ **Enable SSH on Mac host**
2. ✅ **Test connection from dev container**
3. ✅ **Run environment survey scripts**
4. ✅ **Document UE5 and P4 paths**
5. ✅ **Copy FMOD Mac binaries to Perforce workspace**
6. ✅ **Install .NET 8 SDK**
7. ✅ **Patch UE5.6 for Xcode 26 compatibility**
8. ✅ **Fix FMOD UnrealHeaderTool generation bug**
9. ✅ **Successfully build SpiderLily Editor for Mac!**
10. 🔜 **Test opening project in Unreal Editor**
11. 🔜 **Locate or install P4 CLI**
12. 🔜 **Automate build pipeline**

---

## 📝 Notes & Discoveries

### Connection Details
- **Mac Hostname:** `falseworks-Mac-mini.local`
- **Mac Host IP (Docker):** `host.docker.internal` → `192.168.65.254`
- **Mac Host IP (Gateway):** `172.17.0.1` (alternative)
- **SSH Port:** 22 (standard)
- **Username:** `falsework`
- **Password:** Stored in `aesthetic-computer-vault/false.work/mac-builder-credentials.env`
- **SSH Command:** `sshpass -p '$MAC_PASSWORD' ssh -o StrictHostKeyChecking=no falsework@host.docker.internal`

### Discoveries
- ✅ SSH enabled and working!
- ✅ Dev container has SSH client available
- ✅ Docker DNS resolves `host.docker.internal` correctly
- ✅ `sshpass` installed for password-less SSH automation

### Answers ✅
- ✅ **Mac username:** `falsework`
- ✅ **Mac password:** Stored in `aesthetic-computer-vault/false.work/mac-builder-credentials.env`
- ✅ **UE5.6 location:** `/Users/Shared/Epic Games/UE_5.6`
- ✅ **macOS version:** 26.0.1 (Sequoia)
- ✅ **Architecture:** Apple M4 (arm64), 10 cores, 16GB RAM
- ✅ **Disk space:** 460GB total, 297GB free (4% used)
- ✅ **Perforce workspace:** `~/Perforce/spiderlily_build_workspace_macmini/SL_main/`
- ✅ **Project file:** `SpiderLily.uproject` present
- ✅ **FMOD:** Mac binaries copied to Plugins directory
- ⚠️ **P4 CLI:** Not in PATH (need to locate or install)
- ⚠️ **P4V App:** Installed at `/Applications/p4v.app`
- [ ] Do we need code signing for internal testing?

---

## 📚 References

- Windows Builder: `/workspaces/aesthetic-computer/false.work/unreal-builder/`
- Windows README: `README.md`
- GCP Scripts: `scripts/create-gcp-vm.sh`
- LiveCoding Fix: `LIVECODING-WORKAROUND.md`

---

**Last Updated:** October 28, 2025  
**Status:** 🟡 Planning Phase - SSH access needed  
**Next Action:** Enable Remote Login on Mac host

## 📱 iOS Build Status

### Current Situation

Mac builds are **fully working** ✅

iOS builds are **blocked by code signing requirements** ⚠️

Even iOS Simulator builds in Xcode 26.0 require:
- Apple ID signed into Xcode
- Development Team ID configured
- Automatic or manual code signing enabled

### To Enable iOS Builds

**Option 1: Free Apple Developer Account (Simulator Only)**
1. Sign into Xcode with Apple ID (free)
2. Get your Team ID from Xcode preferences
3. Update DefaultEngine.ini:
   ```ini
   [/Script/IOSRuntimeSettings.IOSRuntimeSettings]
   BundleIdentifier=com.falsework.SpiderLily
   bAutomaticSigning=True
   IOSTeamID=YOUR_TEAM_ID_HERE
   ```
4. Run: `./package-spiderlily-ios.sh simulator`

**Option 2: Paid Apple Developer Account (Device Builds)**
1. Enroll in Apple Developer Program ($99/year)
2. Create provisioning profile for SpiderLily
3. Download and install profile on Mac
4. Configure signing certificate
5. Run: `./package-spiderlily-ios.sh device`

### Scripts Created

- `/workspaces/aesthetic-computer/false.work/unreal-builder/scripts/mac/package-spiderlily-ios.sh`
  - Supports both `simulator` and `device` builds
  - Auto-fixes FMOD header bug (same as Mac)
  - Requires code signing setup to complete

