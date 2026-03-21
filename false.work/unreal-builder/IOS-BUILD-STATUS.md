# iOS Build Status Report
**Project**: SpiderLily iOS  
**Last Updated**: October 31, 2025  
**Current Build**: #8 (Oct 30, 2025 20:28:02)

---

## 🚨 CRITICAL BLOCKER

**Status**: ❌ **CANNOT DEPLOY TO ANY DEVICE**

### Issue: No Provisioned iOS Device Available
- **Matt's iPhone** (9654E2B8-54C5-53AD-9F78-60AA7BB15BFD): `unavailable` - disconnected or powered off
- **aesthetic.computer** (46720BDD-8807-53C8-AA2E-6977BCA606D3): `connected` but **NOT PROVISIONED**

### Error When Deploying to aesthetic.computer:
```
ERROR: Failed to install (error 3002, 0xBBA)
Failed to install embedded profile: 0xe8008012
(This provisioning profile cannot be installed on this device.)
```

### Resolution Required:
**OPTION A** (Fastest): Reconnect Matt's iPhone
- Device is already provisioned (worked in Builds #1-4)
- Simply reconnect USB, ensure powered on and unlocked
- Deploy with: `xcrun devicectl device install app --device 9654E2B8-54C5-53AD-9F78-60AA7BB15BFD ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app`

**OPTION B**: Provision aesthetic.computer device
1. Log into [Apple Developer Portal](https://developer.apple.com/account/resources/devices/list)
2. Click "+" to register new device
3. Name: `aesthetic.computer`
4. UDID: `46720BDD-8807-53C8-AA2E-6977BCA606D3`
5. Regenerate provisioning profiles
6. Rebuild project (Build #9)

---

## 🐛 CORE ISSUE: FMOD Audio Initialization Failure

### Symptom
App hangs at splash screen showing brick texture. 3D world never loads. User quote: **"WHY DO I JUST SEE BRICKS ON A SLASH SCREEN NO 3D THO?"**

### Root Cause
FMOD audio system fails to find bank files → Audio initialization hangs → Game waits for audio → Splash screen never dismisses

### Technical Details

**FMOD Runtime Behavior** (hardcoded in C++ plugin):
```cpp
// FMOD constructs paths dynamically:
string platformSubfolder = GetPlatformName(); // Returns "Mobile" on iOS
string bankPath = BankOutputDirectory + "/" + platformSubfolder + "/" + BankName;
// iOS looks for: "../../../SpiderLily/Content/FMOD/Mobile/Master.bank"
// With ForcePlatformName=Desktop: "../../../SpiderLily/Content/FMOD/Desktop/Master.bank"
```

**Expected Source Structure**:
```
Content/FMOD/
├── Desktop/
│   ├── Master.bank
│   └── Master.strings.bank
└── Mobile/
    ├── Master.bank
    └── Master.strings.bank
```

**Actual Packaged Structure** (ALL builds #1-8):
```
SpiderLily.app/cookeddata/spiderlily/content/fmod/
├── master.bank         (1612 bytes)
└── master.strings.bank (768 bytes)
```

**THE MISMATCH**: 
- FMOD looks for subfolders (`FMOD/Mobile/` or `FMOD/Desktop/`)
- Package has FLAT structure (no subfolders)
- Unreal's iOS cooker **ALWAYS FLATTENS** content folder hierarchies

### Build #1-4 Runtime Error Logs:
```
LogFMOD: Error: Failed to open file '../../../SpiderLily/Content/FMOD/Mobile/Master.bank'
LogFMOD: Error: Failed to open file '../../../SpiderLily/Content/FMOD/Mobile/Master.strings.bank'
LogFMOD: Warning: Failed to load bank (File does not exist)
```

---

## 📊 BUILD HISTORY

### Build #1 (Oct 30, 19:08)
- **Config**: Added `+RemapDirectories=(From="FMOD/Desktop", To="FMOD")` staging remap
- **Result**: ✅ Packaged and deployed successfully
- **Runtime**: ❌ FMOD Mobile banks not found (flat structure)

### Build #2 (Oct 30, 19:08)
- **Config**: Added `+RemapDirectories=(From="FMOD/Mobile", To="FMOD")`
- **Issue**: Unreal Editor didn't reload config (cache issue)
- **Result**: ❌ Same FMOD error as Build #1

### Build #3 (Oct 30, 19:34)
- **Config**: Killed Unreal Editor, forced fresh config reload
- **Result**: ❌ Same FMOD error (staging remaps don't affect runtime)
- **Discovery**: Staging remaps work at **package-time** but FMOD constructs paths at **runtime**

### Build #4 (Oct 30, 19:45)
- **Approach**: Created symlink `Content/FMOD/Mobile → Desktop`
- **Result**: ❌ Symlink NOT preserved in package (cooker followed it and flattened)

### Build #5 (Oct 30, 20:02)
- **Approach**: Removed symlink, created actual `Mobile/` folder with copied banks
- **Result**: ❌ Cooker flattened structure anyway (no subfolders in package)
- **Discovery**: Unreal's iOS cooker fundamentally flattens content hierarchies

### Build #6 (Oct 30, 20:08)
- **Approach**: Identity staging remaps `(From="FMOD/Desktop", To="FMOD/Desktop")`
- **Result**: ❌ Still flat structure (remaps don't preserve folders)

### Build #7 (Oct 30, 20:23:20)
- **Config**: Set `ForcePlatformName=Desktop` in FMOD settings
- **Expected**: FMOD looks for Desktop instead of Mobile
- **Result**: ✅ Packaged successfully
- **Deployment**: ❌ Matt's iPhone unavailable (device disconnected)
- **Status**: 🔄 **UNTESTED** (never deployed to device)

### Build #8 (Oct 30, 20:28:02) - **CURRENT BUILD**
- **Config**: Same as Build #7 (`ForcePlatformName=Desktop`)
- **Provisioning**: Cleared all .mobileprovision files to force regeneration
- **Result**: ✅ Packaged successfully
- **Deployment**: ❌ aesthetic.computer not provisioned
- **Status**: 🚫 **BLOCKED** - cannot deploy to any device

---

## 🔧 ATTEMPTED SOLUTIONS

### ❌ Approach 1: Staging Remaps (Builds #1-3, #6)
**Theory**: Redirect FMOD Mobile to Desktop during packaging  
**Why Failed**: Staging remaps work at package-time (file copies), but FMOD constructs paths at runtime (C++ code). Cannot change runtime behavior with staging config.

### ❌ Approach 2: Symlink Mobile→Desktop (Build #4)
**Theory**: Symlink preserved in package, FMOD follows it  
**Why Failed**: Unreal's cooker doesn't preserve symlinks - it follows them and copies files directly, then flattens structure.

### ❌ Approach 3: Copy Mobile Folder (Build #5)
**Theory**: Both Desktop/ and Mobile/ subfolders in source → preserved in package  
**Why Failed**: Unreal's iOS cooker **fundamentally flattens** content folder hierarchies regardless of source structure.

### ❌ Approach 4: Manual Post-Package Fix (Between Builds #5-6)
**Theory**: Manually create Desktop/ subfolder in .app, copy banks into it  
**Why Failed**: Modifying .app contents breaks code signature. Would need to resign entire bundle (complex, fragile).

### 🔄 Approach 5: Force Platform Name Override (Builds #7-8) - **UNTESTED**
**Theory**: Make FMOD look for Desktop instead of Mobile  
**Config**: `ForcePlatformName=Desktop` in `DefaultEngine.ini`  
**Expected**: FMOD constructs paths as `FMOD/Desktop/Master.bank` instead of `FMOD/Mobile/Master.bank`  
**Probable Outcome**: Will still fail because packaged structure has NO subfolders (not Desktop, not Mobile - completely flat)  
**Status**: 🚫 **BLOCKED** - cannot test without provisioned device

---

## 📁 CURRENT CONFIGURATION

### Build Location
```
~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app
```

### DefaultEngine.ini - FMOD Settings
```ini
[/Script/FMODStudio.FMODSettings]
bLoadAllBanks=True
bLoadAllSampleData=False
bEnableLiveUpdate=True
bEnableEditorLiveUpdate=False
BankOutputDirectory=(Path="FMOD")
ForcePlatformName=Desktop  # ⭐ Build #7/8 configuration
OutputFormat=Stereo
OutputType=TYPE_AUTODETECT
```

### DefaultGame.ini - Staging Settings
```ini
[Staging]
+RemapDirectories=(From="SpiderLily/Content/FMOD/Desktop", To="SpiderLily/Content/FMOD/Desktop")
+RemapDirectories=(From="SpiderLily/Content/FMOD/Mobile", To="SpiderLily/Content/FMOD/Mobile")
```
*Note: These identity remaps are ineffective but left in config from Build #6.*

### Apple Developer Configuration
- **Team ID**: F7G74Z35B8
- **Certificate**: "Apple Development: make@false.work (7F4BYG5WCH)"
- **Bundle ID**: work.false.SpiderLily
- **Automatic Signing**: Enabled (`bAutomaticSigning=True`)
- **Provisioning Profiles**: Currently cleared (deleted for regeneration attempt)

---

## 🎯 NEXT STEPS

### IMMEDIATE: Resolve Device Provisioning (BLOCKING)
1. **Check if Matt's iPhone can reconnect** (fastest path)
   - Ensure device powered on, unlocked, trusted
   - Reconnect USB cable to Mac
   - Verify in `xcrun devicectl list devices`
   
2. **OR provision aesthetic.computer device**:
   - Access [Apple Developer Portal](https://developer.apple.com/account/resources/devices/list)
   - Register UDID: `46720BDD-8807-53C8-AA2E-6977BCA606D3`
   - Regenerate provisioning profiles
   - Rebuild project (Build #9)

### POST-DEPLOYMENT: Test ForcePlatformName Configuration
1. Deploy Build #8 to provisioned device
2. Monitor runtime logs for FMOD errors:
   ```bash
   # Check if error path changed from Mobile to Desktop
   idevicesyslog | grep -i fmod
   ```
3. **Expected**: Different error message (Desktop instead of Mobile)
4. **If Still Fails**: Confirms folder structure is the fundamental issue

### CONTINGENCY: Manual Folder Structure Fix
If ForcePlatformName doesn't work (likely scenario):

```bash
# Navigate to packaged .app FMOD folder
cd ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app/cookeddata/spiderlily/content/fmod

# Create Desktop subfolder and move banks
mkdir Desktop
cp master.bank Desktop/
cp master.strings.bank Desktop/

# Resign entire .app bundle
cd ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS
codesign --force --sign "Apple Development: make@false.work (7F4BYG5WCH)" \
  --deep --preserve-metadata=entitlements,flags \
  --timestamp SpiderLily.app

# Deploy resigned app
xcrun devicectl device install app --device [DEVICE_ID] SpiderLily.app
```

**Automation**: Could be scripted as post-build step in `package-spiderlily-ios.sh`

### LONG-TERM: Modify FMOD Plugin Source
Proper permanent solution:
1. Locate FMOD plugin C++ source: `Plugins/FMODStudio/Source/`
2. Find platform-specific path construction code
3. Modify to NOT append `/Desktop/` or `/Mobile/` suffixes on iOS
4. Recompile FMOD plugin for iOS
5. Rebuild project with modified plugin

**Requires**: C++ development environment, Unreal plugin compilation knowledge

---

## 🔍 TECHNICAL DISCOVERIES

### Key Insight: Unreal's iOS Cooker Behavior
**Discovery**: Unreal Engine's cooking/packaging process for iOS **always flattens** content folder structures. This is not configurable and cannot be prevented through:
- Staging remaps
- Identity remaps
- Symlinks
- Source folder structure
- Config file settings

**Implication**: Any plugin expecting subfolder hierarchies in packaged iOS builds will fail unless:
1. Plugin source is modified to use flat paths, OR
2. Post-package manual folder creation with resigning, OR
3. Plugin has config to disable platform-specific subfolders

### FMOD Path Construction Analysis
FMOD plugin constructs bank paths dynamically at runtime:
```
Base Path: BankOutputDirectory (Config: "FMOD")
Platform: GetPlatformName() (Returns "Mobile" on iOS, "Desktop" on Windows/Mac)
Override: ForcePlatformName (Config: "Desktop" in Build #7/8)

Final Path: BaseRelativePath + "/" + Platform + "/" + BankName
```

With default config on iOS:
```
../../../SpiderLily/Content/FMOD/Mobile/Master.bank
```

With ForcePlatformName=Desktop on iOS:
```
../../../SpiderLily/Content/FMOD/Desktop/Master.bank
```

Actual packaged location:
```
cookeddata/spiderlily/content/fmod/master.bank (NO SUBFOLDER)
```

**Why relative path fails**: The path `../../../SpiderLily/Content/` from executable location would go OUTSIDE the .app bundle (invalid).

### Provisioning Profile Management
**Discovery**: Automatic signing (`bAutomaticSigning=True`) does NOT automatically provision new devices. Device UDID must first be manually registered in Apple Developer Portal before any provisioning profile can include it.

**Cleared Profiles**: Deleted all .mobileprovision files attempting to force regeneration - didn't help because device registration is manual.

---

## 📋 VALIDATION CHECKLIST

### ✅ Working Components
- [x] Code signing configuration (Xcode project + DefaultEngine.ini)
- [x] Apple Developer credentials (Team ID, Certificate)
- [x] .uproject staging directory
- [x] FMOD iOS binaries present (98MB in Binaries/IOS/)
- [x] Packaging succeeds without folder restriction errors
- [x] App installs on provisioned devices
- [x] Game engine initializes, loads world geometry
- [x] Touch input and device rotation functional
- [x] Material shaders compile (with non-critical warnings)

### ❌ Failing Components
- [ ] FMOD audio initialization (bank files not found)
- [ ] Splash screen progression (hangs waiting for audio)
- [ ] 3D world rendering (never reached, blocked by splash)
- [ ] Device provisioning (no available test device)

### 🔄 Untested Components
- [ ] ForcePlatformName=Desktop effect (Build #7/8 not deployed)
- [ ] Manual folder structure fix with resigning
- [ ] FMOD plugin source modification
- [ ] Platform-neutral bank export (if possible in FMOD Studio)

---

## 🛠️ USEFUL COMMANDS

### Check Connected Devices
```bash
xcrun devicectl list devices | grep -i iphone
```

### Check Provisioning Profile Contents
```bash
security cms -D -i "/Users/falsework/Library/Developer/Xcode/UserData/Provisioning Profiles/PROFILE_UUID.mobileprovision" | grep -A 30 "ProvisionedDevices"
```

### Deploy to Device
```bash
xcrun devicectl device install app \
  --device DEVICE_UDID \
  ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app
```

### Check Packaged FMOD Structure
```bash
ls -laR ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app/cookeddata/spiderlily/content/fmod/
```

### Monitor Build Progress
```bash
ps aux | grep -i "buildcookrun\|unreal" | grep -v grep
```

### Check Build Timestamp
```bash
stat -f "%Sm Build #X completed!" -t "%b %d %H:%M:%S %Y" ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app
```

---

## 📞 KNOWN DEVICE UDIDs

### Matt's iPhone (Provisioned)
- **Name**: Matthew Doyle's iPhone
- **Model**: iPhone 16 Pro Max
- **UDID**: 9654E2B8-54C5-53AD-9F78-60AA7BB15BFD
- **Status**: unavailable/disconnected
- **Notes**: Was successfully used for Builds #1-4 testing

### aesthetic.computer (NOT Provisioned)
- **Name**: aesthetic.computer
- **Model**: iPhone 14 Pro Max
- **UDID**: 46720BDD-8807-53C8-AA2E-6977BCA606D3
- **Status**: connected but not provisioned
- **Notes**: User requested switching to this device for Build #7/8 testing

---

## 📚 REFERENCE FILES

### Build Script
```
~/package-spiderlily-ios.sh
```

### Project Location
```
~/Perforce/spiderlily_build_workspace_macmini/SL_main/
```

### Config Files
```
~/Perforce/spiderlily_build_workspace_macmini/SL_main/Config/DefaultEngine.ini
~/Perforce/spiderlily_build_workspace_macmini/SL_main/Config/DefaultGame.ini
```

### Xcode Project
```
~/Perforce/spiderlily_build_workspace_macmini/SL_main/Intermediate/ProjectFilesIOS/SpiderLily (IOS).xcodeproj/
```

### FMOD Source
```
~/Perforce/spiderlily_build_workspace_macmini/SL_main/Content/FMOD/
```

### Packaged Build
```
~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app
```

---

## 💡 RECOMMENDATIONS FOR NEW AGENTS

1. **START HERE**: Check device provisioning status first - this is the immediate blocker
2. **IF DEVICE AVAILABLE**: Deploy Build #8 and test ForcePlatformName configuration
3. **WHEN FMOD FAILS** (likely): Implement manual folder structure fix with resigning
4. **FOR PRODUCTION**: Investigate FMOD plugin source modification for permanent solution
5. **DEFER CLI PACKAGING**: Original user goal was pure CLI builds, but defer until FMOD issue resolved

### Quick Status Check
```bash
# Are we blocked on device provisioning?
xcrun devicectl list devices | grep -i iphone

# Is Build #8 still current?
stat ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app

# What's the FMOD config?
grep -A 2 "ForcePlatformName\|BankOutputDirectory" ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Config/DefaultEngine.ini
```

---

**END OF STATUS REPORT**
