# Deploy SpiderLily to iOS via Xcode

Since UnrealBuildTool doesn't properly apply iOS team settings from `DefaultEngine.ini` when regenerating Xcode projects, the most reliable way to deploy to iOS is through Xcode directly.

## Prerequisites

✅ Content already cooked at: `~/Perforce/spiderlily_build_workspace_macmini/SL_main/Saved/Cooked/IOS/`  
✅ Team configured in Xcode: Matthew Doyle (F7G74Z35B8)  
✅ Provisioning profile: `07a4a65c-b32d-4d7b-99f6-1091214c6606`  
✅ Bundle ID: `work.false.SpiderLily`

## Steps

### 1. Open Xcode Workspace
```bash
open ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Intermediate/ProjectFiles/SpiderLily_IOS_SpiderLily.xcworkspace
```

### 2. Select Target & Device
- Select the **SpiderLily** scheme (top left)
- Select your connected **iPhone** as the destination device

### 3. Configure Signing (if needed)
- Select **SpiderLily** target in the project navigator
- Go to **Signing & Capabilities** tab
- Check **"Automatically manage signing"**
- Select **Team: Matthew Doyle**
- Bundle Identifier should be: `work.false.SpiderLily`

### 4. Build and Run
- Press **⌘+R** (or click the Play button)
- Xcode will:
  - Compile the iOS binary
  - Sign the app with your certificate
  - Install to your connected iPhone
  - Launch the app

## Troubleshooting

### "Failed to open descriptor file"
If the app launches but shows this error, the `.uproject` file is missing. We fixed this by creating the directory structure:

```bash
cd ~/Perforce/spiderlily_build_workspace_macmini/SL_main
mkdir -p Binaries/SpiderLily
cp SpiderLily.uproject Binaries/SpiderLily/
```

### "Failed to find cooked content"
The cooked content should be at:
```
~/Perforce/spiderlily_build_workspace_macmini/SL_main/Saved/Cooked/IOS/SpiderLily/
```

If missing, run cook-only:
```bash
./cook-stage-ios-remote.fish device
```

### "Xcode regenerates project"
Unreal's build process regenerates the Xcode project, wiping team settings. Always use Xcode GUI to build iOS, not command line `xcodebuild`.

## Why This Works

When you build in **Xcode GUI**:
1. ✅ Team settings persist in Xcode preferences (not the project file)
2. ✅ Xcode automatically manages provisioning profiles
3. ✅ Code signing works correctly
4. ✅ Installation to device is automatic

When you build via **command line** (BuildCookRun):
1. ❌ UBT regenerates `.xcodeproj` files, wiping team settings
2. ❌ Even with `-provision` flag, settings don't stick
3. ❌ `xcodebuild` requires team in project file, not just Xcode prefs
4. ❌ This is a known UE5 limitation for iOS

## Alternative: Pre-compiled Binary Deployment

If you want to automate iOS deployment, you need to:
1. Build once in Xcode GUI (to create signed `.app`)
2. Use `ios-deploy` or `xcrun devicectl` to install pre-built `.app`
3. Don't rebuild - just re-sign and deploy existing binary

This is beyond scope for now but is the path to automation.
