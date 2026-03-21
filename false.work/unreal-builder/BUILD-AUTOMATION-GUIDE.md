# Spider Lily Build Automation - Complete Guide

## 🎯 Overview

Automated build system for false.work's Spider Lily UE5.6 project. Builds run daily on a GCP VM, syncing from Perforce and creating distributable game packages.

**Status: ✅ OPERATIONAL**

- **VM**: GCP n2-standard-8 (136.114.176.135)
- **Build Time**: ~5-10 min (editor) / 30-60 min (packaged)
- **Perforce**: Connected to falsework.helixcore.io
- **Automation**: GitHub Actions self-hosted runner

---

## 📦 What Gets Built

### Editor Build (Fast - 5-10 minutes)
- Compiles `UnrealEditor-SpiderLily.dll`
- For development/testing in Unreal Editor
- Requires UE5.6 to run

### Packaged Build (Slow - 30-60 minutes)
- Creates standalone `SpiderLily.exe`
- **This is what the team needs!**
- No Unreal Engine required to run
- Includes all assets, cooked content, and game code

---

## 🚀 Quick Start

### For Team Members (Just Want to Test the Game)

1. **Download the latest build** (link will be provided by build admin)
2. **Extract the ZIP file**
3. **Navigate to**: `WindowsNoEditor\SpiderLily\Binaries\Win64\`
4. **Run**: `SpiderLily.exe`

That's it! No installation needed.

### For Build Administrators

See sections below for setup and automation.

---

## 🛠️ Initial Setup (One-Time)

### 1. Create the VM

From your local machine with aesthetic-computer-vault cloned:

```fish
cd /workspaces/aesthetic-computer/false.work/unreal-builder/scripts
source load-secrets.fish
./quick-setup-gcp.sh
```

This creates the GCP VM and runs the bootstrap script automatically.

### 2. Manual Setup (if needed)

RDP into the VM:
```fish
./connect-rdp.sh
```

The bootstrap script installs everything automatically, but if you need to run it manually:
```powershell
# Download and run bootstrap
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/whistlegraph/aesthetic-computer/main/false.work/unreal-builder/scripts/bootstrap-windows-minimal.ps1" -OutFile "C:\bootstrap.ps1"
powershell -ExecutionPolicy Bypass -File "C:\bootstrap.ps1"
```

### 3. Install UE5.6

**IMPORTANT**: The bootstrap can't install UE5 automatically (requires Epic account).

1. Open Epic Games Launcher (installed by bootstrap)
2. Sign in with your Epic account
3. Go to **Library** → **Engine Versions**
4. Click **+** to add engine
5. Select **5.6** and click **Install**
6. **CRITICAL**: In Options, check:
   - ✅ **Core Components**
   - ✅ **Target Platforms → Windows (Desktop)**
   - ⚠️ DO NOT select "Windows (Server)" or other platforms (saves time/space)
7. Install to: `C:\Program Files\Epic Games\UE_5.6`
8. Wait 1-2 hours for download/install

### 4. Set Up Perforce

```powershell
# Trust SSL certificate
p4 trust -y -f ssl:falsework.helixcore.io:1666

# Login (password from vault: ue5-builder.env)
$env:P4PORT = "ssl:falsework.helixcore.io:1666"
$env:P4USER = "machine"
p4 login
# Enter password when prompted: AestheticComp1

# Create workspace
p4 client spiderlily_build_workspace
# Paste this configuration:
```

```
Client: spiderlily_build_workspace
Owner: machine
Root: C:\Perforce
Options: noallwrite noclobber nocompress unlocked nomodtime normdir
SubmitOptions: submitunchanged
LineEnd: local
Stream: //SpiderLily/SL_main
```

```powershell
# Sync the project
cd C:\Perforce
p4 sync
```

### 5. First Build Test

```powershell
cd C:\Perforce
$BuildTool = "C:\Program Files\Epic Games\UE_5.6\Engine\Binaries\DotNET\UnrealBuildTool\UnrealBuildTool.exe"
& $BuildTool SpiderLilyEditor Win64 Development -Project="C:\Perforce\SpiderLily.uproject" -WaitMutex
```

If successful, you'll see:
```
Result: Succeeded
Total execution time: 105.49 seconds
```

---

## 📅 Daily Automated Builds

### Manual Build (On VM)

**Quick Editor Build** (~5 minutes):
```powershell
cd C:\scripts
.\daily-build.ps1
```

**Full Packaged Build** (~60 minutes):
```powershell
cd C:\scripts
.\daily-build.ps1 -Package
```

**Package Only** (for quick team testing):
```powershell
cd C:\scripts
.\package-for-team.ps1
```

### GitHub Actions Automation

#### Set Up Self-Hosted Runner

1. **Get runner token** from: https://github.com/whistlegraph/aesthetic-computer/settings/actions/runners/new

2. **On the VM**, run:
```powershell
cd C:\scripts
.\setup-github-runner.ps1 -GitHubToken "YOUR_TOKEN_HERE"
```

3. **Add GitHub secrets** at: https://github.com/whistlegraph/aesthetic-computer/settings/secrets/actions

Add these secrets:
- `P4_SERVER`: `ssl:falsework.helixcore.io:1666`
- `P4_USER`: `machine`
- `P4_PASSWORD`: `AestheticComp1` (from vault)
- `P4_WORKSPACE`: `spiderlily_build_workspace`

#### Trigger Builds

**Automatic Daily Builds**: Runs at 2 AM UTC every day

**Manual Trigger**:
1. Go to: https://github.com/whistlegraph/aesthetic-computer/actions
2. Select "Spider Lily Daily Build"
3. Click "Run workflow"
4. Choose options:
   - ✅ Package for distribution (if team needs standalone build)
   - ✅ Upload to cloud storage (if configured)

---

## 📤 Distributing Builds to Team

### Option 1: Compress and Upload Manually

```powershell
# After running package-for-team.ps1
$BuildDir = Get-ChildItem C:\Builds\Package_* | Sort-Object LastWriteTime -Descending | Select-Object -First 1

# Compress
Compress-Archive -Path $BuildDir.FullName -DestinationPath "C:\Builds\SpiderLily_$(Get-Date -Format 'yyyyMMdd').zip"

# Upload to Google Drive, Dropbox, etc.
# Share link with team
```

### Option 2: Auto-Upload (Needs Configuration)

Edit `C:\scripts\daily-build.ps1` and add your upload method:

**Google Cloud Storage**:
```powershell
gsutil cp -r $BuildOutputDir gs://falsework-builds/spiderlily/
```

**AWS S3**:
```powershell
aws s3 cp $BuildOutputDir s3://falsework-builds/spiderlily/ --recursive
```

**Azure Blob**:
```powershell
az storage blob upload-batch -d spiderlily-builds -s $BuildOutputDir
```

---

## 🐛 Troubleshooting

### Build Fails: "Platform Win64 is not a valid platform"

**Cause**: UE5 installed without Windows desktop platform support

**Fix**:
1. Open Epic Games Launcher
2. Library → UE 5.6 → Options
3. Check: ✅ **Target Platforms → Windows**
4. Click Apply, wait for install

### Build Fails: "Access is denied" writing DLL

**Cause**: Windows Defender locking files during compilation

**Fix** (already in bootstrap, but if needed manually):
```powershell
Add-MpPreference -ExclusionPath "C:\Perforce"
Add-MpPreference -ExclusionPath "C:\Program Files\Epic Games\UE_5.6"
Add-MpPreference -ExclusionProcess "UnrealBuildTool.exe"
Add-MpPreference -ExclusionProcess "link.exe"
```

### Build Fails: "No valid Visual C++ toolchain"

**Cause**: Missing Visual Studio build tools

**Fix** (should be installed by bootstrap):
```powershell
choco install visualstudio2022buildtools -y --package-parameters "--add Microsoft.VisualStudio.Workload.VCTools --add Microsoft.VisualStudio.Component.VC.14.38.17.8.x86.x64 --add Microsoft.VisualStudio.Component.Windows11SDK.22621"
```

### Perforce: "Client 'spiderlily_build_workspace' unknown"

**Cause**: Workspace not created or wrong P4CLIENT environment variable

**Fix**:
```powershell
$env:P4CLIENT = "spiderlily_build_workspace"
p4 client spiderlily_build_workspace
# Enter configuration (see setup section)
```

### GitHub Runner Not Showing Up

**Check runner status**:
```powershell
cd C:\actions-runner
.\run.cmd  # Test run (Ctrl+C to stop)

# If working, install as service:
.\svc.cmd install
.\svc.cmd start
```

---

## 📂 File Locations

| Path | Contents |
|------|----------|
| `C:\Perforce\` | Perforce workspace (synced project files) |
| `C:\Builds\` | Build outputs (organized by timestamp) |
| `C:\scripts\` | Build automation scripts |
| `C:\actions-runner\` | GitHub Actions self-hosted runner |
| `C:\aesthetic-computer-repo\` | Clone of aesthetic-computer (for scripts) |
| `C:\Program Files\Epic Games\UE_5.6\` | Unreal Engine 5.6 installation |

---

## 🔐 Security

All credentials stored in: `aesthetic-computer-vault/false.work/ue5-builder.env`

**Never commit**:
- Passwords
- API keys
- Service account keys
- VM passwords

**Always use**:
- Environment variables from vault
- `$env:VAR` placeholders in scripts
- GitHub secrets for CI/CD

---

## 📊 Build Metrics

**VM Specs**:
- Type: GCP n2-standard-8
- CPU: 8 vCPU (4 physical cores)
- RAM: 32GB
- Disk: 300GB SSD
- Cost: ~$200/month (running 24/7)

**Build Times** (approximate):
- Editor build (incremental): 2-5 minutes
- Editor build (clean): 5-10 minutes
- Packaged build (clean): 30-60 minutes
- UE5 install: 60-120 minutes (one-time)
- Perforce sync: 2-5 minutes (depends on changes)

---

## 🎓 Next Steps

1. ✅ **Test the system**: Run a manual build
2. ✅ **Set up automation**: Configure GitHub Actions runner
3. ✅ **Share with team**: Distribute first packaged build
4. 📈 **Monitor builds**: Check GitHub Actions for failures
5. 🔄 **Iterate**: Adjust build schedule/configuration as needed

---

## 📞 Support

For issues with:
- **VM/GCP**: Check GCP console or re-run quick-setup-gcp.sh
- **UE5/builds**: Check `C:\Users\builduser\AppData\Local\UnrealBuildTool\Log.txt`
- **Perforce**: Contact false.work team or check P4 logs with `p4 info`
- **GitHub Actions**: Check workflow logs at github.com/whistlegraph/aesthetic-computer/actions

---

**Last Updated**: 2025-10-19  
**Maintained By**: aesthetic.computer / false.work collaboration  
**Project**: Spider Lily (UE5.6)
