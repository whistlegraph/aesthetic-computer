# Manual Bootstrap Instructions for false.work UE5 Builder

Since you're already RDP'd in, here's what to do:

## Quick Bootstrap (Run This Now!)

Open PowerShell **as Administrator** (Right-click PowerShell → Run as Administrator):

```powershell
# Download and run the bootstrap script
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/whistlegraph/aesthetic-computer/main/false.work/unreal-builder/scripts/bootstrap-windows.ps1" -OutFile "C:\bootstrap.ps1"

# Run it
PowerShell -ExecutionPolicy Bypass -File C:\bootstrap.ps1
```

This will automatically install:
- ✅ Chocolatey (package manager)
- ✅ Git for Windows
- ✅ Perforce CLI (P4)
- ✅ GitHub Actions runner (downloaded)
- 📥 Visual Studio installer (downloaded, not installed)
- 📥 Epic Games Launcher installer (downloaded, not installed)

Takes about **5-10 minutes**.

---

## After Bootstrap Completes

### 1. Install Visual Studio Build Tools (~30-60 min)

```powershell
# This runs unattended - you can walk away
C:\vs_buildtools.exe --quiet --wait --norestart --nocache `
  --installPath C:\BuildTools `
  --add Microsoft.VisualStudio.Workload.VCTools `
  --add Microsoft.VisualStudio.Workload.ManagedDesktopBuildTools
```

### 2. Install Epic Games Launcher

```powershell
# Quick install
msiexec /i C:\EpicGamesLauncherInstaller.msi /quiet

# Wait for it to finish, then:
# - Launch Epic Games Launcher from Start menu
# - Sign in with your Epic account
# - Go to "Unreal Engine" tab
# - Install UE 5.4 (or whatever version Spider Lily uses)
# - This takes 1-2 hours
```

### 3. Configure Perforce

```powershell
p4 set P4PORT=ssl:falsework.helixcore.io:1666
p4 set P4USER=machine
p4 set P4PASSWD=AestheticComp1
p4 set P4CLIENT=spiderlily_build_workspace
p4 login
p4 info  # Test connection
```

### 4. Clone aesthetic-computer Repo

```powershell
cd C:\
git clone https://github.com/whistlegraph/aesthetic-computer.git
```

### 5. Run Setup Script

```powershell
cd C:\aesthetic-computer\false.work\unreal-builder
.\scripts\setup-build-machine.ps1 `
  -P4Server "ssl:falsework.helixcore.io:1666" `
  -P4User "machine" `
  -P4Workspace "spiderlily_build_workspace"
```

### 6. Initial Perforce Sync

```powershell
# This downloads the entire Spider Lily project
# May take 30-60 minutes depending on project size
p4 sync
```

### 7. Configure GitHub Actions Runner

Get your runner token:
1. Go to: https://github.com/whistlegraph/aesthetic-computer/settings/actions/runners/new
2. Copy the token shown

Then in PowerShell:
```powershell
cd C:\actions-runner

# Configure runner (use your token)
.\config.cmd --url https://github.com/whistlegraph/aesthetic-computer --token YOUR_TOKEN_HERE

# When prompted for labels, enter:
# windows,ue5,perforce,spiderlily

# Install as service
.\svc.sh install
.\svc.sh start
```

---

## Timeline

| Step | Time | Can Walk Away? |
|------|------|----------------|
| Bootstrap script | 5-10 min | ✅ Yes |
| Visual Studio | 30-60 min | ✅ Yes |
| Epic Launcher | 5 min | ✅ Yes |
| UE5 Install | 1-2 hours | ✅ Yes |
| Perforce setup | 5 min | ❌ No (need to type) |
| Repo clone | 2 min | ✅ Yes |
| Setup script | 5 min | ✅ Yes |
| P4 sync | 30-60 min | ✅ Yes |
| Runner setup | 10 min | ❌ No (need token) |

**Total: 3-5 hours** (mostly automated)

---

## Check Progress

Open a new PowerShell and run:

```powershell
# Check if bootstrap is done
Test-Path C:\bootstrap-log.txt
Get-Content C:\bootstrap-log.txt -Tail 20

# Check if VS is installed
Test-Path "C:\BuildTools\MSBuild\Current\Bin\MSBuild.exe"

# Check if UE5 is installed
Test-Path "C:\Program Files\Epic Games\UE_5.4"

# Check if runner is running
Get-Service actions.runner.*
```

---

## Troubleshooting

**Bootstrap script fails?**
- Make sure you're running as Administrator
- Check internet connection
- View log: `Get-Content C:\bootstrap-log.txt`

**Visual Studio install hangs?**
- Check Task Manager - is it still running?
- Wait - it really does take 30-60 minutes
- Check logs in: `C:\ProgramData\Microsoft\VisualStudio\Packages\_Instances\`

**Can't install UE5?**
- Make sure you're signed into Epic Games Launcher
- Need an Epic account (free)
- UE5 is free for development/internal builds

**P4 connection fails?**
- Check internet connection
- Verify: `p4 -p ssl:falsework.helixcore.io:1666 info`
- Check firewall isn't blocking port 1666

---

## What the Bootstrap Script Does

```powershell
# 1. Installs Chocolatey (package manager)
Set-ExecutionPolicy Bypass -Scope Process -Force
iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# 2. Installs Git
choco install git -y

# 3. Downloads Perforce CLI
Invoke-WebRequest -Uri "https://cdist2.perforce.com/perforce/r24.1/bin.ntx64/p4.exe" -OutFile "C:\Windows\System32\p4.exe"

# 4. Downloads installers for VS and Epic
# (You run these manually because they need user input/sign-in)

# 5. Creates directories
New-Item -ItemType Directory -Path "D:\Perforce" -Force
New-Item -ItemType Directory -Path "D:\Builds" -Force

# 6. Optimizes Windows for builds
Stop-Service WSearch  # Disable search indexing
powercfg /setactive 8c5e7fda-e8bf-4a96-9a85-a6e23a8c635c  # High performance
```

---

## Need Help?

Check:
- Bootstrap log: `C:\bootstrap-log.txt`
- VS install logs: `C:\ProgramData\Microsoft\VisualStudio\`
- Epic Launcher logs: `%LOCALAPPDATA%\EpicGamesLauncher\Saved\Logs\`

Or see: `TROUBLESHOOTING.md` in the repo
