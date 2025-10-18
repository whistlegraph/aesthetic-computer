# false.work UE5 Builder - Quick Start Checklist

## ✅ DONE: VM Created
- IP: 136.114.176.135
- Username: builduser
- Password: 1LAO//dIS89yqq<
- Machine: n2-standard-8 (8 vCPU, 32GB RAM)
- Cost: ~$200/month (24/7) or ~$80/month (8hrs/day)

---

## 🔄 NEXT STEPS

### Step 1: Connect to the VM (NOW)

**Option A: From your local machine (not dev container)**
```bash
# Mac
open rdp://builduser@136.114.176.135

# Windows
mstsc /v:136.114.176.135

# Linux
rdesktop 136.114.176.135
```

**Option B: Use Cloud Console**
Go to: https://console.cloud.google.com/compute/instances?project=aesthetic-computer
Click "RDP" button next to `ue5-builder-falsework`

---

### Step 2: Install Software (IN THE VM)

Once connected via RDP, install these in order:

#### 2.1. Install Git for Windows
https://git-scm.com/download/win
- Run installer with defaults

#### 2.2. Install Perforce Helix Core CLI (P4)
https://www.perforce.com/downloads/helix-command-line-client-p4
- Download Windows x64 version
- Extract `p4.exe` to `C:\Windows\System32\`

#### 2.3. Install Visual Studio 2022 Community
https://visualstudio.microsoft.com/downloads/
- Choose these workloads:
  - ✅ Desktop development with C++
  - ✅ Game development with C++
  - ✅ .NET desktop development
- This takes ~30-60 minutes

#### 2.4. Install Unreal Engine 5
https://www.epicgames.com/store/download
- Install Epic Games Launcher
- Sign in with Epic account
- Go to "Unreal Engine" tab
- Install UE 5.4 (or whichever version Spider Lily uses)
- This takes ~1-2 hours

---

### Step 3: Run Setup Script (IN THE VM)

In the VM, open PowerShell as Administrator:

```powershell
# Clone this repo
cd C:\
git clone https://github.com/whistlegraph/aesthetic-computer.git

# Run setup script
cd C:\aesthetic-computer\false.work\unreal-builder
.\scripts\setup-build-machine.ps1 `
  -P4Server "ssl:falsework.helixcore.io:1666" `
  -P4User "machine" `
  -P4Workspace "spiderlily_build_workspace" `
  -WorkspaceRoot "D:\Perforce"
```

This will:
- Configure Perforce environment
- Create build directories
- Set up Windows Defender exclusions
- Create helper scripts

---

### Step 4: Configure Perforce Password (IN THE VM)

```powershell
p4 set P4PASSWD=AestheticComp1
p4 login
p4 info  # Test connection

# Initial sync (this will take a while)
p4 sync
```

---

### Step 5: Install GitHub Actions Runner (IN THE VM)

1. Go to: https://github.com/whistlegraph/aesthetic-computer/settings/actions/runners/new
2. Choose "Windows"
3. Follow the instructions shown (copy/paste commands into PowerShell)
4. When prompted for labels, enter: `windows,ue5,perforce,spiderlily`
5. Install as a service:
   ```powershell
   cd C:\actions-runner
   .\svc.sh install
   .\svc.sh start
   ```

---

### Step 6: Copy Workflow to Main Repo (YOUR LOCAL MACHINE)

Back in your dev container:

```bash
# Copy the workflow to the main .github/workflows directory
cp /workspaces/aesthetic-computer/false.work/unreal-builder/.github/workflows/ue5-build-self-hosted.yml \
   /workspaces/aesthetic-computer/.github/workflows/

# Commit and push
cd /workspaces/aesthetic-computer
git add .github/workflows/ue5-build-self-hosted.yml
git add false.work/
git commit -m "Add UE5 build automation for false.work Studio"
git push
```

---

### Step 7: Configure GitHub Secrets (YOUR LOCAL MACHINE)

Go to: https://github.com/whistlegraph/aesthetic-computer/settings/secrets/actions

Add these secrets:

| Secret Name | Value |
|-------------|-------|
| `P4_SERVER` | `ssl:falsework.helixcore.io:1666` |
| `P4_USER` | `machine` |
| `P4_PASSWORD` | `AestheticComp1` |
| `P4_WORKSPACE` | `spiderlily_build_workspace` |
| `P4_CLIENT_PATH` | `//depot/SpiderLily/SL_main/...` |
| `PROJECT_NAME` | `SpiderLily` |

---

### Step 8: Test First Build! 🎉

1. Go to: https://github.com/whistlegraph/aesthetic-computer/actions
2. Click "UE5 Build (Self-Hosted)" workflow
3. Click "Run workflow" button
4. Select:
   - Branch: `main`
   - Build Config: `Development`
   - Platform: `Win64`
   - Clean build: ✅ (first time)
5. Click "Run workflow"
6. Watch it build! (30-60 minutes)

---

## 💰 Cost Management

**Stop VM when not building:**
```bash
gcloud compute instances stop ue5-builder-falsework --zone=us-central1-a
```

**Start VM when needed:**
```bash
gcloud compute instances start ue5-builder-falsework --zone=us-central1-a
```

**Check VM status:**
```bash
gcloud compute instances describe ue5-builder-falsework --zone=us-central1-a
```

---

## 🐛 Troubleshooting

**Can't connect via RDP?**
- Check VM is running: `gcloud compute instances list`
- Check firewall allows RDP on port 3389
- Try resetting password: `gcloud compute reset-windows-password ue5-builder-falsework --zone=us-central1-a --user=builduser`

**Perforce connection fails?**
- Verify credentials: `p4 info`
- Check server is reachable: `p4 -p ssl:falsework.helixcore.io:1666 info`
- Re-login: `p4 login`

**GitHub Actions runner offline?**
- In VM, check service: `Get-Service actions.runner.*`
- Restart: `Restart-Service actions.runner.*`

---

## 📚 Documentation Reference

- **Full setup guide:** `SETUP-GUIDE-FOR-FALSEWORK.md`
- **Troubleshooting:** `TROUBLESHOOTING.md`
- **Cloud VM management:** `CLOUD-VM-GUIDE.md`
- **Quick reference:** `quick-reference.md`

---

## Timeline Estimate

- Software installation: 2-3 hours
- Setup & configuration: 30 minutes
- Initial Perforce sync: 1-2 hours (depends on project size)
- GitHub runner setup: 15 minutes
- First build: 30-60 minutes

**Total: ~4-7 hours** (mostly automated, can walk away)

---

## Current Status

- [x] VM Created
- [ ] Software Installed
- [ ] Setup Script Run
- [ ] Perforce Synced
- [ ] GitHub Runner Installed
- [ ] Workflow Copied
- [ ] Secrets Configured
- [ ] First Build Tested

Update this checklist as you go!
