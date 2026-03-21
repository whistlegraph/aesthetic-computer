# CLEAR INSTRUCTIONS - Start Here!

You're already RDP'd into the VM. Here's exactly what to do:

---

## ⚡ Step 1: Run Bootstrap (5 minutes)

In the VM, open PowerShell **as Administrator**:
- Right-click Start menu → "Windows PowerShell" → "Run as Administrator"

Paste this:

```powershell
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/whistlegraph/aesthetic-computer/main/false.work/unreal-builder/scripts/bootstrap-windows-minimal.ps1" -OutFile "C:\bootstrap.ps1"
PowerShell -ExecutionPolicy Bypass -File C:\bootstrap.ps1
```

Press Enter and wait ~5 minutes. This installs Git, Perforce CLI, and creates directories.

---

## 🎮 Step 2: Install UE5 (1-2 hours, walk away)

After bootstrap finishes:

```powershell
# Install Epic Games Launcher
msiexec /i C:\EpicGamesLauncherInstaller.msi /quiet
```

Wait a minute, then:
1. Press Windows key → Type "Epic Games Launcher" → Open it
2. Sign in (or create free Epic account)
3. Click "Unreal Engine" tab on left
4. Click "Install Engine"
5. Choose version (probably 5.4 or whatever Spider Lily uses)
6. Click Install

**Go get coffee - this takes 1-2 hours. ☕**

---

## 🔧 Step 3: Configure Perforce (5 minutes)

While UE5 installs (or after), in PowerShell:

```powershell
# Load credentials from vault
$vaultEnv = "C:\aesthetic-computer-vault\false.work\ue5-builder.env"
Get-Content $vaultEnv | ForEach-Object {
    if ($_ -match '^([^=]+)=(.*)$') {
        Set-Item -Path "env:$($matches[1])" -Value $matches[2]
    }
}

# Set Perforce connection using vault values
p4 set P4PORT=$env:P4_SERVER
p4 set P4USER=$env:P4_USER
p4 set P4PASSWD=$env:P4_PASSWORD
p4 set P4CLIENT=$env:P4_WORKSPACE

# Test connection
p4 login
p4 info
```

Should show your P4 username - you're connected! ✅

---

## 📦 Step 4: Create Workspace & Sync (30-60 minutes)

```powershell
# Create workspace (use name from vault)
p4 client $env:P4_WORKSPACE
```

An editor opens. Set these values (using your vault variables):
```
Client: [value of P4_WORKSPACE from vault]
Owner: [value of P4_USER from vault]
Root: D:\Perforce
Options: noallwrite noclobber nocompress unlocked nomodtime normdir
SubmitOptions: submitunchanged
LineEnd: local

View:
    [value of P4_CLIENT_PATH from vault] //[P4_WORKSPACE]/...
```

Save and close (Ctrl+S, then X).

Then sync:
```powershell
p4 sync
```

**Walk away - downloads entire project. ☕**

---

## 🤖 Step 5: Setup GitHub Runner (10 minutes)

After UE5 finishes installing:

1. **Get runner token:**
   - Go to: https://github.com/whistlegraph/aesthetic-computer/settings/actions/runners/new
   - Click "New self-hosted runner"
   - Choose "Windows"
   - Copy the token (long string starting with "A...")

2. **Configure runner:**
```powershell
cd C:\actions-runner

# Paste your token in the command below
.\config.cmd --url https://github.com/whistlegraph/aesthetic-computer --token YOUR_TOKEN_HERE
```

3. **When prompted:**
   - Runner group: Press Enter (default)
   - Runner name: Press Enter (default)
   - Work folder: Press Enter (default)
   - Labels: Type `windows,ue5,perforce,spiderlily` then Enter

4. **Install as service:**
```powershell
.\svc.sh install
.\svc.sh start

# Verify
Get-Service actions.runner.*
```

Should show "Running" ✅

---

## 🚀 Step 6: Setup GitHub (In your dev container)

Switch back to your dev container terminal:

```bash
cd /workspaces/aesthetic-computer

# Copy workflow
cp false.work/unreal-builder/.github/workflows/ue5-build-self-hosted.yml .github/workflows/

# Commit
git add false.work/ .github/workflows/ aesthetic-computer-vault/
git commit -m "Add UE5 build automation for false.work"
git push
```

---

## 🔐 Step 7: Add GitHub Secrets

1. Go to: https://github.com/whistlegraph/aesthetic-computer/settings/secrets/actions
2. Click "New repository secret" for each (get values from `aesthetic-computer-vault/false.work/ue5-builder.env`):

```
Name: P4_SERVER
Value: [copy from vault: P4_SERVER]
```

```
Name: P4_USER
Value: [copy from vault: P4_USER]
```

```
Name: P4_PASSWORD
Value: [copy from vault: P4_PASSWORD]
```

```
Name: P4_WORKSPACE
Value: [copy from vault: P4_WORKSPACE]
```

```
Name: P4_CLIENT_PATH
Value: [copy from vault: P4_CLIENT_PATH]
```

```
Name: PROJECT_NAME
Value: [copy from vault: PROJECT_NAME]
```

---

## 🎉 Step 8: Test Build!

1. Go to: https://github.com/whistlegraph/aesthetic-computer/actions
2. Click "UE5 Build (Self-Hosted)" in left sidebar
3. Click "Run workflow" button (top right)
4. Select:
   - Branch: `main`
   - Build Configuration: `Development`
   - Build Platform: `Win64`
   - Clean build: ☑️ Check (first time only)
5. Click green "Run workflow" button

Watch it build! First build takes 30-60 minutes.

---

## ✅ Done!

Now you have:
- ✅ One-click UE5 builds from GitHub
- ✅ Automatic builds on every push (if you want)
- ✅ Build artifacts downloadable for 30 days
- ✅ Team can all trigger builds

---

## 💰 Don't Forget!

**Stop the VM when not building:**

```bash
# From your dev container
gcloud compute instances stop ue5-builder-falsework --zone=us-central1-a
```

Saves ~$8/day! 💸

**Start it again:**
```bash
gcloud compute instances start ue5-builder-falsework --zone=us-central1-a
```

---

## 🆘 Problems?

**Bootstrap fails?**
- Make sure you ran as Administrator
- Check: `Get-Content C:\bootstrap-log.txt`

**P4 connection fails?**
- Verify: `p4 info`
- Check internet connection
- Firewall blocking port 1666?

**Runner shows offline?**
- In VM: `Get-Service actions.runner.*`
- Should show "Running"
- Restart: `Restart-Service actions.runner.*`

**Build fails?**
- Check UE5 is installed: `Test-Path "C:\Program Files\Epic Games\UE_5.4"`
- Check project synced: `Test-Path "D:\Perforce\SpiderLily\SpiderLily.uproject"`
- Check GitHub Actions logs for errors

---

## 📚 More Info

- Operations guide: `OPERATIONS.md`
- Troubleshooting: `TROUBLESHOOTING.md`
- Horde/Build tools: `HORDE-AND-BUILDTOOLS.md`
- Complete summary: `COMPLETE-SUMMARY.md`

---

**Current Step:** You're on Step 1 - Run the bootstrap script! 🚀
