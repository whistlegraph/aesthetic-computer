# Getting SpiderLily Project on Windows

You need to get the SpiderLily project from Perforce before you can build it.

## Quick Setup

### Step 1: Install Perforce Client

Download and install P4V (Perforce Visual Client):
https://www.perforce.com/downloads/helix-visual-client-p4v

Or just the command-line client (p4):
https://www.perforce.com/downloads/helix-command-line-client-p4

### Step 2: Run the Setup Script

```powershell
cd \\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows
powershell.exe -ExecutionPolicy Bypass -File .\setup-perforce.ps1
```

This will:
- Test your Perforce connection
- Prompt for your credentials
- Show you available workspaces
- Create a workspace directory at `C:\Perforce\SpiderLily`

### Step 3: Sync the Project

**Option A: Using P4V (GUI)**
1. Open P4V
2. Connect to: `ssl:falsework.helixcore.io:1666`
3. Enter your username and password
4. Create a new workspace or use existing one
5. Map the workspace to: `C:\Perforce\SpiderLily`
6. Right-click the depot and select "Get Latest Revision"

**Option B: Using command line**
```powershell
# Set your Perforce environment
$env:P4PORT = "ssl:falsework.helixcore.io:1666"
$env:P4USER = "your-username"
$env:P4CLIENT = "your-workspace-name"

# Sync the project
cd C:\Perforce\SpiderLily
p4 sync
```

### Step 4: Verify the Project

Check that you have the .uproject file:
```powershell
Test-Path C:\Perforce\SpiderLily\SpiderLily.uproject
```

Should return `True`

### Step 5: Build

Now you can run the build script:
```powershell
cd \\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows
powershell.exe -ExecutionPolicy Bypass -File .\build-false-work.ps1
```

---

## Troubleshooting

**"p4 not found"**
- Install Perforce client (see Step 1)
- Make sure p4.exe is in your PATH

**"Could not connect to server"**
- Check you have network access to falsework.helixcore.io:1666
- Verify your username/password with the Perforce admin
- Try using P4V GUI first to test connection

**"Project not found"**
- Make sure you've synced the files (Step 3)
- Check the path in build-false-work.ps1 matches your workspace location
- Verify SpiderLily.uproject exists in the synced location

**Don't have Perforce credentials?**
- Contact the project administrator to get:
  - Perforce username
  - Perforce password
  - Workspace/depot information
