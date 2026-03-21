# Simplified Setup Using Unreal Horde & Build Tools

## What You Actually Need

Good news! **Unreal Engine includes everything it needs to build**:
- ✅ **UnrealBuildTool** - Builds C++ code
- ✅ **Horde** - Epic's distributed build system
- ✅ **Built-in compilers** - No Visual Studio needed for automation!

## Minimal Setup (Way Simpler!)

### On the VM, you only need:

1. **Unreal Engine 5** (includes build tools)
2. **Perforce CLI** (P4)
3. **Git** (to clone scripts)

That's it! UE5 handles the rest.

## Quick Setup

### 1. Install Dependencies
```powershell
# Install Chocolatey
Set-ExecutionPolicy Bypass -Scope Process -Force
iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

# Install Git and P4
choco install git -y
Invoke-WebRequest -Uri "https://cdist2.perforce.com/perforce/r24.1/bin.ntx64/p4.exe" -OutFile "C:\Windows\System32\p4.exe"
```

### 2. Install Unreal Engine
- Install Epic Games Launcher
- Sign in
- Install UE5 (includes UnrealBuildTool, UnrealHeaderTool, etc.)

### 3. Configure Perforce
```powershell
p4 set P4PORT=ssl:falsework.helixcore.io:1666
p4 set P4USER=machine
p4 set P4PASSWD=AestheticComp1
p4 login
p4 sync
```

### 4. Build with UE5's Tools
```powershell
# UE5's build system handles everything!
"C:\Program Files\Epic Games\UE_5.4\Engine\Build\BatchFiles\RunUAT.bat" BuildCookRun `
  -project="D:\Perforce\SpiderLily\SpiderLily.uproject" `
  -platform=Win64 `
  -clientconfig=Development `
  -cook -build -stage -pak
```

No Visual Studio needed! 🎉

## Optional: Use Horde for Distributed Builds

If false.work wants **really fast builds**, they can set up **Unreal Horde**:

### What is Horde?
- Epic's distributed build system
- Splits compilation across multiple machines
- Can reduce build times by 10x or more
- Used by AAA studios

### Horde Setup
```powershell
# On the VM (becomes a Horde agent)
cd "C:\Program Files\Epic Games\UE_5.4\Engine\Source\Programs\Horde"
.\Horde.Agent.exe install
.\Horde.Agent.exe start
```

Then configure in `Engine/Programs/Horde/HordeAgent/appsettings.json`

## When You DO Need Visual Studio

Only if:
- ❌ Developing in the editor (we're not - just building)
- ❌ Debugging C++ code (not needed for CI/CD)
- ❌ Custom build steps outside UE

For **automated builds**, UE5's tools are sufficient! ✅

## Updated Workflow

The GitHub Actions workflow already uses the right approach:

```yaml
- name: Build UE5 Project
  run: |
    # This is all you need!
    & "$env:UE5_ENGINE_PATH\Engine\Build\BatchFiles\RunUAT.bat" BuildCookRun `
      -project="$ProjectFile" `
      -platform=Win64 `
      -clientconfig=Development `
      -build -cook -stage -pak
```

## What About C++ Compilation?

UE5 includes:
- **UnrealBuildTool (UBT)** - Manages C++ builds
- **Clang** or **MSVC** bundled with engine
- All necessary headers and libraries

It just works! No separate Visual Studio needed for builds.

## Performance Comparison

| Approach | Build Time | Complexity | Cost |
|----------|-----------|------------|------|
| UE5 Built-in Tools | 30-45 min | Low ✅ | VM only |
| + Horde (single machine) | 30-45 min | Medium | VM only |
| + Horde (distributed) | 5-15 min | High | Multiple VMs |
| + IncrediBuild | 10-20 min | High | License + VMs |

## Recommendation

**For false.work:**
1. Start with **UE5 built-in tools** (simplest) ✅
2. If builds are too slow, add **Horde distributed builds** later
3. Visual Studio only needed if devs want to debug on the VM

## Simplified Bootstrap Script

I'll create a much simpler version that just installs:
- Git
- P4
- GitHub Actions runner
- Creates directories

UE5 installer still needs manual install (Epic account sign-in).

Want me to create this simplified version?
