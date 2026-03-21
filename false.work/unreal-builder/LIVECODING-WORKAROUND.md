# WORKAROUND: LiveCoding Issue with Spider Lily Packaging

## Problem
The Spider Lily project has LiveCoding enabled which blocks automated packaging builds.
RunUAT hangs at "Waiting for server" during the cooking phase.

## Root Cause
LiveCoding is hardcoded in the project configuration and cannot be easily disabled via
command-line flags or environment variables. This is likely intentional for the team's
development workflow.

## Workaround Solution

### Option 1: Manual Cook + Package (Works Now)

1. **Cook the content** (bypasses LiveCoding):
```powershell
cd C:\Perforce
$UE5 = "C:\Program Files\Epic Games\UE_5.6"
& "$UE5\Engine\Binaries\Win64\UnrealEditor-Cmd.exe" "C:\Perforce\SpiderLily.uproject" -run=Cook -TargetPlatform=Windows -unattended -stdout -CrashForUAT -NoLogTimes -UTF8Output
```
**Time**: ~18 minutes  
**Output**: Cooked content in `C:\Perforce\Saved\Cooked\Windows\`

2. **Package using cooked content**:
```powershell
# Use the already-cooked content to create package
$Output = "C:\Builds\Package_$(Get-Date -Format 'yyyyMMdd-HHmm')"
& "$UE5\Engine\Build\BatchFiles\RunUAT.bat" BuildCookRun `
    -project="C:\Perforce\SpiderLily.uproject" `
    -platform=Win64 `
    -clientconfig=Development `
    -stage `
    -pak `
    -archive `
    -archivedirectory="$Output" `
    -unattended `
    -noP4 `
    -NoLiveCoding `
    -SkipCook `
    -Prereqs `
    -Build
```

### Option 2: Disable LiveCoding in Project (Recommended)

Ask the false.work team to disable LiveCoding for automated builds by adding to 
`SpiderLily/Config/DefaultEngine.ini`:

```ini
[LiveCoding]
bEnabled=False
```

Or add it only for non-interactive builds in their Target.cs file:
```csharp
if (Target.Type != TargetType.Editor)
{
    BuildEnvironment = TargetBuildEnvironment.Unique;
    bBuildWithLiveCodingConsole = false;
}
```

### Option 3: Use Editor Build Only

Since the editor build works fine, we can:
1. Build the editor modules (works: ~5 minutes)
2. Distribute the editor binaries + cooked content
3. Team runs via UnrealEditor.exe

This is faster but requires UE5 on team machines.

## Current Status

✅ **Working**: Manual editor builds via UnrealBuildTool  
✅ **Working**: Manual cooking via UnrealEditor-Cmd  
❌ **Blocked**: Automated full packaging via RunUAT  

## Recommendation

Contact false.work team and ask them to either:
1. Disable LiveCoding for the project, OR
2. Provide guidance on their preferred build approach

For now, use **Option 1** above for creating builds.

## Updated Scripts

All scripts in `false.work/unreal-builder/scripts/` include `-NoLiveCoding` flag,
but this is not sufficient for Spider Lily. Manual cook + package workflow documented above.

---
Last Updated: 2025-10-19  
Issue: Spider Lily project-specific LiveCoding configuration
