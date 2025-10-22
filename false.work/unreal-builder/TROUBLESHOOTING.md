# Troubleshooting Guide

Common issues and solutions for UE5 + Perforce + GitHub Actions builds.

## Perforce Issues

### "Perforce client error: Connect to server failed"

**Cause:** Cannot reach Perforce server or incorrect server address.

**Solutions:**
1. Verify P4PORT is correct: `p4 set P4PORT`
2. Check firewall allows port 1666 (or your custom port)
3. Test connection: `p4 -p ssl:your-server.com:1666 info`
4. Ensure SSL certificate is trusted if using SSL

### "Password invalid or unset"

**Cause:** Password not configured or expired.

**Solutions:**
1. Set password: `p4 set P4PASSWD=your_password`
2. Login manually: `p4 login`
3. For GitHub Actions, verify P4_PASSWORD secret is set correctly

### "Client 'ue5_build_workspace' unknown"

**Cause:** Workspace doesn't exist on server.

**Solutions:**
1. Create workspace: `p4 client ue5_build_workspace`
2. Verify workspace name matches P4CLIENT setting
3. Run setup script: `.\scripts\setup-build-machine.ps1`

### Sync is very slow

**Cause:** Large files, network latency, or initial sync.

**Solutions:**
1. Use parallel sync: `p4 sync -p`
2. Consider Perforce Proxy for remote offices
3. Exclude unnecessary files in workspace View
4. Use incremental builds instead of clean builds

## Unreal Engine Build Issues

### "Cannot find UnrealBuildTool"

**Cause:** UE5 not installed or incorrect path.

**Solutions:**
1. Verify UE5_ENGINE_PATH in workflow/config
2. Default path: `C:\Program Files\Epic Games\UE_5.4`
3. Check UBT exists: `Test-Path "$UE5Path\Engine\Binaries\DotNET\UnrealBuildTool.exe"`

### "Build failed with exit code 1"

**Cause:** Compilation errors in project code.

**Solutions:**
1. Check build logs in `D:\Builds\Logs`
2. Verify project builds locally in UE5 Editor
3. Sync to a known-good changelist
4. Check Visual Studio version compatibility

### "Out of memory during build"

**Cause:** Insufficient RAM or too many parallel build processes.

**Solutions:**
1. Close other applications
2. Increase VM size (Azure) or RAM
3. Reduce parallel builds: Edit BuildConfiguration.xml
4. Add `-MaxParallelActions=4` to UAT command

### "Cook failed for platform Win64"

**Cause:** Missing content, corrupt assets, or plugin issues.

**Solutions:**
1. Verify all content files are synced
2. Check for broken asset references in logs
3. Disable problematic plugins temporarily
4. Clear Derived Data Cache: Delete `Saved/DerivedDataCache`

### Build takes too long (>2 hours)

**Causes:** Clean builds, slow machine, or large project.

**Solutions:**
1. Use incremental builds (disable clean_build)
2. Enable IncrediBuild or FASTBuild if available
3. Upgrade to faster hardware (more cores, faster SSD)
4. Split into separate cook and package steps
5. Use distributed build system

## GitHub Actions Issues

### "Runner is offline"

**Cause:** Self-hosted runner not running or disconnected.

**Solutions:**
1. Check runner service: `Get-Service actions.runner.*`
2. Restart service: `Restart-Service actions.runner.*`
3. Check runner logs: `C:\actions-runner\_diag`
4. Re-register runner if needed

### "Permission denied" errors

**Cause:** Runner service account lacks permissions.

**Solutions:**
1. Run runner as Administrator
2. Grant permissions to build directories
3. Check Perforce user has read access
4. Verify Windows Defender isn't blocking

### Workflow never starts

**Cause:** No matching runner with required labels.

**Solutions:**
1. Verify runner has labels: `windows`, `ue5`, `perforce`
2. Check runner is online in GitHub repository settings
3. Review workflow `runs-on` requirements

### Artifacts fail to upload

**Cause:** Large build size or network issues.

**Solutions:**
1. Compress artifacts before upload
2. Split into multiple artifacts
3. Consider Azure Blob Storage instead
4. Check artifact size limits (10GB per artifact)

## Azure VM Issues

### VM provisioning fails

**Cause:** Quota limits or invalid configuration.

**Solutions:**
1. Check Azure subscription quotas
2. Try different VM size or region
3. Verify AZURE_CREDENTIALS secret is valid
4. Check Azure service principal permissions

### VM is too slow

**Cause:** Under-provisioned VM size.

**Solutions:**
1. Use larger VM size (Standard_D16s_v5 or higher)
2. Use Premium SSD storage
3. Enable accelerated networking
4. Consider dedicated host for consistent performance

### Build image not found

**Cause:** Custom UE5 image doesn't exist or wrong ID.

**Solutions:**
1. Create VM image with UE5 pre-installed
2. Verify AZURE_UE5_IMAGE_ID in secrets
3. Use marketplace Windows Server image and install UE5 in workflow (slow)

## Performance Optimization

### Slow Perforce Sync

- Use `p4 sync -p` for parallel sync
- Configure Perforce Proxy
- Exclude unnecessary files (binaries, intermediate files)

### Slow UE5 Compilation

- Use multiple cores: `-MaxParallelActions=16`
- Enable IncrediBuild or FASTBuild
- Use SSD storage (NVMe recommended)
- Disable Windows Defender for build directories

### Slow Cooking

- Enable iterative cooking: `-iterativecooking`
- Use multiple cores: `-CookMultiprocess`
- Cook only changed content
- Exclude debug content in Shipping builds

## Getting Help

If you can't resolve the issue:

1. Check GitHub Actions workflow logs
2. Review local logs:
   - Build logs: `D:\Builds\Logs`
   - Runner logs: `C:\actions-runner\_diag`
   - UE5 logs: `Saved\Logs`
3. Verify all prerequisites are installed (README.md)
4. Test build manually on build machine
5. Create GitHub issue with:
   - Error message
   - Workflow run link
   - Build machine specs
   - Relevant log excerpts

## Useful Commands

```powershell
# Check Perforce connection
p4 info

# Test Perforce login
p4 login -s

# View workspace configuration
p4 client -o ue5_build_workspace

# Check runner status
Get-Service actions.runner.*

# View runner logs
Get-Content C:\actions-runner\_diag\Runner_*.log -Tail 50

# Test UE5 installation
Test-Path "C:\Program Files\Epic Games\UE_5.4\Engine\Build\BatchFiles\RunUAT.bat"

# Check disk space
Get-PSDrive C

# View recent builds
Get-ChildItem D:\Builds | Sort-Object CreationTime -Descending | Select-Object -First 10
```
