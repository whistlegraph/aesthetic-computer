# Mac Build & Upload Setup Guide

## Overview
Automated Mac build pipeline that runs from the dev container (like Windows builds). All orchestration happens via SSH - syncs from Perforce, builds with BuildCookRun, and uploads to DigitalOcean Spaces.

## Prerequisites

### 1. Mac Build Machine Setup
- Mac with Xcode and Unreal Engine 5.6 installed
- Perforce workspace configured at `~/Perforce/spiderlily_build_workspace_macmini/SL_main`
- SSH access from dev container
- AWS CLI installed: `brew install awscli`

### 2. Credentials Setup

#### Mac Builder Credentials
In dev container at `aesthetic-computer-vault/false.work/mac-builder-credentials.env`:
```bash
MAC_HOST=host.docker.internal
MAC_USERNAME=falsework
MAC_PASSWORD=BuildM@chine
```

#### DigitalOcean Spaces Credentials
In dev container at `aesthetic-computer-vault/false.work/builds-spaces.env`:

```bash
# false.work Build Storage Configuration
BUILDS_SPACES_ENDPOINT=https://sfo3.digitaloceanspaces.com
BUILDS_SPACES_KEY=<your-spaces-key>
BUILDS_SPACES_SECRET=<your-spaces-secret>
BUILDS_SPACES_BUCKET=falsework-builds
BUILDS_SPACES_REGION=sfo3
```

**Both credential files stay in the dev container vault - nothing needs to be copied to the Mac.**

## Usage

### From Dev Container

#### Run a Build & Upload:
```fish
cd /workspaces/aesthetic-computer
./false.work/unreal-builder/scripts/remote-mac-build.fish
```

#### With Custom Version:
```fish
./false.work/unreal-builder/scripts/remote-mac-build.fish 2025.01.15.01
```

#### With Custom Config:
```fish
./false.work/unreal-builder/scripts/remote-mac-build.fish 2025.01.15.01 Shipping
```

### Pipeline Steps

The Fish script runs from the dev container and executes all steps remotely via SSH:

1. **Sync Perforce** - Gets latest code from P4
2. **Apply FMOD Fix** - Auto-fixes FMOD plugin compilation error
3. **Build** - Runs BuildCookRun with full shader cooking
4. **Compress** - Creates `.zip` archive
5. **Upload** - Uploads to DigitalOcean Spaces (credentials passed via SSH)

### Output

Builds are uploaded to:
- Versioned: `https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/mac/spiderlily-mac-<version>.zip`
- Latest: `https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/mac/spiderlily-mac-latest.zip`

## Architecture

```
Dev Container                           Mac Build Machine
├─ remote-mac-build.fish                ├─ Receives SSH commands
│  ├─ Loads vault credentials           ├─ Executes P4 sync
│  ├─ SSH: P4 sync                      ├─ Applies FMOD fix
│  ├─ SSH: Apply FMOD fix               ├─ Runs BuildCookRun
│  ├─ SSH: BuildCookRun                 ├─ Compresses build
│  ├─ SSH: Compress                     └─ Uploads to Spaces
│  ├─ SSH: Upload (w/ credentials)            (AWS CLI uses passed credentials)
│  └─ Displays results
```

**All orchestration happens in the dev container, just like the Windows build script.**

## Troubleshooting

### Build Fails at P4 Sync
- Check P4 credentials: `p4 info`
- Verify workspace: `p4 client -o spiderlily_build_workspace_macmini`

### Build Fails at FMOD
- The script auto-applies the fix, but verify the file exists:
  ```bash
  ls -la ~/Perforce/.../Plugins/FMODStudio/Source/FMODStudio/Private/FMODBlueprintStatics.cpp
  ```

### Build Fails at BuildCookRun
- Check logs in `~/Perforce/.../Saved/Logs/`
- Verify Unreal Engine path: `/Users/Shared/Epic Games/UE_5.6`

### Upload Fails
- Check AWS CLI on Mac: `ssh falsework@host.docker.internal "aws --version"`
- Verify Spaces credentials in dev container vault: `aesthetic-computer-vault/false.work/builds-spaces.env`
- The script passes credentials via environment variables over SSH, so nothing needs to be stored on the Mac

## Integration with builds.false.work

After a successful build, update the builds page:

1. Copy the CDN URL from the build output
2. Add to `/workspaces/aesthetic-computer/false.work/builds.false.work/index.html`:
   ```html
   <a href="https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/mac/spiderlily-mac-2025.01.15.01.zip">
       SpiderLily Mac 2025.01.15.01
   </a>
   ```
3. Deploy the updated page

## Next Steps

- [ ] Set up GitHub Actions workflow for scheduled builds
- [ ] Add build notifications (Discord/Slack webhook)
- [ ] Implement build retention policy (auto-delete old builds)
- [ ] Add build metadata/changelog generation
- [ ] Create unified cross-platform build dashboard
