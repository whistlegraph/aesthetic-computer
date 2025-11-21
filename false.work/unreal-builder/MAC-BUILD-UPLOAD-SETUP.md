# Mac Build & Upload Setup Guide

## Overview
Automated Mac build pipeline that syncs from Perforce, builds with BuildCookRun, and uploads to DigitalOcean Spaces.

## Prerequisites

### 1. Mac Build Machine Setup
- Mac with Xcode and Unreal Engine 5.6 installed
- Perforce workspace configured at `~/Perforce/spiderlily_build_workspace_macmini/SL_main`
- SSH access from dev container

### 2. Credentials Setup

#### Mac Builder Credentials
Already set up in `/workspaces/aesthetic-computer/aesthetic-computer-vault/false.work/mac-builder-credentials.env`:
```bash
MAC_HOST=host.docker.internal
MAC_USERNAME=falsework
MAC_PASSWORD=BuildM@chine
```

#### DigitalOcean Spaces Credentials
Create `~/aesthetic-computer-vault/false.work/builds-spaces.env` on the **Mac build machine**:

```bash
# false.work Build Storage Configuration
BUILDS_SPACES_ENDPOINT=https://sfo3.digitaloceanspaces.com
BUILDS_SPACES_KEY=<your-spaces-key>
BUILDS_SPACES_SECRET=<your-spaces-secret>
BUILDS_SPACES_BUCKET=falsework-builds
BUILDS_SPACES_REGION=sfo3
```

**To deploy this file to the Mac:**
```fish
# From dev container:
cd /workspaces/aesthetic-computer
scp aesthetic-computer-vault/false.work/builds-spaces.env falsework@host.docker.internal:~/aesthetic-computer-vault/false.work/
```

### 3. Install AWS CLI on Mac
The upload script uses AWS CLI (S3-compatible) for DigitalOcean Spaces:

```bash
# On Mac:
brew install awscli
```

Verify installation:
```bash
aws --version
```

### 4. Create Vault Directory on Mac
```bash
# On Mac:
mkdir -p ~/aesthetic-computer-vault/false.work
```

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

1. **Sync Perforce** - Gets latest code from P4
2. **Apply FMOD Fix** - Auto-fixes FMOD plugin compilation error
3. **Build** - Runs BuildCookRun with full shader cooking
4. **Compress** - Creates `.zip` archive
5. **Upload** - Uploads to DigitalOcean Spaces

### Output

Builds are uploaded to:
- Versioned: `https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/mac/spiderlily-mac-<version>.zip`
- Latest: `https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/mac/spiderlily-mac-latest.zip`

## Architecture

```
Dev Container                    Mac Build Machine
├─ remote-mac-build.fish  ──SSH─>  ├─ mac-build-and-upload.sh
│  (triggers build)                 │  ├─ P4 sync
│                                   │  ├─ Apply FMOD fix
│                                   │  ├─ BuildCookRun
│                                   │  ├─ Compress
│                                   │  └─ Upload to Spaces
└─ Waits for completion             └─ Returns status
```

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
- Check AWS CLI: `aws --version`
- Verify Spaces credentials in `~/aesthetic-computer-vault/false.work/builds-spaces.env`
- Test connection:
  ```bash
  source ~/aesthetic-computer-vault/false.work/builds-spaces.env
  aws s3 ls s3://$BUILDS_SPACES_BUCKET --endpoint-url $BUILDS_SPACES_ENDPOINT
  ```

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
