# SpiderLily Automated Build & Distribution System

Complete automation for building and distributing SpiderLily game builds via DigitalOcean Spaces.

## 🎯 Overview

This system provides **around-the-clock automated builds** for SpiderLily on multiple platforms, with automatic upload to DigitalOcean Spaces and public distribution via `https://builds.false.work`.

### What This Solves

- ✅ **Zero-touch builds**: Trigger Mac/Windows/iOS builds from your dev container
- ✅ **Automatic distribution**: Builds upload to Spaces and appear on builds.false.work
- ✅ **Version management**: Timestamped builds + "latest" aliases
- ✅ **Cost efficient**: Mac builds free, Windows ~$3/build, storage $5/month
- ✅ **Public access**: Anyone can download builds from builds.false.work

### Architecture

```
Dev Container (Linux)
    ↓ SSH/gcloud
Mac Mini M4 ←→ GCP Windows VM
    ↓ build
SpiderLily .app/.exe
    ↓ compress + upload
DigitalOcean Spaces (falsework-builds)
    ↓ CDN
https://builds.false.work
```

## 🚀 Quick Start

### 1. Setup DigitalOcean Spaces

```bash
# Create a Space in DigitalOcean dashboard
# Name: falsework-builds
# Region: sfo3 (San Francisco)
# Enable CDN: Yes
# File Listing: Restricted

# Or via CLI:
doctl spaces create falsework-builds --region sfo3
```

### 2. Generate Access Keys

1. Go to: https://cloud.digitalocean.com/account/api/spaces
2. Click "Generate New Key"
3. Name: `falsework-builds`
4. Save the Key and Secret

### 3. Configure Credentials

Create `aesthetic-computer-vault/false.work/builds-spaces.env`:

```bash
BUILDS_SPACES_ENDPOINT=https://sfo3.digitaloceanspaces.com
BUILDS_SPACES_KEY=DO00XXXXXXXXXXXXXXXXXX
BUILDS_SPACES_SECRET=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
BUILDS_SPACES_BUCKET=falsework-builds
BUILDS_SPACES_REGION=sfo3
BUILD_RETENTION_DAYS=30
BUILD_WEBHOOK_URL=  # Optional: Discord webhook for notifications
```

### 4. Install Dependencies

**On Dev Container (already installed):**
- `s3cmd` - S3-compatible CLI for Spaces
- `sshpass` - SSH automation
- `gcloud` - GCP CLI (for Windows VM)

**On Mac Mini:**
```bash
brew install s3cmd
```

**On Windows VM:**
```powershell
# Install AWS CLI (S3-compatible)
choco install awscli
```

### 5. Test Upload

```bash
# From dev container, trigger Mac build + upload
cd /workspaces/aesthetic-computer/false.work/unreal-builder/scripts
fish schedule-builds.fish nightly mac
```

This will:
1. SSH to Mac Mini
2. Build SpiderLily.app
3. Compress to .tar.gz
4. Upload to Spaces
5. Update builds.false.work/index.html

## 📋 Usage

### Trigger Builds

```bash
cd /workspaces/aesthetic-computer/false.work/unreal-builder/scripts

# Build all platforms
fish schedule-builds.fish nightly all

# Build specific platform
fish schedule-builds.fish release mac
fish schedule-builds.fish release windows
fish schedule-builds.fish release ios

# Custom version
fish schedule-builds.fish release mac 1.0.0
```

### Manual Upload (if build exists)

```bash
# Upload existing Mac build
./upload-to-spaces.sh mac ~/Perforce/.../Packaged/Mac/SpiderLily.app 1.0.0

# Upload Windows build
./upload-to-spaces.sh windows D:/Builds/123/WindowsNoEditor 1.0.0

# Upload iOS build
./upload-to-spaces.sh ios ~/Perforce/.../Packaged/IOS/SpiderLily.app 1.0.0
```

### Update builds.false.work

```bash
# After uploading, update the public index
./update-builds-index.sh mac 1.0.0
./update-builds-index.sh windows 1.0.0
./update-builds-index.sh ios 1.0.0

# Then commit and push
cd /workspaces/aesthetic-computer
git add false.work/builds.false.work/ system/public/builds.false.work/
git commit -m "Add SpiderLily builds"
git push
```

## 🛠️ Scripts Reference

### `schedule-builds.fish`
Orchestrates builds on remote machines from dev container.
```bash
fish schedule-builds.fish <type> <platform>
# type: nightly, release, hotfix
# platform: all, mac, windows, ios
```

### `upload-to-spaces.sh`
Compresses and uploads builds to DigitalOcean Spaces.
```bash
./upload-to-spaces.sh <platform> <build_path> [version]
```

### `update-builds-index.sh`
Updates builds.false.work with new download links.
```bash
./update-builds-index.sh <platform> <version>
```

### `mac/build-and-upload.sh`
Complete Mac pipeline: build → compress → upload → update index.
```bash
./mac/build-and-upload.sh [version]
```

### `windows-build-and-upload.ps1`
Complete Windows pipeline (runs on GCP VM).
```powershell
.\windows-build-and-upload.ps1 -Version "1.0.0" -Config "Development"
```

## 🌐 Distribution

Builds are available at:
- **Public website**: https://builds.false.work
- **Direct CDN URLs**: `https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/{platform}/{filename}`
- **Latest aliases**: `spiderlily-{platform}-latest.tar.gz` or `.zip`

### Download URLs

```
Mac:
https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/mac/spiderlily-mac-2025.11.05.14.30.tar.gz
https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/mac/spiderlily-mac-latest.tar.gz

Windows:
https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/windows/spiderlily-windows-2025.11.05.14.30.zip
https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/windows/spiderlily-windows-latest.zip

iOS:
https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/ios/spiderlily-ios-2025.11.05.14.30.tar.gz
https://falsework-builds.sfo3.cdn.digitaloceanspaces.com/ios/spiderlily-ios-latest.tar.gz
```

## 📊 Storage Management

### List All Builds

```bash
s3cmd ls s3://falsework-builds/mac/
s3cmd ls s3://falsework-builds/windows/
s3cmd ls s3://falsework-builds/ios/
```

### Check Storage Usage

```bash
s3cmd du s3://falsework-builds/
```

### Delete Old Builds

```bash
# Delete builds older than 30 days
s3cmd ls s3://falsework-builds/ | \
  awk '{if ($1 < "2025-10-01") print $4}' | \
  xargs -I {} s3cmd del {}
```

### Set Lifecycle Policy (Auto-delete)

Create `lifecycle.json`:
```json
{
  "Rules": [
    {
      "ID": "delete-old-builds",
      "Status": "Enabled",
      "Prefix": "",
      "Expiration": {
        "Days": 30
      }
    }
  ]
}
```

Apply:
```bash
aws s3api put-bucket-lifecycle-configuration \
  --bucket falsework-builds \
  --endpoint-url https://sfo3.digitaloceanspaces.com \
  --lifecycle-configuration file://lifecycle.json
```

## 💰 Cost Breakdown

| Service | Details | Monthly Cost |
|---------|---------|--------------|
| **DigitalOcean Spaces** | 250GB storage + 1TB bandwidth + CDN | $5 |
| **Mac Builder** | M4 Mac Mini (local) | $0 |
| **Windows Builder** | GCP n2-standard-8 (on-demand) | ~$3/build |
| **Bandwidth** | After 1TB: $0.01/GB | ~$0-5 |
| **Total** | For ~10 builds/month | **~$35-40** |

Compare to AWS EC2 Mac instances: **$800+/month** 🎉

## 🔒 Security

- ✅ Credentials stored in `aesthetic-computer-vault` (private submodule)
- ✅ Files publicly readable but bucket listing restricted
- ✅ HTTPS-only access via CDN
- ✅ SSH access to builders password-protected
- ✅ GCP VM uses service account keys

## 🐛 Troubleshooting

### Upload fails with "403 Forbidden"
- Check Spaces keys in `builds-spaces.env`
- Verify bucket name and region match

### Can't SSH to Mac Mini
- Check credentials in `mac-builder-credentials.env`
- Test: `ssh falseworks@host.docker.internal`

### Windows build fails
- Ensure GCP VM is running: `gcloud compute instances list`
- Check Perforce connection: `p4 info`
- Verify UE5.6 installed at `C:\Program Files\Epic Games\UE_5.6`

### builds.false.work not updating
- Run `./update-builds-index.sh` manually
- Commit and push changes to git
- Netlify will auto-deploy

## 📚 Related Documentation

- [../README.md](../README.md) - Main false.work build system docs
- [MAC-SETUP-PLAN.md](MAC-SETUP-PLAN.md) - Mac builder setup guide
- [BUILD-AUTOMATION-GUIDE.md](BUILD-AUTOMATION-GUIDE.md) - General automation guide
- [../../at/pds/STORAGE.md](../../at/pds/STORAGE.md) - DigitalOcean Spaces reference

---

**Engineered by [Aesthetic Inc.](https://aesthetic.direct)**  
Questions? [@jeffrey on prompt.ac](https://prompt.ac/chat)
