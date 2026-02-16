# Building Aesthetic Computer Desktop App

## Quick Start (Unsigned Builds)

Build for all platforms from this devcontainer:

```bash
cd /workspaces/aesthetic-computer/ac-electron
./build-all-platforms.fish
```

This will create:
- **Linux**: AppImage, deb, rpm (✅ works in devcontainer)
- **Windows**: exe (✅ works via Docker wine container)
- **macOS**: dmg (⚠️ requires macOS host or CI/CD)

## Setup for Signed Builds

### 1. Add Signing Certificates to Vault

```bash
cd /workspaces/aesthetic-computer/aesthetic-computer-vault/ac-electron

# Add your certificates (see README.md in this directory)
# - mac-developer-id.p12
# - windows-code-signing.pfx
# etc.

# Create .env from template
cp .env.template .env

# Edit .env and add your credentials
emacs .env  # or your preferred editor
```

### 2. Run the Build

```bash
cd /workspaces/aesthetic-computer/ac-electron
./build-all-platforms.fish
```

The script will:
1. Load credentials from vault
2. Build Linux packages natively
3. Build Windows .exe using Docker wine container
4. Skip macOS (requires macOS host)
5. Sign all packages with your certificates

## Platform-Specific Builds

### Linux Only (Native)
```bash
npm run build:linux
```

Creates:
- `Aesthetic Computer-{version}-x86_64.AppImage`
- `Aesthetic Computer-{version}-amd64.deb`
- `Aesthetic Computer-{version}-x86_64.rpm`

### Windows Only (via Docker)
```bash
docker run --rm \
  -v $(pwd):/project \
  electronuserland/builder:wine \
  bash -c "cd /project && npm run build:win"
```

Creates:
- `Aesthetic Computer-Setup-{version}.exe`

### macOS Only (requires macOS)
```bash
# On a Mac:
npm run publish:mac

# Or just build without publishing:
npm run build:mac
```

Creates:
- `Aesthetic Computer-{version}-universal.dmg` (Apple Silicon + Intel)

## Publishing to GitHub Releases

### Automatic (via GitHub Actions)
```bash
# Bump version and push tag
node scripts/release-electron.mjs --bump patch

# GitHub Actions will build and publish automatically
```

### Manual
```bash
# Build locally
./build-all-platforms.fish

# Create GitHub release and upload artifacts
VERSION=$(cat package.json | jq -r .version)
gh release create v$VERSION \
  --title "v$VERSION - Release Title" \
  --notes "Release notes here" \
  dist/*.AppImage \
  dist/*.deb \
  dist/*.rpm \
  dist/*.exe
```

## Cross-Platform Build Matrix

| Platform | Build From | Signing | Notes |
|----------|-----------|---------|-------|
| Linux AppImage | ✅ Devcontainer | ❌ Not signed | Works out of box |
| Linux deb/rpm | ✅ Devcontainer | ❌ Not signed | May fail without libcrypt.so.1 |
| Windows exe | ✅ Devcontainer (Docker) | ✅ Authenticode | Requires wine Docker container |
| macOS dmg | ❌ Requires macOS | ✅ Developer ID | Use GitHub Actions or Mac |

## Troubleshooting

### "libcrypt.so.1 not found" (deb/rpm build)
This is a Fedora 43 issue. AppImage still works. To fix:
```bash
# Install compat library
sudo dnf install libxcrypt-compat
```

Or skip deb/rpm and use AppImage:
```bash
npm run build -- --linux AppImage
```

### Docker not available
Install Docker in the devcontainer or on your host system.

### macOS build fails on Linux
macOS builds cannot run on Linux. Options:
1. Use GitHub Actions (automatic on tag push)
2. Run on a Mac locally
3. Use a macOS cloud VM (MacStadium, AWS EC2 Mac, etc.)

## Size Optimization

Current build sizes (~100MB) can be reduced by:

1. **Remove unused dependencies**
   ```bash
   npm prune --production
   ```

2. **Use asar archive** (already enabled in package.json)

3. **Enable compression** in `package.json`:
   ```json
   "compression": "maximum"
   ```

4. **Exclude dev files** - add to `package.json` "files" exclusions

## CI/CD Integration

The `.github/workflows/electron-release.yml` workflow builds all platforms:

- **Trigger**: Push git tag `v*`
- **Builds**: macOS (universal), Windows (x64), Linux (AppImage/deb/rpm)
- **Publishes**: Automatically to GitHub Releases

Just push a version tag and wait for builds to complete!
