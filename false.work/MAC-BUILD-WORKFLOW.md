# SpiderLily Mac Build Workflow

## Complete Automated Pipeline

The Mac build workflow now matches the Windows pattern with full automation:

1. **Build on Mac** - P4 sync, FMOD fix, BuildCookRun
2. **Copy to local assets** - ZIP downloaded to `system/public/assets/false.work/`
3. **Upload to CDN** - `npm run assets:sync:up` syncs to DigitalOcean Spaces
4. **Update website** - Inserts new build entry in `builds.false.work/index.html`
5. **Git commit & push** - Automatic commit with rebase on conflicts

## Usage

From the dev container:

```fish
/workspaces/aesthetic-computer/false.work/unreal-builder/scripts/remote-mac-build.fish
```

This will:
- Generate timestamped version (e.g., `2024.11.21-1430`)
- Build complete Mac package with all shaders
- Upload to `https://assets.aesthetic.computer/false.work/spiderlily-mac-{version}.zip`
- Update https://builds.false.work with new download link
- Commit and push to GitHub

## Architecture

```
Dev Container (Fish)
    ↓ SSH (sshpass)
Mac Mini
    ↓ P4 sync
    ↓ FMOD fix
    ↓ BuildCookRun → ~/Builds/{version}/MacNoEditor/
    ↓ ZIP → ~/spiderlily-mac-{version}.zip
    ↓ SCP back to dev container
Dev Container
    ↓ Copy to system/public/assets/false.work/
    ↓ npm run assets:sync:up → DigitalOcean Spaces
    ↓ Update builds.false.work/index.html
    ↓ git commit & push
```

## Files

- **remote-mac-build.fish** - Main orchestration script (runs from dev container)
- **build-run-spiderlily.sh** - Build script on Mac (accepts version parameter)
- **builds.false.work/index.html** - Website updated with new builds
- **vault/false.work/mac-builder-credentials.env** - SSH credentials
- **vault/false.work/builds-spaces.env** - Not used (uses npm assets:sync instead)

## Build Output

- **Local**: `system/public/assets/false.work/spiderlily-mac-{version}.zip`
- **CDN**: `https://assets.aesthetic.computer/false.work/spiderlily-mac-{version}.zip`
- **Log**: `https://assets.aesthetic.computer/false.work/spiderlily-mac-{version}.txt`
- **Website**: `https://builds.false.work` (auto-updated)

## Comparison with Windows

| Step | Windows | Mac |
|------|---------|-----|
| Orchestration | `windows/remote-update-and-build.fish` | `false.work/unreal-builder/scripts/remote-mac-build.fish` |
| Remote access | SSH + PowerShell | SSH + Bash |
| Build script | `build-false-work.ps1` | `build-run-spiderlily.sh` |
| Upload | `npm run assets:sync:up` | `npm run assets:sync:up` |
| Website update | AWK insert | AWK insert (same) |
| Git automation | Auto-rebase on conflict | Auto-rebase on conflict |

Both platforms now use the **exact same** website update and git workflow!

## Build Size

- Mac .app bundle: ~4.9 GB
- Zipped: ~1.8 GB
- Upload time: ~5-10 minutes depending on connection

## Next Steps

Future enhancements:
- [ ] Add build notifications (Discord/email)
- [ ] Parallel Mac + Windows builds
- [ ] Automated testing before deploy
- [ ] Build artifact retention policy
