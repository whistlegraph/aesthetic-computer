# `kidlisp` Release and Asset Spec

## Goal

Define portable binary release structure so installers and `kidlisp self-update` are deterministic.

## Versioning

- Semantic versioning: `vMAJOR.MINOR.PATCH`
- Channels:
  - `stable` (default)
  - `beta`

## Release Sources

- Primary: GitHub Releases in `whistlegraph/aesthetic-computer`
- Optional mirror/CDN: `https://kidlisp.com/releases/`

## Asset Naming

Per target:

- Unix archives:
  - `kidlisp_<version>_darwin-arm64.tar.gz`
  - `kidlisp_<version>_darwin-x64.tar.gz`
  - `kidlisp_<version>_linux-x64.tar.gz`
  - `kidlisp_<version>_linux-arm64.tar.gz`
- Windows archive:
  - `kidlisp_<version>_windows-x64.zip`

`<version>` is plain semver without leading `v` inside filenames (example: `0.2.0`).

## Archive Contents

### Unix

```text
kidlisp
LICENSE
README.md
```

### Windows

```text
kidlisp.exe
LICENSE
README.md
```

## Checksum Contract

Release must include:

- `kidlisp_<version>_checksums.txt`

Format:

```text
<sha256>  kidlisp_<version>_darwin-arm64.tar.gz
<sha256>  kidlisp_<version>_darwin-x64.tar.gz
<sha256>  kidlisp_<version>_linux-x64.tar.gz
<sha256>  kidlisp_<version>_linux-arm64.tar.gz
<sha256>  kidlisp_<version>_windows-x64.zip
```

Optional:
- `kidlisp_<version>_checksums.txt.sig`

## Channel Metadata

Provide channel manifests for installer + self-update:

- `latest.json` (stable)
- `latest-beta.json` (beta)

Schema:

```json
{
  "version": "0.2.0",
  "channel": "stable",
  "published_at": "2026-03-09T00:00:00Z",
  "assets_base_url": "https://github.com/whistlegraph/aesthetic-computer/releases/download/v0.2.0/",
  "checksums_file": "kidlisp_0.2.0_checksums.txt"
}
```

## Build and Publish Flow

1. Build binaries via Bun compile for all targets.
2. Package target archives.
3. Generate checksum file.
4. Upload release assets.
5. Publish/update channel manifest(s).
6. Smoke test installers against new release.

## Required Smoke Tests

- Fresh install on Linux/macOS/Windows.
- Upgrade install over previous version.
- `kidlisp version` returns expected version.
- `kidlisp --help` executes successfully.
- `kidlisp self-update` no-op on latest version.

## npm Package Interop

- npm package `kidlisp` remains available for runtime-managed installs.
- Portable binary releases are canonical for no-runtime installs.
- `kidlisp version` output should include installation mode when available (future enhancement).
