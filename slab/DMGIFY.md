# Dmgify

`dmgify` turns a local offline directory into a universal macOS application and
distributable DMG. It is the reusable form of Menu Band's certificate pipeline.
It has two runtimes:

- `electron` (default) wraps an arbitrary dependency-free HTML directory.
- `swift-gallery` builds a small native AppKit image archive from `manifest.json`.
  It virtualizes the post grid, downsamples only visible images, caches decoded
  thumbnails, offers a dense responsive thumbnail view and a sortable native
  list, opens originals in Quick Look, and uses AppKit's dedicated sharing
  toolbar item. Shares contain the original image files, untouched caption,
  and canonical source URL. It has no WebView or Electron dependency.

Both runtimes follow the same release chain:

1. wrap the directory in its selected runtime;
2. build universal `arm64 + x86_64` code;
3. sign with the keychain's Developer ID Application identity and hardened runtime;
4. submit the app to Apple's notary service and staple its ticket;
5. create a DMG with an `/Applications` drag target;
6. sign, notarize, and staple the DMG itself;
7. verify signature, staple, and Gatekeeper acceptance; and
8. emit a JSON receipt next to the DMG.

The generated Electron shell also embeds a signed universal AppKit helper. Offline
archives can opt into the preload's two narrow APIs through
`window.archiveBridge`: `share(paths)` opens the native macOS sharing service
picker, while `export(paths)` copies original bundled files to a user-selected
Finder destination. Both reject absolute paths and traversal outside the
sealed archive resources.

The default notarization credentials are the same vault-side
`apple/app-specific-password.env` used by Menu Band. Credentials are read only
for `notarytool`; they never enter the app, DMG, build directory, receipt, or
MCP output.

Notarization is submitted once and then polled by submission ID. Credential
arguments are redacted from command errors, and a transient long-lived Apple
client failure cannot silently create a duplicate submission.

## MCP

The repo `.mcp.json` registers three tools:

- `dmgify_plan` — read-only validation and payload sizing.
- `dmgify_build` — full signed/notarized build.
- `dmgify_verify` — read-only `.app` / `.dmg` trust verification.

All input and output paths must remain beneath the local user's home directory.
The build tool deliberately calls out its Apple-notary side effect in the MCP
schema.

## CLI

```bash
node slab/bin/dmgify.mjs plan \
  --source /absolute/path/to/archive \
  --entry index.html \
  --name "Example Archive" \
  --bundle-id computer.aesthetic.examplearchive \
  --icon /absolute/path/to/icon.png

node slab/bin/dmgify.mjs build \
  --source /absolute/path/to/archive \
  --runtime swift-gallery \
  --entry index.html \
  --name "Example Archive" \
  --bundle-id computer.aesthetic.examplearchive \
  --version 1.0.0 \
  --icon /absolute/path/to/icon.png \
  --output /absolute/path/to/archive/release

node slab/bin/dmgify.mjs verify \
  --path /absolute/path/to/archive/release/Example-Archive-1.0.0.dmg
```

Use repeated `--include` flags (or the MCP `include` array) to narrow the files
embedded under `Contents/Resources/archive/`. Defaults omit common build and
repository directories.

The native gallery expects the archive manifest to contain `title`, `account`,
`counts.posts`, `counts.stills`, and posts with `shortcode`, `date`, `caption`,
and `stills[].file`. The ordinary `index.html` remains in the archive as a
portable double-click fallback, but the native app does not load it.
