# Dmgify

`dmgify` turns a local, dependency-free HTML directory into a universal macOS
Electron application and distributable DMG. It is the reusable form of Menu
Band's certificate pipeline:

1. wrap the directory in a sandboxed Electron shell;
2. build universal `arm64 + x86_64` code;
3. sign with the keychain's Developer ID Application identity and hardened runtime;
4. submit the app to Apple's notary service and staple its ticket;
5. create a DMG with an `/Applications` drag target;
6. sign, notarize, and staple the DMG itself;
7. verify signature, staple, and Gatekeeper acceptance; and
8. emit a JSON receipt next to the DMG.

The default notarization credentials are the same vault-side
`apple/app-specific-password.env` used by Menu Band. Credentials are read only
for `notarytool`; they never enter the app, DMG, build directory, receipt, or
MCP output.

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
