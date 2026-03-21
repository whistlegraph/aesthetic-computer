# FedAC Score

FedAC shipping is complete only when users can receive and apply the new build.

## Scope

- `fedac/` is the OS stack (kiosk + native boot flows).
- `fedac/native/` is the bare-metal kernel+initramfs path.

## Ship Contract

1. Build the release artifact.
2. Publish the artifact to the release CDN.
3. Verify CDN metadata (`.version`, `.sha256`, `releases.json`) and binary hash.
4. Trigger the update signal path for running OS instances.
5. Validate install/flash path on real media.

If one step is missing, it is not shipped.

## Native Release Endpoints

- `https://releases.aesthetic.computer/os/native-notepat-latest.vmlinuz`
- `https://releases.aesthetic.computer/os/native-notepat-latest.version`
- `https://releases.aesthetic.computer/os/native-notepat-latest.sha256`
- `https://releases.aesthetic.computer/os/releases.json`

## Running-Instance Alert Rule

- The authoritative update signal is the CDN version bump (`native-notepat-latest.version`).
- Running native OS instances poll this version URL and emit in-OS update notification (`update!` + beep/TTS) when a newer version is detected.
- Operator message broadcast (chat/system) is recommended for immediate human visibility, but does not replace the CDN version signal.
