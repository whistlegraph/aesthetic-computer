# Apple Notarization Setup for ac-electron

Date: 2026-04-17
Host: AestheticComputers-MacBook-Neo.local (not in machines.json)

## Summary

Wired up Apple notarization so auto-updates from `releases.aesthetic.computer/desktop` no longer trip Gatekeeper's "malware could not be verified" warning. Prior releases (including v0.1.38) were signed but not notarized, so downloaded DMGs refused to launch after being flagged with `com.apple.quarantine`. Built, notarized, and published v0.1.39 and confirmed it auto-installs cleanly over v0.1.38 with no Gatekeeper prompts.

## Changes

### Credentials (vault)

Added to `aesthetic-computer-vault/ac-electron/.env`, then re-encrypted to `.env.gpg` and shredded the plaintext:

- `APPLE_ID=me@jas.life`
- `APPLE_APP_SPECIFIC_PASSWORD` — generated at appleid.apple.com (Sign-In and Security → App-Specific Passwords), labelled `ac-electron-notary`
- `APPLE_TEAM_ID=FB5948YR3S`

Also fixed a stale absolute path: `CSC_LINK` was pointing to `/Users/jas/Desktop/code/aesthetic-computer/...` (the `jeffrey-macbook` host's layout in `machines.json`), which doesn't exist on this machine. Switched to a relative path `../aesthetic-computer-vault/ac-electron/mac-developer-id.p12` so the same `.env` works on any host with the standard repo layout.

### Code

- [ac-electron/package.json](ac-electron/package.json) — bumped version to `0.1.39`, flipped `mac.notarize` from `false` to `true`, removed the `afterSign: scripts/notarize.js` hook (electron-builder's built-in notarize handles the `.app`), and added a new `afterAllArtifactBuild: scripts/notarize-dmg.js` hook (see DMG gotcha below).
- [ac-electron/scripts/notarize-dmg.js](ac-electron/scripts/notarize-dmg.js) — new. After all artifacts are built, this signs each DMG with the Developer ID identity, submits it to `notarytool --wait`, and staples the ticket.
- [ac-electron/scripts/notarize.js](ac-electron/scripts/notarize.js) — kept as a documented fallback. Renamed `APPLE_NOTARIZE_PWD` → `APPLE_APP_SPECIFIC_PASSWORD` (electron-builder's convention; already matches `.env.template`) and removed the `CI`/`FORCE_NOTARIZE` guard so credential presence is the sole gate.
- [ac-electron/CODE-SIGNING.md](ac-electron/CODE-SIGNING.md) — renamed `APPLE_NOTARIZE_PWD` → `APPLE_APP_SPECIFIC_PASSWORD` in docs, removed the now-unnecessary `FORCE_NOTARIZE=1` export from the local-testing example.

## Build + Notarization

Command: `rm -rf dist/ && npm run build:mac` with the vault `.env` sourced beforehand.

- Clean build + `.app` notarization: ~5 min (electron-builder v25.1.8, electron 39.2.7)
- The single log line `notarization successful` from electron-builder covers only the `.app`. The DMG gets built *after* that step and is never submitted. electron-builder v25 does not sign or notarize DMGs automatically — see Gotchas.

## spctl Verification

After the DMG was signed, notarized, and stapled (see Gotchas):

```
$ spctl -a -vvv -t install dist/Aesthetic.Computer-0.1.39-universal.dmg
dist/Aesthetic.Computer-0.1.39-universal.dmg: accepted
source=Notarized Developer ID
origin=Developer ID Application: Jeffrey Scudder (FB5948YR3S)

$ spctl -a -vvv -t exec dist/mac-universal/Aesthetic.Computer.app
dist/mac-universal/Aesthetic.Computer.app: accepted
source=Notarized Developer ID
origin=Developer ID Application: Jeffrey Scudder (FB5948YR3S)
```

Both `.app` and DMG pass the Gatekeeper check as `Notarized Developer ID`. `xcrun stapler validate` succeeds on both.

## Publish

Used `aws s3 cp` against the DO Spaces endpoint with creds from `aesthetic-computer-vault/silo/.env` (`SPACES_KEY` / `SPACES_SECRET`) — the current [publish-release.mjs](ac-electron/scripts/publish-release.mjs) has `requestChecksumCalculation: "WHEN_REQUIRED"` which should work, but the CLI path is a known-good fallback for the 200MB DMG multipart.

Uploaded to `s3://releases-aesthetic-computer/desktop/` with `--acl public-read`:

- `Aesthetic.Computer-0.1.39-universal.dmg` (199 MB, `public, max-age=31536000`)
- `Aesthetic.Computer-0.1.39-universal.dmg.blockmap`
- `Aesthetic.Computer-0.1.39-universal.zip` (192 MB)
- `Aesthetic.Computer-0.1.39-universal.zip.blockmap`
- `latest-mac.yml` (`no-cache, no-store, must-revalidate`)

Then registered with silo: `POST https://silo.aesthetic.computer/api/desktop/register` with `X-Publish-Secret` header → `{"ok":true,"version":"0.1.39"}`. Confirmed `GET /desktop/latest` now returns `0.1.39`.

## Auto-update Test

Launched the installed v0.1.38 (at `/Applications/Aesthetic.Computer.app`) with `--remote-debugging-port=9222`. Timeline:

1. ~5s after launch: electron-updater checked `latest-mac.yml`, saw 0.1.39 available.
2. Downloaded `Aesthetic.Computer-0.1.39-universal.zip` (~200 MB) to `~/Library/Caches/aesthetic-computer-updater/pending/`.
3. Squirrel.Mac spun up its local proxy server and handed the ZIP to ShipIt.
4. `SIGTERM` to the Electron process — ShipIt ran the bundle swap.
5. ShipIt log at `~/Library/Caches/computer.aesthetic.app.ShipIt/ShipIt_stderr.log` shows: `Installation completed successfully`.
6. `/Applications/Aesthetic.Computer.app/Contents/Info.plist` now reports `0.1.39`.
7. `spctl -a -vvv -t exec /Applications/Aesthetic.Computer.app` reports `source=Notarized Developer ID`.
8. Relaunching v0.1.39: no Gatekeeper prompt, app starts normally.

## Gotchas

### electron-builder v25 doesn't notarize the DMG wrapper

`mac.notarize: true` submits the `.app` bundle but not the DMG itself. Symptom: after the first build, `xcrun stapler validate <dmg>` reports "does not have a ticket stapled" and `spctl -t install <dmg>` returns `rejected — source=no usable signature`.

**Fix applied**: added `afterAllArtifactBuild: scripts/notarize-dmg.js`. The script signs each DMG with the Developer ID cert (`codesign --sign "Developer ID Application: …" --timestamp --force <dmg>`), submits via `xcrun notarytool submit … --wait`, then staples. Future `npm run build:mac` runs handle this automatically.

For this 0.1.39 release the hook wasn't wired yet, so the fix was applied manually to the artifacts in `dist/`. Because the DMG was re-signed after the original manifest was written, the size and sha512 of the DMG entry in `latest-mac.yml` were stale — regenerated them with `openssl dgst -sha512 -binary <dmg> | base64` and edited the YAML before upload. The ZIP-based auto-update path uses the top-level `path: …zip` entry which was unaffected.

### ShipIt had a stale failed install from earlier

Found this in `~/Library/Caches/computer.aesthetic.app.ShipIt/ShipIt_stderr.log` from a 00:39 attempt:

```
ShipIt[4629:435820] Installation error: Error Domain=NSOSStatusErrorDomain Code=-67068 "(null)"
```

`errSecAuthFailed` — the earlier unsigned 0.1.39 attempt couldn't pass Gatekeeper's install-time check, so Squirrel.Mac refused the swap. That's the exact failure mode the notarization fixes. The 11:53 run (post-notarization) logged `Installation completed successfully` on `Resuming installation attempt 2` — ShipIt picked up the queued update and completed cleanly.

### Vault `.env` had a stale absolute `CSC_LINK`

Pre-existing `CSC_LINK=/Users/jas/Desktop/code/aesthetic-computer/...` was the `jeffrey-macbook` host's repo layout. Broke on any other machine. Fixed by switching to a relative path (resolved from `ac-electron/` where `npm run build:mac` runs).

### `ADMIN_SUB=auth0|…` in `silo/.env` errors when sourced

The pipe in an unquoted value trips zsh when you `source` the file — cosmetic `command not found: <suffix>` error, the var still gets set to just `auth0`. Didn't break the publish; not touching it here, but worth quoting values if the file gets touched again.
