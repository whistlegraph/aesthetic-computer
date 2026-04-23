# Systray passphrase entry form

**Requested:** 2026-04-22 (during LACMA submission sprint)
**Priority:** post-LACMA
**Status:** Deferred (plan only, not implemented)

## Ask (verbatim)

> can we add to our python daemon systray a passphrase entry form u can pop up
> on macos here?

## Why

Repeatedly: when a Claude agent or fish script needs to decrypt the vault
(`aesthetic-computer-vault/home/.ssh/id_rsa.gpg`, GPG / age / whatever),
the passphrase has to be typed at the terminal. In long-running agent
sessions (like the LACMA submission sprint on 2026-04-22) that means
the agent is blocked or the user has to context-switch to paste the
passphrase into a non-secure text field.

A native macOS passphrase prompt from the existing Python daemon's
systray would solve both: the agent triggers it over IPC, the user
types the passphrase into a real macOS password field (keychain-
integrated, not logged), and the daemon hands the cleartext back to
whatever requested it — or, better, holds it in-memory for N minutes
and re-serves it without re-prompting.

## Sketch

- **Daemon location:** probably `micro/machine/` or `ac-electron/` —
  check the tray app that already exists. Otherwise there's a Python
  component somewhere in `fedac/` or `ants/` that hosts the tray.
- **UI:** macOS `NSAlert` with a `NSSecureTextField` accessory (via
  PyObjC / `pyobjus` / `rumps` — whichever the tray already uses).
  Alternate path: ship a tiny Swift helper binary and call it from
  Python.
- **IPC:**
  - Unix domain socket at `~/.ac-daemon.sock` (or reuse whatever the
    tray already exposes).
  - Protocol: `{"op":"passphrase","label":"vault-ssh","timeout":600}` →
    `{"ok":true,"secret":"<phrase>","cached":true}`.
  - Requester blocks until user types or cancels.
- **Cache:** in-memory with TTL (default 10 minutes). Clear on daemon
  restart or explicit `{"op":"forget"}`. Do NOT persist to disk.
- **Zeroize:** overwrite the `bytes` object before GC (best-effort on
  Python; the point is to not leave it in a file).

## Integration points

Once the daemon has the endpoint, wire:
- `aesthetic-computer-vault/devault.fish` — currently uses `gpg`
  directly; swap to a helper that asks the daemon first.
- `lith/deploy.fish` — SSH key step.
- Any `npm run session:*` scripts that touch vault creds.

## Out of scope

- Browser integration (a Chrome/Safari extension could call the
  daemon too, but save that for later).
- Cross-device sync — this is machine-local only.

## Next time

When picking this up, start by inventorying the current daemon: find
its entry point, confirm it runs as a systray item, and see what IPC
it already speaks. Only add the passphrase endpoint to an existing
daemon — don't spawn a second tray.
