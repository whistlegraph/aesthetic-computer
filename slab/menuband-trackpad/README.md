# TrackDrum for Menu Band

This direct-download helper gives the sandboxed Mac App Store build of Menu
Band its global trackpad percussion input. It has no instrument window and
makes no sound: Menu Band renders and plays its existing Tracktramp widget.

The helper listens only on `127.0.0.1`, accepts one Menu Band client, and opens
the private trackpad device only while Menu Band has explicitly armed a
performance session. The wire format contains at most 16 normalized contacts.
While that session is active, TrackDrum hides and disconnects the system cursor
and consumes mouse buttons, two-finger context clicks, scrolling, and gestures.
It restores them when Menu Band exits the session. Escape or another double-tap
of Command always exits through a permission-free key-state watchdog. A session
event tap provides the cleanest suppression when Accessibility is already
granted; clear, non-activating local panels prevent clicks from escaping when it
is not. TrackDrum never prompts for that permission.

Build the signed universal disk image:

```bash
./build-dmg.sh
```

Build and verify the app without creating a disk image:

```bash
./build-dmg.sh --app-only
```

For public distribution, use `--notarize`, then publish
`build/TrackDrum-for-Menu-Band.dmg` on menuband.app. The App Store Menu Band
remains sandboxed; this separately launched Developer ID app owns the private
trackpad capability.
