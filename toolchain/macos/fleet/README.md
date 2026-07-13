# Fleet Mac config

Small idempotent scripts to keep @jeffrey's Macs (neo, blueberry, chicken, panda)
consistently configured. Run from any host that can `ssh` the targets.

## Stats (menu-bar system monitor)

`stats-shared.plist` is the canonical [exelban/Stats](https://github.com/exelban/stats)
config — it mirrors neo's menu bar: **CPU / RAM / GPU / Disk** as `mini` widgets +
**Network** as `speed`; Sensors and Battery off. Machine-specific keys (`remote_id`,
status-item/window positions) are excluded so each Mac lays out naturally.

```bash
bash stats-sync.sh all              # neo chicken panda
bash stats-sync.sh chicken panda    # specific hosts
```

Each run imports the config, registers Stats as a login item (launches at boot),
and opens it. Stats' own auto-updater keeps versions at parity once it's running.

Login-at-startup uses a classic LaunchServices login item, because Stats 3.x's
"Start at login" toggle is SMAppService-backed and not reliably settable from the
CLI. Both mechanisms launch the same single-instance app, so there's no conflict.

## Cursor color

`cursor-color.sh` sets the macOS **pointer fill color** per Mac so each machine is
identifiable by its cursor: **neo = green, blueberry = blue** (others fall through to
the default black pointer). Colors live in `com.apple.universalaccess`
(`cursorFill` / `cursorOutline`) — the same keys the system Pointer settings write.

```bash
bash cursor-color.sh all              # neo chicken panda (per-host mapping)
bash cursor-color.sh blueberry        # blueberry -> blue
bash cursor-color.sh chicken purple   # one-off override to a named color
```

`universalaccessd` is SIP-protected and can't be hot-reloaded from the CLI, so the
new color appears on the **next login or lock/unlock (⌃⌘Q)**, when accessibility
prefs get re-read. Blueberry's blue is also baked into `blueberry-join.sh` (step 5/5)
so a fresh rejoin restores it.

## Ableton Live

Two scripts extend the colour-as-identity convention into Live and let neo's tuned
Live settings propagate to the other Macs.

### `ableton-theme.mjs` — tint Live to each Mac's macOS accent

The **macOS accent colour is the source of truth**: each host's Live is tinted to the
hue of whatever `AppleAccentColor` that Mac is set to. Change it in System Settings >
Appearance, re-run, and Live follows. Mirrors the cursor mapping — **neo = green,
blueberry = blue**.

```bash
node ableton-theme.mjs                 # every host, matched to its own accent
node ableton-theme.mjs blueberry       # just blueberry
node ableton-theme.mjs --set-accent    # also write each Mac's fleet-identity accent first
node ableton-theme.mjs neo 300         # one-off hue override (degrees)
node ableton-theme.mjs neo --intensity 80
```

Live 12 dropped loadable custom themes — a `.ask` in `User Library/Themes/` is ignored,
even after a restart. What Live 12 offers instead is native **Color Hue (0–360)** and
**Color Intensity (0–100)** sliders under Settings > Theme & Colors > Customization, and
the only way to reach them is Live's UI. So the script drives those sliders over
Accessibility (`AXIncrement`/`AXDecrement`, since they reject `set value`). Consequences:
**Live must be running and the Mac unlocked**, and because the slider values live in
`Preferences.cfg`, an `ableton-sync.sh` run carries the source's hue to the target — so
re-run this after any sync (the sync script prints that reminder).

### `ableton-tiny.mjs` — make Live a tiny concern

```bash
node ableton-tiny.mjs                  # every host: 50% zoom, smallest window
node ableton-tiny.mjs blueberry        # just blueberry
node ableton-tiny.mjs neo --zoom 75 --size 900x600
node ableton-tiny.mjs blueberry --reset  # back to 100% zoom, roomy window
```

Drives Live's native **Zoom** slider (Settings > Display & Input, 50–200%) and resizes the window.
`720x579 @ 50%` is genuinely as small as Live goes — measured by asking for a `{1,1}` window and
reading back what it clamped to. Ask for less and Live silently ignores you.

> The window rect is **not** stored in the `.als` — Live keeps it globally — so a set can't carry
> its own geometry. Homogenizing the window means re-running this after a set opens, not baking it
> into a template.

### Live's global modals

Live throws app-modal alerts (most often **"Audio is disabled. Please choose an audio output
device"** on a Mac with no audio device selected) that **block every other Accessibility call** —
automation that ignores them just hangs. They are also not addressable as normal dialogs:
`click button "OK" of window 1` fails with `-1728`. The OK button actually lives at
`button 1 of group 1 of window 1`, and both scripts above dismiss it before doing anything else.

When AX can't see a dialog at all, fall back to **vision**: `frame <machine>` (see `slab/bin/frame`)
returns the AX tree *and* OCR with click coordinates, so you can locate the button visually and
click its coordinate. That observe→act loop is the general escape hatch for any Ableton automation
that stalls.

### `ableton-sync.sh` — copy neo's Live settings to the fleet

```bash
bash ableton-sync.sh all               # neo -> blueberry chicken panda
bash ableton-sync.sh blueberry         # neo -> blueberry
SOURCE=chicken bash ableton-sync.sh neo
```

Copies `Preferences.cfg`, `Library.cfg`, User Remote Scripts, and the User Library from
the source (default neo) to each target — theme, UI zoom, audio device, warp/record/launch
defaults, plug-in paths. Safe because the Macs are the same model (`Mac17,5`), so even the
Core Audio device selection lands correctly. The target's settings folder is backed up to
`Live <version>.bak-<timestamp>` first. It **refuses to run while Live is open** on the
target, because Live rewrites `Preferences.cfg` on quit and would clobber the sync. After a
sync, run `node ableton-theme.mjs <host>` to restore that Mac's own accent-matched tint.

## SSH mesh

`fleet-keys.pub` holds every fleet Mac's public identity key
(`jas@aesthetic` laptop, `jas-neo`, `chicken-to-neo`, `panda-to-neo`,
`blueberry-to-neo`). `ssh-mesh.sh` merges them into a host's `authorized_keys`
(dedup by key blob, never removes) so any fleet Mac can reach any other.

```bash
bash ssh-mesh.sh all
```

> Tailscale SSH's *server* is Linux-only, so it can't mesh these Macs — classic
> `authorized_keys` distribution is the durable path. The fleet is reachable over
> the tailnet by name/IP (e.g. blueberry = `100.79.75.53`); the tailnet gives the
> network path, `authorized_keys` gives the auth.

## Blueberry bootstrap

Blueberry does **not** trust the fleet key yet and isn't SSH-reachable, so it can't
be pushed to. `blueberry-join.sh` is a self-contained one-shot that adds the fleet
keys to its `authorized_keys` and configures Stats. It's staged on neo, so run this
**on blueberry** (blueberry already holds a key to neo):

```bash
ssh neo 'cat ~/blueberry-join.sh' > /tmp/bj.sh && bash /tmp/bj.sh
```

After that, blueberry joins the mesh and `ssh-mesh.sh` / `stats-sync.sh` work on it
like any other host.
