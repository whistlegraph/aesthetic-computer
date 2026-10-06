# Displays

`displays` inventories connected monitors, shows temporary number labels, and
previews or changes the arrangement and resolution of displays on a Mac.
`displays map` creates an editable physical seat map from JSON, including split
edges where one large monitor meets two laptops.

```sh
bash toolchain/macos/install-displays.sh
displays list neo blueberry chicken panda frisbee
displays identify neo blueberry chicken panda frisbee --seconds 8
displays modes neo:1
```

The CLI uses the fleet's private machine registry and SSH configuration. Each
remote Mac needs `~/.local/bin/slab-displays-native`; run the installer there or
copy the compiled helper from a Mac with a compatible architecture and OS.
No background service is added. Unreachable machines report individual errors.

Addresses such as `neo:1` pair a machine with its persistent local monitor number.
The native helper records UUID → number in
`~/.config/slab/displays/numbers.json` on that Mac. Disconnecting a monitor
does not free its number. macOS display IDs and mode IDs can change; query them
again after reconnecting. If macOS changes a monitor's UUID it receives a new
number. UUIDs are not assumed unique between machines.

## Geometry within a Mac

```sh
# Preview first; append --apply to change the current login session.
displays set neo:2 --x -1920 --y 0
displays set neo:1 --mode 8
displays layout neo /path/to/changes.json
displays restore neo /path/returned/by/apply/layout-UUID.json
```

Mode 8 and the second display above are examples; use the detected numbers and
available modes. A changes file is an array such as
`[{"number":1,"modeID":8},{"number":2,"x":1920,"y":0}]`.

Inventory origins and bounds are **Quartz desktop points**, with x increasing
right and y increasing down. Negative origins are supported. Backing pixels,
refresh rate, physical dimensions reported by macOS, and rotation are separate
fields. This differs from unipointer v1's AppKit y-up coordinates; do not mix
the two without conversion.

Layouts must include an origin at (0,0), share edges, and avoid overlaps. The
controller previews the full layout; the native helper checks it again against
the current display topology before one CoreGraphics transaction. Every apply
saves a restore file on the affected machine, then reports the observed result
and whether it matches the request. macOS may adjust a requested arrangement.
Changes last for the current login session. There is no timed auto-revert;
restore explicitly with the reported backup path and `--apply`.

Mirroring is detected but read-only. Rotation is reported but cannot be changed.
This first version supports macOS only. Applying layouts has not been exercised
on a live multi-monitor Mac; inventory, identification, dry runs, invalid-layout
rejection, and the model's multi-monitor cases are verified independently.

The native transaction follows Apple's
[display configuration API](https://developer.apple.com/documentation/coregraphics/cgconfiguredisplayorigin(_:_:_:_:)).

## Physical arrangement between machines

```sh
displays map toolchain/fleet/two-over-three.example.json --output /tmp/desktop-map.html
open /tmp/desktop-map.html
displays edges /path/to/desktop-map.json
```

The map numbers are seat positions, independent of a Mac's local monitor number.
Assign each to an address, drag its rectangle or edit its coordinates, and use
**Save map** to download JSON. **Reset** returns to the loaded map. Regenerate
HTML with `displays map` to reopen a saved JSON file.

Seat coordinates describe an approximate physical arrangement, not OS pixels.
The photo-derived example closes bezel gaps to make pointer boundaries explicit;
it is not a calibrated measurement. The middle lower display's top edge splits
between the two upper displays. `displays edges` emits both source and
destination fractions for every touching boundary. Gaps have no connection.

Saving the HTML map changes only that map. A photo alone does not establish
which Mac drives an unlabeled screen.

## Native seat editor

Open **Slab → Fleet & System → Deskflow → Desktop Layout…** on the controller with the `displays`
CLI installed. Drag numbered tiles to snap their edges together; arrow keys
nudge the selected tile (Shift moves ten units). The coordinate and size fields
edit the physical seat diagram. They do not change a monitor's pixel resolution.
**Identify** shows the matching seat number and machine address on each online
display. An offline configured screen retains a tile.

**Apply** installs reciprocal Deskflow links, including partial edge ranges,
on connected Macs and reloads the active controller. Macs already offline when
the window loaded keep their tiles and are reported as pending sync. Refresh
and Apply after they return, before changing controllers; deferred writes are
not automatic. A Mac that disconnects or returns after the preview requires a
refresh. Apply requires one available active controller, one active display per
Mac, and a connected layout without overlap. Configuration hashes reject edits
made since the window loaded.
Each host saves `~/.config/slab/displays/deskflow-seat-UUID.json` before writing.
**Restore Previous** restores the preceding configuration if it has not changed
again. A failed transaction attempts to restore earlier writes and reports any
host requiring manual recovery. Per-host backups survive those failures.

Slab navigation reads every ranged link and the physical tile centers. MacPal
and Slab use the generated Control–Option–Shift–F13…F20 shortcuts for direct
screen routing, so controller handoff does not depend on the old fixed grid.
Update Slab on the fleet and MacPal on controller-capable Macs before applying.
Without a saved native layout, existing routing remains the fallback.

The current editor supports up to eight configured Deskflow machines. Local
multi-monitor resolution/geometry remains available through the CLI above.
Native editor development mode: `slab-menubar --display-layout`.

## Agent tools

The existing fleet MCP exposes `fleet_displays`, `fleet_display_identify`,
`fleet_display_modes`, `fleet_display_layout`, and `fleet_display_restore`.
Layout and restore default to previews. A layout apply can pass the `expected`
array from its preview to reject intervening geometry changes.

```sh
node --test toolchain/fleet/display-model.test.mjs toolchain/fleet/deskflow-seat-model.test.mjs
```
