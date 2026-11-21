# Blueberry Wallpaper

A lightweight live desktop for the fleet machine `blueberry`, using the exact
Meshy v6 AC Pals model recovered for the Xbox native app:
`xbox/assets/pals-mesh-nat-amethyst.glb`.

The app bundles the original GLB and a SceneKit-readable USD conversion of the
same mesh. An `SCNRenderer` draws that geometry into a Metal layer the app owns,
one frame per display-link tick: a whole number of refreshes per frame, never
under 60 fps (144 Hz → 72, 120 → 60). Varied-size Pals marks rotate
independently while buoyant acceleration and a gentle sway carry them upward.
Every instance uses shared live mesh data rather than a bitmap sprite. One
imported prototype and its geometry/materials are shared across displays and
appearance rebuilds. Imported texture maps are discarded; the field uses a
single opaque accent material.

Antialiasing is full-scene: 4× MSAA everywhere, and on 1× displays the scene
renders at twice the backing resolution and is averaged down. The multisample
targets are memoryless, so they cost no RAM. `--supersample <1–3>` and
`--msaa <1|2|4>` (or `$BLUEBERRY_WALLPAPER_SUPERSAMPLE`, `$BLUEBERRY_WALLPAPER_MSAA`)
override. stdout carries a frame report — fps, worst frame, late frames, render
size — ten seconds after launch, at a minute, then every ten.

With a fishery feed present, the dark backdrop is morphing water: a fragment
shader on one plane behind the stage walks the hue round the wheel every five
minutes while slow currents bend it across the screen. The haze, the marks'
tint and the menu bar's backing follow about once a second. The lighting
environment stays fixed in that mode — SceneKit refilters it on every change,
which drops a frame.

Keep Core Animation shadows off anything that moves over the field. A layer
shadow without a shadow path is re-blurred on every composite; the fishery's
labels alone held a 144 Hz display to 36 fps. `HaloTextLayer` draws the halo
into the text's own bitmap instead.

The accent-shaded model floats over a blueberry field that
responds to macOS appearance. The window sits beneath Finder's desktop icons,
ignores input, joins every Space, and pauses while the display sleeps.

Appearance changes and display wake recreate the desktop windows after a
2.5-second settling period. WindowServer can retain a light menu-bar gradient
over a dark field even when both macOS and Slab report Dark; resuming animation
or restarting Dock alone does not repair that state. Verify the composed menu
bar as well as the wallpaper window after either transition.

Until the Xbox recovery commit lands in the main checkout, `build.sh` also finds
the GLB in `.worktrees/xbox-native-v11/`.

```bash
./toolchain/macos/blueberry-wallpaper/build.sh
./toolchain/macos/blueberry-wallpaper/remote-install.sh blueberry
```

The launch agent is `computer.aesthetic.blueberry-wallpaper`. Logs land in
`~/.local/share/blueberry-wallpaper/` on Blueberry. Reinstalling moves the prior
app bundle to the Trash before replacing it.

## Fuser minis: fishery, one push, one tank

On panda and chicken the field carries a live fishery (`Sources/Fishery.swift`)
fed by the vault's `fuser-fishery` agent. All of its type is MatrixChunky8
(`Resources/chunky8.ttf`, baked from `rax/chunky-font.mjs` by
`bake-chunky8.py`) at whole multiples of 8 device pixels, with GNU Unifont
(`Resources/unifont.otf`, 17.0.03) behind it for symbols, no antialiasing and
a hard one-pixel shadow. rax is a small isometric model in the bottom left corner.

```bash
./push-minis.sh               # compile + swap on panda and chicken at once
./push-minis.sh --feeder      # also ship the vault feeder
./push-minis.sh --rollback    # back to the previous Sources and binary
./tank-install.sh panda chicken   # one tank: panda left, chicken right
```

A tank (`Sources/Tank.swift`) makes each screen a pane of one wider world (the
hour chart stays whole on the last screen). The
leader simulates every fish and streams them over loopback; a reverse SSH tunnel
from the leader carries that port to each follower, so the app never listens
off-loopback and neither firewall is involved. Followers draw 0.18 s behind,
interpolated, and fall back to their own feed when the stream stops.

## aesthetic.computer: walkers

`toolchain/analytics/ac-fishery-wallpaper.mjs` feeds the same layer from Lith
(signed-in handles from `account-activity`, anonymous interacted visits from
`network-visits`, an hour of visits per site, 24 h of referrers) and asks for
`school: "walkers"`: everyone is drawn as one of rax's people (shirt in a rax
seat colour, white head, purple brain cap, legs stepping with the ground they
cover) instead of a fish. Feeder categories may bring their own `glyph`,
`color` and `chart`. Agent: `computer.aesthetic.ac-fishery.plist`.

## rax from the air

The bottom-left Rax globe rotates once every 48 seconds. East and west
follow longitude. North and south span the map from pole to pole. The
courtyard, tower, open blocks, builds and live visitors sit on the surface.

The model is drawn from rax's own map and from nothing else:

    rax server  GET /rax/map (rax/map.mjs)
      → feeder   ac-fishery-wallpaper.mjs passes it through as `world`,
                 and again as world.json whenever it changes
      → FlyBy    draws the shapes it is given

`rax/map.mjs` works every shape out from the measurements the server walks
people against and the page draws from, and `rax/test/map.test.mjs` fails
when a solid is missing from it. The feeder imports nothing from `rax/` and
FlyBy holds none of rax's measurements, so neither can fall out of step.
A shape is a `flat`, `box`, `stack` or `panel` with a `role`; FlyBy colours
the roles it knows and draws any other in plain ink. To put something new on
the wallpaper, add its shapes to `rax/map.mjs` and deploy rax.

The map feed supplies the wrap distance and the planet's radius. Heights
are compressed as they rise to keep the roof and airborne visitors in
the frame. The top slab remains visible when storeys get too close to draw
individually.

The fast lane is two files. world.json is the map, asked for every five
seconds and looked for twice a second. people.json is only who is where
(`GET /rax/people`), asked for five times a second while anyone is in rax
and looked for ten times a second: each walker slides to where they now are
over the time between two readings, and leaves a line along the ground where
they have been for the last minute and a half. The tower's yard is on the
map as one faint green square, so someone far out in it is still in the
model. The
swing starts over only when what stands for good changes, or the rooms gain
their first thing or lose their last.

## Infra constellation

Top left, in the strip the walkers keep out of: every public host as a star
fanned round lith, and every tailnet machine as a dot round this Mac.
`toolchain/analytics/ac-infra-probe.mjs` runs beside each feeder tick and rides
the feed as `infra`:

    { observedAt, medianMs, uplink: ok|slow,
      nodes:   [{ id, parent, kind, state: up|slow|late|down, ms, coldMs, status, note }],
      tailnet: { state, self: { name, online },
                 peers: [{ name, os, online, lastSeen, direct, node }] } }

Per host it makes one HTTPS GET (up or not, by the doctor's rule) and one HEAD
down the same warm connection (`ms`, the round trip). No SSH. `late` is one
lost request, `down` two in a row; `slow` is over 600 ms and twice the median.
Tailnet names come from the local `tailscale status --json` at run time; a
peer whose name carries a host's id rides on that host's star. When Lith's
SSH call fails the last snapshot is rewritten with the fresh probe, so the
outage shows.

Up twinkles, slow and late are amber, down is red and blinks, away machines
are hollow with how long ago. A pink dot runs each link when a probe answers,
taking as long as the answer did. `⚒` beside the oven means it is baking.
Peers past what the strip holds (two columns of rows) are dropped, longest
away first.
