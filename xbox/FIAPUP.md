# fiapup

A cosy puppy simulator for the Xbox. You are a hand over a small backyard,
and the pup is the star. Written 2026-09-28 as a first pass. It has one pup,
one yard and eight behaviours, and it hasn't been on the console yet.

```
npm run fiapup:play     # http://127.0.0.1:8124 — keyboard or an Xbox pad
npm run fiapup:test     # behaviour, rig and budget tests
npm run fiapup:shots    # headless screenshots → xbox/fiapup/shots/
```

## The game

**Controls.** The game is built for the controller first. On the web the
keys map onto the same pad.

| pad | keys | does |
|---|---|---|
| left stick / d-pad | WASD / arrows | move the hand over the lawn |
| A | space | pet (hold it over the pup) · pick up the ball or the rope · throw · let go |
| B | C | call: a whistle, and the pup comes (it also wakes a napping pup) |
| X (hold) | T | hold up a treat; let go to drop it |
| Y | R | play: a play bow, then zoomies |

**What the pup does.** Each state is a small `tick` and a `pose`, in
`states` in `fiapup.js`.

| state | how it starts | what you see |
|---|---|---|
| idle | nothing else is going on | watches the hand, sits after a moment, breathes, blinks, wags by joy; sometimes wanders off to sniff |
| sniff | now and then, from idle | trots to a spot nose-down and snuffles, with small head twitches |
| come | B | runs to the hand with its ears bouncing, then sits |
| fetch | you throw the ball | chases (leading the ball), catches it in its mouth, carries it back, drops it at the hand and yips |
| petted → rollover | hold A over the pup | leans into the hand with eyes shut and a fast tail; keep petting and it rolls belly-up with paddling paws and hearts; let go and it rolls back |
| beg → eat | hold X | comes under the treat and sits up with front paws raised, whining now and then; drop it and it chomps it |
| sleepy → nap → stretch | energy runs low | yawns, walks to its bed and curls up (Zzz, snores) until it's rested or called |
| playbow → zoomies → flop | Y, or now and then when it's very happy and full of energy | bows and barks, laps the yard at a gallop with its ears streaming and tongue out, then flops down panting |
| tug → parade | pick up the rope near it | grabs the far end, backs off to keep the rope taut and growls with head shakes; drag it with the stick; let go and it parades the rope, then drops it |

**Mood.** Joy and energy are the two bars at the top left. Joy speeds and
widens the wag. Energy drains faster the faster the pup moves, and running
out of it sends the pup to bed.

**Sound.** Every sound is a one-shot `synth(freq, dur)`: a bark, a yip, the
whistle, a squeak, a growl, a chomp, a snore, a whine and a yawn. Every host
has `synth`.

## How it's built (reusing oskiewar's pipeline, without touching oskiewar.js)

- **One global script, `xbox/fiapup/fiapup.js`, with lifecycle functions.**
  It has `boot / sim / paint / act / leave` and the same shape as
  oskiewar.js. Input is `gamepad(0)`, with the console's button names.
  `sim` runs a fixed 60 Hz clock, whatever the host's paint rate.
- **Objects.** The pup, the yard, the ball, the rope, the hand and the treat
  are KidLisp objects in the flat dialect (`xbox/fiapup/objects/*.lisp`),
  compiled by `xbox/live/object-lisp.mjs`.
  - The pup is balls and limbs hung on a skeleton. Every shape bakes, and
    only the bone frames move: body (bob, pitch about the hips, roll), four
    legs (swing and splay), head (yaw, nod, cock), ears, tail, and `if`
    branches for eyes shut, tongue and mouth.
  - The pose reaches the lisp through `(owner part x|y|z)`.
  - The yard is one sketch.
  - The rope is the one object on the per-tick flat path: CAPSULE + ELLIPSE.
- **Drawing.** Every paint is one frame program. That is WIPE, CAMERA,
  SHAPES (once per sketch), SKETCH per part, a few FACE/DISC/TEXT for the
  HUD and hearts, and CAPSULE/ELLIPSE/OUTLINE for the rope.
  - A host with `frame` runs the program itself.
  - Any other host runs it with the copy of `xbox/live/frame-vm.mjs` sealed
    into the script. That covers the console until R6 and fiapup's own web
    shell. On the console the faces go out in one `triangles3d` call.
  - So the web and the Xbox run the same path. When R6's `FrameVm.cpp`
    lands, the console takes it over with no game change.
- **Seal.** `xbox/fiapup/embed.mjs` seals the compiler, the frame VM and the
  objects between `// <sealed>` markers, the way `embed-objects.mjs` does
  for oskiewar. `--check` fails on drift, and so does a test.
- **Web shell.** `xbox/fiapup/index.html` gives the script the console's
  bindings in a browser. `triangle3d` goes through oskiewar's
  `scene3d-webgl.mjs` (depth-tested WebGL2); `write` draws on a text canvas;
  `synth` uses WebAudio; `gamepad` reads the keyboard or the Gamepad API.
  `?stage=fetch|pet|beg|nap|zoomies|tug[&seconds=n][&pause]` opens on a
  staged moment.
- **Where it lives.** `xbox/fiapup/serve.mjs` serves `xbox/fiapup` plus the
  two scene modules. fiapup does **not** live under `xbox/live`, because
  lith's catch-all serves that whole directory on oskiewar.com, and the next
  oskiewar deploy would publish fiapup there.

**HUD.** Panels are FACE triangles at the nearest depth, not `box`, because
the console draws `box` in a CPU layer under the scene.

### Budget (measured by `tests/fiapup.test.mjs`)

| | |
|---|---|
| the pup, per tick | ≤ 12 SKETCH = **≤ 168 numbers**, in every behaviour; nothing projected in JS |
| the whole frame program, steady | **469 numbers** (the first paint is 6074, including every SHAPES) |
| host faces per frame | ~900 (the yard is about half of that) |
| JS tick + paint | 0.11 ms median in Node on this Mac; tripwire at 4 ms |

The pup is a figure, not a prop, so it gets more than a prop's 60 numbers.
Its 168 compares with ~1700 screen-space ops a frame for oskiewar's
figures. OBJECT-DIALECT's option (c), a joint-slot sketch, would bring it to
about 40–50 numbers, but that needs a new op. Here each bone is one SKETCH.

## Getting it onto the Xbox

**The question:** what is the cheapest honest path for a second game on the
same console?

**How the console runs JS today.** The installed package,
`AestheticComputer.NativeBios` (1.0.522), boots the `oskiewar.js` bundled in
it. In dev builds (`AC_DEV_LIVE_PIECE`) it also polls
`LocalState/live-piece.js` every 500 ms and, when it changes, stages and
activates it, rolling back to the last good piece on a throw. `live.mjs
publish` writes that file through Device Portal. That is the whole "live
JS" lane, and oskiewar's releases ride it.

Two properties matter:

1. **The file persists.** The signature starts at 0, so a fresh launch
   loads whatever is there.
2. **There is one slot per package.**

### Option A: borrow the installed app with a live JS push (recommended now)

Publish `fiapup.js` as the package's `live-piece.js`, then launch.

- **Cost:**
  - No native work, no AppVeyor build and no install.
  - fiapup.js is 137 KB, under the 2 MiB limit.
  - It uses only bindings the host already has: `triangles3d`/`triangle3d`,
    `write`, `wipe`, `synth`, `gamepad`, `runtime`.
  - A throw rolls back to the previous piece rather than crashing.
- **The honest price:**
  - While fiapup is there, the console is not running oskiewar. The tile
    still says "oskiewar", and it keeps opening fiapup until the script is
    replaced.
  - oskiewar's release receipt would still claim its Xbox channel is
    `current`. So the tool marks that channel `pending` while fiapup is
    borrowed, and a later `npm run oskiewar:reconcile` would put oskiewar
    back.
- **The tool:** `xbox/fiapup/xbox.mjs` is prepared and has **not been run**.
  - `plan` touches nothing.
  - `borrow --yes` downloads the console's current `live-piece.js` to
    `.git/fiapup-borrowed-live-piece.js`, marks the receipt, then runs
    `live.mjs publish xbox/fiapup/fiapup.js` and `live.mjs launch`.
  - `restore --yes` publishes the saved bytes again.
  - The console's copy of oskiewar has the QR encoder bundled in front, so
    its hash never equals the receipt's. After a restore the channel stays
    `pending` until `npm run oskiewar:reconcile` settles it. That is honest
    rather than tidy.

### Option B: a second package identity (when fiapup should have its own tile)

Build the same NativeBios host a second time as its own package.

- **Exact changes:**
  1. A second manifest, e.g. `xbox/native-bios/fiapup/Package.appxmanifest`,
     with Identity `AestheticComputer.Fiapup`, a new `PhoneProductId`,
     DisplayName `fiapup`, and its own tiles and splash.
  2. `NativeBios.vcxproj` switches on a property (`/p:AcGame=fiapup`): the
     manifest, and which script is packaged. Today it links
     `..\live\oskiewar.js` as `oskiewar.js`.
  3. `App.cpp:560` reads a package file named by a define
     (`AC_PACKAGED_GAME`) rather than `L"oskiewar.js"`.
  4. `appveyor.yml` gets a second UWP `msbuild` with `/p:AcGame=fiapup`,
     plus an artifact. The UWP step is ~86 s of build 500's 4m49s. Add
     `xbox/fiapup/fiapup.js` to `only_commits` only if the packaged copy
     should follow it.
  5. `xbox/tools/live.mjs`:
     - It hard-codes the family `AestheticComputer.NativeBios`.
     - **`prune()` deletes every other `AestheticComputer.*` package.** An
       oskiewar install would uninstall fiapup.
     - The family has to become a parameter, and prune has to narrow to
       stale revisions of the same family.
     - This changes an oskiewar tool's behaviour, so it wants @jeffrey.
  6. `oskiewar-release.mjs` classifies any `xbox/native-bios/` change as
     `xbox-native`. So a fiapup native change would mark oskiewar's next
     release native too, unless the classifier learns about fiapup's
     manifest.
- **Cost:** about a day of native and tooling edits in shared code, plus an
  AppVeyor package per native release and a sideload install. After that,
  fiapup's live pushes go to its own LocalState and never touch oskiewar.
- **Status:** none of this is done, and AppVeyor was not triggered.

**Not recommended:** a launcher or switch *inside* oskiewar.js, which changes
oskiewar's production behaviour. A native launcher menu costs everything B
costs and more.

**Recommendation.** Use **A** to play it on the console this week: minutes,
reversible, with no native build. Do **B** only once fiapup is worth a tile
of its own, and do it together with the `live.mjs` prune fix.

**Exact next step:**

```
node xbox/fiapup/xbox.mjs plan              # on blueberry (the Device Portal vault is there)
node xbox/fiapup/xbox.mjs borrow --yes      # fiapup on the console
node xbox/fiapup/xbox.mjs restore --yes     # oskiewar back (then npm run oskiewar:reconcile)
```

## Screenshots (`xbox/fiapup/shots/`, headless Chrome, SwiftShader WebGL)

- **idle.png:** the pup stands mid-lawn, three-quarter view, looking at the
  gloved hand. You can see the bed back left, the bowl back right, the ball
  and the rope on the grass, the fence, hedge and tree, and the HUD
  ("hangin' out").
- **fetch.png:** 1 s after a throw. The pup is small near the back fence,
  seen from behind as it runs toward the fence. The ball isn't clearly
  visible in the shot.
- **pet.png:** the pup sits leaning into the hand on its head, eyes shut
  ("good pup").
- **rollover.png:** belly up with paws in the air, the head upside down, a
  heart rising. It is partly under the hand and reads as a tumble more than
  a clean pose.
- **beg.png:** the pup sits bolt upright, front paws up, under the fist
  holding the treat. The fist covers most of the face.
- **nap.png:** curled in the bed back left, with a small "z". The camera has
  pulled wide because the hand is far off, so a side fence runs across the
  foreground.
- **zoomies.png:** mid-gallop, small in frame, ears out.
- **tug.png:** the pup facing the hand with the rope in its mouth. The rope
  is mostly hidden behind the glove.

## Open questions for @jeffrey

1. **A or B?** Is borrowing the oskiewar tile acceptable for a first look,
   or should fiapup wait for its own package?
2. **The name on the tile.** Under A the console tile still says
   "oskiewar". Under B, is the name "fiapup", and whose pals art goes on the
   tile?
3. **Who you are.** Right now you are a hand. Is that right, or should the
   pad drive the pup directly?
4. **The camera.** It sits above and behind (28°). Should it be closer, or
   orbitable on the right stick?
5. **Look.** The hand covers the pup's face in the beg and tug shots. Should
   the hand go translucent, shrink near the pup, or sit off to the side?
6. **Promote the objects?** Should `puppy-flat.lisp` join the object lab
   (`xbox/live/objects/`)? That would put it on oskiewar.com's file server.
   The lab would also need a pose panel for the owner parts.
