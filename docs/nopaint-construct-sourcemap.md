# No Paint classic: source map and editing strategy for the compiled build

Draft, 2026-11-17. Read-only investigation; nothing in the repo was changed.
Every mapping below was read out of the files named. Lines marked **GUESS**
were not verified.

Paths are relative to the repo root unless they start with `~`.

## 1. What is where

| Thing | Location | Notes |
| --- | --- | --- |
| Served build (nopaint.art `/` and `/classic/`) | `system/public/nopaint.art/` | Core repo, not the vault. The 2021-08-04 web release. |
| Integrity manifest | `toolchain/nopaint/classic-export.json` | SHA-256 of 195 export files; `index.html` is excluded by policy. |
| Routing test | `lith/nopaint-routing.test.mjs` | Serves the folder through Caddy and compares every file to the manifest. |
| Decoders | `toolchain/nopaint/decode-sheet.mjs`, `toolchain/nopaint/expressions.mjs` | Print an event sheet / the expression table. |
| Editable Construct source | `nopaint/construct/` (symlinks into `aesthetic-computer-vault/products/nopaint/nopaint/construct/`) | Unpacked `.c3p`, resaved by Construct r495.2 (`savedWithRelease: 49502`). |
| Original `.c3p` | `~/Documents/nopaint-recovery/No Paint.c3p` | `savedWithRelease: 26000` (r260). Keep immutable. |
| r495 re-export (not served) | `~/Documents/nopaint-recovery/build/` | Newer export layout: `c3main.js`, `objRefTable.js`, `sw.js`, `offline.json`. |
| Source tools | `nopaint/tools/construct-serve.py`, `nopaint/tools/construct-patch-export.py` | Zip source to a `.c3p`; unpack + patch an editor export. |
| Save backend | `system/netlify/functions/nopaint-classic.mjs`; Caddy block `server.nopaint.art` in `lith/Caddyfile` | `get-code`, `upload-painting`, `painting/<code>.png`. |
| Host routes | `lith/Caddyfile`, block `nopaint.art {` | Root of the site is the build folder; no `Cache-Control` is set. |

State of the served build on 2026-11-17:

- `data.json` is byte-identical to the manifest and to production.
- (Resolved 2026-11-18: the manifest now records the patched hash as
  `served`; see section 7.) `scripts/c3runtime.js` is **not** identical to the manifest. Commit
  `762637e3e1` (2026-11-04) changed one string in it by hand
  (`https://whistlegraph.com` → `https://whistlegraph.org`). Production serves
  the patched file. The manifest was not updated, so the routing test's hash
  check for that file should fail. **GUESS** (the test was not run).
- `index.html` is maintained by hand: visit tracker, the notepat.com sticker,
  the teacher chip and dialog. That is the only "wrapper" pattern used so far.

## 2. Anatomy of the build

- Runtime: Construct 3 `c3runtime`, 2021 export layout. One engine file,
  `scripts/c3runtime.js` (1.2 MB, 2,448 lines). Simple minification: local
  names are short, but API names survive (`_GetNode`, `GetBoundMethod`,
  `C3.Plugins.Sprite.Acts.SetAngle`, object and variable names).
- Worker mode is on. `scripts/main.js` ends with
  `new self.RuntimeInterface({useWorker:!0, workerMainUrl:"workermain.js",
  engineScripts:["scripts/c3runtime.js"], projectScripts:[],
  mainProjectScript:"", scriptFolder:"scripts/",
  workerDependencyScripts:["box2d.wasm.js"], exportType:"html5"})`.
  When the browser has OffscreenCanvas, the runtime lives in a Web Worker.
  Page JavaScript cannot reach `runtime.objects` or `globalVars` directly.
- Project scripts are compiled into the tail of `c3runtime.js`
  (`runOnStartup(async f=>{f.addEventListener("beforeprojectstart", ...)})`,
  then `layoutCheck$$module$str$main`, then the `ScriptsInEvents` object,
  e.g. `Core_Event565_Act1`). `projectScripts` is empty.
- No service worker. `index.html` does not load `scripts/register-sw.js`, and
  the folder has no `sw.js` or `offline.json`. (`/sw.js` on this host is
  Aesthetic Computer's file, routed by Caddy; the classic page does not
  register it.) There is no offline cache version to bump.
- Game data: `data.json` = `{"project": [ ...42 slots... ]}`, nested arrays,
  no keys. `json.dumps(obj, separators=(",",":"))` in Python reproduces the
  file byte for byte, so a load / edit / dump cycle is safe.

### `data.json` → `project[n]`

| Index | Content (verified) |
| --- | --- |
| 0 | Project name `"No Paint"` |
| 1 | First layout name `"Splash"` |
| 2 | Plugin table, 24 rows: `[objectRefIndex, flags…]` |
| 3 | Object types, 206 rows |
| 4 | Families: `[[205, 37]]` (family type index, member type index) |
| 5 | Layouts, 82 rows |
| 6 | Event sheets, 45 rows |
| 7 | Audio files, 130 rows |
| 8 | Audio folder `"media/"` |
| 10, 11 | Viewport 256 × 352 |
| 14 | Sampling `"nearest"` |
| 16 | Project version `"1.0.0"` |
| 20 | Orientation (1 = portrait) |
| 28 | Icons folder `"icons/"` |
| 29 | Fonts: `[name, file, bytes]` × 3 |
| 31 | Project unique id |
| 32 | Fonts folder `"fonts/"` |
| 38 | App id `"art.nopaint"` |
| 39 | Loading logo file |

The runtime's `_LoadDataJson` confirms slots 0, 1, 2, 3, 4, 5, 6, 9, 10, 16,
20, 27 (containers), 31, 33 (timelines), 35 (transitions), 38, 39. Other
slots: **GUESS**.

### Object type row, `project[3][i]`

`[name, pluginRefIndex, isFamily?, instanceVars, behaviorCount?, ?, singleImage, animations, behaviors, isGlobal?, ?, SID, effects, ?, jsPropIndex, ?]`

- `[0]` name, `[11]` SID: verified against `nopaint/construct/objectTypes/*.json`
  (`Painting` → SID `853010152845463` in both).
- `[1]` indexes `C3_GetObjectRefTable()`: 0 = DrawingCanvas, 1 = Sprite,
  3 = Array, 6 = Particles, 7 = TiledBg, 25 = AJAX, 27 = Spritefont2.
- `[3]` instance variables: `[SID, type, name, jsPropIndex]`.
- `[6]` single image (TiledBg, Spritefont2): `[file, bytes, x, y, w, h, …]`.
- `[7]` animations: `[name, speed, loop, repeatCount?, repeatTo?, pingPong?, SID, frames]`.
- Frame: `[sheetFile, bytes, x, y, w, h, duration, originX, originY, imagePoints, collisionPoly, pixelFormat]`.
  `x, y, w, h` is the rectangle inside the sheet. `bytes` does not match the
  file on disk (`stamp-sheet1.png` says 92053, the file is 55804) and the game
  runs, so that field is not enforced.
- `[8]` behaviors: `[name, behaviorRefIndex, SID, jsPropIndex]`.
- Other positions: **GUESS**.

The index `i` of a type is what layouts and events use to refer to it.

### Layout row, `project[5][i]`

`[name, width, height, unboundedScrolling, eventSheetName, SID, layers, nonWorldInstances, effects]`

Layer: `[name, index, SID, visible, bgColor, transparent, parallaxX, parallaxY, opacity?, forceOwnTexture?, useRenderCells?, scaleRate?, blend?, zElevation?, instances, effects, …]`
— name, index, SID and `[14]` instances verified; the rest **GUESS** by
position against the source keys.

Instance: `[world, objectTypeIndex, UID, instanceVarValues, behaviorProps, pluginProps]`

- `world`: `[x, y, z, width, height, depth?, angle, [r,g,b,a], originX, originY, blend?, ?, effects, …]`
  — x, y, width, height, colour and origin verified against `layouts/Index.json`.
- UID equals the source's `"uid"` (Index layout, `DesktopNoPaintBackdrop`,
  UID 424 in both). The instance SID in the source is not exported.
- `pluginProps` for a sprite font starts with the text:
  `["Your Ad Here", true, 8, 8, "<character set>", …]`.
  For a sprite: `[initiallyVisible, initialAnimation, initialFrame, collisions]`.

### Event sheet row, `project[6][i]` = `[name, nodes]`

Node types (first element):

| Type | Shape | Source `eventType` |
| --- | --- | --- |
| 0 | `[0, ?, false, null, SID, displayNumber, conditions, actions, children]` | `block` |
| 1 | `[1, name, type, initialValue, isStatic, isConstant, SID, false, jsPropIndex]` | `variable` (type 0 number, 1 string, 2 boolean) |
| 2 | `[2, sheetName, false]` | `include` |
| 3 | `[3, [activeOnStart, groupName], false, null, SID, n, conditions, actions, children]` | `group` |
| 4 | `[4, [functionName, returnType?, params, …], false, null, SID, n, conditions, actions, children]` | `function-block` |

- Variable SIDs, function-block SIDs and action SIDs equal the source's
  (`DesktopMode` 817667698670587; `UploadPainting` 784578080760308; the
  upload action 653194526514032).
- A variable at the top level of a sheet is a global. There are 74: Core 44,
  Index 10, Buttons 8, Splash 5, Stamp 3, Playlist 3,
  PainterStoriesTransition 1. Nested ones are locals.
- Condition: `[objectTypeIndex, aceRefIndex, behaviorName, …flags, SID (index 7), ?, params (index 9)]`.
- Action: `[objectTypeIndex, aceRefIndex, behaviorName, SID, flags, null, params]`.
  `-1` is System. `[-2, "FunctionName", null, SID, …]` is a function call.
- `aceRefIndex` indexes `C3_GetObjectRefTable()` in `c3runtime.js`
  (409 entries: plugins, behaviors, then every condition, action and
  expression the project uses).
- Parameter: `[paramType, value]`. For an expression parameter the value is
  `[expressionNumber, ...nodes]`; the code is
  `C3_ExpressionFuncs[expressionNumber]` in `c3runtime.js` (1,193 entries).
  Seen node forms: `[3, variableSID]`, `[5, "GetParameter"]`. Seen parameter
  types: 2 = audio file `[name, looping]`, 10 = instance-variable index,
  11 = event-variable SID, 17 = function name.

**No number or string that an event uses is stored in `data.json`.** It is in
the expression table. Layout text, variable initial values, positions and
asset names are in `data.json`.

### Audio row, `project[7][i]`

`[name, [[mime, ".webm", bytes]], isMusic]`. File: `media/<name>.webm`.
The 130 names match the 130 files exactly. `isMusic` is true for
`no paint - theme` and `no paint - painting music`.

### Images

32 sheets in `images/`, all referenced from `project[3]` and nowhere else.
The 1,197 source images (`nopaint/construct/images/`) were packed into these
sheets by the editor; a frame is a rectangle in a sheet.

## 3. Concept → location

| Concept | File | How to find it |
| --- | --- | --- |
| Layout | `data.json` `project[5]` | by name `[0]` or SID `[5]` |
| Layer | layout `[6]` | by name `[0]` or SID `[2]` |
| Instance on a layout | layer `[14]` | by UID `[2]`; type = `project[3][inst[1]][0]` |
| Object type | `data.json` `project[3]` | by name `[0]` or SID `[11]` |
| Animation / frame | object type `[7]` → frames `[7]` | animation name, then frame order |
| Event sheet | `data.json` `project[6]` | by name |
| Group | type-3 node | by `[1][1]` name or SID `[4]` |
| Function | type-4 node | by `[1][0]` name or SID `[4]` |
| Global / local variable | type-1 node | by name `[1]` or SID `[6]`; initial value `[3]` |
| Text on a layout (sprite font) | instance `pluginProps[0]` | layout name + UID |
| Text built by events (help, profiles, URLs) | `c3runtime.js` `C3_ExpressionFuncs` | `node toolchain/nopaint/expressions.mjs --grep <text>` |
| Number used by an event | `C3_ExpressionFuncs` | `node toolchain/nopaint/decode-sheet.mjs <Sheet>` |
| Sound | `project[7]` + `media/<name>.webm` | by name |
| Image | `images/*-sheetN.png` + frame rectangle | object type → animation → frame |
| Font | `project[29]` + `fonts/` | by name |
| Project JavaScript | tail of `c3runtime.js` | `$$module$str$main`, `ScriptsInEvents` |
| Page title, meta, sticker, teacher form, tracker | `index.html` | plain HTML |
| Playlists | `Frog Dies.txt`, `Gift.txt`, `Landscape.txt`, `Polka Dots.txt`, `Smiley.txt`, `Well.txt` | plain text at the build root |

### The save / upload call (verified)

| Step | Sheet → function | Action | Expression |
| --- | --- | --- | --- |
| Reserve a code | Core → `MaybeGetCode` (SID 324895954782445) | `AJAX.Request`, action SID 777790974072313 | #726 `"https://server.nopaint.art/get-code"` |
| Upload | Core → `UploadPainting` (SID 784578080760308) | `AJAX.Post`, action SID 653194526514032, params tag #729, url #730, data #731, method #732 | #730 `"https://server.nopaint.art/upload-painting"` |
| Load by code | Load sheet, block SID 688133908767763 | `LoadSprite` → `Sprite.LoadURL`, action SID 987190512269628 | #476 `"https://server.nopaint.art/painting/" + param + ".png"` |

The source spells the upload body as
`"code=" & LoadCode & "&painting=" & PaintingShare.GetBase64`, method `"POST"`
(`nopaint/construct/eventSheets/Common/Core.json`).

Other outbound links, all in the expression table: #885 Notion manual,
#1172 Discord, #1173 whistlegraph.org (Splash → function `Whistlegraph`,
`Browser.GoToURLWindow`), #1175 Tally form, #1176 App Store.

## 4. Finding a thing again after a re-export

Array offsets move. These do not:

1. **Names**: layout, layer, object type, event sheet, group, function,
   variable, animation, sound.
2. **SIDs**: object types, layouts, layers, variables, blocks, functions,
   actions, conditions, behaviors, animations. The same number is in the
   source JSON under `"sid"`.
3. **UIDs** for placed instances (source `"uid"`).
4. **Literal text** for expressions. Expression numbers are assigned by the
   exporter and will change; match on the function body
   (`()=>"https://server.nopaint.art/upload-painting"`) and require exactly
   one hit, as `construct-patch-export.py` already does.

Do not store `project[6][39][1][46]…` paths or expression numbers in a patch.
Resolve them at patch time from a name, SID, UID or literal.

The r495 export has a different file layout (`c3main.js`, `objRefTable.js`).
The `data.json` schema of that build was not inspected, so the row shapes
above apply to the 2021 build only.

## 5. What can change without the editor

| Change | Feasible | How |
| --- | --- | --- |
| Page title, meta, analytics, DOM overlays | Yes, in use now | Edit `index.html` |
| Text placed on a layout | Yes | `data.json`, instance `pluginProps[0]`. Characters must be in the sprite font's set. |
| Variable initial value | Yes | `data.json`, type-1 node `[3]` |
| Instance position, size, colour, visibility, initial frame | Yes | `data.json` instance `world` / `pluginProps` |
| Remove an instance | Yes, with care | Delete the row; events that pick it then act on nothing |
| Constant or string in an event (URL, help text, timing) | Yes | Edit the function in `C3_ExpressionFuncs` (done once, 762637e3e1). One expression number can be shared by several events; check before editing. |
| Swap a sound | Yes | Replace `media/<name>.webm` (WebM Opus), same name |
| Swap an image, same size | Yes | Repaint the frame rectangle inside the sheet PNG |
| Stamp of a new size, new animation frame | Hard | Needs free sheet space and new frame rows; collision polygon and origin are per frame. **GUESS**: an extra sheet file can be referenced from new frame rows. |
| Disable an action or a whole event | Yes | Delete the action / block row, or turn a group's `activeOnStart` off |
| New event logic with existing actions | Possible, laborious | Hand-write block rows; add functions to `C3_ExpressionFuncs`; an ACE not already used needs a new entry appended to `C3_GetObjectRefTable()`. **GUESS**: the plugin code for unused ACEs is still in the file because the export is not tree-shaken. |
| New behaviour in JavaScript | Yes | Append a `runOnStartup(runtime => …)` block to the end of `c3runtime.js`, or add a file to `engineScripts` in `main.js`. It runs in the worker with the scripting API present in this runtime: `runtime.objects.<Name>`, `runtime.globalVars`, `runtime.callFunction`, `runtime.getAllLayouts`, `addEventListener("tick" / "beforeprojectstart" / "afterprojectstart")`. The game's own scripts already do this in the worker (see section 8), so the API is proven; an appended block was not run. |
| Page ↔ game messages | Possible | Worker-side script plus a `BroadcastChannel` to the page. **GUESS**, not built. |
| New object type, plugin, behaviour or effect | No (practically) | Needs plugin table, ref table, prop-name table, type rows and runtime code. Use the editor. |
| Re-pack sprite sheets | No | Use the editor |

## 6. Worked example: change one text and one initial value

Goal: the Splash line `38 Brushes & ? Stamps` becomes `39 Brushes & ? Stamps`,
and the global `IndexHangTime` goes from `0.2` to `0.3`.

Facts used (verified): the text is the sprite-font instance with UID 398 on
layout `Splash`, layer `Layer 0`; each of `"38 Brushes & ? Stamps"` and
`[1,"IndexHangTime",` occurs once in `data.json`; `IndexHangTime` is a
top-level variable of sheet `Core`, SID 635115374915280.

```python
# run with: python3 -I patch.py <pristine-dir> <out-dir>
import json, shutil, sys
src, out = sys.argv[1], sys.argv[2]
shutil.rmtree(out, ignore_errors=True); shutil.copytree(src, out)
path = f"{out}/data.json"
raw = open(path, encoding="utf-8").read()
project = json.loads(raw)["project"]
assert json.dumps({"project": project}, separators=(",", ":")) == raw  # schema guard

# 1. text: layout name + UID
layout = next(l for l in project[5] if l[0] == "Splash")
hits = [i for layer in layout[6] for i in layer[14] if i[2] == 398]
assert len(hits) == 1 and hits[0][5][0] == "38 Brushes & ? Stamps"
hits[0][5][0] = "39 Brushes & ? Stamps"

# 2. variable: SID
core = next(s for s in project[6] if s[0] == "Core")[1]
var = [n for n in core if n[0] == 1 and n[6] == 635115374915280]
assert len(var) == 1 and var[0][1] == "IndexHangTime" and var[0][3] == 0.2
var[0][3] = 0.3

open(path, "w", encoding="utf-8").write(
    json.dumps({"project": project}, separators=(",", ":")))
```

Then:

1. Make the same edit in the source, so the next editor export keeps it:
   `nopaint/construct/layouts/Splash/…` (instance `"uid": 398`) and
   `nopaint/construct/eventSheets/Common/Core.json`
   (`"sid": 635115374915280`, `"initialValue"`). These are symlinks into the
   vault; edit the vault path.
2. Update the changed file's hash in `toolchain/nopaint/classic-export.json`
   and run `node --test lith/nopaint-routing.test.mjs`.
3. Cache. There is no service worker and no `offline.json` in this build.
   The host sends `ETag` and `Last-Modified` with no `Cache-Control`, so a
   browser may reuse `data.json` and `c3runtime.js` for a while by heuristic.
   `index.html` can carry `?v=` on its own script tags, but `c3runtime.js` is
   requested from inside `main.js` and `data.json` from inside the runtime.
   The clean fix is one Caddy line for the Construct files
   (`header Cache-Control "no-cache"`), which makes every load revalidate.
   **Not applied; a proposal.**
4. Deploy with the full `ac-deploy` (the build folder is in core), then
   compare the served bytes with `shasum`, not the status code.

If a future build does ship `sw.js` + `offline.json` (the r495 export does),
bump `"version"` in `offline.json` and list every changed file there, or
players keep the old cache. **GUESS** from the standard Construct layout; that
build's `offline.json` was not read.

## 7. The workflow (built 2026-11-18)

The served build is the 2021 export plus an ordered list of patches. Three
things keep that true.

1. **The patch list**: `toolchain/nopaint/patch-classic.mjs`, array `PATCHES`.
   A patch names its file and can be applied and taken away. With every patch
   taken away the file must hash to the manifest's `sha256` (the 2021 export).
   The patched file's hash is the manifest's `served`.
   - `npm run nopaint:classic:patch` applies the list and updates the manifest.
   - `npm run test:nopaint:classic` changes nothing and fails on drift.
   - Patch 1 is the 2026-11-04 `whistlegraph.org` edit. Patch 2 appends the
     bridge, `toolchain/nopaint/classic-bridge.js`.
2. **The bridge**: page ↔ game messages on the BroadcastChannel
   `nopaint-classic`. Ops: `state`, `prompt`, `call`, `set`, `goto`, `pixels`,
   `load`. Events: `ready`, `layout`, `painting`, `painted`. The file's header
   is the reference.
3. **The brush test**: `tests/browser/brushbook.e2e.mjs`. It runs every
   brush twice (Paint, then No) in headless Chrome and checks that Paint
   changes the painting and No does not. `npm run test:nopaint:brushes` runs
   it on a fleet Mac (`AC_CAPTURE_HOST`, default panda) and copies the results
   to `tmp/nopaint-brush-test/`. Add `Flip` to run one brush.

To make a change:

1. Add a patch to `PATCHES`: a `replace` of one literal that occurs once, an
   `append`, or a `json` patch on `data.json` that names its target by name,
   SID or UID.
2. `npm run nopaint:classic:patch`.
3. `npm run test:nopaint:brushes`, then `node --test lith/nopaint-routing.test.mjs`.
4. Make the same change in `nopaint/construct/` (edit the vault path) so a
   later editor export keeps it.
5. Deploy with the full `ac-deploy` and compare the served bytes.

The brushbook (`papers/arxiv-nopaint-brushbook/`) prints the test's pictures.
`npm run nopaint:brushbook` runs the test and regenerates its entries.

`data.json` patches (added 2026-11-18): `json("data.json", name, [ops])` with
`set`, `insert` and `remove`. An operation finds its row with a locator from
`at` (`at.layout`, `at.layer`, `at.instance`, `at.objectType`, `at.sheet`,
`at.variable`, `at.block`, `at.function`, `at.action`, `at.condition`) and
then steps to a position inside the row. It states what is there before it
runs, so it can be taken away. The worked example of section 6 is the first
test in `toolchain/nopaint/patch-classic.test.mjs`
(`node --test toolchain/nopaint/patch-classic.test.mjs`).

Not built yet: the Caddy `Cache-Control: no-cache` line for the Construct
files (section 6, step 3).

## 8. Hooks into the current world (added 2026-11-18)

Read from the files; nothing was run in a browser.

### The pixel seam is already in the build

The game's own event scripts read and write the canvases from inside the
worker. They are in the `ScriptsInEvents` object at the tail of
`scripts/c3runtime.js`:

- `Core_Event16_Act4`: `runtime.objects.Painting.getFirstInstance().getImagePixelData()`,
  changes the bytes, then `loadImagePixelData(imageData)`.
- `Flip_Event12_Act1`: the same pair on `PaintingBuffer`. It is `async` and
  the event waits for it.

So a script can take the painting (256 × 256 RGBA), `fetch` a service, and put
a new image into the proposal buffer. No new plugin, object type or ACE is
necessary. `IDrawingCanvasInstance`, `ISpriteInstance`, `IArrayInstance` and
`IStorage` are all present in this runtime.

### A transform is a small unit

`Flip` is one layout (256 × 256, 6 layers, same UI rows as `Invert` and
`Noise`) plus one event sheet that includes `Core`, reads its parameter, and
calls one script. A new proposal made the same way needs:

1. a copy of the `Flip` layout row with a new name and new SIDs / UIDs;
2. a copy of the `Flip` sheet row with its script action pointing at a new
   `ScriptsInEvents` function;
3. the name in the weighted picker and in the prompt's `layoutCheck` list
   (both in the expression table; not located yet);
4. the new function appended to `ScriptsInEvents`.

### Services that exist

| Need | Existing service | Notes |
| --- | --- | --- |
| AI move on a painting | `POST /api/nopaint-inference` (`system/netlify/functions/nopaint-inference.mjs`) | Same endpoint Note Paint uses. Default size 256, the classic canvas size. Without `Authorization` it takes the free-trial path: one turn per browser token, 100 a day in total (`system/backend/nopaint-trial.mjs`). With an AC token it bills Braincells. CORS is open. |
| Classic save | `server.nopaint.art` → `nopaint-classic.mjs`, collection `nopaint-classic-paintings` | 8-character codes, anonymous, write-once. Not connected to AC `#` codes. |
| AC `#` painting | `paintings` collection, `painting-code.mjs`, `/media/paintings/<code>.png` | Read by `painting`, `paintings`, profiles, `nopaint-piece.mjs` (painting → piece). |

The classic save is the one place where the two worlds do not meet. The
smallest join is on the server: when `upload-painting` makes a classic code
permanent, also record an AC `#` code for the same PNG. The compiled build
does not change for that.
