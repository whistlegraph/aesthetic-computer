# No Paint Construct button and cursor specification

This document is the implementation-oriented reconstruction of the Summer 2021
Construct export preserved at `system/public/nopaint.art/`. It describes the
serialized project, not a proposed redesign. All paths below are stable JSON
paths into `system/public/nopaint.art/data.json`; array indexes are zero-based.

## How to read the evidence

- `project[3]` is the object-type table.
- `project[5]` is the layout table.
- `project[6]` is the event-sheet table.
- Object `89` is the invisible generic `Button` hit target; object `90` is the
  custom `Cursor`; object `189` is `Audio`.
- Generated reference names are resolved from `C3_GetObjectRefTable` in
  `scripts/c3runtime.js` around lines 2197-2220. Important IDs are System
  `39=CompareBoolVar`, `42=Else`, `54=Compare`, `63=TriggerOnce`; Sprite
  `65=SetPos`, `66=SetVisible`, `142=SetAnim`, `144=SetAnimFrame`,
  `148=SetDefaultColor`, `160=SetBoolInstanceVar`, `234=StartAnim`; Mouse
  `261=IsButtonDown`, `262=IsOverObject`; Sprite
  `263=IsBoolInstanceVarSet`; Audio `95=PlayByName`.
- The export is minified and has no source map. Event numbers and JSON paths,
  rather than generated line numbers, are the authoritative identifiers.

## Button data model

The generic `Button` type is `project[3][89]`. It is a one-pixel transparent
sprite (`images/shared-2-sheet6.png`, atlas rectangle x=5, y=1, w=1, h=1),
scaled to form hit regions. Its instance variables are:

| variable | type | default | role |
|---|---:|---:|---|
| `Disabled` | boolean | false | suppresses pointer routing |
| `Pressed` | boolean | false | pointer/touch is currently carried by the target |
| `For` | string | empty | semantic button enum dispatched to an action function |
| `Hovered` | boolean | false | passive desktop hover state |

The shared Buttons event sheet also owns `AnyButtonPressed`, `CarriedHover`,
`Nudging`, pointer-history coordinates, and touch-drag state at
`project[6][38][1][8]`. `CarriedHover` starts true on layout start and is cleared
after the first hover transition, deliberately suppressing an initial rollover.

### Complete `For` enum inventory

There are 2,862 layout instances because the common UI is copied into 39 tool
layouts. The unique semantic values are the complete useful inventory:

`No`, `Paint`, `Save`, `Back`, `Pause`, `Prompt`, `NameEntry`, `FlipCamera`,
`CameraFX`, `Clear`, `PlayMode`, `Peek`, `GoBack`, `Load`, `MoreLess`, `Home`,
`Volume`, `Music`, `Manual`, `Focus`, `Camera`, `Fill`, `EmojiButton1` through
`EmojiButton7`, `K25` through `K66`, `Next`, `About`, `Community`, `App`,
`Enter`, `LightSwitch`, `Whistlegraph`, `IvoProfile`, `AdelaideProfile`,
`SeanProfile`, `WilsansProfile`, and `PainterStories`.

Common-tool values occur 39 times each; `Back` occurs 79 times and `Next` 40
times because profile layouts add full-height navigation targets. Stable examples
of the shared instances are `project.5.0.6.2.14.9` through `.14` (Save, Paint,
No, Back, Pause) and `project.5.0.6.3.14.128` onward (keyboard and utility
buttons). Profile navigation is under `project.5.39` through `project.5.78`;
the index targets are `project.5.79.6.1.14.9` and following.

### Related visual objects/families

These are separate sprites driven by semantic dispatch, not hit targets:

- `67 NoPressed`, `68 PaintPressed`, `69 SavePressed`, `70 BackPressed`
- `74 NamePressed`, `83 PlayModePressed`
- `107 InstructionsPressed`, `108 ChatPressed`, `114 AboutPressed`,
  `116 AppPressed`, `123 WhistlegraphPressed`
- `124 KeyPressed`, `131 ClearPressed`, `132 LinkPressed`, `136 PeekPressed`,
  `138 GoBackPressed`, `141 LoadPressed`, `142 GoPressed`, `148 EmojiPressed`
- `158 FocusPressed`, `159 CameraPressed`, `162 FillPressed`,
  `177 PainterStoriesHomePressed`
- `71 TapAndHoldBackground` and `75 TapAndHold` provide the hold overlay.

## Pointer/touch router: every generic transition

The sole generic router is function `CheckButtons` at
`project.6.38.1.8.8.52` (Buttons event 105). All of its callers are in this same
sheet. The normalized transition graph is:

| source | evidence | transition/action |
|---|---|---|
| initial pointer release | `project.6.38.1.8.8.9.7.9` | dispatches `CheckButtonsCore(Pause, Release, false)` |
| mouse held | parent event 52 at `project.6.38.1.8.8.35` | requires Mouse IsButtonDown, `AnyButtonPressed`, and enabled target |
| leave pressed target | event 53 `.35.8.0` | `Pressed=false`; call `CheckButtons(For, Release)` |
| enter destination while held | event 54 `.35.8.1` | `Pressed=true`; call `CheckButtons(For, Push)` |
| no eligible held target | event 55 `.35.8.2` | Else + TriggerOnce; play `generic - cancel` |
| passive pointer motion | event 60 `.38` | only while coordinates change and `CarriedHover` is true |
| passive unhover | event 62 `.38.8.0.8.0` | call `CheckButtons(For, Unhover)`; clear `Hovered` and `CarriedHover` |
| passive hover | event 63 `.38.8.0.8.1` | call `CheckButtons(For, Hover)`; set `Hovered`; clear `CarriedHover` |
| touch end | event 87 `.46` | clear `Pressed`; dispatch Release |
| touch held leave/enter | events 90-95 `.49` | mirror mouse Release/Push; Else + TriggerOnce cancel |
| forced release/reset | `.51.7.0` | dispatch Release |

The calls use serialized constants `586=Release`, `612=Push`, `615=Unhover`,
and `616=Hover`. There is no time debounce: Push/Hover branches use
`TriggerOnce`, plus `Pressed`/`Hovered` booleans. Held crossover replays the
destination's Push treatment; it is not passive rollover.

## Semantic dispatch functions and all button event families

There are exactly five `CheckButtons*` functions in the export:

| sheet | function path | responsibility |
|---|---|---|
| Buttons | `project.6.38.1.8.8.52` | pointer/touch normalization |
| Core | `project.6.39.1.50` | main No Paint UI |
| PainterStories | `project.6.40.1.1.8.4` | profile Back/Next page turns |
| PainterStoriesIndex | `project.6.41.1.11` | profile-index controls |
| Splash | `project.6.44.1.6.8.19` | splash buttons |

### Core nested actions

`CheckButtonsCore(For, Event, SinglePush)` begins at Core event 265. Its complete
semantic branches are events 269-397 under `project.6.39.1.50.8`:

| `For` family | Push visual/action/audio | Release or hover behavior |
|---|---|---|
| No | events 269-279 | Push event 270 shows `NoPressed`, starts hold timing, and plays `generic - no button pressed (metal brush)`; Hover 277/278 uses TriggerOnce and, after carried-hover suppression, `generic - button rollover`; Release hides pressed state and invokes `No` only for a valid release |
| Paint | events 280-290 | Push 281 shows `PaintPressed` and plays `generic - paint button pressed (psst)`; Hover 288/289 plays rollover and applies its playback treatment; valid Release invokes `Paint` |
| Save | events 291-297 | Push 292 shows `SavePressed` and plays `generic - save button pressed`; valid Release invokes `Save` |
| Back | events 298-300 | Push 299 shows `BackPressed`, changes the cursor treatment, and plays `generic - button press`; valid Release invokes `Back` |
| Pause | events 301-305 | Push 302 reveals overlay/icon and plays `generic - pressing pause`; valid Release invokes `Pause` |
| Prompt/Name | events 306-312 | Push uses `keyboard - beep press`; release opens the mapped entry UI |
| PlayMode, Peek, GoBack, MoreLess, Home, Volume, Music, Manual, Focus, Camera, Fill, Load, Clear | events 313-369 and 393-397 | pressed sprites or icon frames; clicks/tooth-flick/beep sounds; release calls the corresponding mapped function |
| graphic keyboard and link/emoji targets | events 371-392 | `KeyPressed`, `LinkPressed`, or `EmojiPressed`; click/tooth-flick audio; release invokes keyboard/link/emoji action |

Core also invokes this function directly for keyboard-generated synthetic Push
and Release events. Those call sites are all under `project.6.39.1.48` (events
for Name/Prompt/Save/No/Paint/Pause and utilities), `project.6.39.1.110`
(synthetic No/Paint), and `project.6.39.1.48.8.22-.23` (remaining key mapping).
`SinglePush=true` prevents repeated keyboard-held activation; pointer routing
passes false.

Final action audio is outside `CheckButtonsCore`: `No` and `Paint` live at
`project.6.39.1.44`, `Pause` at `.45`, and `Save` at `.46`. Thus Push audio and
successful-release audio are intentionally distinct.

### Profile, index, and splash nested actions

- `CheckButtonsProfile`, `project.6.40.1.1.8.4`: Back and Next pairs are events
  11-16. Push plays `generic - page turn pressed 1`; Release plays
  `generic - page turn released 1`. Calls are at `project.6.40.1.2.8.2`.
- `CheckButtonsProfileIndex`, `project.6.41.1.11`: five index controls occupy
  events 20-34. Every pair uses `generic - button press` on Push and
  `generic - button release` on Release, with pressed visual state.
- `CheckButtonsSplash`, `project.6.44.1.6.8.19`: splash controls occupy events
  50-75. Push/Release use `generic - button press` / `generic - button release`.
  LightSwitch additionally has passive Hover/Unhover events 74/75 with
  `generic - button rollover`. TriggerOnce guards each edge.

These five functions plus their listed call sites are the complete set: an
exhaustive recursive scan finds 49 `CheckButtons*` calls and no sixth handler.

## Custom wavy-hand cursor

### Object, instance, and atlas

The cursor is Sprite object `90`, type definition `project[3][90]`. It is global
and initially present only on Splash at `project.5.38.6.8.14.0`: position
(-11,-15), displayed size 11x15, animation `Normal`, frame 0. The source atlas is
`images/cursor-sheet0.png`, a 64x128 PNG (1,026 bytes; the frame metadata records
the imported source size as 2,233 bytes). CSS/system mouse is hidden separately;
the sprite follows pointer coordinates.

Frame rectangle format below is `(x,y,w,h; originX,originY)`, with normalized
origins/hotspots:

- `Normal`, speed 0, non-looping: `(52,33,11,15; .0909,.0667)` and
  `(1,33,12,19; .6667,.2218)`.
- `Over Button`, speed 0, non-looping, seven hand poses:
  `(35,33,15,17;.2,.1176)`, `(35,1,15,17;.2,.1176)`,
  `(18,20,15,17;.2,.1176)`, `(1,1,15,18;.0667,0)`,
  `(15,39,15,15;.2667,.1333)`, `(18,1,15,17;.2,.0588)`,
  `(33,65,14,16;.1429,0)`.
- `Nudging`, speed 5, looping: two 14x14 frames at `(49,97)` and `(49,81)`,
  both origin (.4286,.4286).
- `No Nudge`, speed 20, looping: one 13x13 frame at `(1,97)`, origin
  (.4615,.4615).
- Legacy/auxiliary animations retained in the atlas: `Animation 1` has three
  10x10 frames at y=113 with top-left origins; `Animation 3` has two centered
  10x10 frames at y=113; `Animation 4` has two centered 10x10 frames at y=97;
  `Animation 5` has three centered 10x10 frames at y=81/65.

Collision polygons immediately following each origin in `project[3][90]` are
presentation hit polygons only; pointer anchoring must use the origins above.

### Cursor state machine and exact event evidence

Cursor behavior is centralized in Buttons sheet `project[6][38]`:

- Nudge start/stop is events around `.1.8.8.9-.11`. Event 5 calls
  `StartAnim(fromBeginning=true)` at `project.6.38.1.8.8.9.8.1.7.0` when
  `DesktopMode=true`. This starts whichever animation is current; the later
  every-tick decision at events 64-68 selects `Nudging`. Desktop NoNudge event
  11 selects `No Nudge` with
  `SetAnim(name, fromBeginning=true)` at
  `project.6.38.1.8.8.11.8.0.7.0`.
- Normal desktop restoration selects `Normal` at
  `project.6.38.1.8.8.36.7.2` (mouse release, a button was carried, target is
  enabled) and `.40.7.0` (the Else branch while not nudging). Both restart the
  selected animation from frame zero.
- Passive or carried button targeting selects `Over Button` at
  `project.6.38.1.8.8.39.8.0.8.1.7.0` and `.41.7.0`; nudge selection occurs at
  `.39.8.0.8.0.8.0.7.0`. These serialized constants resolve to
  `618=Nudging`, `619=Over Button`, `613=Normal`.
- Event 72 (`project.6.38.1.8.8.41`) selects and restarts `Over Button` when
  Mouse is over an enabled Button in desktop mode, not peeking, and the target
  semantic value is No, Paint, or PlayMode. This is name selection, not frame
  cycling.
- Mouse OnObjectClicked event 51 (`project.6.38.1.8.8.34`) runs for an enabled
  Button and, in action order, sets `AnyButtonPressed=true`, Cursor visible,
  opacity 100, then the canonical packed color. It does not select an animation;
  the subsequent state-selection events do that.
- Event 74 (`project.6.38.1.8.8.43`) is `WagCursorTimer` while the current
  animation name is `Over Button`; it explicitly sets animation frame **0**.
- Event 75 and its descendants (`project.6.38.1.8.8.44`) are the cursor
  position/wag/visibility loop, not events 43-45 in Construct's displayed event
  numbering. Event 76, desktop mode, sets position to
  `(floor(Mouse.X), floor(Mouse.Y))` at `.44.8.0.7.0`. Event 78 sets frame **2**
  when horizontal displacement from `MouseLastX` is greater than 1
  (`.44.8.0.8.0.8.0.7.0`); sibling Else event 79 sets frame **1** for the
  opposite horizontal direction threshold (`.44.8.0.8.0.8.1.7.0`). Event 81
  hides the cursor when its bounds fail the exported on-canvas test. Event 83
  re-shows it after pointer movement and restores opacity/color.
- Event 86 (`project.6.38.1.8.8.45`) is `HideCursorTimer` while no button is
  carried: it changes the color to expression 625 and opacity to 25. It does
  not select an animation.
- PainterStoriesIndex hides Cursor at `project.6.41.1.1.7.32`; transition
  sheet hides it at `project.6.42.1.1.7.0`; Index moves it to the proper UI
  layer and toggles visibility at `project.6.43.1.10.8.5.8.3`; Splash performs
  equivalent layer/visibility initialization at `project.6.44.1.6.8.6`.

Implementation invariant: render the custom cursor only in desktop mode, update
its sprite position from the Construct-space mouse coordinates every tick, use
`Over Button` for generic Button hit targets, use `Nudging`/`No Nudge` during
nudge modes, and restore `Normal` on unhover/release. Preserve each frame's
per-frame origin or the hand visibly jumps while waving.

### Explicit cursor transition table

Construct animation frames are **zero-based**. `SetAnimFrame(0)` means the first
frame, and the runtime floors/clamps the requested index. Every cursor
`SetAnim` action shown here passes `fromBeginning=true`; selection therefore
immediately resets to frame 0 even though `Normal` and `Over Button` have speed
zero.

| prior/input state | condition and path | ordered cursor actions | resulting state |
|---|---|---|---|
| nudge begins, desktop | event 5, `.9.8.1` | `StartAnim(true)` | current animation restarts; events 64-68 subsequently select the nudge/hand state |
| enabled Button clicked | event 51, `.34` | `SetVisible(true)`, `SetOpacity(100)`, `SetDefaultColor(-281492157629439)` | visible full-strength cursor; animation name unchanged until later selection |
| desktop NoNudge | event 11, `.11.8.0` | `SetAnim("No Nudge", true)` | one-frame No Nudge visual, animation speed 20 is immaterial |
| mouse release after carried button | event 56, `.36` | `SetAnim("Normal", true)` | Normal frame 0 |
| nudge mode chosen | events 64/65/66/67, `.39.8.0.8.0.8.0` | `SetAnim("Nudging", true)` | looping frames 0/1 at speed 5 |
| same decision's Else | event 68, `.39.8.0.8.1` | `SetAnim("Over Button", true)` | hand frame 0 |
| no applicable button/nudge | event 70, `.40` | `SetAnim("Normal", true)` | Normal frame 0 |
| over enabled No/Paint/PlayMode Button | event 72, `.41` | `SetAnim("Over Button", true)` | hand frame 0 |
| wag timer fires while Over Button | event 74, `.43` | `SetAnimFrame(0)` | neutral hand pose |
| desktop pointer loop | event 76, `.44.8.0` | `SetPos(floor(Mouse.X), floor(Mouse.Y))` | hotspot follows pointer |
| horizontal movement one direction | event 78, `.44.8.0.8.0.8.0` | `SetAnimFrame(2)` | directional wag pose 2 |
| horizontal movement opposite direction | event 79, `.44.8.0.8.0.8.1` | `SetAnimFrame(1)` | directional wag pose 1 |
| cursor bounds outside accepted canvas test | event 81, `.44.8.0.8.1` | `SetVisible(false)` | hidden |
| pointer moved since last mouse sample | event 83, `.44.8.0.8.2.8.0` | `SetVisible(true)`, `SetOpacity(100)`, `SetDefaultColor(-281492157629439)` | visible, fully opaque, canonical packed color |
| hide timer, no carried button | event 86, `.45` | `SetDefaultColor(-140746078815231)`, `SetOpacity(25)` | dimmed/tinted idle cursor |

The packed color constants above are the exact evaluated values of expression
611 (`-281492157629439`, hexadecimal `-0x10004001003ff`) and expression 625
(`-140746078815231`, hexadecimal `-0x8002000803ff`). They should be decoded with
Construct's packed-color helper if exact channel values are needed; do not treat
them as CSS integers. Expression 103 is opacity 100, 613 is `Normal`, 618 is
`Nudging`, 619 is `Over Button`, 621 is `WagCursorTimer`, and 624 is
`HideCursorTimer`.

Events 43-45 in the displayed Construct numbering are unrelated to the cursor:
event 43 ends a touch text-cursor drag, while events 44-50 implement Peek
dragging. Their proximity in the serialized sheet is easy to misread; the
cursor's displayed events begin at 51 and resume at 56, 64, 70, 72, 74-86.

## Animation selection versus frame selection

The export does not imply that every spritesheet should auto-cycle.

- `SetAnim(name, fromBeginning)` selects a named animation. With
  `fromBeginning=true`, it resets to zero-based frame 0. The authored animation's
  speed and loop flags then control whether it advances.
- `SetAnimFrame(index)` chooses a specific zero-based pose. It does not mean
  “play the sheet.” The cursor hand uses only frames 0, 1, and 2 of the
  seven-frame `Over Button` animation in these events; the other authored poses
  are retained assets, not evidence for automatic playback.
- `StartAnim(fromBeginning)` resumes/starts the currently selected animation;
  it does not select a different animation name.
- Cursor `Normal` and `Over Button` have speed 0 and non-looping flags, so all
  motion is event-selected. `Nudging` is speed 5 and looping; `No Nudge` is
  marked looping but contains one frame.
- Pressed feedback generally uses separate logical replacement objects
  (`NoPressed`, `PaintPressed`, `SavePressed`, `BackPressed`, and the other
  `*Pressed` types). Core button events show/hide those objects or select their
  frames. They are not alternate frames of the invisible generic Button.
- Consequently, a faithful importer should model atlas frame rectangles,
  per-frame origins, named-animation speed/loop metadata, explicit frame
  selection, and separate replacement-object visibility as distinct concepts.
  Never infer cycling merely because multiple frames share a spritesheet.

## Fidelity checklist

1. Keep `Button` hit regions invisible and drive them by `For` plus the three
   booleans.
2. Separate passive Hover from held crossover: Hover gets rollover; crossover
   gets the destination Push cue and visual.
3. Preserve initial `CarriedHover=true` suppression and edge `TriggerOnce`.
4. Do not add artboard/canvas hover audio; none exists in the export.
5. Pair Push visuals/audio with successful Release functions; cancel when the
   carried pointer/touch ends outside an eligible target.
6. Use the cursor atlas rectangles and per-frame origins verbatim.
7. Keep profile page-turn sounds distinct from generic splash/index controls.
