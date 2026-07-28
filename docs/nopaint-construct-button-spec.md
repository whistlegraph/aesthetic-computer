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

- Nudge start/stop is events around `.1.8.8.9-.11`. Nudge makes Cursor visible
  and starts its animation; desktop NoNudge selects `No Nudge` at
  `project.6.38.1.8.8.11.8.0.7.0`.
- Normal desktop restoration selects `Normal` at
  `project.6.38.1.8.8.36.7.2`, `.40.7.0`, and related release/reset branches.
- Passive or carried button targeting selects `Over Button` at
  `project.6.38.1.8.8.39.8.0.8.1.7.0` and `.41.7.0`; nudge selection occurs at
  `.39.8.0.8.0.8.0.7.0`. These serialized constants resolve to
  `618=Nudging`, `619=Over Button`, `613=Normal`.
- Event 43 (`project.6.38.1.8.8.43`) advances/starts the hand animation for
  touch activity. Event 44 (`.44`) sets Cursor position to current Mouse X/Y.
  Its children set frame 0/4 according to pointer/button state, hide on
  incompatible touch mode, and re-show/reset the sprite when appropriate.
- Event 45 (`.45`) applies the current cursor color and opacity treatment.
- PainterStoriesIndex hides Cursor at `project.6.41.1.1.7.32`; transition
  sheet hides it at `project.6.42.1.1.7.0`; Index moves it to the proper UI
  layer and toggles visibility at `project.6.43.1.10.8.5.8.3`; Splash performs
  equivalent layer/visibility initialization at `project.6.44.1.6.8.6`.

Implementation invariant: render the custom cursor only in desktop mode, update
its sprite position from the Construct-space mouse coordinates every tick, use
`Over Button` for generic Button hit targets, use `Nudging`/`No Nudge` during
nudge modes, and restore `Normal` on unhover/release. Preserve each frame's
per-frame origin or the hand visibly jumps while waving.

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
