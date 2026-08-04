# OSKIEWAR pose and frame system

Target: original geometry-only fighters in `xbox/live/hello.js`, simulated at 60 Hz. Street Fighter references are timing and readability research only. Do not import, trace, ship, or derive coordinates from copyrighted sprites, captures, models, or hitbox images.

## Current behavior to replace

- `startMelee` gives punch and kick the same 220 ms duration: about 13.2 frames.
- `meleePulse` drives both pose extension and attack collision with one sine wave. The strike can hit through almost the entire startup, extension, and retraction.
- Punch and kick differ only in height, radius, and sound. Both reach `70..220` world units.
- The rendered stick figure is also the hurt volume. Every head and limb capsule can be hit in every pose.
- Full walk velocity continues during attacks. A new button edge can overwrite the current attack.
- Block has no startup, release, hit-stop, or block-stun state. A blocked player can move on the next simulation call.
- `attackKind` is not cleared when `attackUntil` expires.
- Combat timing uses variable `dt`; an overloaded 40 ms step can skip two active frames. The replay already defines `replayTickUs = 16667`, but combat does not use that clock.
- Wall bounds expand with animated hands and feet. An attack near a wall can move the fighter's root.

The first refactor should leave scoring and ball rules intact while replacing time-derived attack collision with frame windows.

## Initial frame table

Frame 1 is the first frame after an input is accepted. `S/A/R` here means non-active startup, active, recovery. Public fighting-game tables often call the first active frame the “startup” number, so this document's `5/3/9` punch would be listed there as frame-6 startup. The numbers are a starting balance target, not copied move data.

| Action | Frames at 60 Hz | Root motion | Collision and input rule |
|---|---:|---|---|
| `idle` | 48-frame loop | none | Can enter any grounded action. Four broad key poses are enough; breathing may interpolate continuously. |
| `walkForward` | 24-frame loop | 25 units/frame | Foot plants at 1 and 13. Keep the head nearly level; pass poses at 7 and 19. |
| `walkBack` | 28-frame loop | 20 units/frame | Same contacts, shorter stride and rearward torso lean. |
| `crouchEnter` | 4 | none | Standing hurt profile on 1, blended profile on 2, crouch profile on 3-4. |
| `crouchHold` | held | none | May punch, kick, shield, or release. No walk in the first pass. |
| `crouchExit` | 4 | none | Crouch profile through 2; standing profile from 3. |
| `jumpSquat` | 4 | none | Grounded through 4; launch after frame 4. Buffer one air action. |
| `jumpRise` | velocity-driven | physics | Tucked launch silhouette, then open silhouette. No new grounded action. |
| `jumpApex` | while `abs(vy) < 140` | physics | One readable suspended pose; do not stall physics. |
| `jumpFall` | velocity-driven | physics | Feet prepare beneath root during final 90 world units. |
| `land` | 4 | none | Frames 1-3 locked; frame 4 cancellable to walk/crouch/shield. |
| `punch` | `5/3/9` = 17 | 30% walk on 1-5; zero on 6-10 | Hand/forearm hit capsule exists only on 6-8. Contact pose held on 6-7. |
| `kick` | `8/4/14` = 26 | 20% walk on 1-8; zero on 9-18 | Shin/foot hit capsule exists only on 9-12. Supporting foot stays planted. |
| `shieldEnter` | 3 visual frames | zero | Guard is effective on frame 1 for responsive defense; body closes over 1-3. |
| `shieldHold` | held | zero | Guard hurt profile and ball return enabled. Facing is locked; back-block remains a separate automatic rule if retained. |
| `shieldExit` | 6 | zero | Guard remains through frame 2; neutral hurt profile from 3; actionable on 7. |
| `blockReactPunch` | 9 block-stun | pushback only | 4 frames block-stop, then 9 locked frames. |
| `blockReactKick` | 13 block-stun | pushback only | 6 frames block-stop, then 13 locked frames. |
| `hitReactPunch` | 13 hit-stun | knockback only | 5 frames hit-stop, then 13 locked frames if non-KO combat is added. |
| `hitReactKick` | 17 hit-stun | knockback only | 7 frames hit-stop, then 17 locked frames if non-KO combat is added. |
| `ko` | 8 impact + fall | knockback | Current one-hit melee rule can transition directly here after hit-stop. |

At 60 Hz, punch is 283 ms and kick is 433 ms. These sit near the readable light/medium-normal envelope: modern reference normals commonly reach their first active frame in 4-12 frames, stay active for 2-4 frames, and recover longer than they are active. For example, the current Ryu reference lists standing light punch as frame-4 startup, 3 active, 7 recovery and standing medium kick as frame-9 startup, 3 active, 18 recovery. Treat that as scale calibration, not a target to clone.

### Hit-stop, stun, and cancels

| Contact | Attacker stop | Defender stop | Defender stun after stop | Pushback |
|---|---:|---:|---:|---:|
| Punch hit | 5 | 5 | 13 or KO | attacker 35, defender 110 |
| Punch block | 4 | 4 | 9 | attacker 45, defender 70 |
| Kick hit | 7 | 7 | 17 or KO | attacker 50, defender 180 |
| Kick block | 6 | 6 | 13 | attacker 65, defender 115 |
| Ball melee return | 4 | 4 | 0 | existing ball impulse |
| Ball shield return | 6 | 6 | 8 | existing ball and shield impulse |

Hit-stop freezes the contact participants' action clocks and root physics. A ball contact freezes the fighter and ball; a melee contact freezes both fighters. It does not freeze the camera, impact particles, UI, or audio. Resolve a contact once, latch the hit ID, apply stop, then resume on the same action frame. This preserves the contact drawing instead of advancing directly into recovery.

Start with narrow cancel rules:

- Punch frames 6-10 may buffer another punch on hit or block; the next punch starts after hit-stop and no earlier than frame 9.
- Punch frames 6-8 may buffer kick on hit only; kick starts after hit-stop.
- Shield may cancel grounded idle, walk, crouch hold, and the final two recovery frames of punch. It cannot cancel kick recovery.
- Jump may be buffered during the last three frames of any recovery but begins only when the fighter becomes actionable.
- Store at most one buffered action for six frames. Replace it only with a higher-priority defensive action.

The schema should carry these windows even if the first implementation leaves them disabled. That avoids burying future cancel logic in button handlers.

## Named-pose schema

Author one reusable skeleton in fighter-local coordinates. Positive X points forward; mirroring by `facing` happens only when the sampled pose is placed in world space.

```js
const SKELETON = {
  root: null,
  hip: "root", chest: "hip", neck: "chest", head: "neck",
  frontShoulder: "chest", frontElbow: "frontShoulder",
  frontHand: "frontElbow",
  rearShoulder: "chest", rearElbow: "rearShoulder", rearHand: "rearElbow",
  frontKnee: "hip", frontFoot: "frontKnee",
  rearKnee: "hip", rearFoot: "rearKnee",
};

const POSES = {
  guard: {
    root: [0, 0], hip: [0, -58], chest: [2, -118], neck: [1, -152],
    head: [3, -180],
    frontShoulder: [10, -135], frontElbow: [35, -118], frontHand: [48, -145],
    rearShoulder: [-8, -132], rearElbow: [12, -105], rearHand: [28, -132],
    frontKnee: [16, -31], frontFoot: [38, 0],
    rearKnee: [-20, -30], rearFoot: [-42, 0],
  },
  punchAnticipate: { /* complete joint set */ },
  punchContact: { /* complete joint set */ },
  punchOvershoot: { /* complete joint set */ },
  kickAnticipate: { /* complete joint set */ },
  kickContact: { /* complete joint set */ },
  kickRetract: { /* complete joint set */ },
  shieldClosed: { /* complete joint set */ },
  crouch: { /* complete joint set */ },
  jumpLaunch: { /* complete joint set */ },
  jumpOpen: { /* complete joint set */ },
  jumpPrepareLand: { /* complete joint set */ },
};
```

Every named pose must provide every joint. Explicit poses are easier to inspect and diff than inheritance chains with hidden defaults. Character identity belongs in proportions and pose offsets, not separate combat geometry:

```js
const FIGHTER_RIGS = {
  OSKIE: {
    scale: 1,
    proportions: { head: 22, torsoWidth: 12, limbWidth: 10 },
    poseOffsets: { guard: { head: [0, -2] } },
    palette: { body: [24, 55, 120], accent: [60, 125, 230] },
  },
  JEFFREY: {
    scale: 1,
    proportions: { head: 22, torsoWidth: 12, limbWidth: 10 },
    poseOffsets: {},
    palette: { body: [116, 24, 38], accent: [220, 48, 62] },
  },
};
```

Keep both palettes dark blue and dark red in normal play. Debug collision colors are a separate layer.

## Actions and keyframes

An action owns gameplay frames, pose sampling, collision profiles, movement, and events:

```js
const ACTIONS = {
  punch: {
    frames: 17,
    loop: false,
    keyframes: [
      { at: 1, pose: "guard", ease: "outCubic" },
      { at: 4, pose: "punchAnticipate", ease: "inCubic" },
      { at: 6, pose: "punchContact", ease: "step" },
      { at: 8, pose: "punchOvershoot", ease: "outQuad" },
      { at: 17, pose: "guard", ease: "inOutQuad" },
    ],
    phases: { startup: [1, 5], active: [6, 8], recovery: [9, 17] },
    movement: [
      { frames: [1, 5], control: 0.3 },
      { frames: [6, 10], control: 0 },
      { frames: [11, 17], control: 0.15 },
    ],
    hitboxes: [{ id: "punch-0", frames: [6, 8], bone: "frontElbow:frontHand",
      shape: "capsule", from: 0.15, to: 1.18, radius: 18,
      hit: "punch" }],
    hurtProfile: "standingAttack",
    hurtOverrides: [
      { frames: [4, 8], id: "frontArm", radius: 13 },
    ],
    cancels: [
      { frames: [6, 10], on: ["hit", "block"], into: ["punch"] },
      { frames: [6, 8], on: ["hit"], into: ["kick"] },
      { frames: [16, 17], on: ["always"], into: ["shield"] },
    ],
    events: [
      { at: 1, type: "sound", name: "snare" },
      { at: 6, type: "swingPeak" },
    ],
  },
};
```

Keyframes are inclusive integer simulation frames. Interpolation affects drawing and limb-attached collision geometry, but hitbox enable/disable, cancel windows, events, and hurt-profile switches are discrete. A skipped render frame must never skip a simulation frame.

Sample each joint independently between the surrounding named poses. `step` means hold the previous pose until the destination frame, useful for a one-frame snap into contact. Otherwise evaluate the destination keyframe's easing function. Decorative settle animation may continue after `frames`; gameplay actionability still ends exactly at `frames`, matching the useful distinction between visible animation and actionable recovery.

## Collision geometry

Use four independent layers:

1. `pushbox`: one stable grounded capsule or AABB around hips and torso. It handles walls and fighter separation and does not follow attacking limbs.
2. `hurtboxes`: head, torso, upper/lower arms, upper/lower legs. Capsules are generated from sampled joints, then selectively resized or disabled by the action.
3. `hitboxes`: short-lived spheres or capsules attached to a bone or pair of joints. They exist only during explicit active frames.
4. `guardbox`: a forward-facing capsule used for the ball and explicit shield. Ordinary back-block can still test attack-versus-hurt overlap before choosing block outcome.

```js
const HURT_PROFILES = {
  standing: [
    { id: "head", shape: "sphere", joint: "head", radius: 23 },
    { id: "torso", shape: "capsule", joints: ["hip", "neck"], radius: 18 },
    { id: "frontArm", shape: "capsule",
      joints: ["frontShoulder", "frontHand"], radius: 11 },
    { id: "rearArm", shape: "capsule",
      joints: ["rearShoulder", "rearHand"], radius: 10 },
    { id: "frontLeg", shape: "capsule", joints: ["hip", "frontFoot"], radius: 13 },
    { id: "rearLeg", shape: "capsule", joints: ["hip", "rearFoot"], radius: 12 },
  ],
  crouch: [ /* lower head/torso plus bent limb capsules */ ],
  shield: [ /* compact body; no arbitrary invulnerability */ ],
};
```

For a bone-attached capsule, `from` and `to` are scalar positions along the vector from the first joint to the second; values beyond 1 extend past the distal joint. Optional `normalOffset` moves it perpendicular to the bone. This lets the punch hitbox extend slightly beyond the hand without a separate world-space strike point.

Broad-phase collision should first compare cached AABBs. Narrow phase needs sphere-sphere, sphere-capsule, and capsule-capsule distance. Keep all calculations in world space and derive screen geometry only for drawing. Generate one `sampleFighterFrame(player)` result per simulation frame and reuse it for rendering, wall/push resolution, ball contact, and melee contact.

Do not let visual thickness silently set collision radius. A limb may draw at width 12 but hurt at radius 10. This is necessary for readable silhouettes without accidental contact.

## State and fixed-step order

Use an accumulator with `STEP_US = 16667`. Cap catch-up work, but advance combat one integer frame at a time:

1. Sample both gamepads and update the input buffer.
2. If global intro/round state is active, update that state only.
3. Decrement hit-stop. Fighters in hit-stop keep their current action frame.
4. Choose/advance each fighter action by priority: KO, hit-stun, block-stun, attack, shield, crouch transition, airborne, walk, idle.
5. Apply action movement control, gravity, and root motion.
6. Sample both pose/action frames once.
7. Resolve stable pushboxes and walls.
8. Resolve simultaneous melee contacts from both samples; latch `attackInstanceId + hitbox.id + defender.id`.
9. Resolve the ball against hitboxes, guardboxes, then hurtboxes.
10. Queue hit-stop, stun, impulses, scoring, sound, and effects. Apply the queue after all collision queries so iteration order cannot decide trades.

Rendering may interpolate roots between the previous and current simulation samples, but it must render the current named pose/action frame and must not create extra collision samples.

## Debug frame-data overlay

The overlay should be usable on Xbox without devtools and should not alter release play unless explicitly enabled.

- Toggle: debug build flag plus keyboard `F2`; optional controller chord `View + Y`.
- Pause/resume: `Space` or `View + A`.
- Step backward/forward through a captured 120-frame ring buffer: `[` / `]` or `LB` / `RB` while paused. Backward viewing is inspection only; resuming returns to the newest live frame unless deterministic rollback is implemented.
- Draw hurtboxes translucent blue/cyan, active hitboxes red, guardboxes violet, and pushboxes white. Do not use yellow for fighter bodies or collision layers.
- Above each fighter show `ACTION  frame/total  PHASE`, then `STOP`, `HITSTUN`, `BLOCKSTUN`, and buffered input.
- Bottom timeline: 8-pixel columns for the newest 30 frames, blue startup, red active, gray recovery, violet hit-stop, cyan stun. Mark contact and cancel frames with vertical ticks; page through older frames while paused.
- Side panel: active hitbox IDs, world bounds, attack instance, last contact pair, cancel candidates, and input age.
- Add toggles for skeleton, pose names, collision IDs, and world coordinates. Keep the default overlay sparse enough to read at couch distance.

Record the sampled pose, action frame, collision shapes, root position/velocity, input buffer, and contact result in the debug ring. That makes a reported bad hit reproducible without storing copyrighted visual assets.

## Original silhouette plan

- `idle`: uneven boxing stance, hands at different heights, knees soft; head and torso form a clear forward wedge.
- `walkForward`: shoulder leads the step; rear hand protects the face. Foot contacts are visibly held for two frames.
- `walkBack`: hips recede before the front foot lifts; shorter step avoids looking like a reversed forward walk.
- `crouch`: hips sit behind heels, front forearm crosses chest, head remains identifiable above the torso.
- `jump`: four-frame anticipation, compact launch, open apex, feet beneath hips before landing.
- `punch`: pull the striking shoulder and hand backward, snap a straight line through shoulder-elbow-hand at contact, then overshoot the shoulder before retracting.
- `kick`: load weight over rear foot, chamber front knee, make hip-knee-foot a strong contact line, then retract the knee before replacing the foot.
- `shield`: elbows close around the torso and forearms make one forward arc. The silhouette must differ from crouch even at thumbnail size.
- `hit/block`: bend the spine away on hit; compress toward the strike on block. Hit-stop holds the strongest silhouette.

These are pose contracts, not instructions to imitate a particular character.

## Implementation slices

1. Add fixed-step `combatFrame`, action state, and a six-frame input buffer without changing visuals.
2. Define the skeleton, `guard` pose, and sampler; reproduce the current neutral figure through named joints.
3. Add idle, walk, crouch, jump, punch, kick, and shield action definitions. Keep the existing renderer fed by sampled segments.
4. Replace `meleePulse`, `meleeStrike`, and `attackUntil` checks with action-frame hitboxes. Clear action state deterministically.
5. Split stable pushbox, authored hurtboxes, attack hitboxes, and guardbox. Reuse one sample for melee and ball collision.
6. Add queued simultaneous contact, hit-stop, block-stun, and per-attack pushback. Preserve current one-hit KO until health is intentionally designed.
7. Add cancel/buffer rules behind data flags, then tune using the frame-step overlay.
8. Add the 120-frame debug ring and couch-readable overlay. Verify every action at normal speed, half speed, and one-frame stepping.

Acceptance checks:

- Punch cannot hit on frames 1-5 or 9-17; kick cannot hit on 1-8 or 13-26.
- A 40 ms render stall does not skip a combat frame or extend an active window.
- Hit-stop holds the contact pose and never creates a second hit.
- Walking, wall contact, ball collision, and melee all use the same pose sample for a given combat frame.
- An extended fist cannot push the fighter away from a wall.
- P1 and P2 can trade on the same frame independent of array order.
- Blue and red fighters remain legible against the dark stage in idle, crouch, air, punch, kick, and shield silhouettes.

## Research references

- [Capcom: Character Animation—Ed](https://game.capcom.com/cfn/sfv/column/132209) describes production as authored “motions,” with planned poses revised after capture. The useful lesson is pose-first authorship and deliberate adjustment, not asset reuse.
- [Capcom SFV Ryu frame data](https://game.capcom.com/cfn/sfv/character/ryu/frame?lang=en) is an official reference for the startup/active/recovery vocabulary and move-scale checks.
- [Capcom SSFIV AE 2012 change list](https://static.capcom.com/streetfighter/downloads/SSFIV%20Arcade%20Edition%202012%20Final%20Change%20List%20-%20US.pdf) documents per-frame cancel, invulnerability, hitbox, and hurtbox changes, supporting data-authored windows rather than animation-wide collision.
- [Ultimate Frame Data: SF6 Ryu](https://ultimateframedata.com/sf6/ryu) provides current frame-steppable normal-attack calibration. Its site notes that visible recovery can continue after gameplay recovery ends, which supports separating decorative settle from actionability.
- [SF6Frames](https://sf6frames.com/luke) is a known frame-by-frame visual archive with captured hit/hurt overlays. Use it only for timing and silhouette study; its imagery remains owned by its respective rights holders.
- [Frame Trapped](https://frametrapped.com/) demonstrates useful hitbox-debug controls: accurate units, live advantage, rulers, and active-only display.
- [Shoryuken/SuperCombo movement-speed archive](https://archive.supercombo.gg/t/movement-speed-comparison/136204) records that a walk may delay translation until its second animation frame, a useful precedent for synchronizing foot plants and root motion.
- [Rivals Workshop posing guide](https://www.rivalslib.com/workshop_guide/art/pose.html) explains readable anticipation, action, and recovery silhouettes in fighting-game animation without requiring any Street Fighter art.
