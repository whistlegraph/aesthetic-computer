# KidLisp Decree '26

Status: Draft  
Audience: Runtime implementers, piece authors, tooling authors  
Canonical URL target: `learn.kidlisp.com/decree/26`

## 1. Purpose

`KidLisp Decree '26` defines the platform-independent, stable ABI guarantee for KidLisp runtimes in 2026.

This decree is normative for:
- lifecycle hooks
- host API surface
- rendering/compositing semantics
- timing and audio global semantics
- compatibility expectations

## 2. Versioning Model

- Year-based versions: `'26`, `'27`, `'28`, ...
- `KidLisp Decree '26` is a frozen baseline once ratified.
- Future decrees may extend behavior, but MUST publish compatibility notes against `'26`.

Required compatibility terms:
- `compatible-with-'26`: runtime preserves all required `'26` behaviors.
- `native-extensions`: runtime adds non-decree APIs without changing decree-required behavior.
- `deviation`: runtime intentionally diverges and MUST declare the divergence.

## 3. Conformance Levels

A runtime MAY claim one or more conformance levels:

1. `Core`
- parser/evaluator semantics
- required lifecycle ABI
- required host object shape

2. `Render`
- offscreen buffer model
- page/paste semantics
- alpha compositing semantics

3. `Audio`
- audio globals (`amp`, `mic`, etc.) range and update semantics

Minimum public claim format:
- `KidLisp Decree '26: Core`
- `KidLisp Decree '26: Core + Render`
- `KidLisp Decree '26: Core + Render + Audio`

## 4. Stable Lifecycle ABI

A conforming runtime MUST expose and invoke:
- `boot(api)`
- `act(api)`
- `sim(api)`
- `paint(api)`
- `leave()`
- `beat(api)` (optional if no beat clock exists; MUST be declared)

Required ordering guarantee per frame:
1. `act`
2. `beat` (if supported and triggered)
3. `sim`
4. `paint`
5. present

Runtimes MAY batch or skip frames for performance, but MUST preserve ordering of calls they do execute.

## 5. Host API Shape (Required Keys)

At minimum, `api` MUST provide:
- drawing primitives: `wipe`, `ink`, `line`, `box`, `circle`, `plot`, `write`
- geometry/system: `screen.width`, `screen.height`
- timing: `clock` and/or deterministic frame counter semantics
- buffering (Render level): `painting`, `page`, `paste`

If a function is unsupported, runtime MUST:
- expose capability flags indicating unsupported state, and
- fail predictably (no silent corruption).

## 6. Rendering Semantics (Render Level)

`KidLisp Decree '26` render model:

1. Base layer (`layer0`)
2. Optional bake layers
3. Optional embedded layers
4. Composite result to display buffer

Required semantics:
- `painting(w,h,cb)` creates an isolated drawable buffer.
- `page(buffer)` switches active render target.
- `paste(buffer,x,y[,scale])` composites source onto current target.
- Alpha blending behavior MUST be consistent across hosts for decree tests.

Hosts MAY optimize compositor internals (GPU, SIMD, tiled), but output MUST satisfy decree conformance tolerances.

## 7. Timing and Audio Globals

Required timing globals:
- `frame` monotonic non-decreasing
- `clock` wall-time access (or declared fallback behavior)

Audio level (Audio conformance):
- `amp` MUST be defined and numeric.
- Recommended normalized range: `0..10`.
- If no audio source exists, runtime MUST define deterministic fallback behavior.

## 8. Capability Discovery

A conforming runtime SHOULD expose a capability object equivalent to:
- decree version
- conformance levels (`Core`/`Render`/`Audio`)
- supported optional features (e.g. `compositeLayers`, `mask`, `blur`, `zoom`, `embed`)

Suggested shape:
- `system.kidlisp.decree.version = "26"`
- `system.kidlisp.decree.levels = ["Core", "Render"]`
- `system.kidlisp.decree.features = { ... }`

## 9. Compatibility and Deprecation Rules

Within a decree year:
- Required ABI behaviors MUST remain stable.
- Additive APIs MAY be introduced.
- Breaking changes MUST NOT be introduced under the same decree version.

Across decree years:
- New decree MUST publish:
  - added guarantees
  - deprecated behaviors
  - removed behaviors
  - migration notes

## 10. Conformance Test Suite

`KDL-26` (planned) SHOULD include:
- lifecycle ordering tests
- host shape tests
- render reference scenes (including layering + alpha)
- timing behavior tests
- audio global behavior tests

A runtime claiming decree conformance SHOULD publish its `KDL-26` result set.

## 11. Publishing Map

Recommended URL layout:
- `learn.kidlisp.com/decree` -> latest stable decree
- `learn.kidlisp.com/decree/26` -> frozen `KidLisp Decree '26`
- `learn.kidlisp.com/decree/changelog` -> year-over-year compatibility notes

## 12. Open Draft Items

- precise alpha blend reference equation
- exact tolerance policy for render conformance frames
- required vs optional transformation set (`scroll`, `zoom`, `blur`, `contrast`, etc.)
- embed semantics requirements for minimum Render conformance

## 13. Reference Profiles (`'26`)

This decree defines named reference profiles for practical conformance targets.

### 13.1 `RBP-26` (`$roz` Baseline Profile)

`RBP-26` is the minimal feature profile required to run `$roz` with decree-consistent behavior.

Required heads/tokens:
- `ink`
- `line`
- `circle`
- `scroll`
- `spin`
- `zoom`
- `contrast`
- `?` (choice operator)
- timing heads: `1s...`, `2s...`, `0.5s`
- fade shorthand head pattern: `fade:<color>-<color>-...`

Required symbols:
- `w`, `h`
- `w/2`, `h/2`

Required color/token support used by profile:
- `red`, `blue`, `black`, `white`
- `rainbow`
- `cyan`, `yellow`, `magenta`

Semantics requirements:
- `?` MUST evaluate as runtime choice across provided options.
- `1s...` and `2s...` MUST behave as repeating timed forms.
- `0.5s` MUST behave as a timed one-shot form (unless host explicitly declares a deviation).
- transform/effect heads (`spin`, `zoom`, `contrast`, `scroll`) MUST apply in a stable per-frame order.
- `w/2` and `h/2` MUST resolve from current frame dimensions.

Conformance claim string:
- `KidLisp Decree '26: RBP-26`
