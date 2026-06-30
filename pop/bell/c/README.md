# bell — a physically-modeled bell

A reusable bell voice whose sound comes from a real **finite-element model** of a
vibrating shell, so its **material parameters** (Young's modulus, density,
Poisson ratio, wall thickness, damping) and **geometry** genuinely shape the
timbre — not a table of hand-typed partial ratios. The same model drives a 3-D
visualization of the bell shell deforming by mode.

Zero dependencies beyond libm (C) and `canvas`/ffmpeg (the JS viz).

```
pop/bell/
  c/bell.c, c/bell.h   the engine + public API
  c/build.sh           cc -O3 -std=c11 -Wall -Wextra -o bell bell.c -lm
  c/run-c.mjs          pop entry point: render mp3 (+ optional viz)
  c/compare.mjs        C-vs-JS modal-render parity harness
  bin/viz.mjs          3-D deforming-shell mp4 renderer
  materials.json       material presets (mirror of the C table)
  geometries.json      geometry presets (mirror of the C table)
```

## The physics

A bell is a **surface of revolution**, so expanding the displacement field in a
Fourier series in the angular coordinate θ — `u(s,θ) = Σ_m u_m(s)·cos(mθ)` —
decouples the full 3-D shell into one **1-D meridian problem per circumferential
order `m`**. For each `m` we assemble small stiffness `K` and mass `M` matrices
along the meridian (conical-frustum thin-shell elements, 4 DOF/node: meridional
`u`, circumferential `v`, normal `w`, meridional rotation `β`; selective reduced
integration on the transverse shear avoids locking) and solve the generalized
symmetric eigenproblem

```
K φ = ω² M φ
```

with a hand-written Cholesky + cyclic-Jacobi eigensolver (no LAPACK). The
eigenvalues are the modal frequencies; the eigenvectors are the meridian mode
shapes. The bell **tone** is the rim-flexural family `m ≥ 2` — the hum, prime,
tierce, quint and nominal are all `m=2`/`m=3` modes with differing numbers of
nodal circles. (`m=0` breathing and `m=1` whole-body sway are excluded: a rim
strike barely excites them and they are not part of the tone.)

- **Damping** comes from the material loss factor η: amplitude decay
  `δ = π·f·η`, so `τ = 1/δ`. Constant η ⇒ higher partials decay faster — exactly
  the bell-like behavior.
- **Strike** at the mouth rim sets each mode's initial amplitude from its
  participation (its normal shape sampled at the strike point).
- **Pitch** is set by `bell_retune()`, a uniform geometric scale that multiplies
  every modal frequency by the same factor so the strike note lands on the
  requested pitch while the inharmonic ratio set is preserved.

Material parameters behave correctly by construction: `K ∝ E`, `M ∝ ρ`, so pitch
scales as `√(E/ρ)`; thickness raises pitch; a larger bell lowers it as ~`1/size²`.

## Validation (`./bell --selftest`)

The eigensolver is gated against analytic limits before anything trusts it:

1. Jacobi on a known symmetric matrix.
2. Generalized eig on a known `(K, M)` pair.
3. **Free-free Euler-Bernoulli beam** vs `(βL)²√(EI/ρA L⁴)` — validates
   meridional bending (matches to ~1e-6).
4. **Cylinder → analytic in-plane ring flexural series**
   `f_m = (1/2π)·(m(m²−1)/√(m²+1))·(h/a²)·√(E/12ρ)` for `m=2,3,4` (with ν=0) —
   validates the hoop physics (**0.0 % error**).

`compare.mjs` separately confirms the C render equals a JS reimplementation of
the same mode table to ~1e-8 (with the strike transient + normalization off).

## CLI

```bash
./build.sh
./bell --note A4 --material bronze --geometry church --dur 8 --out bell.wav
./bell ... --modes bell-modes.json     # export geometry + modes + shapes (viz)
./bell ... --print-modes               # print the partial table
./bell --selftest
```

Flags: `--note` (name like `C#5` or a bare Hz), `--material`, `--geometry`,
`--dur`, `--vel`, `--sr`, `--maxm`, `--nostrike`, `--nonorm` (last two for parity).

### Pop pipeline

```bash
node run-c.mjs --note A4 --material glass --geometry church \
     --out bell.mp3 --master bell --viz bell.mp4
```

Materials: `bronze brass steel aluminum silver glass gold`
Geometries: `church handbell tubular bowl glass`

### Visualization

```bash
node bin/viz.mjs --modes bell-modes.json --audio bell.wav --out bell.mp4 \
     [--dur 9] [--fps 30] [--portrait]
```

A dependency-free software 3-D pipeline projects the deforming surface of
revolution (the displacement is the live sum of excited mode shapes as the sound
decays), with a HUD of material params, strike note and a per-partial spectrum
hued by circumferential order. Frames are BGRA → ffmpeg via
`pop/lib/preview-shared.mjs` (`spawnFFmpegEncode`), audio muxed in.

## Public API (`bell.h`)

```c
bell_geometry_preset(&g, "church");
bell_material_preset(&mat, "bronze");
bell_solve_modes(&g, &mat, /*max_m*/8, /*max_modes*/32, &modes);
bell_retune(&modes, 440.0);
bell_render(&modes, /*vel*/0.9, /*sr*/48000, /*dur*/8, L, R, nsamp);
bell_export_modes_json(&g, &mat, &modes, "modes.json");
```

## Reuse across the monorepo + AC OS (follow-up)

`bell.c`/`bell.h` are self-contained with the same shape as
`fedac/native/src/gm_synth.c`, which is compiled into the AC OS kernel
(`fedac/native/Makefile`) **and** symlinked into menuband
(`slab/menuband/Sources/CGMSynth/`). To make the bell live everywhere:

1. Add `bell.c` to the `fedac/native` Makefile SRCS; call `bell_solve_modes`
   once per voice config and `bell_render`-style modal playback in the audio mix
   (modes can be precomputed at note-on; the eigensolve is sub-millisecond per
   bell but is best cached).
2. Symlink `bell.c`/`bell.h` into a `slab/menuband/Sources/CBell/` target.
3. Optionally have `gm_synth.c`'s bell programs source their ratios from a
   solved `BellModes` instead of the hand-tabulated `gm_chromperc_programs`.

## Notes / honest limits

The element is a faceted-conical thin-shell (Kirchhoff/Mindlin) reduced model.
It reproduces the analytic ring and beam limits exactly and gives a genuinely
inharmonic, material-driven bell spectrum, but it is not a research-grade shell
solver: the absolute ratio set of a *specific* historic bell is the product of
centuries of profile tuning. Edit the `build_profile` control points (or the
geometry presets) to chase a particular bell's hum/prime/tierce/nominal.
