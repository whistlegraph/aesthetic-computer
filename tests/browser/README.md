# Browser e2e (`tests/browser/`)

Drives a real local Chrome (puppeteer, already a repo dep) against a live AC
URL — types, presses keys, drags, screenshots, and asserts prompt state.

## Run

```bash
npm run test:browser            # headless, production (https://aesthetic.computer)
npm run test:browser:headed     # watch it (visible + slow-mo)
npm run test:browser:local      # against a local `npm run site` (localhost:8888)
```

Env overrides: `AC_TEST_URL`, `AC_HEADED=1`, `AC_SLOWMO=<ms>`, `AC_SHOT_DIR`.

Screenshots land in `tests/browser/__screens__/` (gitignored).

## No Paint 3.0: tests become tutorials

The canonical first-run journey has two synchronized surfaces:

- `nopaint-journey.test.mjs` is the fast functional e2e. It drives a seeded
  real browser and asserts the read-only `window.__acNoPaintTest()` contract.
- `captutor/screenplays/nopaint-first-painting.mjs` performs the same journey
  as a narrated Captutor production. Its required `check()` events make the
  encoded tutorial fail closed when the visual story does not prove the UX.

Puppeteer screenshots are always produced. On a macOS host with SlabMenubar's
Screen Recording permission, `AC_FRAME_RECEIPTS=1` also captures whole-display
Frame receipts at every milestone. Set `AC_FRAME_REQUIRED=1` when missing
native receipts should fail the run.

```bash
npm run test:nopaint:e2e:local   # starts/stops a static local site when necessary
npm run test:nopaint:e2e:film

# With a dedicated CDP Chrome already open:
CAPTUTOR_AC_URL=https://localhost:8888 \
CAPTUTOR_AC_MATCH=localhost:8888 \
node captutor/captutor.mjs render nopaint-first-painting
```

Each No Paint journey writes
`tests/browser/__screens__/nopaint-journey/performance.json` with proposal FPS
and frame-time percentiles, Long Tasks, JS heap use where Chrome supports it,
and input-to-next-proposal latency for No and Paint. Results are informational
by default because headed and headless hardware differ. Set
`NOPAINT_PERF_STRICT=1` to enforce budgets; override them with
`NOPAINT_PERF_FPS_FLOOR` (default 59.5, the practical sustained-60 gate),
`NOPAINT_PERF_P95_MS` (default 20), `NOPAINT_PERF_LATENCY_MS`, and
`NOPAINT_PERF_SAMPLE_MS`. The canonical receipt uses the recovered interface's
4:3 landscape proportion (1200×900); set `AC_VIEWPORT_WIDTH` and
`AC_VIEWPORT_HEIGHT` for the additional profiling matrix.

Use a dedicated browser profile for filmed tutorials. The journey deliberately
commits and saves a painting, so a personal AC painting store is the wrong test
fixture.

## How it works

The AC prompt renders to `<canvas>`, so there are no DOM nodes to assert on.
The harness sets `window.acDEBUG` before navigation; `prompt.mjs` then exposes
a **read-only** `window.__acPromptTest()` snapshot (autocomplete visibility,
active trigger, items + colors, `navigated`, input text, rolodex state). The
harness reads that for assertions and always captures screenshots.

Against a target **without** the hook (e.g. current production, before the
`universal-search` branch ships) state assertions soft-skip and the run
degrades to screenshot-only smoke — so point `AC_TEST_URL` at a local dev
server running this branch to get the full validation.

## Model

- `ac-harness.mjs` — `ACSession` (launch/boot/type/press/drag/shot/state) +
  a dependency-free `scenario()` / `report()` runner. One Chrome instance
  only (this laptop is 8 GB — never parallel).
- `prompt-search.test.mjs` — scenarios: boot, sigil `$`, universal bare-word
  search (color-typed), Enter-discipline + Tab-complete, rolodex drag,
  UNITICKER deprecation.
