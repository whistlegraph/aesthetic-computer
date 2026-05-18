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
