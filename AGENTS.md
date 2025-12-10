# Repository Guidelines

## Project Structure & Module Organization
- `system/` holds the main web app (Netlify config, public assets, templates, scripts) and is the primary entry for feature work.  
- `session-server/` contains the real-time session backend (Jamsocket).  
- `shared/` offers reusable browser/server utilities; use these before adding new helpers.  
- `utilities/` scripts generate or sync assets (e.g., `generate-new-piece.mjs`).  
- `tests/` covers integration/performance; `spec/` is for KidLisp language specs; project-wide docs live at the repo root. Place new feature code next to its runtime (client in `system/`, server in `session-server/`).

## Build, Test, and Development Commands
Run from the repo root unless noted:
- `npm run aesthetic` — starts site, session server, edge services, Stripe mocks, and URL helper; use for full-stack local work.  
- `npm run site` — launches the client stack only (`system/`).  
- `npm test` — runs the top-level smoke/integration tests.  
- `npm run test:perf` or `npm run test:perf:chrome` — performance harnesses in `tests/performance/`.  
- `npm run test:kidlisp` — watches and runs KidLisp specs in `spec/`.  
- `npm run url` — prints your local tunnel URL for hitting the app in browsers/devices.

## Coding Style & Naming Conventions
- JavaScript/TypeScript modules use ESM (`.mjs`); prefer 2-space indentation and trailing commas.  
- Run Prettier where available (`npx prettier --write <files>`); respect existing file conventions (some legacy scripts mix shell/Fish).  
- Name new pieces and assets with lowercase-hyphenated paths; colocate tests as `<name>.test.mjs` near related code or under `tests/`.  
- Keep configuration in `.env`-style files out of version control; use sample files if needed.

## Testing Guidelines
- Add focused `.test.mjs` files; use Jasmine for KidLisp specs and Node test runners for integration.  
- Prefer running `npm test` before PRs; include performance runs when touching rendering/audio paths.  
- For perf tests, document baseline numbers and hardware in the PR description.  
- Snapshot or fixture data should live in `tests/` subfolders; avoid large binaries in git.

## Commit & Pull Request Guidelines
- Follow the existing history: short, action-led subjects (`fix: ...`, `docs: ...`, or descriptive phrases).  
- PRs should describe the change, runtime targets (web/session), and how to reproduce/test.  
- Link related issues or user-facing commands; add screenshots or clips for UI/visual changes.  
- Keep commits logically scoped; prefer small, reviewable units over large drops.  
- Ensure `npm test` (and relevant perf or KidLisp suites) are green before requesting review.

## Security & Configuration Tips
- Never commit secrets; use environment variables and the Netlify/Jamsocket env management scripts in `system/`.  
- When syncing assets, use `npm run assets:sync:down`/`up` and verify paths under `system/public/assets`.  
- Treat production endpoints in scripts as sensitive; dry-run changes against local or dev tunnels first.
