# PR Review Cockpit

A small standalone Electron desktop app that hosts many GitHub PRs as a grid of
**live webviews**, lets you **focus** one, and **drive** it.

It replaces a flaky window-tiling script (`open-prs`) that hit Chrome's
~500px minimum-window-width limit (tiles overlapped) and couldn't use iframes
(GitHub sends `X-Frame-Options: DENY`). Electron `<webview>` tags are real
browser contexts, so they load github.com fine and we size them freely with no
minimum — panes can go far below Chrome's window minimum with zero overlap.

## Run

```bash
cd /Users/jas/pr-cockpit
npm install
npm start
```

Requires the [`gh` CLI](https://cli.github.com/) authenticated on this machine
(`gh auth login`). The first time a PR webview loads you may need to sign in to
github.com inside the app; the login persists (shared `persist:github`
session partition) across tiles and restarts.

## Configure

Copy the example config and edit it (it is gitignored, so your repo/author
never gets committed):

```bash
cp cockpit.config.example.json cockpit.config.json
```

```json
{
  "repo": "owner/repo",
  "author": "your-login",
  "me": "your-login",
  "state": "open",
  "limit": 100,
  "onlyToday": false
}
```

`author: ""` fetches **all** open PRs. Environment variables override the file
(`COCKPIT_REPO`, `COCKPIT_AUTHOR`, `COCKPIT_ME`, `COCKPIT_STATE`,
`COCKPIT_LIMIT`, `COCKPIT_ONLY_TODAY`).

On launch (and on **Refresh**) the main process shells out to:

```
gh pr list --repo <repo> --author <author> --state <state> \
  --json number,title,url,reviewDecision,isDraft,statusCheckRollup --limit <limit>
```

Each PR gets a derived CI status from `statusCheckRollup`:

- **red** — any check FAILURE / timed-out / cancelled
- **yellow** — any check queued / in-progress / pending
- **green** — checks present and all passing
- **neutral** — no checks

## Features

### Grid mode

- CSS grid of tiles, one `<webview>` per PR.
- Slim header per tile: `#<num> · <title>`, colored by CI status
  (green/yellow/red/neutral), plus `draft` / `✓ approved` badges.
- **Cols** control (2–6) — resize panes freely, no overlap.
- **Sort**: Newest (desc PR number) / CI status / Draft-last.

### Focus mode

- Click a tile header (or its `#num` placeholder) to focus.
- Focused PR expands to the large right pane; other PRs collapse to a thin
  scrollable left rail of header-only **mini-strips**.
- Focused tile gets a blue highlighted border.
- **Esc** or the **⤺ Grid** button exits. Click any mini-strip to switch focus.

### Drive the focused PR

Focus-mode toolbar:

- **Open in browser** — `shell.openExternal(pr.url)` in your real browser.
- **Reload** — `webview.reload()`.
- **Run JS** — text field that runs arbitrary JS in the focused PR page via
  `webview.executeJavaScript(...)` and shows the result. This is the seam for
  future CDP/automation.

## Performance / lazy-loading choice

10+ live Chromium webviews are heavy, so:

- Every PR gets a tile, but each tile's `<webview>` `src` is **lazy** — attached
  only when the tile first becomes live. Unloaded tiles show a clickable
  `#num` placeholder.
- In **grid mode** the first `EAGER_LOAD_COUNT` (default **6**) tiles auto-load
  for instant usefulness; the rest load on click.
- In **focus mode** only the focused webview is live; other PRs are
  header-only mini-strips (their `<webview>` body is CSS-hidden but the element
  and its loaded state persist, so re-focusing an already-loaded PR is instant).

Tune `EAGER_LOAD_COUNT` at the top of `renderer/renderer.js`.

## Files

- `package.json` — deps + `start` script.
- `main.js` — main process: window, `gh` IPC, external-open, CI-status derivation.
- `preload.js` — context-isolated bridge (`window.cockpit`).
- `renderer/index.html` / `style.css` / `renderer.js` — the UI.

## TODO / v2 ideas

- Deeper CDP automation off the Run JS seam (scripted review flows).
- Review actions via the GitHub API/`gh` (approve / request-changes / comment)
  from the focus toolbar.
- Auto-refresh on an interval; desktop notifications on CI status change.
- Per-PR screenshot thumbnails in the rail instead of header-only strips.
- Persist column/sort/focus preferences between runs.
