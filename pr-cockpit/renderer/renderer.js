// PR Review Cockpit — renderer.
//
// Owns the <webview> grid, focus mode, sorting, and the focused-PR toolbar.
//
// PERF DESIGN (documented for future me):
//   - Every PR gets a tile, but a tile's <webview> src is LAZY: we only attach
//     the real github.com src the first time a tile becomes visible/live. Until
//     then the tile shows a clickable placeholder. This keeps startup cheap even
//     with 30+ PRs.
//   - In GRID mode we load webviews eagerly-on-first-render but capped: the first
//     N tiles auto-load, the rest wait for a click (see EAGER_LOAD_COUNT). This
//     is a balance — enough live previews to be useful, not 50 Chromium tabs.
//   - In FOCUS mode only the focused webview is live. The other PRs collapse to
//     header-only "mini-strips" in the left rail (their <webview> body is hidden
//     via CSS but the element — and thus its loaded state — is preserved, so
//     switching focus back is instant if it was loaded before).

"use strict";

const EAGER_LOAD_COUNT = 6; // how many grid tiles auto-load their webview

const stage = document.getElementById("stage");
const meta = document.getElementById("meta");
const statusEl = document.getElementById("status");
const backBtn = document.getElementById("backBtn");
const focusToolbar = document.getElementById("focusToolbar");
const focusLabel = document.getElementById("focusLabel");
const colsSelect = document.getElementById("colsSelect");
const sortSelect = document.getElementById("sortSelect");
const jsInput = document.getElementById("jsInput");
const jsResult = document.getElementById("jsResult");
const reviewBody = document.getElementById("reviewBody");
const approveBtn = document.getElementById("approveBtn");
const reqChangesBtn = document.getElementById("reqChangesBtn");
const commentBtn = document.getElementById("commentBtn");

const state = {
  prs: [],
  loaded: new Set(), // PR numbers whose webview src is attached
  tiles: new Map(), // PR number -> { el, header, webview, placeholder }
  focused: null, // focused PR number or null
  cols: 3,
  sort: "priority",
};

// --- helpers -----------------------------------------------------------------

// A short "what are we looking at" label for the top-right meta area.
function scopeLabel(config) {
  if (!config) return "";
  const who = config.author
    ? `${config.repo} · @${config.author}`
    : `${config.repo} · all open`;
  return config.onlyToday ? `${who} · started today` : who;
}

function setStatus(msg, isError = false) {
  statusEl.textContent = msg || "";
  statusEl.classList.toggle("error", !!isError);
  if (msg && !isError) {
    clearTimeout(setStatus._t);
    setStatus._t = setTimeout(() => {
      if (statusEl.textContent === msg) statusEl.textContent = "";
    }, 3500);
  }
}

// --- settledness -------------------------------------------------------------
// A short human label + css-class for "where is this PR in its lifecycle?"
// Order matters: the first matching rule wins.
function settledness(pr) {
  const ci = pr.ciStatus;
  const rd = pr.reviewDecision;
  const mg = pr.mergeable;

  if (pr.isDraft) return { label: "draft", cls: "draft" };
  if (mg === "CONFLICTING") return { label: "conflicting", cls: "conflicting" };
  if (rd === "CHANGES_REQUESTED")
    return { label: "changes req", cls: "changes-req" };
  if (ci === "fail") return { label: "ci failing", cls: "ci-failing" };
  if (rd === "APPROVED" && mg === "MERGEABLE" && ci === "ok")
    return { label: "ready to merge", cls: "ready-merge" };
  if (
    mg === "MERGEABLE" &&
    (ci === "ok" || ci === "run") &&
    rd === "REVIEW_REQUIRED"
  )
    return { label: "ready for review", cls: "ready-review" };
  if (mg === "UNKNOWN") return { label: "stale?", cls: "stale" };
  return { label: "open", cls: "open" };
}

// --- review priority ---------------------------------------------------------
// Higher score = should be reviewed sooner. Tiebreak by updatedAt desc.
function reviewPriority(pr) {
  const teammate = !pr.isMine;
  const ci = pr.ciStatus;
  const rd = pr.reviewDecision;
  const mg = pr.mergeable;
  const mergeableCi = ci === "ok" || ci === "run";

  if (teammate && !pr.isDraft && mg === "MERGEABLE" && mergeableCi &&
      rd === "REVIEW_REQUIRED")
    return 100; // their PR is waiting on review
  if (teammate && !pr.isDraft && mg === "MERGEABLE" && ci === "fail")
    return 90; // teammate, mergeable, CI red (e.g. security)
  if (pr.isMine && !pr.isDraft && mg === "MERGEABLE" && ci === "ok")
    return 80; // my ready-to-land
  if (pr.isMine && !pr.isDraft && ci === "fail")
    return 70; // my CI red — fix
  if (!pr.isDraft && (mg === "CONFLICTING" || rd === "CHANGES_REQUESTED"))
    return 60; // needs work
  if (pr.isDraft) return 30;
  if (mg === "UNKNOWN") return 10; // stale
  return 40;
}

function updatedDesc(a, b) {
  // newer updatedAt first; missing timestamps sort last
  return (b.updatedAt || "").localeCompare(a.updatedAt || "");
}

function sortPRs(prs, mode) {
  const arr = [...prs];
  if (mode === "priority") {
    arr.sort(
      (a, b) =>
        reviewPriority(b) - reviewPriority(a) || updatedDesc(a, b)
    );
  } else if (mode === "newest") {
    arr.sort((a, b) => b.number - a.number);
  } else if (mode === "status") {
    const rank = { fail: 0, run: 1, none: 2, ok: 3 };
    arr.sort(
      (a, b) =>
        (rank[a.ciStatus] ?? 9) - (rank[b.ciStatus] ?? 9) ||
        b.number - a.number
    );
  } else if (mode === "draft-last") {
    arr.sort(
      (a, b) =>
        Number(a.isDraft) - Number(b.isDraft) || b.number - a.number
    );
  }
  return arr;
}

// --- status hue injection into the PR webview --------------------------------
// We inject CSS into each PR's <webview> so the live GitHub page itself takes on
// its CI-status hue — the WHOLE pr reads as green/yellow/red, not just the tile
// chrome. mix-blend-mode:color shifts hue while preserving luminance, so text
// stays readable. Bump TINT_ALPHA for a stronger shift.

const TINT_ALPHA = 0.16; // 0..1 — how hard the PR page takes its status hue

const STATUS_HUE = {
  ok: { c: "#3fb950", glow: "rgba(63,185,80,0.14)" },
  run: { c: "#d29922", glow: "rgba(210,153,34,0.14)" },
  fail: { c: "#f85149", glow: "rgba(248,81,73,0.16)" },
  none: { c: "#6e7681", glow: "rgba(110,118,129,0.10)" },
};

function statusTintCSS(ciStatus) {
  const s = STATUS_HUE[ciStatus] || STATUS_HUE.none;
  // neutral gets only a frame — a gray "color" blend would desaturate the page.
  const hueLayer =
    ciStatus === "none"
      ? ""
      : `html::before{content:"";position:fixed;inset:0;pointer-events:none;` +
        `z-index:2147483646;background:${s.c};mix-blend-mode:color;` +
        `opacity:${TINT_ALPHA};}`;
  const frame =
    `html::after{content:"";position:fixed;inset:0;pointer-events:none;` +
    `z-index:2147483647;box-shadow:inset 0 0 0 2px ${s.c},inset 0 0 20px ${s.glow};}`;
  return hueLayer + frame;
}

// Attach a dom-ready handler that (re)injects the tint on every load/navigation.
function injectStatusTint(webview, ciStatus) {
  webview.addEventListener("dom-ready", async () => {
    try {
      if (webview._tintKey) {
        await webview.removeInsertedCSS(webview._tintKey).catch(() => {});
      }
      webview._tintKey = await webview.insertCSS(statusTintCSS(ciStatus));
    } catch {
      /* webview not ready / navigated away — ignore */
    }
  });
}

// --- tile construction -------------------------------------------------------

function makeTile(pr) {
  const el = document.createElement("div");
  el.className = "tile";
  el.classList.add(`st-${pr.ciStatus}`); // color-code the whole tile by CI status
  el.dataset.number = String(pr.number);

  const header = document.createElement("div");
  header.className = `tile-header st-${pr.ciStatus}`;

  const num = document.createElement("span");
  num.className = "num";
  num.textContent = `#${pr.number}`;

  const title = document.createElement("span");
  title.className = "title";
  title.textContent = pr.title;
  title.title = pr.title;

  const badges = document.createElement("span");
  badges.className = "badges";

  // settledness pill — the headline "where is this PR?" marker.
  const settle = settledness(pr);
  const pill = document.createElement("span");
  pill.className = `pill settle-${settle.cls}`;
  pill.textContent = settle.label;
  badges.appendChild(pill);

  // author login (+ MINE marker for my own PRs)
  const author = document.createElement("span");
  author.className = "badge author" + (pr.isMine ? " mine" : "");
  author.textContent = pr.isMine
    ? `MINE · ${pr.authorLogin}`
    : pr.authorLogin;
  author.title = pr.authorLogin;
  badges.appendChild(author);

  if (pr.approved) {
    const a = document.createElement("span");
    a.className = "badge";
    a.textContent = "✓ approved";
    badges.appendChild(a);
  }

  header.append(num, title, badges);
  header.addEventListener("click", () => enterFocus(pr.number));

  const body = document.createElement("div");
  body.className = "tile-body";

  // webview element (created up front, src attached lazily)
  const webview = document.createElement("webview");
  webview.setAttribute("partition", "persist:github"); // shared login session
  webview.setAttribute("allowpopups", "true");
  body.appendChild(webview);
  injectStatusTint(webview, pr.ciStatus); // hue-shift the live PR page by status

  const placeholder = document.createElement("div");
  placeholder.className = "placeholder";
  const big = document.createElement("div");
  big.className = "big";
  big.textContent = `#${pr.number}`;
  const hint = document.createElement("div");
  hint.textContent = "click to load PR";
  placeholder.append(big, hint);
  placeholder.addEventListener("click", (e) => {
    e.stopPropagation();
    loadWebview(pr.number);
  });
  body.appendChild(placeholder);

  el.append(header, body);

  const rec = { el, header, webview, placeholder };
  state.tiles.set(pr.number, rec);
  return rec;
}

function loadWebview(number) {
  const rec = state.tiles.get(number);
  const pr = state.prs.find((p) => p.number === number);
  if (!rec || !pr) return;
  if (!state.loaded.has(number)) {
    rec.webview.setAttribute("src", pr.url);
    state.loaded.add(number);
  }
  rec.placeholder.classList.add("hidden");
}

// --- rendering ---------------------------------------------------------------

function render() {
  stage.style.setProperty("--cols", String(state.cols));

  if (state.focused == null) {
    renderGrid();
  } else {
    renderFocus();
  }
}

function renderGrid() {
  stage.className = "grid";
  backBtn.classList.add("hidden");
  focusToolbar.classList.add("hidden");
  stage.replaceChildren();

  const sorted = sortPRs(state.prs, state.sort);
  sorted.forEach((pr, i) => {
    const rec = state.tiles.get(pr.number) || makeTile(pr);
    rec.el.classList.remove("mini", "focused");
    // restore placeholder visibility for tiles not yet loaded
    if (!state.loaded.has(pr.number)) {
      rec.placeholder.classList.remove("hidden");
    }
    stage.appendChild(rec.el);
    // eager-load the first few for instant usefulness
    if (i < EAGER_LOAD_COUNT) loadWebview(pr.number);
  });
}

function renderFocus() {
  stage.className = "focus";
  backBtn.classList.remove("hidden");
  focusToolbar.classList.remove("hidden");
  stage.replaceChildren();

  const rail = document.createElement("div");
  rail.className = "rail";
  const mainPane = document.createElement("div");
  mainPane.className = "main-pane";

  const sorted = sortPRs(state.prs, state.sort);
  for (const pr of sorted) {
    const rec = state.tiles.get(pr.number) || makeTile(pr);
    if (pr.number === state.focused) {
      rec.el.classList.add("focused");
      rec.el.classList.remove("mini");
      mainPane.appendChild(rec.el);
      loadWebview(pr.number);
    } else {
      rec.el.classList.add("mini");
      rec.el.classList.remove("focused");
      rail.appendChild(rec.el);
    }
  }

  stage.append(rail, mainPane);

  const focusedPr = state.prs.find((p) => p.number === state.focused);
  focusLabel.textContent = focusedPr
    ? `#${focusedPr.number} · ${focusedPr.title}`
    : "";
}

// --- focus mode transitions --------------------------------------------------

function enterFocus(number) {
  state.focused = number;
  render();
}

function exitFocus() {
  state.focused = null;
  render();
}

function focusedWebview() {
  if (state.focused == null) return null;
  const rec = state.tiles.get(state.focused);
  return rec ? rec.webview : null;
}

// --- toolbar wiring ----------------------------------------------------------

document.getElementById("openExtBtn").addEventListener("click", async () => {
  const pr = state.prs.find((p) => p.number === state.focused);
  if (!pr) return;
  await window.cockpit.openExternal(pr.url);
});

document.getElementById("reloadBtn").addEventListener("click", () => {
  const wv = focusedWebview();
  if (wv) {
    try {
      wv.reload();
    } catch (e) {
      setStatus("Reload failed: " + e.message, true);
    }
  }
});

async function runJs() {
  const wv = focusedWebview();
  if (!wv) return;
  const code = jsInput.value.trim();
  if (!code) return;
  jsResult.textContent = "running…";
  try {
    const result = await wv.executeJavaScript(code, false);
    let out;
    try {
      out = JSON.stringify(result);
    } catch {
      out = String(result);
    }
    jsResult.textContent = "→ " + (out ?? "undefined");
  } catch (e) {
    jsResult.textContent = "✗ " + e.message;
  }
}

document.getElementById("runJsBtn").addEventListener("click", runJs);
jsInput.addEventListener("keydown", (e) => {
  if (e.key === "Enter") runJs();
});

// --- real GitHub review actions (via gh CLI, not the webview) ----------------
// These POST publicly to GitHub, so every one asks for an explicit confirm that
// names the action + PR number first. The buttons disable while a request is in
// flight so a double-click can't fire twice.

const REVIEW_BTNS = [approveBtn, reqChangesBtn, commentBtn];

function setActionsBusy(busy) {
  for (const b of REVIEW_BTNS) b.disabled = busy;
  reviewBody.disabled = busy;
}

// kind: "approve" | "request-changes" | "comment"
async function fireReviewAction(kind) {
  if (state.focused == null) return;
  const number = state.focused;
  const body = reviewBody.value.trim();

  // request-changes and comment require a non-empty body.
  if ((kind === "request-changes" || kind === "comment") && !body) {
    setStatus(
      kind === "comment"
        ? "Comment needs a body — type it in the review field first."
        : "Request-changes needs a body — type it in the review field first.",
      true
    );
    reviewBody.focus();
    return;
  }

  const verb =
    kind === "approve"
      ? "Approve"
      : kind === "request-changes"
        ? "Request changes on"
        : "Comment on";
  // Real, public GitHub action — require an explicit confirm.
  const bodyNote = body ? `\n\nBody:\n${body}` : "";
  if (!window.confirm(`${verb} #${number}?${bodyNote}`)) return;

  setActionsBusy(true);
  setStatus(`${verb} #${number}…`);
  let res;
  try {
    res = await window.cockpit.prAction({ kind, number, body });
  } catch (e) {
    setStatus(`Action failed: ${e.message}`, true);
    setActionsBusy(false);
    return;
  }
  setActionsBusy(false);

  if (!res || !res.ok) {
    setStatus(`Action failed: ${(res && res.error) || "unknown"}`, true);
    return;
  }
  setStatus(`${verb} #${number} ✓ ${res.output || "done"}`);
  if (kind !== "approve" || body) reviewBody.value = "";
}

approveBtn.addEventListener("click", () => fireReviewAction("approve"));
reqChangesBtn.addEventListener("click", () =>
  fireReviewAction("request-changes")
);
commentBtn.addEventListener("click", () => fireReviewAction("comment"));

backBtn.addEventListener("click", exitFocus);
document.addEventListener("keydown", (e) => {
  if (e.key === "Escape" && state.focused != null) exitFocus();
});

colsSelect.addEventListener("change", () => {
  state.cols = Number(colsSelect.value);
  render();
});
sortSelect.addEventListener("change", () => {
  state.sort = sortSelect.value;
  render();
});

document.getElementById("refreshBtn").addEventListener("click", refresh);

// --- GitHub auth: sign in once (Path B) + import from Chrome (Path A) ---------

// Reload every tile whose webview is live (after a login changes the session).
function reloadAllWebviews() {
  for (const [number, rec] of state.tiles) {
    if (state.loaded.has(number) && rec.webview) {
      try {
        rec.webview.reload();
      } catch {
        /* ignore a webview that isn't ready */
      }
    }
  }
}

// Path B — open a full-window GitHub login in the SAME persist:github session
// the tiles share, so signing in once here authenticates (and persists for)
// every tile and every future launch.
function openGithubLogin() {
  if (document.getElementById("loginOverlay")) return;

  const overlay = document.createElement("div");
  overlay.id = "loginOverlay";

  const bar = document.createElement("div");
  bar.className = "login-bar";

  const titleEl = document.createElement("span");
  titleEl.className = "login-title";
  titleEl.textContent = "Sign in to GitHub";

  const hint = document.createElement("span");
  hint.className = "login-hint";
  hint.textContent =
    "This login is shared by every PR tile and is remembered across restarts.";

  const spacer = document.createElement("span");
  spacer.className = "spacer";

  const done = document.createElement("button");
  done.textContent = "✓ Done — reload PRs";
  done.addEventListener("click", () => {
    overlay.remove();
    reloadAllWebviews();
    setStatus("Signed in — reloaded tiles.");
  });

  bar.append(titleEl, hint, spacer, done);

  const wv = document.createElement("webview");
  wv.className = "login-webview";
  wv.setAttribute("partition", "persist:github");
  wv.setAttribute("allowpopups", "true");
  wv.setAttribute("src", "https://github.com/login");

  overlay.append(bar, wv);
  document.body.appendChild(overlay);
}

document.getElementById("signinBtn").addEventListener("click", openGithubLogin);

// Path A — pull the existing github.com login out of Chrome via the main
// process (gated at runtime by the macOS keychain prompt). Degrades gracefully
// if the Chrome-import glue isn't wired into main.js yet.
document.getElementById("syncBtn").addEventListener("click", async () => {
  if (!window.cockpit || typeof window.cockpit.syncGithub !== "function") {
    setStatus("Chrome import isn't available in this build.", true);
    return;
  }
  setStatus("Importing your GitHub login from Chrome…");
  let res;
  try {
    res = await window.cockpit.syncGithub();
  } catch (e) {
    setStatus("Chrome import failed: " + e.message, true);
    return;
  }
  if (!res || !res.ok) {
    setStatus("Chrome import failed: " + ((res && res.error) || "unknown"), true);
    return;
  }
  setStatus(
    `Imported ${res.set}/${res.total} cookies from Chrome “${res.profile}”. Reloading…`
  );
  reloadAllWebviews();
});

// --- data load ---------------------------------------------------------------

async function refresh() {
  setStatus("Loading PRs via gh…");
  const res = await window.cockpit.listPRs();
  if (!res.ok) {
    setStatus(res.error, true);
    meta.textContent = res.config ? scopeLabel(res.config) : "";
    return;
  }

  // Reset tile cache (PRs may have changed). Keep it simple: rebuild.
  state.tiles.clear();
  state.loaded.clear();
  state.focused = null;
  state.prs = res.prs;

  meta.textContent = `${scopeLabel(res.config)} · ${res.prs.length} PRs`;
  setStatus(res.prs.length ? "" : "No open PRs found.");
  render();
}

// In the horizontal band (grid mode), translate a vertical mouse wheel into
// sideways scroll so mouse users can move through columns (trackpads already
// swipe horizontally; hold Shift or scroll horizontally to bypass).
stage.addEventListener(
  "wheel",
  (e) => {
    if (state.focused != null) return; // focus mode scrolls normally
    if (e.shiftKey || e.deltaY === 0) return;
    stage.scrollLeft += e.deltaY;
    e.preventDefault();
  },
  { passive: false }
);

// boot
refresh();
