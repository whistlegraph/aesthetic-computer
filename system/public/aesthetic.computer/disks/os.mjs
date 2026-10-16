// os, 2026.05.15
// FedAC OS — image options, download, and release history.

const OVEN = "https://oven-edge.aesthetic-computer.workers.dev";
const OVEN_WS = "wss://oven.aesthetic.computer/ws";
const OVEN_ORIGIN = "https://oven.aesthetic.computer";
const IMAGE_BASE = OVEN + "/os/latest.img";
function OVEN_BASE() { return OVEN; }
function RELEASES_URL() { return OVEN + "/os-releases"; }
function OVEN_WS_URL() { return OVEN_WS; }
function templateImageUrl() {
  const base = IMAGE_BASE;
  if (variantIdx === 0) return base;
  // CL variant: replace 'native-notepat' with 'cl-native-notepat'
  return base.replace("native-notepat", "cl-native-notepat");
}
const CONFIG_MARKER = '{"handle":"","piece":"notepat","sub":"","email":""}';
const CONFIG_PAD = 4096;
const BOOT_PIECES = ["notepat", "prompt", "chat", "laer-klokken"];
const VARIANTS = [
  { id: "c", label: "C" },
  { id: "cl", label: "Common Lisp" },
];

let releases = null;
let loading = true;
let error = null;
let handle = null;
let token = null;
let downloading = false;
let downloadProgress = 0;
let downloadMB = 0;
let downloadTotalMB = 0;
let downloadStatus = "";
let downloadBtn = null;
let bootPieceIdx = 0; // index into BOOT_PIECES
let bootBtn = null;   // "boot to: X" button
let variantIdx = 0;   // index into VARIANTS (0=C, 1=Common Lisp)
let variantBtn = null; // "build: C" selector
let clAvailable = false; // true once we confirm the CL image exists on CDN
let wifiEnabled = true;
let wifiBtn = null;   // "internet: ON/OFF" toggle
let scrollY = 0;
let dragged = false;
let themeBtn = null;
let darkModeOn = false;
let appearanceOverride = null;
let storeRef = null;
let totalContentH = 0;
let buildsViewH = 0;
let headerBottom = 0;
let dlFn = null;
let ovenWs = null;
let wsReconnectTimer = null;
let npRef = null; // stored needsPaint for live updates

// Device token status
let hasClaude = null; // null=loading, true/false
let hasGit = null;
let setupBtn = null; // "set up tokens" button shown when missing
let showTokenHint = false;

// Collapsible sections
let installExpanded = false;
let installBtn = null;
let buildsExpanded = false;
let buildsBtn = null;
const DEFAULT_BUILDS_VISIBLE = 5;

// Build telemetry
let activeBuild = null;    // { id, status, stage, percent, ref, error }
let buildLogLines = [];    // last few log lines
const MAX_BUILD_LOGS = 20;
let buildPollTimer = null;

// Color palettes for dark/light mode (deprecated builds use red tones)
const scheme = {
  dark: {
    bg: [10, 12, 18],
    name: [80, 255, 140],
    nameOld: [50, 80, 65],
    hash: [255, 180, 100],
    hashOld: [120, 90, 55],
    date: [150, 160, 180],
    error: [255, 140, 130],
    row: [18, 22, 30],
    rowAlt: [23, 28, 37],
    msg: [200, 200, 220],
    msgOld: [80, 85, 100],
    handle: [180, 150, 255],
    handleOld: [80, 70, 110],
    current: [80, 255, 140],
    bar: [40, 50, 70],
    loginHint: [160, 150, 180],
    progress: [100, 180, 255],
    progressBg: [40, 45, 65],
    bootLabel: [140, 160, 200],
    bootPiece: [255, 200, 120],
    instHeader: [180, 200, 255],
    instText: [155, 165, 185],
    instKey: [120, 140, 180],
    divider: [30, 35, 50],
    secDlBg: [12, 18, 14],       // download section bg (green tint)
    secInstBg: [12, 14, 22],     // install section bg (blue tint)
    secBuildBg: [14, 12, 20],    // builds section bg (purple tint)
    dlBtnBg: [20, 60, 30],
    dlBtnBorder: [40, 120, 50],
    dlBtnText: [80, 255, 140],
    dlBtnHoverBg: [40, 100, 50],
    dlBtnHoverBorder: [60, 160, 70],
    bootBtnBg: [30, 35, 55],
    bootBtnBorder: [50, 60, 90],
    bootBtnHoverBg: [45, 50, 75],
    bootBtnHoverBorder: [65, 75, 110],
    depName: [120, 50, 50],
    depHash: [100, 45, 40],
    depMsg: [80, 40, 45],
    depHandle: [90, 50, 70],
    depBar: [60, 30, 30],
    depStrike: [120, 40, 40],
  },
  light: {
    bg: [248, 248, 244],
    name: [20, 130, 60],
    nameOld: [100, 140, 110],
    hash: [180, 110, 30],
    hashOld: [150, 120, 80],
    date: [85, 95, 110],
    error: [165, 40, 35],
    row: [237, 239, 234],
    rowAlt: [244, 245, 240],
    msg: [40, 40, 50],
    msgOld: [130, 130, 145],
    handle: [110, 70, 200],
    handleOld: [140, 120, 170],
    current: [20, 130, 60],
    bar: [200, 205, 215],
    loginHint: [100, 85, 120],
    progress: [40, 120, 200],
    progressBg: [210, 215, 225],
    bootLabel: [80, 100, 150],
    bootPiece: [110, 75, 15],
    instHeader: [50, 60, 100],
    instText: [100, 110, 130],
    instKey: [60, 80, 130],
    divider: [210, 215, 225],
    secDlBg: [232, 242, 234],
    secInstBg: [232, 234, 244],
    secBuildBg: [236, 232, 242],
    dlBtnBg: [210, 240, 215],
    dlBtnBorder: [80, 170, 100],
    dlBtnText: [20, 100, 40],
    dlBtnHoverBg: [180, 230, 190],
    dlBtnHoverBorder: [60, 150, 80],
    bootBtnBg: [220, 225, 240],
    bootBtnBorder: [160, 170, 200],
    bootBtnHoverBg: [200, 210, 230],
    bootBtnHoverBorder: [130, 145, 180],
    depName: [180, 100, 100],
    depHash: [170, 110, 100],
    depMsg: [170, 120, 120],
    depHandle: [160, 110, 140],
    depBar: [200, 160, 160],
    depStrike: [180, 90, 90],
  },
};

let uiRef = null; // store ui for later button creation

function fetchReleases(ui, needsPaint) {
  fetch(RELEASES_URL())
    .then((r) => r.json())
    .then((data) => {
      releases = data;
      error = null;
      loading = false;
      if (handle && token && ui) makeButtons(ui);
      // Log recent builds to console
      const builds = data?.releases || [];
      console.log(`[os] ${builds.length} builds loaded:`);
      builds.slice(0, 5).forEach((b, i) => {
        const dep = b.deprecated ? " [deprecated]" : " [latest]";
        console.log(`  ${i}: ${b.name} ${(b.git_hash||"").slice(0,7)} @${b.handle || "?"}${dep} — ${b.commit_msg || ""}`);
      });
      needsPaint();
    })
    .catch((err) => {
      if (!releases) {
        error = err.message;
        loading = false;
        needsPaint();
      }
      console.error("[os] Fetch failed:", err);
    });
}

function fetchBuildStatus(needsPaint) {
  fetch(OVEN_BASE() + "/native-build")
    .then(r => r.json())
    .then(data => {
      if (data.active) {
        activeBuild = data.active;
        // Fetch log lines for active build
        if (data.activeJobId) {
          fetch(OVEN_BASE() + "/native-build/" + data.activeJobId + "?logs=true&tail=10")
            .then(r => r.json())
            .then(detail => {
              if (detail.logs) {
                buildLogLines = detail.logs.slice(-MAX_BUILD_LOGS).map(l => l.line || l);
              }
              needsPaint();
            })
            .catch(() => needsPaint());
        } else {
          needsPaint();
        }
      } else {
        if (activeBuild) { activeBuild = null; buildLogLines = []; needsPaint(); }
      }
    })
    .catch(() => {});
}

function connectOvenWs(ui, needsPaint) {
  if (ovenWs) { try { ovenWs.close(); } catch (_) {} }
  if (wsReconnectTimer) { clearTimeout(wsReconnectTimer); wsReconnectTimer = null; }

  try {
    ovenWs = new WebSocket(OVEN_WS_URL());
  } catch (err) {
    console.error("[os] WS connect error:", err);
    wsReconnectTimer = setTimeout(() => connectOvenWs(ui, needsPaint), 10000);
    return;
  }

  ovenWs.onopen = () => console.log("[os] Oven WS connected");

  ovenWs.onmessage = (ev) => {
    try {
      const msg = JSON.parse(ev.data);
      if (msg.type === "os:new-build" && msg.releases) {
        console.log("[os] Live build push:", msg.releases?.releases?.[0]?.name);
        releases = msg.releases;
        if (handle && token && ui) makeButtons(ui);
        needsPaint();
      }
      if (msg.type === "os:build-progress" && msg.build) {
        const b = msg.build;
        // Lightweight per-line message (has .line but no full snapshot fields)
        if (b.line && !b.status) {
          if (buildLogLines.length >= MAX_BUILD_LOGS) buildLogLines.shift();
          buildLogLines.push(b.line);
          // Update stage/percent from lightweight msg
          if (activeBuild) {
            if (b.stage) activeBuild.stage = b.stage;
            if (b.percent) activeBuild.percent = b.percent;
          }
          needsPaint();
          return;
        }
        // Full snapshot
        activeBuild = b;
        if (b.recentLines) {
          buildLogLines = b.recentLines.slice(-MAX_BUILD_LOGS);
        }
        if (b.status === "success" || b.status === "failed" || b.status === "cancelled") {
          // Build done — refresh releases list
          setTimeout(() => {
            activeBuild = null;
            buildLogLines = [];
            fetchReleases(ui, needsPaint);
            needsPaint();
          }, 5000);
        }
        needsPaint();
      }
    } catch (_) {}
  };

  ovenWs.onclose = () => {
    console.log("[os] Oven WS closed, reconnecting in 5s...");
    ovenWs = null;
    wsReconnectTimer = setTimeout(() => connectOvenWs(ui, needsPaint), 5000);
  };

  ovenWs.onerror = () => {}; // onclose will fire after this
}

async function boot({ user, handle: getHandle, api, ui, needsPaint, dark, store, darkMode }) {
  storeRef = store;
  const savedAppearance = await store?.retrieve("os-appearance");
  appearanceOverride = ["light", "dark"].includes(savedAppearance) ? savedAppearance : null;
  darkModeOn = appearanceOverride ? appearanceOverride === "dark" : !!dark;
  if (appearanceOverride) darkMode?.(darkModeOn);
  themeBtn = new ui.TextButton(darkModeOn ? "light" : "dark", { x: 0, y: 0 });
  const h = getHandle?.();
  handle = (h ? h.replace(/^@/, "") : null) || user?.handle || null;
  uiRef = ui;
  npRef = needsPaint;

  fetchReleases(ui, needsPaint); // initial load
  connectOvenWs(ui, needsPaint); // live updates

  // Check for active builds
  fetchBuildStatus(needsPaint);
  buildPollTimer = setInterval(() => fetchBuildStatus(needsPaint), 30000);

  // Probe CL variant availability (HEAD request)
  fetch(IMAGE_BASE.replace("native-notepat", "cl-native-notepat"), { method: "HEAD" })
    .then(r => { clAvailable = r.ok; console.log("[os] CL variant:", clAvailable ? "available" : "not yet"); needsPaint(); })
    .catch(() => { clAvailable = false; });

  if (handle) {
    console.log("[os] Authorizing @" + handle + "...");
    api?.authorize?.().then((t) => {
      token = t;
      console.log("[os] Authorized, token:", t ? "ok" : "missing");
      if (releases) makeButtons(ui);
      // Check device token status (Claude + GitHub)
      if (t) {
        fetch("/.netlify/functions/claude-token", {
          headers: { Authorization: "Bearer " + t },
        })
          .then((r) => r.json())
          .then((data) => {
            hasClaude = !!data.token;
            hasGit = !!data.githubPat;
            console.log("[os] Device tokens: claude=" + hasClaude + " git=" + hasGit);
            if (!hasClaude || !hasGit) {
              setupBtn = new ui.TextButton("set up device tokens", { x: 6, y: 0 });
            }
            needsPaint();
          })
          .catch((err) => {
            console.error("[os] Token check failed:", err);
            hasClaude = false;
            hasGit = false;
            needsPaint();
          });
      }
      needsPaint();
    });
  } else {
    console.log("[os] No handle — template download available");
    makeButtons(ui);
  }
}

function leave() {
  if (wsReconnectTimer) { clearTimeout(wsReconnectTimer); wsReconnectTimer = null; }
  if (buildPollTimer) { clearInterval(buildPollTimer); buildPollTimer = null; }
  if (ovenWs) { ovenWs.onclose = null; try { ovenWs.close(); } catch (_) {} ovenWs = null; }
}

function makeButtons(ui) {
  updateDownloadBtn(ui);
  updateBootBtn(ui);
  updateVariantBtn(ui);
  updateWifiBtn(ui);
  updateInstallBtn(ui);
  updateBuildsBtn(ui);
}

function updateInstallBtn(ui) {
  installBtn = new ui.TextButton(
    installExpanded ? "hide install" : "how to install",
    { x: 6, y: 0 },
  );
}

function updateBuildsBtn(ui) {
  buildsBtn = new ui.TextButton(
    buildsExpanded ? "show fewer" : "show all",
    { x: 6, y: 0 },
  );
}

function updateVariantBtn(ui) {
  const label = VARIANTS[variantIdx].label.toLowerCase();
  variantBtn = new ui.TextButton("build: " + label, { x: 6, y: 0 });
}

function updateDownloadBtn(ui) {
  downloadBtn = new ui.TextButton("download .img", { x: 6, y: 0 });
}

function osLabel() {
  const latest = releases?.releases?.[0];
  const variantPrefix = variantIdx === 1 ? "CL-" : "";
  const coreName = variantPrefix + "AC-" + (latest?.name || "native");
  const piece = BOOT_PIECES[bootPieceIdx];
  return `@${handle}-os-${piece}-${coreName}`;
}

function updateBootBtn(ui) {
  const piece = BOOT_PIECES[bootPieceIdx];
  bootBtn = new ui.TextButton("piece: " + piece, { x: 6, y: 0 });
}

function updateWifiBtn(ui) {
  wifiBtn = new ui.TextButton("wifi: " + (wifiEnabled ? "on" : "off"), { x: 6, y: 0 });
}

function timeAgo(ts) {
  if (!ts) return "";
  const now = Date.now();
  const then = new Date(ts).getTime();
  const sec = Math.max(0, Math.floor((now - then) / 1000));
  if (sec < 60) return sec + "s";
  const min = Math.floor(sec / 60);
  if (min < 60) return min + "m";
  const hr = Math.floor(min / 60);
  if (hr < 24) return hr + "h";
  const day = Math.floor(hr / 24);
  if (day < 30) return day + "d";
  const mo = Math.floor(day / 30);
  if (mo < 12) return mo + "mo";
  return Math.floor(mo / 12) + "y";
}

function buildStatusInfo(build, isCurrent, isDep, darkMode) {
  let status = (build?.status || "").toLowerCase();
  if (!status) status = isDep ? "deprecated" : "success";

  if (status === "done") status = "success";
  if (status === "queued") status = "queue";

  const dark = {
    success: { label: "ok", color: [100, 225, 140] },
    failed: { label: "fail", color: [255, 125, 125] },
    cancelled: { label: "stop", color: [235, 185, 115] },
    running: { label: "run", color: [120, 185, 255] },
    queue: { label: "queue", color: [170, 185, 220] },
    deprecated: { label: "old", color: [170, 130, 130] },
    unknown: { label: "?", color: [150, 155, 170] },
  };
  const light = {
    success: { label: "ok", color: [20, 115, 45] },
    failed: { label: "fail", color: [170, 35, 35] },
    cancelled: { label: "stop", color: [140, 95, 15] },
    running: { label: "run", color: [30, 95, 175] },
    queue: { label: "queue", color: [90, 105, 145] },
    deprecated: { label: "old", color: [130, 95, 95] },
    unknown: { label: "?", color: [120, 125, 135] },
  };
  const table = darkMode ? dark : light;
  return table[status] || table.unknown;
}

function paint($) {
  const { screen, ink, line: drawLine, dark: systemDark } = $;
  const dark = appearanceOverride ? appearanceOverride === "dark" : !!systemDark;
  darkModeOn = dark;
  const { width: w, height: h } = screen;
  const C = dark ? scheme.dark : scheme.light;
  $.wipe(...C.bg);

  const isMobile = w < 360;
  const pad = isMobile ? 6 : 10;
  const charW = 6;
  const rowH = 10;
  const matrixH = 9;
  const matrixW = 4;
  const wrapW = w - pad * 2;
  let y = pad - scrollY;
  buildsViewH = h;

  // Truncate helper — clips text to fit available pixel width
  const trunc = (s, cw, availW) => {
    const max = Math.floor((availW || wrapW) / cw);
    return s.length > max ? s.slice(0, max - 1) + "~" : s;
  };

  // Word-wrap plain text to a max char width, returning an array of lines.
  const wrap = (text, maxChars) => {
    const words = String(text || "").split(/\s+/).filter(Boolean);
    const lines = [];
    let cur = "";
    for (const word of words) {
      const next = cur ? cur + " " + word : word;
      if (next.length <= maxChars) {
        cur = next;
      } else {
        if (cur) lines.push(cur);
        // Word itself longer than line — hard truncate
        cur = word.length > maxChars ? word.slice(0, maxChars - 1) + "~" : word;
      }
    }
    if (cur) lines.push(cur);
    return lines;
  };

  // Section header: small caps label + thin divider.
  function section(title) {
    y += isMobile ? 2 : 4;
    ink(...C.instHeader);
    $.write(title, { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    y += matrixH + 2;
    ink(...C.divider);
    drawLine(pad, y, w - pad, y);
    y += isMobile ? 4 : 6;
  }

  // Keep the title and appearance control above scrolling content and the AC icon.
  const headerH = pad + (themeBtn?.height || 20) + 8;
  headerBottom = headerH;
  function drawHeader() {
    ink(...C.bg).box(0, 0, w, headerH);
    ink(...C.msg).write("native os", { x: pad + 18, y: pad + 5 });
    if (themeBtn) {
      themeBtn.replaceLabel(dark ? "light" : "dark");
      themeBtn.reposition({ x: w - pad - themeBtn.width, y: pad });
      themeBtn.paint($,
        [C.bootBtnBg, C.bootBtnBorder, C.instHeader, 255],
        [C.bootBtnHoverBg, C.bootBtnHoverBorder, C.instHeader, 255],
        undefined,
        [C.bootBtnHoverBg, C.bootBtnHoverBorder, C.instHeader, 255]);
    }
  }
  y = headerH - scrollY;

  if (loading) {
    ink(...C.date).write("loading builds...", { x: pad, y });
    drawHeader();
    return;
  }

  if (error) {
    ink(...C.error).write(trunc(error, charW), { x: pad, y });
    drawHeader();
    return;
  }

  if (!releases) { drawHeader(); return; }

  const builds = releases.releases || [];
  const latest = builds[0];
  const isPersonal = handle && token;

  // Latest release.

  if (latest) {
    // Big build name in the "current" accent color
    ink(...C.current);
    $.write(trunc(latest.name, charW), { x: pad, y });
    y += rowH + 4;

    // Meta: hash · ago · size
    const hash = (latest.git_hash || "").slice(0, 7);
    const ago = timeAgo(latest.build_ts);
    const sizeMB = latest.size ? (latest.size / 1048576).toFixed(0) + "mb" : "";
    const meta = [hash, ago, sizeMB].filter(Boolean).join(" · ");
    ink(...C.date);
    $.write(meta, { x: pad, y });
    y += rowH + 5;

    // Commit message — wrap up to 2-3 lines
    if (latest.commit_msg) {
      const maxChars = Math.floor(wrapW / charW);
      const maxLines = 2;
      const lines = wrap(latest.commit_msg, maxChars).slice(0, maxLines);
      for (const line of lines) {
        ink(...C.msg);
        $.write(line, { x: pad, y });
        y += rowH + 1;
      }
    }
  } else {
    ink(...C.instText).write("no builds yet", { x: pad, y });
    y += rowH;
  }

  y += 6;

  // === OPTIONS: uniform pills (piece / build / wifi) ===
  if (bootBtn) {

    const pills = [bootBtn];
    if (variantBtn && clAvailable) pills.push(variantBtn);
    if (wifiBtn) pills.push(wifiBtn);

    const pillGap = 4;
    const pillH = bootBtn.height;
    let px = pad;
    let py = y;
    for (const pill of pills) {
      const pw = pill.width;
      if (px + pw > w - pad && px > pad) {
        px = pad;
        py += pillH + pillGap;
      }
      pill.reposition({ x: px, y: py });
      pill.paint(
        $,
        [C.bootBtnBg, C.bootBtnBorder, C.bootPiece, 230],
        [C.bootBtnHoverBg, C.bootBtnHoverBorder, C.instHeader, 255],
        undefined,
        [C.bootBtnHoverBg, C.bootBtnBorder, C.bootPiece, 255],
      );
      px += pw + pillGap;
    }
    y = py + pillH + 6;

    // Token-setup nudge (only when missing)
    if (isPersonal && hasClaude !== null && (!hasClaude || !hasGit) && setupBtn) {
      setupBtn.reposition({ x: pad, y });
      setupBtn.paint(
        $,
        [C.bootBtnBg, C.bootBtnBorder, C.error, 255],
        [C.bootBtnHoverBg, C.bootBtnHoverBorder, C.error, 255],
        undefined,
        [C.bootBtnHoverBg, C.bootBtnBorder, C.error, 255],
      );
      y += setupBtn.height + 4;
      if (showTokenHint) {
        ink(...C.instText);
        $.write("on device, type in prompt:", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
        y += matrixH + 1;
        if (!hasClaude) {
          ink(...C.instKey);
          $.write("  claude sk-ant-XXXX", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
          y += matrixH + 1;
        }
        if (!hasGit) {
          ink(...C.instKey);
          $.write("  git ghp_XXXX", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
          y += matrixH + 1;
        }
      }
    }
  }

  // === DOWNLOAD ===
  if (downloading) {
    section("downloading");
    const barH = 18;
    ink(...C.progressBg).box(pad, y, wrapW, barH);
    const barW = Math.floor((wrapW - 4) * downloadProgress);
    ink(...C.progress).box(pad + 2, y + 2, barW, barH - 4);
    const pct = Math.floor(downloadProgress * 100);
    const mb = downloadMB.toFixed(1);
    const total = downloadTotalMB > 0 ? "/" + downloadTotalMB.toFixed(0) : "";
    y += barH + 4;
    ink(...C.msg).write(mb + total + "mb " + pct + "%", { x: pad, y });
    y += rowH + 4;
    if (downloadStatus) {
      ink(...C.bootLabel);
      $.write(trunc(downloadStatus, charW), { x: pad, y });
      y += rowH + 2;
    }
  } else if (downloadBtn) {
    downloadBtn.reposition({ x: pad, y });
    downloadBtn.paint(
      $,
      [C.dlBtnBg, C.dlBtnBorder, C.dlBtnText, 255],
      [C.dlBtnHoverBg, C.dlBtnHoverBorder, C.dlBtnText, 255],
      undefined,
      [C.dlBtnHoverBg, C.dlBtnBorder, C.dlBtnText, 255],
    );
    y += downloadBtn.height + 4;

    // Identity hint
    if (isPersonal) {
      ink(...C.handle, 220);
      $.write("signed in as @" + handle, { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    } else {
      ink(...C.loginHint);
      $.write("log in for a personalized build", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    }
    y += matrixH + 2;
    if (downloadStatus) {
      ink(...(downloadStatus.startsWith("error:") ? C.error : C.current));
      for (const line of wrap(downloadStatus, Math.floor(wrapW / charW))) {
        $.write(line, { x: pad, y });
        y += rowH + 2;
      }
    }
  }

  // === ACTIVE BUILD (live oven telemetry) ===
  if (activeBuild) {
    const rawStage = activeBuild.stage || "starting";
    const isCLBuild = rawStage.startsWith("cl-") || activeBuild.variant === "cl";
    const buildVariant = activeBuild.variant === "both" ? "c + cl" : isCLBuild ? "common lisp" : "c";
    section("building (" + buildVariant + ")");

    const stageLabel = isCLBuild ? rawStage.slice(3) : rawStage;
    const pct = activeBuild.percent || 0;
    const elapsed = activeBuild.elapsedMs ? Math.floor(activeBuild.elapsedMs / 1000) : 0;
    const elapsedStr = elapsed > 0 ? " · " + Math.floor(elapsed / 60) + "m" + (elapsed % 60) + "s" : "";
    ink(...C.current);
    $.write(trunc(stageLabel + " " + pct + "%" + elapsedStr, charW), { x: pad, y });
    y += rowH + 2;

    const buildBarW = wrapW;
    const buildBarH = isMobile ? 6 : 8;
    ink(...C.progressBg).box(pad, y, buildBarW, buildBarH);
    const fillW = Math.floor((buildBarW - 2) * pct / 100);
    ink(...C.progress).box(pad + 1, y + 1, fillW, buildBarH - 2);
    y += buildBarH + 4;

    if (activeBuild.buildName || (activeBuild.ref && activeBuild.ref !== "unknown")) {
      const namePart = activeBuild.buildName || "";
      const refPart = activeBuild.ref && activeBuild.ref !== "unknown" ? activeBuild.ref.slice(0, 7) : "";
      ink(...C.name);
      $.write(trunc(namePart, charW), { x: pad, y });
      if (refPart) {
        const nx = pad + (namePart.length + 1) * charW;
        ink(...C.hash);
        $.write(refPart, { x: nx, y });
      }
      y += rowH + 2;
    }

    if (activeBuild.commitMsg) {
      ink(...C.msg);
      $.write(trunc(activeBuild.commitMsg, charW), { x: pad, y });
      y += rowH + 2;
    }

    if (activeBuild.error) {
      ink(...C.error);
      $.write(trunc(activeBuild.error, charW), { x: pad, y });
      y += rowH + 2;
    }

    if (buildLogLines.length > 0) {
      y += 2;
      const visibleLogs = isMobile ? buildLogLines.slice(-8) : buildLogLines.slice(-12);
      for (const line of visibleLogs) {
        ink(...C.instText);
        $.write(trunc(line, matrixW), { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
        y += matrixH + 1;
      }
    }
  }

  // === INSTALL (collapsible) ===
  if (installBtn) {
    y += 4;
    installBtn.reposition({ x: pad, y });
    installBtn.paint(
      $,
      [C.bootBtnBg, C.bootBtnBorder, C.instHeader, 220],
      [C.bootBtnHoverBg, C.bootBtnHoverBorder, C.instHeader, 255],
      undefined,
      [C.bootBtnHoverBg, C.bootBtnBorder, C.instHeader, 240],
    );
    y += installBtn.height + 4;

    if (installExpanded) {
      const instLines = isMobile ? [
        [C.instText, "1 flash .img (fedora media writer)"],
        [C.instText, "2 plug usb into x86 pc"],
        [C.instText, "3 bios boot menu:"],
        [C.instKey, "  f12 dell/lenovo  f9 hp"],
        [C.instKey, "  f2 asus/acer  esc others"],
        [C.instText, "4 select usb drive"],
      ] : [
        [C.instText, "1 flash .img with fedora media writer"],
        [C.instText, "2 plug usb into any x86 pc"],
        [C.instText, "3 enter bios boot menu:"],
        [C.instKey, "  f12 dell/lenovo   f9 hp"],
        [C.instKey, "  f2 asus/acer   esc others"],
        [C.instText, "4 select usb drive to boot"],
      ];
      for (const [color, text] of instLines) {
        ink(...color).write(trunc(text, matrixW), { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
        y += matrixH + (isMobile ? 2 : 3);
      }
    }
  }

  // === BUILDS list ===
  section("builds");

  const entryH = 36;
  const buildsLimit = buildsExpanded ? builds.length : Math.min(DEFAULT_BUILDS_VISIBLE, builds.length);
  for (let i = 0; i < buildsLimit; i++) {
    const b = builds[i];
    if (y + entryH >= 0 && y < h) {
      const current = i === 0 && !b.deprecated;
      const status = buildStatusInfo(b, current, !!b.deprecated, dark);
      const tag = current ? "latest" : status.label;
      const tagW = (tag.length + 1) * charW;
      ink(...(i % 2 ? C.rowAlt : C.row)).box(pad, y, wrapW, entryH);
      ink(...(current ? C.current : C.msg));
      $.write(trunc(b.name || "?", charW, wrapW - tagW - 8), { x: pad + 4, y: y + 3 });
      ink(...status.color).write(tag, { x: w - pad - tagW, y: y + 3 });
      const meta = [(b.git_hash || "").slice(0, 7), timeAgo(b.build_ts), b.handle ? "@" + b.handle : ""].filter(Boolean).join(" · ");
      ink(...C.date).write(trunc(meta, charW, wrapW - 8), { x: pad + 4, y: y + 14 });
      ink(...C.msg).write(trunc(b.commit_msg || "", charW, wrapW - 8), { x: pad + 4, y: y + 25 });
    }
    y += entryH;
  }
  y += 4;
  if (buildsBtn && builds.length > DEFAULT_BUILDS_VISIBLE) {
    buildsBtn.reposition({ x: pad, y });
    buildsBtn.paint($,
      [C.bootBtnBg, C.bootBtnBorder, C.instHeader, 255],
      [C.bootBtnHoverBg, C.bootBtnHoverBorder, C.instHeader, 255],
      undefined,
      [C.bootBtnHoverBg, C.bootBtnHoverBorder, C.instHeader, 255]);
    y += buildsBtn.height + 4;
  }
  totalContentH = y + scrollY + pad;
  drawHeader();
  const maxScroll = Math.max(0, totalContentH - h);
  if (scrollY > maxScroll) { scrollY = maxScroll; $.needsPaint(); }
  if (maxScroll) {
    const thumbH = Math.max(12, Math.floor(h * h / totalContentH));
    const thumbY = Math.round(scrollY / maxScroll * (h - thumbH));
    ink(...C.bar).box(w - 3, thumbY, 2, thumbH);
  }
}

function act({ event: e, needsPaint, download, jump, darkMode }) {
  dlFn = download;

  if (e.is("dark-mode") || e.is("light-mode")) {
    if (uiRef && (handle && token || releases)) makeButtons(uiRef);
    needsPaint();
    return;
  }

  if (e.is("touch")) dragged = false;
  if (e.is("scroll") || e.is("draw")) {
    const delta = e.is("scroll") ? e.y : -(e.delta?.y ?? e.dy ?? 0);
    if (delta) dragged = true;
    scrollY = Math.max(0, Math.min(scrollY + delta, Math.max(0, totalContentH - buildsViewH)));
    needsPaint();
    return;
  }
  if (e.is("lift") && dragged) {
    for (const button of [themeBtn, bootBtn, variantBtn, wifiBtn, installBtn, buildsBtn, setupBtn, downloadBtn]) {
      button?.btn.act(e, {});
    }
    return;
  }
  if (themeBtn) themeBtn.btn.act(e, {
    push: () => {
      darkModeOn = !darkModeOn;
      appearanceOverride = darkModeOn ? "dark" : "light";
      if (storeRef) {
        storeRef["os-appearance"] = appearanceOverride;
        storeRef.persist("os-appearance");
      }
      darkMode(darkModeOn);
      needsPaint();
    },
  });

  // Scrolled controls beneath the fixed header must not receive header taps.
  if (e.y < headerBottom && (e.is("touch") || e.is("lift") || e.is("move"))) {
    if (e.is("lift")) {
      for (const button of [bootBtn, variantBtn, wifiBtn, installBtn, buildsBtn, setupBtn, downloadBtn]) {
        button?.btn.act(e, {});
      }
    }
    return;
  }

  // Recreate buttons on resize for responsive layout
  if (e.is("reframed")) {
    if (uiRef && (handle && token || releases)) makeButtons(uiRef);
    needsPaint();
    return;
  }

  if (downloading) return;

  // Boot-to button: cycle through pieces on tap
  if (bootBtn) {
    bootBtn.btn.act(e, {
      push: () => {
        bootPieceIdx = (bootPieceIdx + 1) % BOOT_PIECES.length;
        if (uiRef) updateBootBtn(uiRef);
        needsPaint();
      },
    });
  }

  // Build variant selector (only when CL is available)
  if (variantBtn && clAvailable) {
    variantBtn.btn.act(e, {
      push: () => {
        variantIdx = (variantIdx + 1) % VARIANTS.length;
        if (uiRef) updateVariantBtn(uiRef);
        needsPaint();
      },
    });
  }

  // WiFi toggle
  if (wifiBtn) {
    wifiBtn.btn.act(e, {
      push: () => {
        wifiEnabled = !wifiEnabled;
        if (uiRef) updateWifiBtn(uiRef);
        needsPaint();
      },
    });
  }

  // Install section toggle
  if (installBtn) {
    installBtn.btn.act(e, {
      push: () => {
        installExpanded = !installExpanded;
        if (uiRef) updateInstallBtn(uiRef);
        needsPaint();
      },
    });
  }

  // Builds show all/fewer toggle
  if (buildsBtn) {
    buildsBtn.btn.act(e, {
      push: () => {
        buildsExpanded = !buildsExpanded;
        if (!buildsExpanded) {
          scrollY = 0;
        }
        if (uiRef) updateBuildsBtn(uiRef);
        needsPaint();
      },
    });
  }

  // Setup tokens button: show hint text
  if (setupBtn && token) {
    setupBtn.btn.act(e, {
      push: () => {
        showTokenHint = !showTokenHint;
        needsPaint();
      },
    });
  }

  if (!downloadBtn) return;

  downloadBtn.btn.act(e, {
    push: () => {
      if (handle && token) {
        startDownload(needsPaint);
      } else {
        startTemplateDownload(needsPaint);
      }
    },
  });
}

async function startDownload(needsPaint) {
  downloading = true;
  downloadProgress = 0;
  downloadMB = 0;
  downloadTotalMB = 0;
  const piece = BOOT_PIECES[bootPieceIdx];
  downloadStatus = "preparing @" + (handle || "user") + " os... (boot to: " + piece + ")";
  console.log("[os] Starting download:", osLabel(), "piece:", piece);
  needsPaint();

  try {
    const query =
      "?piece=" + encodeURIComponent(piece) +
      "&wifi=" + (wifiEnabled ? "1" : "0") +
      "&cb=" + Date.now() +
      (variantIdx === 1 ? "&variant=cl" : "");
    const imageQuery = query + "&layout=img&strict=1";
    const downloadCandidates = [
      {
        url: OVEN_BASE() + "/os-image" + imageQuery,
        allowedLayouts: ["img"],
        rejectOriginFallback: true,
      },
      {
        url: OVEN_ORIGIN + "/os-image" + imageQuery,
        allowedLayouts: ["img"],
        rejectOriginFallback: false,
      },
    ];
    const MIN_EXPECTED_IMAGE_BYTES = 300 * 1024 * 1024;

    let res = null;
    let usedUrl = "";
    let lastErr = "";
    for (const candidate of downloadCandidates) {
      console.log("[os] Fetching:", candidate.url);
      let attempt;
      try {
        attempt = await fetch(candidate.url, {
          headers: { Authorization: "Bearer " + token },
        });
      } catch (err) {
        lastErr = err?.message || String(err);
        continue;
      }

      if (!attempt.ok) {
        const err = await attempt.json().catch(() => ({}));
        lastErr = err.error || ("Download failed: " + attempt.status);
        continue;
      }

      const patchMode = (attempt.headers.get("x-patch") || "").toLowerCase();
      if (candidate.rejectOriginFallback && patchMode === "origin-fallback") {
        console.warn("[os] Rejecting edge origin fallback from", candidate.url);
        try { attempt.body?.cancel(); } catch (_) {}
        lastErr = "Edge personalized download unavailable";
        continue;
      }

      const servedLayout = (attempt.headers.get("x-ac-os-layout") || "").toLowerCase();
      if (!servedLayout) {
        console.warn("[os] Missing x-ac-os-layout header from", candidate.url);
        try { attempt.body?.cancel(); } catch (_) {}
        lastErr = "Server did not report image layout (x-ac-os-layout missing)";
        continue;
      }
      if (!candidate.allowedLayouts.includes(servedLayout)) {
        console.warn("[os] Rejecting unexpected layout:", servedLayout, "from", candidate.url);
        try { attempt.body?.cancel(); } catch (_) {}
        lastErr = "Server returned '" + servedLayout + "' image";
        continue;
      }

      const len = parseInt(attempt.headers.get("content-length") || "0");
      const minExpectedBytes = MIN_EXPECTED_IMAGE_BYTES;
      if (len > 0 && len < minExpectedBytes) {
        console.warn("[os] Rejecting suspiciously small image response:", len, "bytes from", candidate.url, "layout:", servedLayout || "?");
        try { attempt.body?.cancel(); } catch (_) {}
        lastErr = "Received suspiciously small image (" + len + " bytes)";
        continue;
      }

      res = attempt;
      usedUrl = candidate.url;
      break;
    }

    if (!res) {
      throw new Error(lastErr || "Download failed");
    }
    console.log("[os] Download source:", usedUrl, "layout:", res.headers.get("x-ac-os-layout") || "?");

    const contentLength = parseInt(res.headers.get("content-length") || "0");
    downloadTotalMB = contentLength / 1048576;
    downloadStatus = "downloading...";
    needsPaint();

    const reader = res.body.getReader();
    const chunks = [];
    let received = 0;

    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      chunks.push(value);
      received += value.length;
      downloadMB = received / 1048576;
      if (contentLength > 0) downloadProgress = received / contentLength;
      needsPaint();
    }

    downloadStatus = "preparing file...";
    needsPaint();

    const total = chunks.reduce((s, c) => s + c.length, 0);
    const servedLayout = (res.headers.get("x-ac-os-layout") || "").toLowerCase();
    const minExpectedBytes = MIN_EXPECTED_IMAGE_BYTES;
    if (total < minExpectedBytes) {
      throw new Error("Image too small (" + (total / 1048576).toFixed(1) + "MB), refusing to save");
    }
    if (contentLength > 0 && total !== contentLength) {
      throw new Error(
        "Download truncated (" +
        (total / 1048576).toFixed(1) +
        "MB of " +
        (contentLength / 1048576).toFixed(1) +
        "MB)"
      );
    }
    const combined = new Uint8Array(total);
    let offset = 0;
    for (const chunk of chunks) {
      combined.set(chunk, offset);
      offset += chunk.length;
    }

    const latest = releases?.releases?.[0];
    const variantPrefix = variantIdx === 1 ? "CL-" : "";
    const coreName = variantPrefix + "AC-" + (latest?.name || "native");
    const d = new Date();
    const p = (n) => String(n).padStart(2, "0");
    const ts = `${d.getFullYear()}.${p(d.getMonth()+1)}.${p(d.getDate())}.${p(d.getHours())}.${p(d.getMinutes())}.${p(d.getSeconds())}`;
    const extension = "img";
    const mimeType = "application/octet-stream";
    const filename = `@${handle || "user"}-os-${piece}-${coreName}-${ts}.${extension}`;

    console.log("[os] Download complete:", filename, (total / 1048576).toFixed(1) + "MB");
    dlFn(filename, combined, { type: mimeType });
    downloadStatus = "done!";
  } catch (err) {
    console.error("[os] Download failed:", err);
    downloadStatus = "error: " + err.message;
  }

  downloading = false;
  downloadProgress = 0;
  needsPaint();
}

async function startTemplateDownload(needsPaint) {
  downloading = true;
  downloadProgress = 0;
  downloadMB = 0;
  downloadTotalMB = 0;
  const piece = BOOT_PIECES[bootPieceIdx];
  downloadStatus = "downloading template os... (boot to: " + piece + ")";
  console.log("[os] Template download, piece:", piece, "wifi:", wifiEnabled);
  needsPaint();

  try {
    const res = await fetch(templateImageUrl());
    if (!res.ok) throw new Error("Download failed: " + res.status);

    const contentLength = parseInt(res.headers.get("content-length") || "0");
    downloadTotalMB = contentLength / 1048576;
    downloadStatus = "downloading...";
    needsPaint();

    const reader = res.body.getReader();
    const chunks = [];
    let received = 0;

    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      chunks.push(value);
      received += value.length;
      downloadMB = received / 1048576;
      if (contentLength > 0) downloadProgress = received / contentLength;
      needsPaint();
    }

    downloadStatus = "patching config...";
    needsPaint();

    const total = chunks.reduce((s, c) => s + c.length, 0);
    const combined = new Uint8Array(total);
    let offset = 0;
    for (const chunk of chunks) {
      combined.set(chunk, offset);
      offset += chunk.length;
    }

    // Patch the config marker with selected piece + wifi flag
    const config = JSON.stringify({
      handle: "",
      piece,
      sub: "",
      email: "",
      ...(wifiEnabled ? {} : { wifi: false }),
    });
    const markerBytes = new TextEncoder().encode(CONFIG_MARKER);
    const configBytes = new TextEncoder().encode(config);
    const padded = new Uint8Array(CONFIG_PAD);
    padded.fill(0x20); // space-pad
    padded.set(configBytes);

    // Find and replace all occurrences of the marker
    let patched = 0;
    for (let i = 0; i <= combined.length - CONFIG_PAD; i++) {
      let match = true;
      for (let j = 0; j < markerBytes.length; j++) {
        if (combined[i + j] !== markerBytes[j]) { match = false; break; }
      }
      if (match) {
        combined.set(padded, i);
        patched++;
        i += CONFIG_PAD - 1;
      }
    }
    console.log("[os] Patched", patched, "config regions");

    const latest = releases?.releases?.[0];
    const variantPrefix = variantIdx === 1 ? "CL-" : "";
    const coreName = variantPrefix + "AC-" + (latest?.name || "native");
    const d = new Date();
    const p = (n) => String(n).padStart(2, "0");
    const ts = `${d.getFullYear()}.${p(d.getMonth()+1)}.${p(d.getDate())}.${p(d.getHours())}.${p(d.getMinutes())}.${p(d.getSeconds())}`;
    const filename = `ac-os-${piece}-${coreName}-${ts}.img`;

    console.log("[os] Template download complete:", filename, (total / 1048576).toFixed(1) + "MB");
    dlFn(filename, combined, { type: "application/octet-stream" });
    downloadStatus = "done!";
  } catch (err) {
    console.error("[os] Template download failed:", err);
    downloadStatus = "error: " + err.message;
  }

  downloading = false;
  downloadProgress = 0;
  needsPaint();
}

function sim() {}

export const desc = "FedAC OS — view builds and download your copy.";
export { boot, paint, act, sim, leave };
