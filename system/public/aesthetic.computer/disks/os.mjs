// os, 2026.03.12
// FedAC OS — build list with commit messages; download your personalized copy.

const OVEN = "https://oven-edge.aesthetic-computer.workers.dev";
const OVEN_WS = "wss://oven.aesthetic.computer/ws";
const OVEN_ORIGIN = "https://oven.aesthetic.computer";
const ISO_BASE = OVEN + "/os/latest.iso";
function OVEN_BASE() { return OVEN; }
function RELEASES_URL() { return OVEN + "/os-releases"; }
function OVEN_WS_URL() { return OVEN_WS; }
function templateIsoUrl() {
  const base = ISO_BASE;
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
let clAvailable = false; // true once we confirm CL ISO exists on CDN
let wifiEnabled = true;
let wifiBtn = null;   // "internet: ON/OFF" toggle
let scrollY = 0;
let autoScroll = false;
let autoScrollSpeed = 0.3;
let autoScrollDelay = 3000;
let loadTime = 0;
let totalContentH = 0;
let buildsViewH = 0;
let dlFn = null;
let ovenWs = null;
let wsReconnectTimer = null;
let npRef = null; // stored needsPaint for live updates

// Device token status
let hasClaude = null; // null=loading, true/false
let hasGit = null;
let setupBtn = null; // "set up tokens" button shown when missing
let showTokenHint = false;

// Build telemetry
let activeBuild = null;    // { id, status, stage, percent, ref, error }
let buildLogLines = [];    // last few log lines
const MAX_BUILD_LOGS = 10;
let buildPollTimer = null;

// Color palettes for dark/light mode (deprecated builds use red tones)
const scheme = {
  dark: {
    bg: [10, 12, 18],
    name: [80, 255, 140],
    nameOld: [50, 80, 65],
    hash: [255, 180, 100],
    hashOld: [120, 90, 55],
    date: [90, 100, 120],
    msg: [200, 200, 220],
    msgOld: [80, 85, 100],
    handle: [180, 150, 255],
    handleOld: [80, 70, 110],
    current: [80, 255, 140],
    bar: [40, 50, 70],
    loginHint: [70, 60, 80],
    progress: [100, 180, 255],
    progressBg: [40, 45, 65],
    bootLabel: [140, 160, 200],
    bootPiece: [255, 200, 120],
    instHeader: [180, 200, 255],
    instText: [60, 70, 90],
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
    bg: [240, 240, 245],
    name: [20, 130, 60],
    nameOld: [100, 140, 110],
    hash: [180, 110, 30],
    hashOld: [150, 120, 80],
    date: [120, 130, 150],
    msg: [40, 40, 50],
    msgOld: [130, 130, 145],
    handle: [110, 70, 200],
    handleOld: [140, 120, 170],
    current: [20, 130, 60],
    bar: [200, 205, 215],
    loginHint: [140, 120, 160],
    progress: [40, 120, 200],
    progressBg: [210, 215, 225],
    bootLabel: [80, 100, 150],
    bootPiece: [180, 120, 30],
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
      loading = false;
      if (!loadTime) loadTime = Date.now();
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

function boot({ user, handle: getHandle, api, ui, needsPaint }) {
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
  fetch(ISO_BASE.replace("native-notepat", "cl-native-notepat"), { method: "HEAD" })
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
  if (ovenWs) { try { ovenWs.close(); } catch (_) {} ovenWs = null; }
}

function makeButtons(ui) {
  updateDownloadBtn(ui);
  updateBootBtn(ui);
  updateVariantBtn(ui);
  updateWifiBtn(ui);
}

function updateVariantBtn(ui) {
  variantBtn = new ui.TextButton(VARIANTS[variantIdx].label, { x: 6, y: 0 });
}

function updateDownloadBtn(ui) {
  downloadBtn = new ui.TextButton("download", { x: 6, y: 0 });
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
  bootBtn = new ui.TextButton(piece, { x: 6, y: 0 });
}

function updateWifiBtn(ui) {
  wifiBtn = new ui.TextButton(wifiEnabled ? "ON" : "OFF", { x: 6, y: 0 });
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
  const { screen, ink, line: drawLine, dark, mask, unmask } = $;
  const { width: w, height: h } = screen;
  const C = dark ? scheme.dark : scheme.light;
  $.wipe(...C.bg);

  const isMobile = w < 360;
  const isNarrow = w < 500;
  const pad = isMobile ? 6 : isNarrow ? 8 : 12;
  const charW = 6;
  const rowH = 10;
  const matrixH = 9;
  const matrixW = 4; // MatrixChunky8 char width
  const wrapW = w - pad * 2;
  const btnGap = isMobile ? 8 : 4;
  // Always left-align buttons
  const btnX = (btn) => pad;
  let y = isMobile ? 28 : 22;

  if (loading) {
    ink(100).write("loading...", { x: pad, y });
    return;
  }

  if (error) {
    ink(255, 80, 80).write(error, { x: pad, y });
    return;
  }

  if (!releases) return;

  const builds = releases.releases || [];

  // Section rendering — header bar + tinted content background
  const secBarH = matrixH + 8;
  function sectionHeader(title, barColor, bgColor, bgH) {
    // Content background (paint first, behind everything)
    if (bgColor && bgH) {
      ink(...bgColor).box(0, y, w, secBarH + bgH);
    }
    ink(...barColor).box(0, y, w, secBarH);
    ink(...C.instHeader).write(title, { x: pad, y: y + 4 }, undefined, undefined, false, "MatrixChunky8");
    y += secBarH + (isMobile ? 10 : 8);
  }

  // --- ABOUT ---
  const descText = "A Linux kernel with an embedded initramfs — boots any x86 PC from USB.";
  ink(...C.instText);
  $.write(descText, { x: pad, y, wrap: wrapW }, undefined, undefined, false, "MatrixChunky8");
  const descLines = Math.ceil((descText.length * matrixW) / wrapW);
  y += matrixH * descLines + (isMobile ? 14 : 10);

  // --- ACTIVE BUILD ---
  if (activeBuild) {
    const rawStage = activeBuild.stage || "starting";
    const isCLBuild = rawStage.startsWith("cl-");
    const buildVariant = isCLBuild ? "Common Lisp" : "C";
    const logH = buildLogLines.length * (matrixH + 1) + 8;
    sectionHeader("Building (" + buildVariant + ")", dark ? [20, 28, 20] : [215, 235, 215], dark ? [14, 20, 14] : [228, 240, 228], 120 + logH);

    // Status line: stage + percentage
    const stageLabel = isCLBuild ? rawStage.slice(3) : rawStage;
    const pct = activeBuild.percent || 0;
    ink(...C.current);
    $.write(stageLabel + " " + pct + "%", { x: pad, y });
    y += rowH + 4;

    // Progress bar
    const barW = w - pad * 2;
    const barH = 8;
    ink(...C.progressBg).box(pad, y, barW, barH);
    const fillW = Math.floor((barW - 2) * pct / 100);
    ink(...C.progress).box(pad + 1, y + 1, fillW, barH - 2);
    y += barH + 6;

    // Build name (adjective-animal from build-name.sh)
    if (activeBuild.buildName) {
      ink(...C.name);
      $.write(activeBuild.buildName, { x: pad, y });
      y += rowH + 2;
    }

    // Build ref (short hash)
    if (activeBuild.ref && activeBuild.ref !== "unknown") {
      const refTxt = activeBuild.ref.slice(0, 11);
      ink(...C.hash);
      $.write(refTxt, { x: pad, y });
      y += rowH + 2;
    }

    // Commit message
    if (activeBuild.commitMsg) {
      ink(...C.msg);
      $.write(activeBuild.commitMsg, { x: pad, y, wrap: wrapW });
      const msgLines = Math.ceil((activeBuild.commitMsg.length * charW) / wrapW);
      y += rowH * msgLines + 2;
    }

    // Error message if failed
    if (activeBuild.error) {
      ink(255, 80, 80);
      $.write(activeBuild.error, { x: pad, y, wrap: wrapW });
      y += rowH + 2;
    }

    // Live log lines
    if (buildLogLines.length > 0) {
      y += 4;
      for (const line of buildLogLines) {
        const maxChars = Math.floor(wrapW / matrixW);
        const display = line.length > maxChars ? line.slice(0, maxChars - 1) + "~" : line;
        ink(...C.instText);
        $.write(display, { x: pad, y, wrap: wrapW }, undefined, undefined, false, "MatrixChunky8");
        y += matrixH + 1;
      }
    }

    y += isMobile ? 10 : 6;
  }

  // --- DOWNLOAD section ---
  if (downloadBtn && !downloading) {
    const isPersonal = handle && token;

    sectionHeader("Download", dark ? [18, 24, 40] : [215, 220, 235], C.secDlBg, 300);

    // OS label
    if (isPersonal) {
      const label = osLabel();
      ink(...C.handle).write(label, { x: pad, y, wrap: wrapW });
      const labelLines = Math.ceil((label.length * charW) / wrapW);
      y += rowH * labelLines + 6;
    } else {
      const latest = releases?.releases?.[0];
      const label = "AC Native OS" + (latest ? " — " + latest.name : "");
      ink(...C.handle).write(label, { x: pad, y, wrap: wrapW });
      const labelLines = Math.ceil((label.length * charW) / wrapW);
      y += rowH * labelLines + 6;
    }

    // Device token status (logged-in only)
    if (isPersonal && hasClaude !== null) {
      const cIcon = hasClaude ? "+" : "-";
      const gIcon = hasGit ? "+" : "-";
      ink(...(hasClaude ? C.current : [255, 80, 80]));
      $.write(cIcon + " claude", { x: pad, y });
      ink(...(hasGit ? C.current : [255, 80, 80]));
      $.write(gIcon + " git", { x: pad + 60, y });
      y += rowH + 4;

      if ((!hasClaude || !hasGit) && setupBtn) {
        setupBtn.reposition({ x: btnX(setupBtn), y });
        setupBtn.paint(
          $,
          [[60, 30, 30], [140, 60, 60], [255, 120, 100], 255],
          [[80, 40, 40], [180, 80, 80], [255, 255, 255], 255],
          undefined,
          [[60, 30, 30], [140, 60, 60], [255, 120, 100], 230],
        );
        y += setupBtn.height + btnGap;
        if (showTokenHint) {
          ink(...C.instText);
          $.write("on device, type in prompt:", { x: pad, y, wrap: wrapW }, undefined, undefined, false, "MatrixChunky8");
          y += matrixH + 2;
          if (!hasClaude) {
            ink(...C.instKey);
            $.write("  claude sk-ant-XXXX", { x: pad, y, wrap: wrapW }, undefined, undefined, false, "MatrixChunky8");
            y += matrixH + 2;
          }
          if (!hasGit) {
            ink(...C.instKey);
            $.write("  git ghp_XXXX", { x: pad, y, wrap: wrapW }, undefined, undefined, false, "MatrixChunky8");
            y += matrixH + 2;
          }
          y += 4;
        }
      } else if (hasClaude && hasGit) {
        ink(...C.date);
        $.write("device ready", { x: pad + 120, y: y - rowH - 4 });
      }
    }

    // Boot-to selector: label + value button
    if (bootBtn) {
      const labelW = "boot ".length * charW;
      ink(...C.bootLabel);
      $.write("boot", { x: pad, y: y + 4 });
      bootBtn.reposition({ x: pad + labelW, y });
      bootBtn.paint(
        $,
        [C.bootBtnBg, C.bootBtnBorder, ...C.bootPiece, 200],
        [C.bootBtnHoverBg, C.bootBtnHoverBorder, [255, 255, 255], 255],
        undefined,
        [C.bootBtnBg, C.bootBtnBorder, ...C.bootPiece, 230],
      );
      y += bootBtn.height + btnGap;
    }

    // Build variant selector (only show when CL is available)
    if (variantBtn && clAvailable) {
      const labelW = "build ".length * charW;
      ink(...C.bootLabel);
      $.write("build", { x: pad, y: y + 4 });
      variantBtn.reposition({ x: pad + labelW, y });
      variantBtn.paint(
        $,
        [C.bootBtnBg, C.bootBtnBorder, ...C.bootPiece, 200],
        [C.bootBtnHoverBg, C.bootBtnHoverBorder, [255, 255, 255], 255],
        undefined,
        [C.bootBtnBg, C.bootBtnBorder, ...C.bootPiece, 230],
      );
      y += variantBtn.height + btnGap;
    }

    // WiFi toggle: label + value button
    if (wifiBtn) {
      const labelW = "internet ".length * charW;
      ink(...C.bootLabel);
      $.write("internet", { x: pad, y: y + 4 });
      const wOn = wifiEnabled;
      wifiBtn.reposition({ x: pad + labelW, y });
      wifiBtn.paint(
        $,
        [C.bootBtnBg, C.bootBtnBorder, ...(wOn ? C.current : [180, 80, 80]), 200],
        [C.bootBtnHoverBg, C.bootBtnHoverBorder, [255, 255, 255], 255],
        undefined,
        [C.bootBtnBg, C.bootBtnBorder, ...(wOn ? C.current : [180, 80, 80]), 230],
      );
      y += wifiBtn.height + btnGap;
    }

    // Mirror selector
    y += isMobile ? 8 : 4;

    // Download button
    downloadBtn.reposition({ x: btnX(downloadBtn), y });
    downloadBtn.paint(
      $,
      [C.dlBtnBg, C.dlBtnBorder, C.dlBtnText, 255],
      [C.dlBtnHoverBg, C.dlBtnHoverBorder, [255, 255, 255], 255],
      undefined,
      [C.dlBtnBg, C.dlBtnBorder, C.dlBtnText, 255],
    );
    y += downloadBtn.height + btnGap;

    // Hint for template users
    if (!isPersonal) {
      ink(...C.loginHint).write("log in for a personalized build", { x: pad, y, wrap: wrapW }, undefined, undefined, false, "MatrixChunky8");
      y += matrixH + 4;
    }

    y += isMobile ? 14 : 10;

    // --- INSTALL section ---
    sectionHeader("How to Install", dark ? [14, 20, 32] : [210, 215, 230], C.secInstBg, 120);

    const instLines = [
      [C.instText, "1 flash .iso with Fedora Media Writer"],
      [C.instText, "2 plug USB into any x86 PC"],
      [C.instText, "3 enter BIOS boot menu:"],
      [C.instKey, "  F12 Dell/Lenovo  F9 HP"],
      [C.instKey, "  F2 ASUS/Acer  ESC others"],
      [C.instText, "4 select USB drive to boot"],
    ];
    for (const [color, text] of instLines) {
      ink(...color).write(text, { x: pad, y, wrap: wrapW }, undefined, undefined, false, "MatrixChunky8");
      const lines = Math.ceil((text.length * matrixW) / wrapW);
      y += matrixH * lines + 4;
    }

    y += isMobile ? 14 : 10;

    // --- BUILDS section ---
    sectionHeader("Builds", dark ? [16, 22, 36] : [218, 222, 238], C.secBuildBg, 2000);
  } else if (downloading) {
    // Progress bar
    ink(...C.progressBg).box(pad, y, w - pad * 2, 18);
    const barW = Math.floor((w - pad * 2 - 4) * downloadProgress);
    ink(...C.progress).box(pad + 2, y + 2, barW, 14);
    const pct = Math.floor(downloadProgress * 100);
    const mb = downloadMB.toFixed(1);
    const total = downloadTotalMB > 0 ? "/" + downloadTotalMB.toFixed(0) : "";
    ink(255).write(mb + total + "MB " + pct + "%", { x: pad + 4, y: y + 4 });
    y += 22;
    if (downloadStatus) {
      ink(140, 160, 180).write(downloadStatus, { x: pad, y, wrap: wrapW });
      const statusLines = Math.ceil((downloadStatus.length * charW) / wrapW);
      y += rowH * statusLines + 2;
    }
  }

  // Build list — masked scrollable area with alternating strips
  const buildsTopY = y;
  const buildsH = h - buildsTopY - 16; // leave room for footer
  buildsViewH = buildsH;
  mask({ x: 0, y: buildsTopY, width: w, height: buildsH });
  y -= scrollY; // apply scroll offset only to builds

  const maxNameChars = isNarrow ? Math.floor(wrapW / charW / 3) : 30;
  const entryH = isNarrow ? 42 : 34;
  const stripA = dark ? [14, 17, 26] : [232, 234, 240];
  const stripB = dark ? [18, 22, 32] : [240, 242, 248];

  for (let i = 0; i < builds.length; i++) {
    if (y > buildsTopY + buildsH + entryH) break;
    if (y < -entryH) { y += entryH; continue; }

    const b = builds[i];
    const isCurrent = i === 0 && !b.deprecated;
    const isDep = !!b.deprecated;
    const rawName = b.name || "?";
    const name = rawName.length > maxNameChars ? rawName.slice(0, maxNameChars - 1) + "~" : rawName;
    const hash = (b.git_hash || "?").slice(0, 7);
    const ago = timeAgo(b.build_ts);
    const msg = b.commit_msg || "";
    const who = b.handle || "";
    const sizeMB = b.size ? (b.size / 1048576).toFixed(0) + "MB" : "";
    const statusInfo = buildStatusInfo(b, isCurrent, isDep, dark);

    // Alternating strip background
    const strip = i % 2 === 0 ? stripA : stripB;
    if (isCurrent) {
      ink(...(dark ? [12, 24, 16] : [225, 240, 228])).box(0, y, w, entryH);
    } else if (isDep) {
      ink(...(dark ? [20, 14, 14] : [245, 235, 235])).box(0, y, w, entryH);
    } else {
      ink(...strip).box(0, y, w, entryH);
    }

    const ry = y + 4; // row start with padding
    const marker = isCurrent ? "> " : "  ";
    let x = pad;

    // Row 1: name + hash + time + size
    ink(...(isCurrent ? C.current : isDep ? C.depName : C.nameOld));
    $.write(marker + name, { x, y: ry });
    x = pad + (marker.length + name.length) * charW + charW;

    ink(...(isCurrent ? C.hash : isDep ? C.depHash : C.hashOld));
    $.write(hash, { x, y: ry });
    x += (hash.length + 1) * charW;

    if (ago) {
      ink(...(isDep ? C.depHash : C.date));
      $.write(ago, { x, y: ry });
      x += (ago.length + 1) * charW;
    }

    if (sizeMB && !isNarrow) {
      ink(...C.date);
      $.write(sizeMB, { x, y: ry });
      x += (sizeMB.length + 1) * charW;
    }

    if (who && !isNarrow) {
      ink(...(isCurrent ? C.handle : isDep ? C.depHandle : C.handleOld));
      $.write("@" + who, { x, y: ry });
    }

    // Status tag on right edge
    if (statusInfo) {
      const tag = "[" + statusInfo.label + "]";
      const tagX = w - pad - tag.length * charW;
      ink(...statusInfo.color);
      $.write(tag, { x: tagX, y: ry });
    }

    // Strikethrough for deprecated
    if (isDep) {
      ink(...C.depStrike, 140);
      const lineEndX = who && !isNarrow ? x + (who.length + 1) * charW : x;
      drawLine(pad + charW * 2, ry + 4, lineEndX, ry + 4);
    }

    // Row 2: commit message (+ handle and size on narrow)
    const r2y = ry + rowH + 2;
    if (isNarrow) {
      let nx = pad + charW * 2;
      if (who) {
        ink(...(isCurrent ? C.handle : isDep ? C.depHandle : C.handleOld));
        $.write("@" + who, { x: nx, y: r2y });
        nx += (who.length + 2) * charW;
      }
      if (sizeMB) {
        ink(...C.date);
        $.write(sizeMB, { x: nx, y: r2y });
      }
    }

    // Row 3 (narrow) or Row 2 (wide): commit message
    const msgY = isNarrow ? r2y + rowH + 1 : r2y;
    if (msg) {
      const maxChars = Math.floor((wrapW - charW * 2) / charW);
      const display = msg.length > maxChars ? msg.slice(0, maxChars - 1) + "~" : msg;
      ink(...(isCurrent ? C.msg : isDep ? C.depMsg : C.msgOld));
      $.write("  " + display, { x: pad, y: msgY });
      if (isDep) {
        ink(...C.depStrike, 100);
        const msgW = Math.min(display.length + 2, maxChars) * charW;
        drawLine(pad, msgY + 4, pad + msgW, msgY + 4);
      }
    }

    y += entryH;
  }

  unmask();

  // Track total scrollable height (builds only)
  totalContentH = builds.length * entryH;

  // Footer
  if (builds.length > 0) {
    const countLabel = builds.length + " builds" + (autoScroll ? " (auto)" : "");
    ink(...C.date);
    $.write(countLabel, { x: w - pad - countLabel.length * charW, y: h - rowH - pad });
  }
}

function act({ event: e, needsPaint, download }) {
  dlFn = download;

  if (e.is("dark-mode") || e.is("light-mode")) {
    if (uiRef && (handle && token || releases)) makeButtons(uiRef);
    needsPaint();
    return;
  }

  // Touch drag scrolling only (mouse doesn't interrupt auto-scroll)
  if (e.is("draw")) {
    autoScroll = false;
    scrollY = Math.max(0, Math.min(scrollY - (e.dy || 0), Math.max(0, totalContentH - buildsViewH)));
    needsPaint();
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
  downloadStatus = "building @" + (handle || "user") + " os... (boot to: " + piece + ")";
  console.log("[os] Starting download:", osLabel(), "piece:", piece);
  needsPaint();

  try {
    const query = "?piece=" + encodeURIComponent(piece) + "&wifi=" + (wifiEnabled ? "1" : "0") + (variantIdx === 1 ? "&variant=cl" : "");
    const isoCandidates = [
      OVEN_ORIGIN + "/os-image" + query,
      OVEN + "/os-image" + query,
    ];
    const MIN_EXPECTED_ISO_BYTES = 100 * 1024 * 1024;

    let res = null;
    let usedUrl = "";
    let lastErr = "";
    for (const url of isoCandidates) {
      console.log("[os] Fetching:", url);
      let attempt;
      try {
        attempt = await fetch(url, {
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

      const len = parseInt(attempt.headers.get("content-length") || "0");
      if (len > 0 && len < MIN_EXPECTED_ISO_BYTES) {
        console.warn("[os] Rejecting suspiciously small ISO response:", len, "bytes from", url);
        try { attempt.body?.cancel(); } catch (_) {}
        lastErr = "Received suspiciously small image (" + len + " bytes)";
        continue;
      }

      res = attempt;
      usedUrl = url;
      break;
    }

    if (!res) {
      throw new Error(lastErr || "Download failed");
    }
    console.log("[os] Download source:", usedUrl);

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
    const filename = `@${handle || "user"}-os-${piece}-${coreName}-${ts}.iso`;

    console.log("[os] Download complete:", filename, (total / 1048576).toFixed(1) + "MB");
    dlFn(filename, combined, { type: "application/octet-stream" });
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
    const res = await fetch(templateIsoUrl());
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
    const filename = `ac-os-${piece}-${coreName}-${ts}.iso`;

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

function sim({ needsPaint }) {
  // Auto-start after delay
  if (loadTime > 0 && !autoScroll && Date.now() - loadTime >= autoScrollDelay && scrollY === 0) {
    autoScroll = true;
  }
  // Auto-scroll builds list only
  if (autoScroll && totalContentH > buildsViewH) {
    scrollY += autoScrollSpeed;
    if (scrollY >= totalContentH - buildsViewH) {
      scrollY = 0; // loop
    }
    needsPaint();
  }
}

export const desc = "FedAC OS — view builds and download your copy.";
export { boot, paint, act, sim, leave };
