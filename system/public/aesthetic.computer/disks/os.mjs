// os, 2026.03.12
// FedAC OS — build list with commit messages; download your personalized copy.

const OVEN_BASE = "https://oven.aesthetic.computer";
const RELEASES_URL = OVEN_BASE + "/os-releases";
const OVEN_IMAGE_URL = OVEN_BASE + "/os-image";
const OVEN_WS_URL = "wss://oven.aesthetic.computer/ws";
const BOOT_PIECES = ["notepat", "prompt", "chat", "laer-klokken"];

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
let scrollY = 0;
let dlFn = null;
let ovenWs = null;
let wsReconnectTimer = null;
let npRef = null; // stored needsPaint for live updates

// Device token status
let hasClaude = null; // null=loading, true/false
let hasGit = null;
let setupBtn = null; // "set up tokens" button shown when missing
let showTokenHint = false;

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
  fetch(RELEASES_URL)
    .then((r) => r.json())
    .then((data) => {
      releases = data;
      loading = false;
      if (handle && token && ui) makeButtons(ui, data);
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

function connectOvenWs(ui, needsPaint) {
  if (ovenWs) { try { ovenWs.close(); } catch (_) {} }
  if (wsReconnectTimer) { clearTimeout(wsReconnectTimer); wsReconnectTimer = null; }

  try {
    ovenWs = new WebSocket(OVEN_WS_URL);
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
        if (handle && token && ui) makeButtons(ui, releases);
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

  if (handle) {
    console.log("[os] Authorizing @" + handle + "...");
    api?.authorize?.().then((t) => {
      token = t;
      console.log("[os] Authorized, token:", t ? "ok" : "missing");
      if (releases) makeButtons(ui, releases);
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
    console.log("[os] No handle — download disabled");
  }
}

function leave() {
  if (wsReconnectTimer) { clearTimeout(wsReconnectTimer); wsReconnectTimer = null; }
  if (ovenWs) { try { ovenWs.close(); } catch (_) {} ovenWs = null; }
}

function makeButtons(ui, rel) {
  const latest = rel?.releases?.[0];
  if (!latest) return;
  updateDownloadBtn(ui);
  updateBootBtn(ui);
}

function updateDownloadBtn(ui) {
  downloadBtn = new ui.TextButton("download", { x: 6, y: 0 });
}

function osLabel() {
  const latest = releases?.releases?.[0];
  const coreName = "AC-" + (latest?.name || "native");
  const piece = BOOT_PIECES[bootPieceIdx];
  return `@${handle}-os-${piece}-${coreName}`;
}

function updateBootBtn(ui) {
  const piece = BOOT_PIECES[bootPieceIdx];
  bootBtn = new ui.TextButton("boot to: " + piece, { x: 6, y: 0 });
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

function paint($) {
  const { screen, ink, line: drawLine, dark } = $;
  const { width: w, height: h } = screen;
  const C = dark ? scheme.dark : scheme.light;
  $.wipe(...C.bg);

  const pad = 6;
  const charW = 6;
  const rowH = 10;
  const matrixH = 9;
  let y = 18 - scrollY;

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

  // One-line description
  ink(...C.instText);
  $.write("A Linux kernel with an embedded initramfs — boots any x86 PC from USB.", { x: pad, y, wrap: w - pad * 2 }, undefined, undefined, false, "MatrixChunky8");
  const descLen = 70 * 6; // approximate char count * charW
  const descLines = Math.ceil(descLen / (w - pad * 2));
  y += matrixH * descLines + 6;

  // Divider
  ink(...C.divider);
  drawLine(pad, y, w - pad, y);
  y += 6;

  // Download section for logged-in users
  if (handle && token && downloadBtn && !downloading) {
    // OS label (big, personal)
    const label = osLabel();
    ink(...C.handle).write(label, { x: pad, y, wrap: w - pad * 2 });
    const labelLines = Math.ceil(label.length * charW / (w - pad * 2));
    y += rowH * labelLines + 4;

    // Device token status
    if (hasClaude !== null) {
      const cIcon = hasClaude ? "+" : "-";
      const gIcon = hasGit ? "+" : "-";
      ink(...(hasClaude ? C.current : [255, 80, 80]));
      $.write(cIcon + " claude", { x: pad, y });
      ink(...(hasGit ? C.current : [255, 80, 80]));
      $.write(gIcon + " git", { x: pad + 60, y });
      y += rowH + 2;

      if ((!hasClaude || !hasGit) && setupBtn) {
        setupBtn.reposition({ x: pad, y });
        setupBtn.paint(
          $,
          [[60, 30, 30], [140, 60, 60], [255, 120, 100], 255],
          [[80, 40, 40], [180, 80, 80], [255, 255, 255], 255],
          undefined,
          [[60, 30, 30], [140, 60, 60], [255, 120, 100], 230],
        );
        y += setupBtn.height + 4;
        if (showTokenHint) {
          ink(...C.instText);
          $.write("on device, type in prompt:", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
          y += matrixH + 2;
          if (!hasClaude) {
            ink(...C.instKey);
            $.write("  claude sk-ant-XXXX", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
            y += matrixH + 2;
          }
          if (!hasGit) {
            ink(...C.instKey);
            $.write("  git ghp_XXXX", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
            y += matrixH + 2;
          }
          y += 4;
        }
      } else if (hasClaude && hasGit) {
        ink(...C.date);
        $.write("device ready", { x: pad + 120, y: y - rowH - 2 });
      }
    }

    // Boot-to selector
    if (bootBtn) {
      bootBtn.reposition({ x: pad, y });
      bootBtn.paint(
        $,
        [C.bootBtnBg, C.bootBtnBorder, ...C.bootPiece, 200],
        [C.bootBtnHoverBg, C.bootBtnHoverBorder, [255, 255, 255], 255],
        undefined,
        [C.bootBtnBg, C.bootBtnBorder, ...C.bootPiece, 230],
      );
      y += bootBtn.height + 4;
    }

    // Download button
    downloadBtn.reposition({ x: pad, y });
    downloadBtn.paint(
      $,
      [C.dlBtnBg, C.dlBtnBorder, C.dlBtnText, 255],
      [C.dlBtnHoverBg, C.dlBtnHoverBorder, [255, 255, 255], 255],
      undefined,
      [C.dlBtnBg, C.dlBtnBorder, C.dlBtnText, 255],
    );
    y += downloadBtn.height + 6;

    // Divider
    ink(...C.divider);
    drawLine(pad, y, w - pad, y);
    y += 6;

    // "How to Install" header
    ink(...C.instHeader).write("How to Install", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    y += matrixH + 4;

    // Instructions (matrix font)
    ink(...C.instText).write("1 flash .iso with Fedora Media Writer", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    y += matrixH + 2;
    ink(...C.instText).write("2 plug USB into any x86 PC", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    y += matrixH + 2;
    ink(...C.instText).write("3 enter BIOS boot menu:", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    y += matrixH + 2;
    ink(...C.instKey).write("  F12 Dell/Lenovo  F9 HP", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    y += matrixH + 2;
    ink(...C.instKey).write("  F2 ASUS/Acer  ESC others", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    y += matrixH + 2;
    ink(...C.instText).write("4 select USB drive to boot", { x: pad, y }, undefined, undefined, false, "MatrixChunky8");
    y += matrixH + 8;

    // Divider before builds
    ink(...C.divider);
    drawLine(pad, y, w - pad, y);
    y += 6;
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
      ink(140, 160, 180).write(downloadStatus, { x: pad, y });
      y += rowH + 2;
    }
  } else if (!handle || !token) {
    ink(...C.loginHint).write("log in to download your os", { x: pad, y });
    y += rowH + 4;
  }

  // Build list — 2 rows per build
  for (let i = 0; i < builds.length; i++) {
    const entryH = 24;
    if (y > h + entryH) break;

    const b = builds[i];
    const isCurrent = i === 0 && !b.deprecated;
    const isDep = !!b.deprecated;
    const name = b.name || "?";
    const hash = (b.git_hash || "?").slice(0, 7);
    const ago = timeAgo(b.build_ts);
    const msg = b.commit_msg || "";
    const who = b.handle || "";

    if (y >= -entryH) {
      const marker = isCurrent ? "> " : "  ";
      let x = pad;

      ink(...(isCurrent ? C.current : isDep ? C.depName : C.nameOld));
      $.write(marker + name, { x, y });
      const nameEndX = x + (marker.length + name.length) * charW;
      x = nameEndX + charW;

      ink(...(isCurrent ? C.hash : isDep ? C.depHash : C.hashOld));
      $.write(hash, { x, y });
      x += (hash.length + 1) * charW;

      if (ago) {
        ink(...(isDep ? C.depHash : C.date));
        $.write(ago, { x, y });
        x += (ago.length + 1) * charW;
      }

      if (who) {
        ink(...(isCurrent ? C.handle : isDep ? C.depHandle : C.handleOld));
        $.write("@" + who, { x, y });
      }

      // Strikethrough line for deprecated builds
      if (isDep) {
        ink(...C.depStrike, 140);
        const lineEndX = who ? x + (who.length + 1) * charW : x;
        drawLine(pad + charW * 2, y + 4, lineEndX, y + 4);
      }

      if (msg) {
        const maxChars = Math.floor((w - pad * 2 - charW) / charW);
        const display = msg.length > maxChars ? msg.slice(0, maxChars - 1) + "~" : msg;
        ink(...(isCurrent ? C.msg : isDep ? C.depMsg : C.msgOld));
        $.write("  " + display, { x: pad, y: y + rowH + 1 });
        if (isDep) {
          ink(...C.depStrike, 100);
          const msgW = Math.min(display.length + 2, maxChars) * charW;
          drawLine(pad, y + rowH + 5, pad + msgW, y + rowH + 5);
        }
      }

      if (i < builds.length - 1) {
        ink(...(isDep ? C.depBar : C.bar), isCurrent ? 40 : 20);
        drawLine(pad, y + entryH - 2, w - pad, y + entryH - 2);
      }
    }
    y += entryH;
  }

  // Footer
  if (builds.length > 0) {
    const countLabel = builds.length + " builds";
    ink(...C.date);
    $.write(countLabel, { x: w - pad - countLabel.length * charW, y: h - rowH - 2 });
  }
}

function act({ event: e, needsPaint, download }) {
  dlFn = download;

  if (e.is("dark-mode") || e.is("light-mode")) {
    needsPaint();
    return;
  }

  if (e.is("scroll")) {
    scrollY = Math.max(0, scrollY + (e.delta || 0));
    needsPaint();
    return;
  }

  if (downloading) return;

  // Boot-to button: cycle through pieces on tap
  if (bootBtn && token) {
    bootBtn.btn.act(e, {
      push: () => {
        bootPieceIdx = (bootPieceIdx + 1) % BOOT_PIECES.length;
        if (uiRef) {
          updateBootBtn(uiRef);
        }
        needsPaint();
      },
    });
  }

  // Setup tokens button: show hint text
  if (setupBtn && token) {
    setupBtn.btn.act(e, {
      push: () => {
        // Toggle hint visibility
        showTokenHint = !showTokenHint;
        needsPaint();
      },
    });
  }

  if (!token || !downloadBtn) return;

  downloadBtn.btn.act(e, {
    push: () => startDownload(needsPaint),
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
    const url = OVEN_IMAGE_URL + "?piece=" + encodeURIComponent(piece);
    console.log("[os] Fetching:", url);
    const res = await fetch(url, {
      headers: { Authorization: "Bearer " + token },
    });

    if (!res.ok) {
      const err = await res.json().catch(() => ({}));
      throw new Error(err.error || "Download failed: " + res.status);
    }

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
    const coreName = "AC-" + (latest?.name || "native");
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

export const desc = "FedAC OS — view builds and download your copy.";
export { boot, paint, act, leave };
