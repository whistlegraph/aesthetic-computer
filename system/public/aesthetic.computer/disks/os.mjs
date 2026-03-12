// os, 2026.03.12
// FedAC OS — build list with commit messages; download your personalized copy.

const RELEASES_URL = "https://oven.aesthetic.computer/os-releases";
const OVEN_IMAGE_URL = "https://oven.aesthetic.computer/os-image";
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

// Color palette (inspired by commits.mjs)
const C = {
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
};

let uiRef = null; // store ui for later button creation

function boot({ user, api, ui, needsPaint }) {
  handle = user?.handle || null;
  uiRef = ui;

  fetch(RELEASES_URL)
    .then((r) => r.json())
    .then((data) => {
      releases = data;
      loading = false;
      if (handle && token) makeButtons(ui, data);
      needsPaint();
    })
    .catch((err) => {
      error = err.message;
      loading = false;
      needsPaint();
    });

  if (handle) {
    api?.authorize?.().then((t) => {
      token = t;
      if (releases) makeButtons(ui, releases);
      needsPaint();
    });
  }
}

function makeButtons(ui, rel) {
  const latest = rel?.releases?.[0];
  if (!latest) return;
  const name = latest.name || "latest";
  downloadBtn = new ui.TextButton("download " + name, { x: 6, y: 0 });
  updateBootBtn(ui);
}

function updateBootBtn(ui) {
  const piece = BOOT_PIECES[bootPieceIdx];
  bootBtn = new ui.TextButton("boot to: " + piece, { x: 6, y: 0 });
}

function timeAgo(ts) {
  if (!ts) return "";
  const now = Date.now();
  const then = new Date(ts).getTime();
  const sec = Math.floor((now - then) / 1000);
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
  const { screen, ink, line: drawLine } = $;
  const { width: w, height: h } = screen;
  $.wipe(...C.bg);

  const pad = 6;
  const charW = 6;
  const rowH = 10;
  let y = 22 - scrollY;

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

  // Download section for logged-in users
  if (handle && token && downloadBtn && !downloading) {
    // Row 1: download button + @handle
    downloadBtn.reposition({ x: pad, y });
    downloadBtn.paint(
      $,
      [[20, 60, 30], [40, 120, 50], [80, 255, 140], 255],
      [[40, 100, 50], [60, 160, 70], [255, 255, 255], 255],
      undefined,
      [[30, 80, 40], [50, 140, 60], [200, 255, 220], 255],
    );
    ink(...C.handle).write("@" + handle, {
      x: pad + downloadBtn.width + 8,
      y: y + 4,
    });
    y += downloadBtn.height + 4;

    // Row 2: boot-to selector (tap to cycle)
    if (bootBtn) {
      bootBtn.reposition({ x: pad, y });
      bootBtn.paint(
        $,
        [[30, 35, 55], [50, 60, 90], ...C.bootPiece, 200],
        [[45, 50, 75], [65, 75, 110], [255, 255, 255], 255],
        undefined,
        [[38, 42, 65], [58, 68, 100], [255, 220, 150], 230],
      );
      y += bootBtn.height + 6;
    }
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
    ink(...C.loginHint).write("log in to download", { x: pad, y });
    y += rowH + 4;
  }

  // Build list — 2 rows per build
  for (let i = 0; i < builds.length; i++) {
    const entryH = 24;
    if (y > h + entryH) break;

    const b = builds[i];
    const isCurrent = i === 0;
    const name = b.name || "?";
    const hash = (b.git_hash || "?").slice(0, 7);
    const ago = timeAgo(b.build_ts);
    const msg = b.commit_msg || "";
    const who = b.handle || "";

    if (y >= -entryH) {
      const marker = isCurrent ? "> " : "  ";
      let x = pad;

      ink(...(isCurrent ? C.current : C.nameOld));
      $.write(marker + name, { x, y });
      x += (marker.length + name.length + 1) * charW;

      ink(...(isCurrent ? C.hash : C.hashOld));
      $.write(hash, { x, y });
      x += (hash.length + 1) * charW;

      if (ago) {
        ink(...C.date);
        $.write(ago, { x, y });
        x += (ago.length + 1) * charW;
      }

      if (who) {
        ink(...(isCurrent ? C.handle : C.handleOld));
        $.write("@" + who, { x, y });
      }

      if (msg) {
        const maxChars = Math.floor((w - pad * 2 - charW) / charW);
        const display = msg.length > maxChars ? msg.slice(0, maxChars - 1) + "~" : msg;
        ink(...(isCurrent ? C.msg : C.msgOld));
        $.write("  " + display, { x: pad, y: y + rowH + 1 });
      }

      if (i < builds.length - 1) {
        ink(...C.bar, isCurrent ? 40 : 20);
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
        if (uiRef) updateBootBtn(uiRef);
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
  downloadStatus = "connecting... (boot to: " + piece + ")";
  needsPaint();

  try {
    const url = OVEN_IMAGE_URL + "?piece=" + encodeURIComponent(piece);
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
    const buildName = "ac-" + (latest?.name || "native");
    const pieceSuffix = piece !== "notepat" ? "-" + piece : "";
    const filename = `@${handle || "user"}-os-${buildName}${pieceSuffix}.img`;

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
export { boot, paint, act };
