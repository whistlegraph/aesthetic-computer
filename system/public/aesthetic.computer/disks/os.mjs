// os, 2026.03.12
// FedAC OS — flat list of builds; tap the latest (green) to download
// a personalized .img with your handle baked in.

const RELEASES_URL = "https://oven.aesthetic.computer/os-releases";
const OVEN_IMAGE_URL = "https://oven.aesthetic.computer/os-image";

let releases = null;
let loading = true;
let error = null;
let handle = null;
let token = null;
let downloading = false;
let downloadProgress = 0;
let downloadBtn = null;
let scrollY = 0;

function boot({ user, api, ui, needsPaint }) {
  handle = user?.handle || null;

  fetch(RELEASES_URL)
    .then((r) => r.json())
    .then((data) => {
      releases = data;
      loading = false;
      if (handle && token) makeBtn(ui, data);
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
      if (releases) makeBtn(ui, releases);
      needsPaint();
    });
  }
}

function makeBtn(ui, rel) {
  const latest = rel?.releases?.[0];
  if (!latest) return;
  const name = latest.name || "latest";
  downloadBtn = new ui.TextButton("download " + name, { x: 14, y: 0 });
}

function paint($) {
  const { screen, ink } = $;
  const { width: w, height: h } = screen;
  $.wipe(20, 12, 30);

  const pad = 14;
  let y = 22 - scrollY; // Start below HUD corner label

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

  // Download button row for logged-in users
  if (handle && token && downloadBtn && !downloading) {
    downloadBtn.reposition({ x: pad, y });
    downloadBtn.paint(
      $,
      [[30, 80, 40], [50, 140, 60], [80, 255, 140], 255],
      [[50, 120, 60], [70, 180, 80], [255, 255, 255], 255],
      undefined,
      [[40, 100, 50], [60, 160, 70], [200, 255, 220], 255],
    );
    if (handle) {
      ink(200, 160, 255).write("@" + handle, {
        x: pad + downloadBtn.width + 10,
        y: y + 4,
      });
    }
    y += downloadBtn.height + 8;
  } else if (downloading) {
    ink(60, 60, 100).box(pad, y, w - pad * 2, 20);
    const barW = Math.floor((w - pad * 2 - 4) * downloadProgress);
    ink(100, 180, 255).box(pad + 2, y + 2, barW, 16);
    ink(255).write(
      Math.floor(downloadProgress * 100) + "%",
      { x: pad + 4, y: y + 5 },
    );
    y += 28;
  } else if (!handle || !token) {
    ink(80, 60, 50).write("log in to download", { x: pad, y });
    y += 16;
  }

  // Flat build list
  for (let i = 0; i < builds.length; i++) {
    if (y > h + 14) break;
    const b = builds[i];
    const name = b.name || "?";
    const hash = (b.git_hash || "?").slice(0, 9);
    const ts = (b.build_ts || "").slice(0, 10);
    const isCurrent = i === 0;

    if (y >= -14) {
      ink(isCurrent ? [80, 255, 140] : [60, 70, 90]).write(
        (isCurrent ? "> " : "  ") + name + "  " + hash + "  " + ts,
        { x: pad, y },
      );
    }
    y += 14;
  }
}

function act({ event: e, needsPaint }) {
  if (e.is("scroll")) {
    scrollY = Math.max(0, scrollY + (e.delta || 0));
    needsPaint();
    return;
  }

  if (downloading || !token || !downloadBtn) return;

  downloadBtn.btn.act(e, {
    push: () => startDownload(needsPaint),
  });
}

async function startDownload(needsPaint) {
  downloading = true;
  downloadProgress = 0;
  needsPaint();

  try {
    const res = await fetch(OVEN_IMAGE_URL, {
      headers: { Authorization: "Bearer " + token },
    });

    if (!res.ok) {
      const err = await res.json().catch(() => ({}));
      throw new Error(err.error || "Download failed: " + res.status);
    }

    const contentLength = parseInt(res.headers.get("content-length") || "0");
    const reader = res.body.getReader();
    const chunks = [];
    let received = 0;

    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      chunks.push(value);
      received += value.length;
      if (contentLength > 0) {
        downloadProgress = received / contentLength;
        needsPaint();
      }
    }

    const blob = new Blob(chunks, { type: "application/octet-stream" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    const latest = releases?.releases?.[0];
    const name = latest?.name || "os";
    const ts = (latest?.build_ts || "").replace(/[T:]/g, "-");
    a.download = `ac-native-${name}-${handle || "user"}-${ts}.img`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  } catch (err) {
    console.error("[os] Download failed:", err);
  }

  downloading = false;
  downloadProgress = 0;
  needsPaint();
}

export const desc = "FedAC OS — view builds and download your copy.";
export { boot, paint, act };
