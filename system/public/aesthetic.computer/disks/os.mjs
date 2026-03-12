// os, 2026.03.12
// FedAC OS — view builds, download personalized bootable USB image.
// Logged-in users with a handle get a .img with their identity baked in.

const RELEASES_URL =
  "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os/releases.json";
const OVEN_IMAGE_URL = "https://oven.aesthetic.computer/os-image";

let releases = null;
let loading = true;
let error = null;
let handle = null;
let token = null;
let downloading = false;
let downloadProgress = 0;
let btn;

function boot({ net, user, api, ui: { Button } }) {
  handle = user?.handle || null;

  // Fetch releases
  fetch(RELEASES_URL)
    .then((r) => r.json())
    .then((data) => {
      releases = data;
      loading = false;
    })
    .catch((err) => {
      error = err.message;
      loading = false;
    });

  // Get auth token if logged in
  if (handle) {
    api?.authorize?.().then((t) => {
      token = t;
    });
  }

  btn = new Button("download your copy", { box: [0, 0, 1, 1] });
}

function paint({ wipe, ink, write, screen, ui, box }) {
  const { width: w, height: h } = screen;
  wipe(20, 12, 30);

  const pad = 14;
  let y = pad;

  // Title
  ink(180, 120, 255);
  write("FedAC OS", { x: pad, y, size: 3 });
  y += 32;

  if (loading) {
    ink(100);
    write("loading releases...", { x: pad, y });
    return;
  }

  if (error) {
    ink(255, 80, 80);
    write("error: " + error, { x: pad, y });
    return;
  }

  if (!releases) return;

  // Latest build
  const latest = releases.releases?.[0];
  if (latest) {
    ink(80, 255, 140);
    write(releases.latest_name || "latest", { x: pad, y, size: 2 });
    y += 22;

    ink(140, 160, 180);
    write("git: " + (latest.git_hash || "?"), { x: pad, y });
    y += 14;
    write("built: " + (latest.build_ts || "?"), { x: pad, y });
    y += 14;
    write(
      "size: " + ((latest.size || 0) / 1048576).toFixed(1) + " MB",
      { x: pad, y },
    );
    y += 20;
  }

  // Download button (only for logged-in users with handle)
  if (handle && token) {
    if (downloading) {
      ink(60, 60, 100);
      box(pad, y, w - pad * 2, 28);
      // Progress bar
      const barW = Math.floor((w - pad * 2 - 4) * downloadProgress);
      ink(100, 180, 255);
      box(pad + 2, y + 2, barW, 24);
      ink(255);
      write(
        "downloading... " + Math.floor(downloadProgress * 100) + "%",
        { x: pad + 8, y: y + 8 },
      );
    } else {
      // Position button
      const btnW = Math.min(240, w - pad * 2);
      btn.box = [pad, y, btnW, 28];
      btn.paint(ui);

      ink(200, 160, 255);
      write("@" + handle, { x: pad + btnW + 10, y: y + 8 });
    }
    y += 38;
  } else {
    ink(120, 100, 80);
    write("log in with a handle to download", { x: pad, y });
    y += 20;
  }

  // Build history
  y += 10;
  ink(100, 80, 140);
  write("recent builds", { x: pad, y, size: 1 });
  y += 16;

  const maxBuilds = Math.floor((h - y - 10) / 14);
  const builds = releases.releases || [];
  for (let i = 0; i < Math.min(builds.length, maxBuilds); i++) {
    const b = builds[i];
    const name = b.name || "?";
    const hash = (b.git_hash || "?").slice(0, 9);
    const ts = (b.build_ts || "").slice(0, 10);
    const isCurrent = i === 0;

    ink(isCurrent ? [80, 255, 140] : [80, 90, 110]);
    write(
      (isCurrent ? "> " : "  ") + name + "  " + hash + "  " + ts,
      { x: pad, y },
    );
    y += 14;
  }
}

async function act({ event: e, download }) {
  if (btn?.trigger(e) && token && !downloading) {
    downloading = true;
    downloadProgress = 0;

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
        }
      }

      // Combine chunks and trigger browser download
      const blob = new Blob(chunks, { type: "application/octet-stream" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = "ac-native-" + (handle || "user") + ".img";
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    } catch (err) {
      console.error("[os] Download failed:", err);
    }

    downloading = false;
    downloadProgress = 0;
  }
}

export const desc = "FedAC OS — view builds and download your copy.";
export { boot, paint, act };
