// os-builder.mjs — FedAC OS image assembly for the /os endpoint
//
// Downloads a pre-baked base image from CDN, injects a piece bundle into
// the FEDAC-PIECE ext4 partition via debugfs, and streams the result.
//
// After building, uploads the finished ISO to DO Spaces CDN so repeat
// downloads are served at CDN speed (~100+ MB/s) instead of droplet speed.
//
// Requires: e2fsprogs (debugfs) installed on the server.

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import { execSync } from "child_process";
import { createHash, randomUUID } from "crypto";
import { createBundle, createJSPieceBundle } from "./bundler.mjs";
import { S3Client, PutObjectCommand, HeadObjectCommand } from "@aws-sdk/client-s3";
import { MongoClient } from "mongodb";

// ─── MongoDB (OS build history) ─────────────────────────────────────

let mongoClient;
let mongoDB;

async function connectMongo() {
  if (mongoDB) return mongoDB;
  const uri = process.env.MONGODB_CONNECTION_STRING;
  const dbName = process.env.MONGODB_NAME;
  if (!uri || !dbName) return null;
  try {
    mongoClient = await MongoClient.connect(uri);
    mongoDB = mongoClient.db(dbName);
    console.log("✅ [os] Connected to MongoDB for OS build history");
    return mongoDB;
  } catch (err) {
    console.error("❌ [os] MongoDB connect failed:", err.message);
    return null;
  }
}

connectMongo();

async function logOSBuild(record) {
  try {
    const db = await connectMongo();
    if (!db) return;
    await db.collection("oven-os-builds").insertOne({ ...record, when: new Date() });
  } catch (err) {
    console.error("[os] Failed to log build:", err.message);
  }
}

// ─── Configuration ──────────────────────────────────────────────────

const CACHE_DIR = process.env.OS_CACHE_DIR || "/opt/oven/cache";
const TEMP_DIR = process.env.OS_TEMP_DIR || "/tmp";
const BASE_IMAGE_URLS = {
  fedora: process.env.FEDAC_BASE_IMAGE_URL ||
    "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/os/fedac-base-latest.img",
  alpine: process.env.ALPINE_BASE_IMAGE_URL ||
    "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/os/alpine-base-latest.img",
};
const MANIFEST_URLS = {
  fedora: process.env.FEDAC_MANIFEST_URL ||
    "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/os/fedac-base-manifest.json",
  alpine: process.env.ALPINE_MANIFEST_URL ||
    "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/os/alpine-base-manifest.json",
};
// Legacy aliases
const BASE_IMAGE_URL = BASE_IMAGE_URLS.fedora;
const MANIFEST_URL = MANIFEST_URLS.fedora;

// ─── CDN Cache (DO Spaces) ──────────────────────────────────────────
// After building an ISO, upload it to Spaces so repeat downloads bypass
// the droplet entirely and go through the CDN edge network.

const SPACES_REGION = process.env.OS_SPACES_REGION || "us-east-1";
const SPACES_ENDPOINT =
  process.env.OS_SPACES_ENDPOINT ||
  process.env.ART_SPACES_ENDPOINT ||
  "https://sfo3.digitaloceanspaces.com";
const SPACES_BUCKET = process.env.OS_SPACES_BUCKET || "assets-aesthetic-computer";
const SPACES_CDN_BASE = (
  process.env.OS_SPACES_CDN_BASE ||
  "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com"
).replace(/\/+$/, "");
const SPACES_PREFIX = "os/builds";

function getSpacesClient() {
  const accessKeyId = process.env.OS_SPACES_KEY || process.env.ART_SPACES_KEY;
  const secretAccessKey = process.env.OS_SPACES_SECRET || process.env.ART_SPACES_SECRET;
  if (!accessKeyId || !secretAccessKey) return null;
  return new S3Client({
    region: SPACES_REGION,
    endpoint: SPACES_ENDPOINT,
    credentials: { accessKeyId, secretAccessKey },
  });
}

function buildCDNKey(target, density, bundleHash, baseVersion, flavor = "alpine") {
  // e.g. os/builds/notepat-d8-ab12cd34-v2025-02-26-alpine.img
  const safe = String(target).replace(/[^a-zA-Z0-9_-]/g, "_");
  return `${SPACES_PREFIX}/${safe}-d${density}-${bundleHash}-v${baseVersion}-${flavor}.img`;
}

function buildCDNUrl(key) {
  return `${SPACES_CDN_BASE}/${key}`;
}

// Check if a cached ISO already exists on CDN.
async function checkCDNCache(key) {
  const client = getSpacesClient();
  if (!client) return false;
  try {
    await client.send(new HeadObjectCommand({ Bucket: SPACES_BUCKET, Key: key }));
    return true;
  } catch {
    return false;
  }
}

// Upload a built ISO to CDN for fast repeat downloads.
async function uploadToCDN(imagePath, key, target) {
  const client = getSpacesClient();
  if (!client) return null;
  await client.send(
    new PutObjectCommand({
      Bucket: SPACES_BUCKET,
      Key: key,
      Body: fsSync.createReadStream(imagePath),
      ContentType: "application/octet-stream",
      ContentDisposition: `attachment; filename="${target}-os.iso"`,
      ACL: "public-read",
      CacheControl: "public, max-age=86400", // 24h — rebuild invalidates via new hash
    }),
  );
  return buildCDNUrl(key);
}

function hashContent(content) {
  return createHash("sha256").update(content).digest("hex").slice(0, 8);
}

// Concurrency limit — each build copies ~3GB, so cap parallel builds.
const MAX_CONCURRENT_BUILDS = 2;
let activeBuildCount = 0;
const recentBuilds = [];
const MAX_RECENT = 20;

function formatSeconds(ms) {
  return `${(ms / 1000).toFixed(1)}s`;
}

function runCommand(command) {
  execSync(command, { stdio: "pipe" });
}

function copyBaseImageFast(basePath, tempImagePath) {
  // Reflink/sparse copy is usually much faster for large images on CoW filesystems.
  try {
    runCommand(`cp --reflink=auto --sparse=always "${basePath}" "${tempImagePath}"`);
    return "reflink";
  } catch {
    return "regular";
  }
}

// ─── Manifest ───────────────────────────────────────────────────────

const cachedManifests = {};

async function fetchManifest(onProgress, flavor = "alpine") {
  if (cachedManifests[flavor]) return cachedManifests[flavor];
  const manifestUrl = MANIFEST_URLS[flavor] || MANIFEST_URLS.fedora;
  onProgress?.({ stage: "manifest", message: `Fetching ${flavor} base image manifest...`, step: 1, totalSteps: 9 });
  const res = await fetch(manifestUrl);
  if (!res.ok) throw new Error(`Manifest fetch failed (${flavor}): ${res.status}`);
  cachedManifests[flavor] = await res.json();
  const m = cachedManifests[flavor];
  const distroLabel = m.alpine ? `Alpine ${m.alpine}` : m.fedora ? `Fedora ${m.fedora}` : flavor;
  onProgress?.({
    stage: "manifest",
    message: `Base image v${m.version}, ${distroLabel}`,
    step: 1,
    totalSteps: 9,
  });
  return m;
}

// Invalidate manifest cache (call after base image update).
export function invalidateManifest(flavor) {
  if (flavor) {
    delete cachedManifests[flavor];
  } else {
    for (const key of Object.keys(cachedManifests)) delete cachedManifests[key];
  }
}

// ─── Base Image Cache ───────────────────────────────────────────────

async function ensureBaseImage(onProgress, flavor = "alpine") {
  await fs.mkdir(CACHE_DIR, { recursive: true });
  const manifest = await fetchManifest(onProgress, flavor);
  const basePath = path.join(CACHE_DIR, `${flavor}-base.img`);
  const hashPath = `${basePath}.sha256`;

  // Check if cached image matches manifest size AND sha256 hash.
  try {
    const stat = await fs.stat(basePath);
    const cachedHash = (await fs.readFile(hashPath, "utf-8")).trim();
    if (stat.size === manifest.totalSize && cachedHash === manifest.sha256) {
      onProgress?.({ stage: "base", message: `${flavor} base image cached and ready`, step: 1, totalSteps: 9 });
      return { basePath, manifest };
    }
    onProgress?.({ stage: "base", message: `${flavor} base image outdated (hash mismatch), re-downloading...`, step: 1, totalSteps: 9 });
  } catch {
    // File or hash sidecar doesn't exist — need to download.
  }

  // Download base image.
  const baseImageUrl = BASE_IMAGE_URLS[flavor] || BASE_IMAGE_URLS.fedora;
  onProgress?.({ stage: "base", message: `Downloading ${flavor} base image from CDN...`, step: 1, totalSteps: 9 });
  const res = await fetch(baseImageUrl);
  if (!res.ok) throw new Error(`Base image download failed (${flavor}): ${res.status}`);

  const tmpPath = `${basePath}.downloading`;
  const writer = fsSync.createWriteStream(tmpPath);
  const reader = res.body.getReader();
  let downloaded = 0;
  const total = manifest.totalSize || parseInt(res.headers.get("content-length"), 10) || 0;

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    writer.write(value);
    downloaded += value.length;
    if (total > 0 && downloaded % (50 * 1024 * 1024) < value.length) {
      const pct = Math.round((downloaded / total) * 100);
      onProgress?.({ stage: "base", message: `Downloading base image... ${pct}%` });
    }
  }

  writer.end();
  await new Promise((resolve, reject) => {
    writer.on("finish", resolve);
    writer.on("error", reject);
  });

  await fs.rename(tmpPath, basePath);
  // Write sha256 sidecar so future cache checks validate the hash, not just size.
  if (manifest.sha256) await fs.writeFile(hashPath, manifest.sha256 + "\n");
  onProgress?.({ stage: "base", message: "Base image downloaded and cached" });
  return { basePath, manifest };
}

// ─── Piece Injection ────────────────────────────────────────────────

function extractPartition(imagePath, manifest, tempPartPath) {
  const { piecePartitionOffset, piecePartitionSize } = manifest;
  try {
    runCommand(
      `dd if="${imagePath}" of="${tempPartPath}" bs=4M iflag=skip_bytes,count_bytes skip=${piecePartitionOffset} count=${piecePartitionSize} status=none`,
    );
  } catch {
    runCommand(
      `dd if="${imagePath}" of="${tempPartPath}" bs=512 skip=${Math.floor(piecePartitionOffset / 512)} count=${Math.floor(piecePartitionSize / 512)} 2>/dev/null`,
    );
  }
}

function injectFilesIntoPartition(tempPartPath, files) {
  for (const [filename, content] of Object.entries(files)) {
    const safeName = filename.replace(/[^a-zA-Z0-9._-]/g, "_");
    const tmpPath = `${tempPartPath}.${safeName}.${randomUUID().slice(0, 8)}`;
    fsSync.writeFileSync(tmpPath, content);

    try {
      try {
        runCommand(`debugfs -w -R "rm ${filename}" "${tempPartPath}" 2>/dev/null`);
      } catch {
        // File may not exist yet.
      }
      runCommand(`debugfs -w -R "write ${tmpPath} ${filename}" "${tempPartPath}"`);
    } finally {
      try {
        fsSync.unlinkSync(tmpPath);
      } catch {
        // ignore
      }
    }
  }
}

function writePartitionBack(imagePath, manifest, tempPartPath) {
  const { piecePartitionOffset, piecePartitionSize } = manifest;
  try {
    runCommand(
      `dd if="${tempPartPath}" of="${imagePath}" bs=4M oflag=seek_bytes seek=${piecePartitionOffset} count=${piecePartitionSize} iflag=count_bytes conv=notrunc,fsync status=none`,
    );
  } catch {
    runCommand(
      `dd if="${tempPartPath}" of="${imagePath}" bs=512 seek=${Math.floor(piecePartitionOffset / 512)} count=${Math.floor(piecePartitionSize / 512)} conv=notrunc 2>/dev/null`,
    );
  }
}

function buildFedOSShellHTML(target) {
  const escapedTarget = String(target || "piece").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1,viewport-fit=cover">
  <title>FedOS - ${escapedTarget}</title>
  <style>
    :root {
      --bg: #06060b;
      --surface: rgba(17, 13, 27, 0.88);
      --surface-strong: rgba(29, 20, 44, 0.95);
      --border: rgba(248, 134, 206, 0.28);
      --text: #f8efff;
      --muted: #a99bc7;
      --accent: #f46ac7;
      --ok: #85f2c8;
      --warn: #ffd08a;
      --danger: #ff9bb4;
    }
    * { box-sizing: border-box; }
    html, body {
      width: 100%;
      height: 100%;
      margin: 0;
      padding: 0;
      overflow: hidden;
      background:
        radial-gradient(1200px 500px at 10% -20%, rgba(244, 106, 199, 0.28), transparent 60%),
        radial-gradient(1200px 500px at 100% 0%, rgba(110, 120, 255, 0.18), transparent 55%),
        var(--bg);
      color: var(--text);
      font-family: "IBM Plex Mono", "Fira Mono", monospace;
    }
    .shell {
      position: absolute;
      inset: 0;
      display: grid;
      grid-template-rows: auto 1fr;
      gap: 8px;
      padding: 10px;
    }
    .topbar {
      min-height: 44px;
      border: 1px solid var(--border);
      border-radius: 14px;
      background: var(--surface);
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
      padding: 6px 10px;
      backdrop-filter: blur(8px);
    }
    .title {
      font-size: 11px;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      color: var(--muted);
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      max-width: 34vw;
    }
    .status-row {
      display: flex;
      align-items: center;
      gap: 6px;
      margin-left: auto;
      min-width: 0;
    }
    .pill {
      border: 1px solid var(--border);
      border-radius: 999px;
      padding: 4px 9px;
      min-width: 84px;
      text-align: center;
      font-size: 11px;
      line-height: 1;
      background: rgba(8, 7, 14, 0.7);
      color: var(--text);
      white-space: nowrap;
    }
    .pill.offline { color: var(--danger); border-color: rgba(255, 155, 180, 0.6); }
    .pill.low { color: var(--warn); border-color: rgba(255, 208, 138, 0.6); }
    .pill.ok { color: var(--ok); border-color: rgba(133, 242, 200, 0.55); }
    .pill.muted { color: var(--muted); border-color: rgba(169, 155, 199, 0.4); }
    .vol-wrap {
      position: relative;
      display: flex;
      align-items: center;
    }
    .vol-slider-panel {
      position: absolute;
      top: calc(100% + 8px);
      right: 0;
      border: 1px solid var(--border);
      border-radius: 10px;
      background: var(--surface-strong);
      padding: 10px 14px;
      display: none;
      z-index: 40;
      backdrop-filter: blur(10px);
      min-width: 160px;
    }
    .vol-slider-panel.show { display: block; }
    .vol-slider-panel input[type="range"] {
      width: 100%;
      accent-color: var(--accent);
      cursor: pointer;
    }
    .wifi-toggle {
      border: 1px solid var(--border);
      border-radius: 999px;
      background: rgba(8, 7, 14, 0.72);
      color: var(--text);
      font: inherit;
      font-size: 11px;
      padding: 4px 10px;
      cursor: pointer;
      display: none;
    }
    .wifi-toggle.show { display: inline-block; }
    .frame-wrap {
      position: relative;
      min-height: 0;
      border: 1px solid var(--border);
      border-radius: 16px;
      overflow: hidden;
      background: #000;
      box-shadow: 0 16px 34px rgba(0, 0, 0, 0.42);
    }
    iframe {
      width: 100%;
      height: 100%;
      border: 0;
      display: block;
      background: #000;
    }
    .wifi-panel {
      position: absolute;
      top: 58px;
      right: 10px;
      width: min(360px, calc(100vw - 20px));
      max-height: calc(100vh - 78px);
      overflow: auto;
      border: 1px solid var(--border);
      border-radius: 14px;
      background: var(--surface-strong);
      padding: 10px;
      display: none;
      z-index: 30;
      backdrop-filter: blur(10px);
    }
    .wifi-panel.show { display: block; }
    .wifi-meta {
      font-size: 11px;
      color: var(--muted);
      margin-bottom: 8px;
    }
    .wifi-list {
      display: grid;
      gap: 6px;
      margin-bottom: 8px;
    }
    .wifi-item {
      border: 1px solid var(--border);
      border-radius: 10px;
      padding: 8px;
      background: rgba(8, 7, 14, 0.64);
      cursor: pointer;
      font-size: 12px;
      color: var(--text);
    }
    .wifi-item.selected {
      border-color: rgba(133, 242, 200, 0.8);
      box-shadow: inset 0 0 0 1px rgba(133, 242, 200, 0.55);
    }
    .wifi-row {
      display: flex;
      align-items: center;
      gap: 6px;
      margin-top: 8px;
    }
    .wifi-row input {
      flex: 1;
      min-width: 0;
      border-radius: 10px;
      border: 1px solid var(--border);
      background: rgba(8, 7, 14, 0.7);
      color: var(--text);
      padding: 8px;
      font: inherit;
      font-size: 12px;
    }
    .wifi-row button {
      border-radius: 10px;
      border: 1px solid var(--border);
      background: rgba(244, 106, 199, 0.2);
      color: var(--text);
      padding: 8px 10px;
      font: inherit;
      font-size: 12px;
      cursor: pointer;
    }
    .wifi-row button:disabled { opacity: 0.45; cursor: default; }
  </style>
</head>
<body>
  <div class="shell">
    <div class="topbar" aria-live="polite">
      <div class="title">fedos - ${escapedTarget}</div>
      <div class="status-row">
        <div class="vol-wrap">
          <div id="status-volume" class="pill" style="cursor:pointer">VOL --</div>
          <div id="vol-slider-panel" class="vol-slider-panel">
            <input id="vol-slider" type="range" min="0" max="150" value="50" step="1">
          </div>
        </div>
        <div id="status-battery" class="pill">BAT --</div>
        <div id="status-network" class="pill">NET --</div>
        <button id="wifi-toggle" class="wifi-toggle" type="button">WIFI</button>
      </div>
    </div>

    <div class="frame-wrap">
      <iframe id="piece-frame" src="./piece-app.html" allow="autoplay; fullscreen"></iframe>
    </div>
  </div>

  <aside id="wifi-panel" class="wifi-panel" aria-live="polite">
    <div id="wifi-meta" class="wifi-meta">WiFi API unavailable</div>
    <div id="wifi-list" class="wifi-list"></div>
    <div class="wifi-row">
      <input id="wifi-pass" type="password" placeholder="password">
      <button id="wifi-connect" type="button" disabled>Connect</button>
    </div>
  </aside>

  <script>
    (function () {
      const batteryEl = document.getElementById("status-battery");
      const networkEl = document.getElementById("status-network");
      const volumeEl = document.getElementById("status-volume");
      const volPanel = document.getElementById("vol-slider-panel");
      const volSlider = document.getElementById("vol-slider");
      const wifiToggle = document.getElementById("wifi-toggle");
      const wifiPanel = document.getElementById("wifi-panel");
      const wifiMeta = document.getElementById("wifi-meta");
      const wifiList = document.getElementById("wifi-list");
      const wifiPass = document.getElementById("wifi-pass");
      const wifiConnect = document.getElementById("wifi-connect");

      const suppressedKeys = new Set(["?", "/", "'"]);
      const wifiApi = {
        status: "/api/status",
        networks: "/api/networks",
        connect: "/api/connect",
      };

      let batteryManager = null;
      let wifiAvailable = false;
      let selectedNetwork = null;
      let networks = [];
      let volDragging = false;

      function isEditable(target) {
        if (!target) return false;
        if (target.isContentEditable) return true;
        const tag = target.tagName;
        return tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT";
      }

      window.addEventListener("keydown", (event) => {
        if (event.defaultPrevented || event.ctrlKey || event.metaKey || event.altKey) return;
        if (isEditable(event.target)) return;
        if (!suppressedKeys.has(event.key)) return;
        event.preventDefault();
        event.stopPropagation();
      }, true);

      function setBattery(levelText, cls) {
        batteryEl.textContent = levelText;
        batteryEl.classList.remove("low", "ok");
        if (cls) batteryEl.classList.add(cls);
      }

      function setNetwork(labelText, isOffline) {
        networkEl.textContent = labelText;
        networkEl.classList.toggle("offline", Boolean(isOffline));
      }

      async function initBattery() {
        if (!navigator.getBattery) {
          setBattery("BAT N/A");
          return;
        }

        try {
          batteryManager = await navigator.getBattery();
        } catch {
          setBattery("BAT N/A");
          return;
        }

        const update = () => {
          const level = Number.isFinite(batteryManager.level)
            ? Math.round(batteryManager.level * 100)
            : null;
          if (level == null) {
            setBattery("BAT N/A");
            return;
          }
          const charging = batteryManager.charging ? " CHG" : "";
          const cls = level <= 20 ? "low" : "ok";
          setBattery("BAT " + level + "%" + charging, cls);
        };

        update();
        batteryManager.addEventListener("levelchange", update);
        batteryManager.addEventListener("chargingchange", update);
      }

      function updateNetworkStatus() {
        if (!navigator.onLine) {
          setNetwork("NET OFFLINE", true);
          return;
        }

        const connection = navigator.connection || navigator.mozConnection || navigator.webkitConnection;
        const typeLabel = connection && connection.effectiveType
          ? String(connection.effectiveType).toUpperCase()
          : "ONLINE";
        const downlink = connection && Number.isFinite(connection.downlink)
          ? (" " + connection.downlink.toFixed(1) + "M")
          : "";
        setNetwork("NET " + typeLabel + downlink, false);
      }

      async function fetchJSON(url, options, timeoutMs) {
        const controller = new AbortController();
        const timeout = setTimeout(() => controller.abort(), timeoutMs || 2500);
        try {
          const res = await fetch(url, { ...(options || {}), signal: controller.signal });
          if (!res.ok) throw new Error("HTTP " + res.status);
          return await res.json();
        } finally {
          clearTimeout(timeout);
        }
      }

      function renderWifiList() {
        wifiList.innerHTML = "";
        for (let i = 0; i < networks.length; i += 1) {
          const item = networks[i];
          const row = document.createElement("button");
          row.type = "button";
          row.className = "wifi-item" + (selectedNetwork && selectedNetwork.ssid === item.ssid ? " selected" : "");
          const locked = item.security ? " [L]" : "";
          row.textContent = item.ssid + " | " + (item.signal || 0) + "%" + locked;
          row.addEventListener("click", () => {
            selectedNetwork = item;
            renderWifiList();
            wifiConnect.disabled = false;
          });
          wifiList.appendChild(row);
        }

        if (networks.length === 0) {
          const empty = document.createElement("div");
          empty.className = "wifi-meta";
          empty.textContent = "No WiFi networks found";
          wifiList.appendChild(empty);
        }
      }

      async function refreshNetworks() {
        wifiMeta.textContent = "Scanning WiFi networks...";
        try {
          const data = await fetchJSON(wifiApi.networks, {}, 5000);
          networks = Array.isArray(data) ? data : [];
          if (selectedNetwork) {
            selectedNetwork = networks.find((n) => n.ssid === selectedNetwork.ssid) || null;
          }
          wifiConnect.disabled = !selectedNetwork;
          wifiMeta.textContent = "Select network";
          renderWifiList();
        } catch {
          wifiMeta.textContent = "WiFi scan failed";
          networks = [];
          selectedNetwork = null;
          wifiConnect.disabled = true;
          renderWifiList();
        }
      }

      async function connectWifi() {
        if (!selectedNetwork) return;
        wifiConnect.disabled = true;
        wifiMeta.textContent = "Connecting to " + selectedNetwork.ssid + "...";

        const payload = { ssid: selectedNetwork.ssid };
        if (selectedNetwork.security) payload.password = wifiPass.value;

        try {
          const result = await fetchJSON(
            wifiApi.connect,
            {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify(payload),
            },
            30000,
          );
          if (result && result.ok) {
            wifiMeta.textContent = "Connected to " + selectedNetwork.ssid;
            setTimeout(updateWifiStatus, 250);
          } else {
            wifiMeta.textContent = (result && result.error) || "WiFi connection failed";
            wifiConnect.disabled = false;
          }
        } catch {
          wifiMeta.textContent = "WiFi connection error";
          wifiConnect.disabled = false;
        }
      }

      async function updateWifiStatus() {
        if (!wifiAvailable) return;
        try {
          const status = await fetchJSON(wifiApi.status, {}, 1500);
          const connected = Boolean(status && status.connected);
          wifiToggle.textContent = connected ? "WIFI ON" : "WIFI";
          wifiToggle.classList.toggle("show", true);
          wifiMeta.textContent = connected
            ? "Network connected"
            : "Not connected - select a network";
        } catch {
          wifiToggle.textContent = "WIFI";
        }
      }

      async function initWifi() {
        try {
          await fetchJSON(wifiApi.status, {}, 1200);
          wifiAvailable = true;
        } catch {
          wifiAvailable = false;
        }

        if (!wifiAvailable) {
          wifiPanel.classList.remove("show");
          wifiToggle.classList.remove("show");
          return;
        }

        wifiToggle.classList.add("show");
        wifiToggle.addEventListener("click", async () => {
          const next = !wifiPanel.classList.contains("show");
          wifiPanel.classList.toggle("show", next);
          if (next) await refreshNetworks();
        });

        wifiConnect.addEventListener("click", connectWifi);
        await updateWifiStatus();
      }

      // ── Volume ──
      function setVolume(level, muted) {
        if (muted) {
          volumeEl.textContent = "VOL MUTE";
          volumeEl.classList.add("muted");
          volumeEl.classList.remove("ok", "low");
        } else {
          const pct = Math.round(level * 100);
          volumeEl.textContent = "VOL " + pct + "%";
          volumeEl.classList.remove("muted");
          volumeEl.classList.toggle("low", pct <= 15);
          volumeEl.classList.toggle("ok", pct > 15);
        }
        if (!volDragging) volSlider.value = Math.round(level * 100);
      }

      async function fetchVolume() {
        try {
          const res = await fetchJSON("/api/volume", {}, 1500);
          if (res && res.volume != null) setVolume(res.volume, res.muted);
        } catch {
          volumeEl.textContent = "VOL N/A";
        }
      }

      volumeEl.addEventListener("click", () => {
        // Toggle mute on click
        fetch("/api/volume", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ mute: "toggle" }),
        }).then((r) => r.json()).then((d) => {
          if (d && d.volume != null) setVolume(d.volume, d.muted);
        }).catch(() => {});
      });

      volSlider.addEventListener("input", () => {
        volDragging = true;
        const vol = volSlider.value / 100;
        volumeEl.textContent = "VOL " + volSlider.value + "%";
      });

      volSlider.addEventListener("change", () => {
        volDragging = false;
        const vol = (volSlider.value / 100).toFixed(2);
        fetch("/api/volume", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ volume: vol }),
        }).then((r) => r.json()).then((d) => {
          if (d && d.volume != null) setVolume(d.volume, d.muted);
        }).catch(() => {});
      });

      // Show/hide slider panel on hover or focus within the vol-wrap
      const volWrap = volumeEl.parentElement;
      let volHideTimer = null;
      volWrap.addEventListener("mouseenter", () => {
        clearTimeout(volHideTimer);
        volPanel.classList.add("show");
      });
      volWrap.addEventListener("mouseleave", () => {
        volHideTimer = setTimeout(() => volPanel.classList.remove("show"), 400);
      });

      updateNetworkStatus();
      window.addEventListener("online", updateNetworkStatus);
      window.addEventListener("offline", updateNetworkStatus);
      const conn = navigator.connection || navigator.mozConnection || navigator.webkitConnection;
      if (conn && typeof conn.addEventListener === "function") {
        conn.addEventListener("change", updateNetworkStatus);
      }

      initBattery();
      initWifi();
      fetchVolume();
      setInterval(fetchVolume, 5000);
      setInterval(updateWifiStatus, 10000);
    })();
  </script>
</body>
</html>`;
}

// ─── Main Build Flow ────────────────────────────────────────────────

export async function streamOSImage(res, target, isJSPiece, density, onProgress, flavor = "alpine") {
  if (activeBuildCount >= MAX_CONCURRENT_BUILDS) {
    throw new Error(
      `Server busy: ${activeBuildCount}/${MAX_CONCURRENT_BUILDS} OS builds in progress. Try again shortly.`,
    );
  }

  activeBuildCount++;
  const buildId = randomUUID().slice(0, 8);
  const startTime = Date.now();
  const timing = {};
  const tempImagePath = path.join(TEMP_DIR, `fedac-os-${buildId}.img`);
  const tempPartPath = path.join(TEMP_DIR, `fedac-part-${buildId}.img`);

  try {
    // 1. Ensure base image is cached + fetch manifest for version tag.
    const baseStart = Date.now();
    const { basePath, manifest } = await ensureBaseImage(onProgress, flavor);
    timing.base = Date.now() - baseStart;

    if (!manifest.piecePartitionOffset || manifest.piecePartitionOffset === 0) {
      throw new Error(
        `Base image manifest missing piecePartitionOffset (${flavor}) — rebuild base image with --base-image flag`,
      );
    }

    // 2. Build piece bundle.
    const bundleStart = Date.now();
    onProgress?.({ stage: "bundle", message: `Bundling ${target}...`, step: 2, totalSteps: 9 });
    const bundleResult = isJSPiece
      ? await createJSPieceBundle(target, (p) => onProgress?.({ stage: "bundle", message: p.message, step: 2, totalSteps: 9 }), true, density)
      : await createBundle(target, (p) => onProgress?.({ stage: "bundle", message: p.message, step: 2, totalSteps: 9 }), true, density);
    const pieceHtml = bundleResult.html;
    timing.bundle = Date.now() - bundleStart;
    onProgress?.({
      stage: "bundle",
      message: `Bundle ready: ${bundleResult.sizeKB}KB (${formatSeconds(timing.bundle)})`,
      step: 2,
      totalSteps: 9,
    });

    // 2b. Compute bundle hash + CDN key for caching.
    const bundleHash = hashContent(pieceHtml);
    const baseVersion = manifest.version || "unknown";
    const cdnKey = buildCDNKey(target, density, bundleHash, baseVersion, flavor);

    // 3. Check CDN cache — skip the entire build if an identical ISO exists.
    const cacheStart = Date.now();
    onProgress?.({ stage: "cache-check", message: "Checking CDN cache...", step: 3, totalSteps: 9 });
    const cdnHit = await checkCDNCache(cdnKey);
    timing.cacheCheck = Date.now() - cacheStart;

    if (cdnHit) {
      const cdnUrl = buildCDNUrl(cdnKey);
      const elapsed = Date.now() - startTime;
      timing.total = elapsed;
      onProgress?.({
        stage: "done",
        message: `CDN cache hit — redirecting (${formatSeconds(elapsed)})`,
        cdnUrl,
        cached: true,
        timings: timing,
        step: 9,
        totalSteps: 9,
      });

      recentBuilds.unshift({
        buildId,
        target,
        isJSPiece,
        density,
        flavor,
        elapsed,
        timings: timing,
        cached: true,
        cdnUrl,
        time: new Date().toISOString(),
      });
      if (recentBuilds.length > MAX_RECENT) recentBuilds.pop();

      logOSBuild({ buildId, target, isJSPiece, density, flavor, elapsed, timings: timing, cached: true, cdnUrl, baseVersion: manifest.version, baseSha256: manifest.sha256, success: true });

      // Redirect to CDN for download (fast edge delivery).
      if (res) {
        res.redirect(302, cdnUrl);
      }
      return { buildId, elapsed, timings: timing, cdnUrl, cached: true, filename: `${target}-os.iso` };
    }

    // 4. Copy base image to temp.
    const copyStart = Date.now();
    onProgress?.({ stage: "copy", message: "Copying base image to workspace...", step: 4, totalSteps: 9 });
    const copyMode = copyBaseImageFast(basePath, tempImagePath);
    if (copyMode === "regular") {
      await fs.copyFile(basePath, tempImagePath);
    }
    timing.copy = Date.now() - copyStart;
    onProgress?.({
      stage: "copy",
      message: `Base image copied (${formatSeconds(timing.copy)}, ${copyMode})`,
      step: 4,
      totalSteps: 9,
    });

    // 5. Extract FEDAC-PIECE partition.
    const extractStart = Date.now();
    onProgress?.({ stage: "extract", message: "Extracting piece partition from image...", step: 5, totalSteps: 9 });
    extractPartition(tempImagePath, manifest, tempPartPath);
    timing.extract = Date.now() - extractStart;
    onProgress?.({
      stage: "extract",
      message: `Partition extracted (${formatSeconds(timing.extract)})`,
      step: 5,
      totalSteps: 9,
    });

    // 6. Inject piece-app.html and shell piece.html via debugfs.
    const injectStart = Date.now();
    onProgress?.({ stage: "inject", message: "Writing piece-app.html into partition via debugfs...", step: 6, totalSteps: 9 });
    injectFilesIntoPartition(tempPartPath, {
      "piece-app.html": pieceHtml,
      "piece.html": buildFedOSShellHTML(target),
    });
    timing.inject = Date.now() - injectStart;
    onProgress?.({
      stage: "inject",
      message: `Piece files injected (${formatSeconds(timing.inject)})`,
      step: 6,
      totalSteps: 9,
    });

    // 7. Write modified partition back into the image.
    const writeStart = Date.now();
    onProgress?.({ stage: "write-back", message: "Writing modified partition back to image...", step: 7, totalSteps: 9 });
    writePartitionBack(tempImagePath, manifest, tempPartPath);
    timing.writeBack = Date.now() - writeStart;
    onProgress?.({
      stage: "write-back",
      message: `Partition written back (${formatSeconds(timing.writeBack)})`,
      step: 7,
      totalSteps: 9,
    });

    // 8. Upload to CDN (concurrent with streaming to client).
    onProgress?.({ stage: "upload", message: "Uploading ISO to CDN for caching...", step: 8, totalSteps: 9 });
    const uploadPromise = uploadToCDN(tempImagePath, cdnKey, target)
      .then((url) => {
        if (url) {
          console.log(`[os] CDN upload complete: ${url}`);
          onProgress?.({ stage: "upload", message: "ISO cached to CDN", step: 8, totalSteps: 9 });
        }
        return url;
      })
      .catch((err) => {
        console.error(`[os] CDN upload failed (non-fatal): ${err.message}`);
        onProgress?.({ stage: "upload", message: `CDN upload skipped: ${err.message}`, step: 8, totalSteps: 9 });
        return null;
      });

    // 9. Stream to client (if res provided).
    let cdnUrl = null;
    if (res) {
      const streamStart = Date.now();
      const fileStat = await fs.stat(tempImagePath);
      const filename = `${target}-os.iso`;
      res.set({
        "Content-Type": "application/octet-stream",
        "Content-Disposition": `attachment; filename="${filename}"`,
        "Content-Length": fileStat.size,
        "Cache-Control": "no-cache",
      });

      const sizeMB = Math.round(fileStat.size / 1024 / 1024);
      onProgress?.({ stage: "stream", message: `Streaming ${sizeMB}MB ISO to client...`, step: 9, totalSteps: 9 });

      const readStream = fsSync.createReadStream(tempImagePath);
      await new Promise((resolve, reject) => {
        readStream.pipe(res);
        readStream.on("end", resolve);
        readStream.on("error", reject);
        res.on("close", () => {
          readStream.destroy();
          resolve();
        });
      });
      timing.stream = Date.now() - streamStart;
    }

    // Wait for CDN upload before cleaning up temp file.
    cdnUrl = await uploadPromise;

    const elapsed = Date.now() - startTime;
    timing.total = elapsed;
    onProgress?.({
      stage: "done",
      message: `OS ISO ready in ${formatSeconds(elapsed)} (copy ${formatSeconds(timing.copy)}, extract ${formatSeconds(timing.extract)}, inject ${formatSeconds(timing.inject)}, write-back ${formatSeconds(timing.writeBack)})${cdnUrl ? " — cached to CDN" : ""}`,
      cdnUrl,
      timings: timing,
      step: 9,
      totalSteps: 9,
    });

    recentBuilds.unshift({
      buildId,
      target,
      isJSPiece,
      density,
      flavor,
      elapsed,
      timings: timing,
      cdnUrl,
      time: new Date().toISOString(),
    });
    if (recentBuilds.length > MAX_RECENT) recentBuilds.pop();

    logOSBuild({ buildId, target, isJSPiece, density, flavor, elapsed, timings: timing, cached: false, cdnUrl, bundleHash, baseVersion, baseSha256: manifest.sha256, success: true });

    return {
      buildId,
      elapsed,
      timings: timing,
      cdnUrl,
      flavor,
      filename: `${target}-os.iso`,
    };
  } catch (err) {
    const elapsed = Date.now() - startTime;
    logOSBuild({ buildId, target, isJSPiece, density, flavor, elapsed, timings: timing, cached: false, success: false, error: err.message });
    throw err;
  } finally {
    activeBuildCount--;
    try {
      await fs.unlink(tempImagePath);
    } catch {
      // ignore
    }
    try {
      await fs.unlink(tempPartPath);
    } catch {
      // ignore
    }
  }
}

// ─── Status ─────────────────────────────────────────────────────────

export function getOSBuildStatus() {
  return {
    activeBuildCount,
    maxConcurrent: MAX_CONCURRENT_BUILDS,
    recentBuilds: recentBuilds.slice(0, 10),
    baseImageUrl: BASE_IMAGE_URL,
    manifestUrl: MANIFEST_URL,
  };
}

export { ensureBaseImage };
