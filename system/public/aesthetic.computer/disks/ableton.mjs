// Ableton - AC Max for Live device browser + builder, 2026.03.31

let plugins = [];
let loading = true;
let error = null;
let frame = 0;
let downloading = null;
let hoverId = null;
let regions = [];
let needsPaintRef = null;

let customPiece = "notepat";
let customPieceOptions = ["notepat"];
let customPieceIndex = 0;
let customBusy = null;
let customMessage = "";

const { sin, cos, floor, abs, max } = Math;

function stripEmoji(str = "") {
  return str.replace(/[\u{1F000}-\u{1FFFF}]/gu, "").trim();
}

function playClick(sound) {
  sound?.synth?.({ type: "sine", tone: 660, beats: 0.05, volume: 0.2 });
  sound?.synth?.({ type: "sine", tone: 880, beats: 0.05, volume: 0.15, delay: 0.05 });
}

function playError(sound) {
  sound?.synth?.({ type: "square", tone: 180, beats: 0.08, volume: 0.16 });
}

function bytesToBinaryString(bytes) {
  let binary = "";
  const chunk = 8192;
  for (let i = 0; i < bytes.length; i += chunk) {
    binary += String.fromCharCode.apply(null, bytes.subarray(i, i + chunk));
  }
  return binary;
}

function syncCustomPieceOptions() {
  const pieces = [];
  for (const plugin of plugins) {
    const piece = plugin?.metadata?.piece;
    if (piece && !pieces.includes(piece)) pieces.push(piece);
  }

  if (customPiece && !pieces.includes(customPiece)) {
    pieces.unshift(customPiece);
  }

  if (pieces.length === 0) pieces.push("notepat");

  customPieceOptions = pieces;
  customPieceIndex = max(0, customPieceOptions.indexOf(customPiece));
  customPiece = customPieceOptions[customPieceIndex] || "notepat";
}

function setCustomPiece(piece) {
  if (!piece) return;
  customPiece = piece;
  syncCustomPieceOptions();
}

function cycleCustomPiece(step = 1) {
  if (!customPieceOptions.length) return;
  customPieceIndex = (customPieceIndex + step + customPieceOptions.length) % customPieceOptions.length;
  customPiece = customPieceOptions[customPieceIndex];
}

function drawWave(ink, line, width, y, dark, t) {
  const amp = 3;
  const wavColor = dark ? [40, 60, 100] : [180, 200, 230];
  for (let x = 0; x < width; x += 2) {
    const v = sin((x * 0.04) + t * 0.03) * amp +
      sin((x * 0.07) - t * 0.02) * amp * 0.5;
    ink(...wavColor, 80 + abs(v) * 15);
    line(x, y + v, x + 1, y + v);
  }
}

function iconMidi(ink, x, y, t) {
  const pulse = sin(t * 0.08) * 0.3 + 0.7;
  const b = floor(180 * pulse);
  ink(b, b, 255).box(x, y, 9, 7);
  ink(0).box(x + 2, y, 1, 4);
  ink(0).box(x + 5, y, 1, 4);
}

function iconInstrument(ink, plot, x, y, t) {
  for (let i = 0; i < 9; i++) {
    const v = sin((i * 0.8) + t * 0.06) * 3;
    ink(100, 200, 255).plot(x + i, y + 3 + floor(v));
  }
}

function iconEffect(ink, circle, line, x, y, t) {
  const angle = t * 0.04;
  ink(255, 150, 80).circle(x + 4, y + 3, 3, false);
  const dx = floor(cos(angle) * 2);
  const dy = floor(sin(angle) * 2);
  ink(255, 200, 100).line(x + 4, y + 3, x + 4 + dx, y + 3 + dy);
}

function buildOnlinePatcher(pieceRef) {
  const displayName = pieceRef;
  const liveUrl = `https://aesthetic.computer/${pieceRef}?nogap=true&daw=true&nolabel=true`;
  const width = 400;
  const height = 200;
  const patcher = {
    patcher: {
      fileversion: 1,
      appversion: { major: 9, minor: 0, revision: 7, architecture: "x64", modernui: 1 },
      classnamespace: "box",
      rect: [134, 174, 800, 600],
      openrect: [0, 0, width, height],
      openinpresentation: 1,
      gridsize: [15, 15],
      enablehscroll: 0,
      enablevscroll: 0,
      devicewidth: width,
      description: `Aesthetic Computer ${displayName} (online)`,
      boxes: [
        {
          box: {
            disablefind: 0,
            id: "obj-jweb",
            latency: 0,
            maxclass: "jweb~",
            numinlets: 1,
            numoutlets: 3,
            outlettype: ["signal", "signal", ""],
            patching_rect: [10, 50, width, height],
            presentation: 1,
            presentation_rect: [0, 0, width + 1, height + 1],
            rendermode: 1,
            url: liveUrl,
          },
        },
        {
          box: {
            id: "obj-plugout",
            maxclass: "newobj",
            numinlets: 2,
            numoutlets: 0,
            patching_rect: [10, 280, 75, 22],
            text: "plugout~ 1 2",
          },
        },
        {
          box: {
            id: "obj-thisdevice",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 3,
            outlettype: ["bang", "int", "int"],
            patching_rect: [350, 50, 85, 22],
            text: "live.thisdevice",
          },
        },
        {
          box: {
            id: "obj-print",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 0,
            patching_rect: [350, 80, 170, 22],
            text: `print [AC-${displayName.toUpperCase()}]`,
          },
        },
        {
          box: {
            id: "obj-route",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 2,
            outlettype: ["", ""],
            patching_rect: [350, 140, 60, 22],
            text: "route ready",
          },
        },
        {
          box: {
            id: "obj-activate",
            maxclass: "message",
            numinlets: 2,
            numoutlets: 1,
            outlettype: [""],
            patching_rect: [350, 170, 60, 22],
            text: "activate 1",
          },
        },
      ],
      lines: [
        { patchline: { destination: ["obj-plugout", 0], source: ["obj-jweb", 0] } },
        { patchline: { destination: ["obj-plugout", 1], source: ["obj-jweb", 1] } },
        { patchline: { destination: ["obj-print", 0], source: ["obj-thisdevice", 0] } },
        { patchline: { destination: ["obj-route", 0], source: ["obj-jweb", 2] } },
        { patchline: { destination: ["obj-activate", 0], source: ["obj-route", 0] } },
        { patchline: { destination: ["obj-jweb", 0], source: ["obj-activate", 0] } },
      ],
      dependency_cache: [],
      latency: 0,
      is_mpe: 0,
      external_mpe_tuning_enabled: 0,
      minimum_live_version: "",
      minimum_max_version: "",
      platform_compatibility: 0,
      autosave: 0,
    },
  };

  const header = "ampf\x04\x00\x00\x00iiiimeta\x04\x00\x00\x00\x00\x00\x00\x00ptch";
  const patcherJson = JSON.stringify(patcher);
  const jsonBytes = new TextEncoder().encode(patcherJson);
  const headerBytes = new Uint8Array([...header].map((char) => char.charCodeAt(0)));
  const lenBytes = new Uint8Array(4);
  new DataView(lenBytes.buffer).setUint32(0, jsonBytes.length, true);
  const binary = new Uint8Array(headerBytes.length + lenBytes.length + jsonBytes.length);
  binary.set(headerBytes, 0);
  binary.set(lenBytes, headerBytes.length);
  binary.set(jsonBytes, headerBytes.length + lenBytes.length);
  return binary;
}

async function buildOfflineDevice(pieceRef, download) {
  const isKidlisp = pieceRef.startsWith("$");
  const query = isKidlisp
    ? `code=${encodeURIComponent(pieceRef)}`
    : `piece=${encodeURIComponent(pieceRef)}`;
  const response = await fetch(`/api/pack-html?${query}&format=m4d`);
  if (!response.ok) {
    const err = await response.json().catch(() => ({ error: `HTTP ${response.status}` }));
    throw new Error(err.error || `M4D build failed (${response.status})`);
  }
  const bytes = new Uint8Array(await response.arrayBuffer());
  download(`AC ${pieceRef} (offline).amxd`, bytesToBinaryString(bytes), { encoding: "binary" });
}

async function buildOnlineDevice(pieceRef, download) {
  const bytes = buildOnlinePatcher(pieceRef);
  download(`AC ${pieceRef} (online).amxd`, bytesToBinaryString(bytes), { encoding: "binary" });
}

function registerRegion(id, x, y, w, h, action) {
  regions.push({ id, x, y, w, h, action });
}

function hitRegion(pen) {
  if (!pen) return null;
  for (const region of regions) {
    if (
      pen.x >= region.x &&
      pen.x < region.x + region.w &&
      pen.y >= region.y &&
      pen.y < region.y + region.h
    ) {
      return region;
    }
  }
  return null;
}

function drawButton({ ink, box }, region, label, colors, hovered) {
  const { x, y, w, h } = region;
  const [bg, border, text] = colors;
  ink(...bg).box(x, y, w, h);
  ink(...border).box(x, y, w, h, "outline");
  if (hovered) {
    ink(255, 255, 255, 22).box(x, y, w, h);
  }
  ink(...text).write(label, { x: x + 4, y: y + 3 });
}

export function boot({ params, needsPaint }) {
  needsPaintRef = needsPaint;
  if (params?.[0]) setCustomPiece(params[0]);

  fetch("/m4l-plugins")
    .then((res) => {
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      return res.json();
    })
    .then((data) => {
      plugins = Array.isArray(data) ? data : [];
      syncCustomPieceOptions();
      loading = false;
      needsPaintRef?.();
    })
    .catch((err) => {
      console.error("Failed to load plugins:", err);
      error = "Failed to load plugins.";
      loading = false;
      needsPaintRef?.();
    });
}

function sim() {
  frame += 1;
}

function paint({ wipe, ink, screen, box, line, circle, plot, dark }) {
  const bg = dark ? [15, 15, 20] : [242, 242, 238];
  const fg = dark ? 240 : 10;
  const dim = dark ? 95 : 135;
  const accent = dark ? [100, 170, 255] : [20, 80, 200];
  const sep = dark ? 30 : 215;
  const btn = {
    primary: [dark ? [36, 86, 165] : [30, 90, 200], dark ? [90, 160, 255] : [50, 120, 255], [255, 255, 255]],
    alt: [dark ? [35, 58, 40] : [215, 236, 220], dark ? [95, 170, 115] : [70, 150, 90], dark ? [180, 255, 195] : [20, 95, 35]],
    warm: [dark ? [52, 42, 24] : [242, 228, 208], dark ? [220, 165, 80] : [185, 125, 35], dark ? [255, 225, 170] : [105, 68, 12]],
    danger: [dark ? [70, 30, 35] : [245, 220, 225], dark ? [220, 110, 130] : [190, 85, 105], dark ? [255, 190, 205] : [115, 38, 52]],
  };

  wipe(...bg);
  regions = [];

  const { width } = screen;
  const margin = 12;
  const contentWidth = width - margin * 2;
  const cardWidth = max(120, contentWidth);
  let y = 20;

  drawWave(ink, line, width, y + 4, dark, frame);

  ink(fg).write("AC Max for Live", { x: margin, y });
  y += 12;

  ink(dim).write("Downloads + instrument builder for Ableton Live", { x: margin, y });
  y += 14;

  ink(sep).line(margin, y, width - margin, y);
  const dotX = margin + ((frame * 0.5) % (width - margin * 2));
  ink(...accent).plot(floor(dotX), y);
  ink(...accent).plot(floor(dotX) + 1, y);
  y += 12;

  ink(...(dark ? [22, 24, 34] : [228, 232, 242])).box(0, y - 4, width, 74);
  ink(...accent).write("Custom Instrument Builder", { x: margin, y });
  y += 12;

  const currentPieceLabel = `piece ${customPiece}`;
  ink(dim).write("Cycle pieces or open ableton <piece> for any target.", { x: margin, y });
  y += 13;

  const pieceBtn = { id: "builder-piece", x: margin, y, w: max(92, currentPieceLabel.length * 6 + 10), h: 14 };
  const offlineBtn = { id: "builder-offline", x: pieceBtn.x + pieceBtn.w + 6, y, w: 84, h: 14 };
  const onlineBtn = { id: "builder-online", x: offlineBtn.x + offlineBtn.w + 6, y, w: 76, h: 14 };
  const openBtn = { id: "builder-open", x: onlineBtn.x + onlineBtn.w + 6, y, w: 58, h: 14 };

  registerRegion(pieceBtn.id, pieceBtn.x, pieceBtn.y, pieceBtn.w, pieceBtn.h, { type: "cycle-piece" });
  registerRegion(offlineBtn.id, offlineBtn.x, offlineBtn.y, offlineBtn.w, offlineBtn.h, { type: "build-offline", piece: customPiece });
  registerRegion(onlineBtn.id, onlineBtn.x, onlineBtn.y, onlineBtn.w, onlineBtn.h, { type: "build-online", piece: customPiece });
  registerRegion(openBtn.id, openBtn.x, openBtn.y, openBtn.w, openBtn.h, { type: "open-piece", piece: customPiece });

  drawButton({ ink, box }, pieceBtn, currentPieceLabel, btn.warm, hoverId === pieceBtn.id);
  drawButton({ ink, box }, offlineBtn, "offline", btn.primary, hoverId === offlineBtn.id);
  drawButton({ ink, box }, onlineBtn, "online", btn.alt, hoverId === onlineBtn.id);
  drawButton({ ink, box }, openBtn, "open", btn.warm, hoverId === openBtn.id);

  y += 20;
  if (customBusy) {
    ink(...accent).write(customBusy, { x: margin, y });
    y += 11;
  } else if (customMessage) {
    ink(dim).write(customMessage, { x: margin, y });
    y += 11;
  } else {
    y += 2;
  }

  y += 8;

  if (loading) {
    const dots = ".".repeat((floor(frame / 15) % 3) + 1);
    ink(dim).write("Loading published devices" + dots, { x: margin, y });
    return;
  }

  if (error) {
    ink(255, 80, 80).write(error, { x: margin, y });
    return;
  }

  if (plugins.length === 0) {
    ink(dim).write("No published devices yet.", { x: margin, y });
    return;
  }

  ink(...accent).write("Published Devices", { x: margin, y });
  y += 14;

  for (let i = 0; i < plugins.length; i++) {
    const plugin = plugins[i];
    const name = stripEmoji(plugin.device?.displayName || plugin.device?.name || plugin.code);
    const category = plugin.device?.category || "instrument";
    const piece = plugin.metadata?.piece || plugin.device?.name || "notepat";
    const desc = stripEmoji(plugin.metadata?.description || "");
    const version = plugin.version?.string || "0.0.0";
    const borderColor = category === "midi"
      ? [120, 150, 255]
      : category === "effect"
        ? [255, 170, 90]
        : [90, 210, 255];
    const cardBg = dark ? [22, 22, 28] : [248, 248, 244];

    ink(...cardBg).box(margin - 6, y - 4, cardWidth + 12, 58);
    ink(...borderColor, 120).box(margin - 6, y - 4, cardWidth + 12, 58, "outline");

    if (category === "midi") iconMidi(ink, margin, y, frame + i * 40);
    else if (category === "effect") iconEffect(ink, circle, line, margin, y, frame + i * 40);
    else iconInstrument(ink, plot, margin, y, frame + i * 40);

    ink(fg).write(name, { x: margin + 14, y });
    y += 12;

    ink(...accent).write(`v${version}  ${category}  ${piece}`, { x: margin, y });
    y += 12;

    ink(dim).write(desc, { x: margin, y });

    const btnY = y + 13;
    const downloadBtn = { id: `download-${plugin.code}`, x: margin, y: btnY, w: 84, h: 14 };
    const builderBtn = { id: `use-${plugin.code}`, x: margin + 90, y: btnY, w: 68, h: 14 };
    const pieceBtnCard = { id: `open-${plugin.code}`, x: margin + 164, y: btnY, w: 58, h: 14 };

    registerRegion(downloadBtn.id, downloadBtn.x, downloadBtn.y, downloadBtn.w, downloadBtn.h, {
      type: "download-plugin",
      plugin,
    });
    registerRegion(builderBtn.id, builderBtn.x, builderBtn.y, builderBtn.w, builderBtn.h, {
      type: "use-piece",
      piece,
    });
    registerRegion(pieceBtnCard.id, pieceBtnCard.x, pieceBtnCard.y, pieceBtnCard.w, pieceBtnCard.h, {
      type: "open-piece",
      piece,
    });

    const isDownloading = downloading === plugin.code;
    if (isDownloading) {
      drawButton({ ink, box }, downloadBtn, "loading", btn.alt, hoverId === downloadBtn.id);
    } else {
      drawButton({ ink, box }, downloadBtn, "download", btn.primary, hoverId === downloadBtn.id);
    }
    drawButton({ ink, box }, builderBtn, "builder", btn.warm, hoverId === builderBtn.id);
    drawButton({ ink, box }, pieceBtnCard, "piece", btn.alt, hoverId === pieceBtnCard.id);

    y += 35;
    ink(sep).line(margin, y, width - margin, y);
    y += 12;
  }
}

export async function act({ event: e, pen, sound, download, jump, needsPaint }) {
  if (e.is("move") || e.is("draw")) {
    const hit = hitRegion(pen);
    const nextHoverId = hit?.id || null;
    if (nextHoverId !== hoverId) {
      hoverId = nextHoverId;
      needsPaint?.();
    }
    return;
  }

  if (!e.is("touch")) return;

  const hit = hitRegion(pen);
  if (!hit) return;

  const { action } = hit;

  if (action.type === "cycle-piece") {
    playClick(sound);
    cycleCustomPiece(1);
    customMessage = `Builder piece: ${customPiece}`;
    needsPaint?.();
    return;
  }

  if (action.type === "use-piece") {
    playClick(sound);
    setCustomPiece(action.piece);
    customMessage = `Builder piece: ${customPiece}`;
    needsPaint?.();
    return;
  }

  if (action.type === "open-piece") {
    playClick(sound);
    jump?.(action.piece);
    return;
  }

  if (action.type === "download-plugin") {
    const plugin = action.plugin;
    playClick(sound);
    downloading = plugin.code;
    customMessage = `Downloading ${stripEmoji(plugin.device?.displayName || plugin.code)}...`;
    needsPaint?.();

    try {
      const url = plugin.m4l.downloadUrl;
      const assetUrl = url.includes("assets.aesthetic.computer")
        ? url
        : `https://assets.aesthetic.computer/m4l/${encodeURIComponent(plugin.m4l.fileName)}`;
      const res = await fetch(assetUrl);
      const buf = await res.arrayBuffer();
      download(plugin.m4l.fileName, new Uint8Array(buf), { sharing: true });
      fetch(`/m4l-plugins/${plugin.code}/download`, { method: "POST" }).catch(() => {});
      customMessage = `Downloaded ${plugin.m4l.fileName}`;
    } catch (err) {
      console.error("Download failed:", err);
      playError(sound);
      customMessage = `Download failed: ${err.message}`;
    } finally {
      downloading = null;
      needsPaint?.();
    }
    return;
  }

  if (action.type === "build-offline" || action.type === "build-online") {
    if (customBusy) return;

    playClick(sound);
    const piece = action.piece;
    customBusy = action.type === "build-offline"
      ? `Building offline .amxd for ${piece}...`
      : `Building online .amxd for ${piece}...`;
    customMessage = "";
    needsPaint?.();

    try {
      if (action.type === "build-offline") {
        await buildOfflineDevice(piece, download);
        customMessage = `Downloaded AC ${piece} (offline).amxd`;
      } else {
        await buildOnlineDevice(piece, download);
        customMessage = `Downloaded AC ${piece} (online).amxd`;
      }
    } catch (err) {
      console.error("Custom Ableton build failed:", err);
      playError(sound);
      customMessage = `Build failed: ${err.message}`;
    } finally {
      customBusy = null;
      needsPaint?.();
    }
  }
}

export function meta() {
  return {
    title: "AC Max for Live",
    desc: "Published Ableton devices plus a custom AC instrument builder.",
  };
}
