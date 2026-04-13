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

// iOS Safari (and to a lesser extent Android) discards the user-gesture
// "activation" that allows downloads whenever the chain between tap and
// download includes an async `fetch()`. To keep downloads working on iPhone
// we route every download through `send({ type: "download-url" })` which is
// a synchronous `window.open(url, "_blank")` on the main thread — the same
// pattern `prompt.mjs` uses for ISO downloads. The server's
// `Content-Disposition: attachment` header (or the `.amxd` octet-stream
// MIME) triggers the iOS download sheet directly.

const FEATURED_DOWNLOADS = [
  {
    id: "featured-spreadnob-clean",
    label: "spreadnob clean",
    badge: "FOR TOM",
    piece: "spreadnob-clean",
    fileName: "AC 🎹 spreadnob-clean (aesthetic.computer).amxd",
    downloadLabel: "FOR TOM",
    blurb: "main version - compact, octave-aware, and the one to grab",
  },
  {
    id: "featured-spreadnob",
    label: "spreadnob rack",
    piece: "spreadnob",
    fileName: "AC 🎹 spreadnob (aesthetic.computer).amxd",
    blurb: "expanded rack view with the full module layout",
  },
];

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

function bytesToBase64(bytes) {
  let binary = "";
  const chunk = 8192;
  for (let i = 0; i < bytes.length; i += chunk) {
    binary += String.fromCharCode.apply(null, bytes.subarray(i, i + chunk));
  }
  return btoa(binary);
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

function offlineDeviceUrl(pieceRef) {
  const isKidlisp = pieceRef.startsWith("$");
  const query = isKidlisp
    ? `code=${encodeURIComponent(pieceRef)}`
    : `piece=${encodeURIComponent(pieceRef)}`;
  return `/api/pack-html?${query}&format=m4d`;
}

function onlineDeviceDataUrl(pieceRef) {
  // The "online" variant is built entirely client-side — a tiny .amxd that
  // embeds the live URL in a jweb~ object. We pack it sync and return a
  // `data:` URL so it can be handed to `download-url` in the same user
  // gesture as the touch. Small (~3KB) so base64 inflation is fine.
  const bytes = buildOnlinePatcher(pieceRef);
  return `data:application/octet-stream;base64,${bytesToBase64(bytes)}`;
}

function registerRegion(id, x, y, w, h, action) {
  regions.push({ id, x, y, w, h, action });
}

function trimLabel(str = "", maxChars = 24) {
  if (str.length <= maxChars) return str;
  return `${str.slice(0, max(1, maxChars - 1))}…`;
}

function layoutButtons(startX, startY, maxWidth, specs, gap = 6, rowGap = 5) {
  let x = startX;
  let y = startY;
  let rowHeight = 0;
  const limitX = startX + maxWidth;
  const buttons = [];

  for (const spec of specs) {
    if (x !== startX && x + spec.w > limitX) {
      x = startX;
      y += rowHeight + rowGap;
      rowHeight = 0;
    }

    buttons.push({ ...spec, x, y });
    x += spec.w + gap;
    rowHeight = max(rowHeight, spec.h);
  }

  return buttons;
}

function buttonsBottom(buttons, fallbackY = 0) {
  if (!buttons.length) return fallbackY;
  return buttons.reduce((bottom, button) => max(bottom, button.y + button.h), fallbackY);
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

function featuredAssetUrl(featured) {
  return `https://assets.aesthetic.computer/m4l/${encodeURIComponent(featured.fileName)}`;
}

function pluginAssetUrl(plugin) {
  const url = plugin?.m4l?.downloadUrl;
  if (url && url.includes("assets.aesthetic.computer")) return url;
  return `https://assets.aesthetic.computer/m4l/${encodeURIComponent(plugin.m4l.fileName)}`;
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
  const maxTextChars = max(18, floor(contentWidth / 6));
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

  const builderCardY = y - 4;
  const currentPieceLabel = `piece ${trimLabel(customPiece, max(10, maxTextChars - 8))}`;
  // Title line (10) + 2 gap + subtitle line (10) + 6 gap = 28 from y to buttons.
  const builderButtonsY = y + 28;
  const builderButtons = layoutButtons(margin, builderButtonsY, contentWidth, [
    { id: "builder-piece", w: max(92, currentPieceLabel.length * 6 + 10), h: 14, label: currentPieceLabel, colors: btn.warm, action: { type: "cycle-piece" } },
    { id: "builder-offline", w: 84, h: 14, label: "offline", colors: btn.primary, action: { type: "build-offline", piece: customPiece } },
    { id: "builder-online", w: 76, h: 14, label: "online", colors: btn.alt, action: { type: "build-online", piece: customPiece } },
    { id: "builder-open", w: 58, h: 14, label: "open", colors: btn.warm, action: { type: "open-piece", piece: customPiece } },
  ]);
  const builderButtonsBottom = buttonsBottom(builderButtons, builderButtonsY);
  const builderStatus = customBusy || customMessage;
  const builderStatusY = builderButtonsBottom + 10;
  const builderCardHeight = (builderStatus ? builderStatusY + 12 : builderButtonsBottom + 6) - builderCardY;

  ink(...(dark ? [22, 24, 34] : [228, 232, 242])).box(0, builderCardY, width, builderCardHeight);
  ink(...accent).write("Custom Instrument Builder", { x: margin, y });
  ink(dim).write("Cycle pieces or open ableton <piece> for any target.", { x: margin, y: y + 12 });

  for (const button of builderButtons) {
    registerRegion(button.id, button.x, button.y, button.w, button.h, button.action);
    drawButton({ ink, box }, button, button.label, button.colors, hoverId === button.id);
  }

  if (customBusy) {
    ink(...accent).write(trimLabel(customBusy, maxTextChars), { x: margin, y: builderStatusY });
  } else if (customMessage) {
    ink(dim).write(trimLabel(customMessage, maxTextChars), { x: margin, y: builderStatusY });
  }

  y = builderCardY + builderCardHeight + 12;

  const featuredCardStartY = y - 4;
  let featuredY = y + 14;
  const featuredLayouts = FEATURED_DOWNLOADS.map((featured) => {
    const innerX = margin + 6;
    let labelY = featuredY + 6;
    if (featured.badge) labelY += 14; // badge box 12 tall + 2 gap
    const blurbY = labelY + 12;       // label line (10 tall) + 2 gap
    const buttonsY = blurbY + 14;     // blurb line (10 tall) + 4 gap
    const buttonSpecs = layoutButtons(innerX, buttonsY, contentWidth - 12, [
      {
        id: `${featured.id}-download`,
        w: featured.badge ? 92 : 84,
        h: 14,
        label: featured.downloadLabel || "download",
        colors: featured.badge ? btn.warm : btn.primary,
        action: { type: "download-featured", featured },
      },
      {
        id: `${featured.id}-open`,
        w: 58,
        h: 14,
        label: "piece",
        colors: btn.alt,
        action: { type: "open-piece", piece: featured.piece },
      },
    ]);
    const cardHeight = buttonsBottom(buttonSpecs, buttonsY) - featuredY + 8;
    const layout = { featured, y: featuredY, labelY, blurbY, buttons: buttonSpecs, cardHeight };
    featuredY += cardHeight + 8;
    return layout;
  });
  const featuredSectionHeight = featuredY - featuredCardStartY;

  ink(...(dark ? [24, 19, 33] : [247, 236, 244])).box(0, featuredCardStartY, width, featuredSectionHeight);
  ink(255, 118, 184).write("Featured Downloads", { x: margin, y });
  for (const layout of featuredLayouts) {
    const { featured, labelY, blurbY, buttons, cardHeight } = layout;
    const cardBg = featured.badge
      ? (dark ? [48, 26, 40] : [255, 235, 244])
      : (dark ? [30, 24, 38] : [252, 243, 248]);
    const cardBorder = featured.badge ? [255, 118, 184] : [212, 135, 180];

    ink(...cardBg).box(margin - 6, layout.y, cardWidth + 12, cardHeight);
    ink(...cardBorder, featured.badge ? 170 : 110).box(margin - 6, layout.y, cardWidth + 12, cardHeight, "outline");

    if (featured.badge) {
      // Badge: 12 tall box so the 10-tall font fits without clipping below.
      ink(255, 118, 184).box(margin, layout.y + 4, 48, 12);
      ink(35, 12, 22).write(featured.badge, { x: margin + 4, y: layout.y + 5 });
    }

    ink(fg).write(trimLabel(featured.label, maxTextChars - 2), { x: margin, y: labelY });
    ink(dim).write(trimLabel(featured.blurb, maxTextChars), { x: margin, y: blurbY });

    for (const button of buttons) {
      registerRegion(button.id, button.x, button.y, button.w, button.h, button.action);
      drawButton({ ink, box }, button, button.label, button.colors, hoverId === button.id);
    }
  }

  y = featuredCardStartY + featuredSectionHeight + 14;

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
  y += 16;

  for (let i = 0; i < plugins.length; i++) {
    const plugin = plugins[i];
    const name = trimLabel(stripEmoji(plugin.device?.displayName || plugin.device?.name || plugin.code), maxTextChars - 3);
    const category = plugin.device?.category || "instrument";
    const piece = plugin.metadata?.piece || plugin.device?.name || "notepat";
    const desc = trimLabel(stripEmoji(plugin.metadata?.description || ""), maxTextChars);
    const version = plugin.version?.string || "0.0.0";
    const metaLabel = trimLabel(`v${version}  ${category}  ${piece}`, maxTextChars);
    const borderColor = category === "midi"
      ? [120, 150, 255]
      : category === "effect"
        ? [255, 170, 90]
        : [90, 210, 255];
    const cardBg = dark ? [22, 22, 28] : [248, 248, 244];
    const cardY = y - 4;
    // Three text rows × 12px line pitch = 36px, then 6px breathing room
    // before buttons so the description doesn't sit on top of them.
    const btnY = y + 42;
    const cardButtons = layoutButtons(margin, btnY, contentWidth, [
      { id: `download-${plugin.code}`, w: 84, h: 14, label: downloading === plugin.code ? "loading" : "download", colors: downloading === plugin.code ? btn.alt : btn.primary, action: { type: "download-plugin", plugin } },
      { id: `use-${plugin.code}`, w: 68, h: 14, label: "builder", colors: btn.warm, action: { type: "use-piece", piece } },
      { id: `open-${plugin.code}`, w: 58, h: 14, label: "piece", colors: btn.alt, action: { type: "open-piece", piece } },
    ]);
    const cardHeight = buttonsBottom(cardButtons, btnY) - cardY + 8;

    ink(...cardBg).box(margin - 6, cardY, cardWidth + 12, cardHeight);
    ink(...borderColor, 120).box(margin - 6, cardY, cardWidth + 12, cardHeight, "outline");

    if (category === "midi") iconMidi(ink, margin, y, frame + i * 40);
    else if (category === "effect") iconEffect(ink, circle, line, margin, y, frame + i * 40);
    else iconInstrument(ink, plot, margin, y, frame + i * 40);

    ink(fg).write(name, { x: margin + 14, y });
    y += 12;

    ink(...accent).write(metaLabel, { x: margin, y });
    y += 12;

    ink(dim).write(desc, { x: margin, y });

    for (const button of cardButtons) {
      registerRegion(button.id, button.x, button.y, button.w, button.h, button.action);
      drawButton({ ink, box }, button, button.label, button.colors, hoverId === button.id);
    }

    y = cardY + cardHeight + 10;
    ink(sep).line(margin, y, width - margin, y);
    y += 14;
  }
}

export function act({ event: e, pen, sound, send, jump, needsPaint }) {
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
    // Synchronous handoff — user gesture is preserved through `send`, so
    // iOS treats the resulting `window.open` as a real user navigation
    // and honors `Content-Disposition: attachment`.
    send?.({
      type: "download-url",
      content: {
        url: pluginAssetUrl(plugin),
        filename: plugin.m4l.fileName,
      },
    });
    customMessage = `Opening ${plugin.m4l.fileName}...`;
    needsPaint?.();
    // Fire-and-forget analytics — safe to let this async after the gesture.
    try {
      fetch(`/m4l-plugins/${plugin.code}/download`, { method: "POST" }).catch(() => {});
    } catch (_) { /* ignore */ }
    return;
  }

  if (action.type === "download-featured") {
    const featured = action.featured;
    playClick(sound);
    send?.({
      type: "download-url",
      content: {
        url: featuredAssetUrl(featured),
        filename: featured.fileName,
      },
    });
    customMessage = `Opening ${featured.fileName}...`;
    needsPaint?.();
    return;
  }

  if (action.type === "build-offline") {
    playClick(sound);
    const piece = action.piece;
    send?.({
      type: "download-url",
      content: {
        url: offlineDeviceUrl(piece),
        filename: `AC ${piece} (offline).amxd`,
      },
    });
    customMessage = `Building AC ${piece} (offline).amxd...`;
    needsPaint?.();
    return;
  }

  if (action.type === "build-online") {
    playClick(sound);
    const piece = action.piece;
    // Generated client-side — embed as a data: URL so the touch handler can
    // kick off `window.open` synchronously, preserving the iOS user gesture.
    let dataUrl;
    try {
      dataUrl = onlineDeviceDataUrl(piece);
    } catch (err) {
      console.error("Online build failed:", err);
      playError(sound);
      customMessage = `Build failed: ${err.message}`;
      needsPaint?.();
      return;
    }
    send?.({
      type: "download-url",
      content: {
        url: dataUrl,
        filename: `AC ${piece} (online).amxd`,
      },
    });
    customMessage = `Opening AC ${piece} (online).amxd...`;
    needsPaint?.();
  }
}

export function meta() {
  return {
    title: "AC Max for Live",
    desc: "Published Ableton devices plus a custom AC instrument builder.",
  };
}
