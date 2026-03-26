// Profile, 2026.03.26.00.00.00
// @handle profile — activity + portfolio hybrid.

const FETCHING = "Fetching";
const REFRESH_MS = 30000;
const RECONNECT_MS = 3000;
const MOOD_LIMIT = 40;
const CHAT_LIMIT = 120;
const THUMB_MAX = 4;
const ACTIVITY_MAX = 30;

const { max, min, floor } = Math;

let debug;
let visiting;
let profile;
let noprofile = FETCHING;
let noprofileAction;
let noprofileBtn;
let paintingsBtn;
let ellipsisTicker;

let loading = false;
let refreshing = false;
let dataError = null;

let scorecard = makeEmptyScorecard();
let presence = makeOfflinePresence();

// Painting thumbnails.
let thumbs = []; // [{ slug, code, img, when, btn, route }]
let thumbsLoading = false;
let getApi = null; // stored from boot for async image loading

let statusSocket = null;
let reconnectTimer = null;
let refreshTimer = null;
let disposed = false;

// Activity button regions (rebuilt each frame).
let activityBtns = [];

// Scroll state for activity feed.
let scrollOffset = 0;
let maxScroll = 0;

function makeEmptyScorecard() {
  return {
    counts: {
      paintings: 0,
      pieces: 0,
      kidlisp: 0,
      clocks: 0,
      tapes: 0,
      moods: 0,
      chats: 0,
    },
    recentMedia: {
      paintings: [],
      kidlisp: [],
      clocks: [],
      moods: [],
      chats: [],
    },
    activity: [],
    updatedAt: null,
  };
}

function makeOfflinePresence() {
  return {
    online: false,
    currentPiece: null,
    worldPiece: null,
    showing: null,
    connections: 0,
    ping: null,
    lastSeenAt: null,
  };
}

function meta({ piece }) {
  return {
    title: `${piece} - aesthetic.computer`,
    desc: `Profile for ${piece}.`,
  };
}

async function boot({
  params,
  user,
  gizmo,
  handle,
  hud,
  net,
  get,
  debug: d,
}) {
  disposed = false;
  debug = d;
  getApi = get;

  const hand = normalizeHandle(handle());
  visiting = normalizeHandle(params[0] || hand);

  ellipsisTicker = new gizmo.EllipsisTicker();

  resetUiState();
  clearTimersAndSocket();
  scorecard = makeEmptyScorecard();
  presence = makeOfflinePresence();
  profile = null;
  dataError = null;
  thumbs = [];
  thumbsLoading = false;
  scrollOffset = 0;

  if (visiting) {
    hud.label(visiting);
    net.rewrite(visiting);
    noprofile = FETCHING;
    noprofileAction = null;

    refreshProfile(true);
    startRefreshLoop();
    connectProfileStream();
    return;
  }

  if (user) {
    if (user.email_verified) {
      noprofile = "Create handle.";
      noprofileAction = "handle";
    } else {
      noprofile = "Check email to verify.";
      noprofileAction = "email";
    }
  } else {
    noprofile = "Log in or Sign up";
    noprofileAction = "imnew";
  }
}

function paint({ api, geo, wipe, help, ink, screen, ui, pen, paste }) {
  if (!pen?.drawing) wipe(98);

  if (!visiting) {
    paintNoProfileState({ api, help, ink, screen, ui });
    return;
  }

  const HUD_H = 22;
  const M = 4;
  const BTN_H = 16;

  // Loading state.
  if (!profile && noprofile === FETCHING) {
    ink(255).write(
      `${FETCHING}${ellipsisTicker.text(help.repeat)}`,
      { center: "xy" },
      "black",
    );
    return;
  }

  // Error / no-profile state.
  if (!profile && noprofile && noprofile !== FETCHING) {
    ink(255).write(noprofile, { center: "xy" }, "black");
    return;
  }

  // Thin divider below HUD.
  ink(50).line(0, HUD_H - 2, screen.width, HUD_H - 2);

  const maxY = screen.height - BTN_H - 2;
  const isLandscape = screen.width > screen.height && screen.width >= 200;

  if (isLandscape) {
    paintLandscape({ geo, help, ink, screen, paste, HUD_H, M, maxY });
  } else {
    paintPortrait({ geo, help, ink, screen, paste, HUD_H, M, maxY });
  }

  // Bottom bar.
  paintingsBtn ||= new ui.TextButton("Paintings", {
    x: M,
    y: screen.height - BTN_H + 1,
  });
  paintingsBtn.paint(api);

  // Refreshing indicator.
  if (refreshing) {
    ink(100).write(
      `Refreshing${ellipsisTicker.text(help.repeat)}`,
      { x: screen.width - 84, y: screen.height - BTN_H + 1 },
      "black",
      80,
    );
  }
}

function act({ event: e, jump, store, user, geo }) {
  // Paintings button.
  paintingsBtn?.act(e, () => {
    if (visiting) jump(`paintings~${visiting}`);
  });

  if (e.is("keyboard:down:g") && visiting) {
    jump(`paintings~${visiting}`);
  }

  if (e.is("keyboard:down:r")) {
    refreshProfile(true);
  }

  // Thumbnail taps — use simple hit testing.
  if (e.is("touch") || e.is("lift")) {
    for (const t of thumbs) {
      if (t.btn?.box && t.route && e.is("lift")) {
        const b = t.btn.box;
        const px = e.x ?? e.pen?.x;
        const py = e.y ?? e.pen?.y;
        if (
          px >= b.x &&
          px <= b.x + b.w &&
          py >= b.y &&
          py <= b.y + b.h
        ) {
          jump(t.route);
          break;
        }
      }
    }
  }

  // Activity item taps — simple hit testing.
  if (e.is("lift")) {
    const px = e.x ?? e.pen?.x;
    const py = e.y ?? e.pen?.y;
    for (const ab of activityBtns) {
      if (ab.route && ab.box) {
        const b = ab.box;
        if (
          px >= b.x &&
          px <= b.x + b.w &&
          py >= b.y &&
          py <= b.y + b.h
        ) {
          jump(ab.route);
          break;
        }
      }
    }
  }

  // Scroll activity feed.
  if (e.is("scroll")) {
    scrollOffset = max(0, min(maxScroll, scrollOffset + (e.delta || 0)));
  }

  // No-profile button.
  noprofileBtn?.act(e, {
    push: () => {
      let slug;
      if (noprofileAction === "handle") {
        slug = "prompt~handle";
      } else if (noprofileAction === "email") {
        slug = "prompt~email~" + user?.email;
      } else if (noprofileAction === "imnew") {
        slug = "prompt";
        store["prompt:splash"] = true;
      }
      if (slug) jump(slug);
    },
  });

  if (e.is("reframed")) {
    noprofileBtn = null;
    paintingsBtn = null;
    activityBtns = [];
    for (const t of thumbs) t.btn = null;
  }
}

function sim() {
  ellipsisTicker?.sim();
}

function leave() {
  disposed = true;
  clearTimersAndSocket();
}

// --- Layout ---

function paintPortrait({ geo, help, ink, screen, paste, HUD_H, M, maxY }) {
  let y = HUD_H;
  const w = screen.width - M * 2;

  // Mood.
  y = paintMood(ink, M, y, w);

  // Presence line.
  y = paintPresenceLine(ink, M, y, w);

  // Divider.
  ink(50).line(M, y, screen.width - M, y);
  y += 4;

  // Thumbnail strip.
  y = paintThumbStrip(ink, paste, M, y, w);

  // Divider.
  ink(50).line(M, y, screen.width - M, y);
  y += 4;

  // Activity feed.
  paintActivityFeed(help, ink, M, y, maxY, w);
}

function paintLandscape({ geo, help, ink, screen, paste, HUD_H, M, maxY }) {
  const midX = floor(screen.width / 2);
  const leftW = midX - M * 2;
  const rightX = midX + M;
  const rightW = screen.width - midX - M * 2;

  // Vertical divider.
  ink(50).line(midX - 1, HUD_H, midX - 1, maxY);

  // Left column: mood + presence + thumbnails.
  let y = HUD_H;

  y = paintMood(ink, M, y, leftW);
  y = paintPresenceLine(ink, M, y, leftW);
  ink(50).line(M, y, midX - M, y);
  y += 4;
  paintThumbStrip(ink, paste, M, y, leftW);

  // Right column: activity feed.
  paintActivityFeed(help, ink, rightX, HUD_H, maxY, rightW);
}

// --- Sections ---

function paintMood(ink, x, y, w) {
  if (profile?.mood) {
    ink(220).write(profile.mood, { x, y }, "black", w);
  } else {
    ink(80).write("no mood", { x, y }, "black", w);
  }
  return y + 12;
}

function paintPresenceLine(ink, x, y, w) {
  const on = presence.online;

  // Status dot.
  ink(on ? 80 : 70, on ? 220 : 70, on ? 80 : 70).box(x, y + 2, 5, 5);

  const parts = [];
  parts.push(on ? "online" : "offline");

  if (on && presence.currentPiece) {
    parts.push(formatPieceLabel(presence.currentPiece));
  }

  if (presence.ping) parts.push(`${presence.ping}ms`);

  if (!on && presence.lastSeenAt) {
    parts.push(`seen ${formatTimeAgo(presence.lastSeenAt)}`);
  }

  ink(on ? 255 : 120).write(
    parts.join("  "),
    { x: x + 8, y },
    "black",
    w - 8,
  );
  return y + 12;
}

function paintThumbStrip(ink, paste, x, y, w) {
  const paintings = scorecard.recentMedia.paintings;

  if (paintings.length === 0 && !thumbsLoading) {
    ink(80).write("no paintings yet", { x, y }, "black", w);
    return y + 14;
  }

  if (thumbs.length === 0 && thumbsLoading) {
    ink(120).write("loading thumbnails...", { x, y }, "black", w);
    return y + 14;
  }

  // Compute thumbnail size to fit across the width.
  const gap = 3;
  const count = min(thumbs.length, THUMB_MAX);
  if (count === 0) return y + 2;

  const thumbW = floor((w - gap * (count - 1)) / count);
  const thumbH = floor(thumbW * 0.75); // 4:3 aspect

  for (let i = 0; i < count; i++) {
    const t = thumbs[i];
    if (!t) continue;
    const tx = x + i * (thumbW + gap);

    // Background.
    ink(60).box(tx, y, thumbW, thumbH);

    // Paste image if loaded.
    if (t.img && paste) {
      const scale = min(thumbW / t.img.width, thumbH / t.img.height);
      const ix = tx + floor((thumbW - t.img.width * scale) / 2);
      const iy = y + floor((thumbH - t.img.height * scale) / 2);
      paste(t.img, ix, iy, { scale });
    } else if (!t.img) {
      ink(90).write("...", { x: tx + 2, y: y + 2 }, "black");
    }

    // Code label below thumbnail.
    const label = t.code ? `#${t.code}` : shortSlug(t.slug);
    ink(160).write(label, { x: tx, y: y + thumbH + 1 }, "black", thumbW);

    // Store hit region for tap handling in act().
    t.btn = { box: { x: tx, y, w: thumbW, h: thumbH + 10 } };
    t.route = t.slug ? `painting~${visiting}/${t.slug}` : null;
  }

  return y + thumbH + 14;
}

function paintActivityFeed(help, ink, x, y, maxY, w) {
  const startY = y;
  activityBtns = [];

  if ((loading || refreshing) && scorecard.activity.length === 0) {
    ink(200).write(
      `${FETCHING}${ellipsisTicker.text(help.repeat)}`,
      { x, y },
      "black",
      w,
    );
    return;
  }

  if (scorecard.activity.length === 0) {
    ink(80).write(dataError || "no activity yet", { x, y }, "black", w);
    return;
  }

  const lineH = 11;
  const items = scorecard.activity;
  const totalH = items.length * lineH;
  maxScroll = max(0, totalH - (maxY - startY));

  let drawY = startY - scrollOffset;

  for (let i = 0; i < items.length; i++) {
    const item = items[i];
    const iy = drawY + i * lineH;

    // Clip.
    if (iy + lineH < startY) continue;
    if (iy > maxY) break;

    const c = ACTIVITY_COLORS[item.type] || ACTIVITY_COLORS.default;

    // Type prefix in color.
    const prefix = ACTIVITY_PREFIX[item.type] || item.type || "?";
    ink(c.r, c.g, c.b).write(prefix, { x, y: iy }, "black", 36);

    // Label.
    const labelX = x + 38;
    const labelW = w - 38 - 40;
    ink(item.route ? 240 : 180).write(
      item.label || "",
      { x: labelX, y: iy },
      "black",
      labelW,
    );

    // Timestamp right-aligned.
    const when = item.when ? formatTimeAgo(item.when) : "";
    if (when) {
      ink(100).write(when, { x: x + w - 38, y: iy }, "black", 38);
    }

    // Store hit region for tappable items.
    if (item.route) {
      activityBtns.push({
        box: { x, y: iy, w, h: lineH },
        route: item.route,
      });
    }
  }
}

const ACTIVITY_COLORS = {
  mood: { r: 180, g: 120, b: 255 },
  painting: { r: 100, g: 200, b: 255 },
  kidlisp: { r: 120, g: 255, b: 120 },
  clock: { r: 255, g: 200, b: 80 },
  chat: { r: 200, g: 200, b: 200 },
  default: { r: 180, g: 180, b: 180 },
};

const ACTIVITY_PREFIX = {
  mood: "mood",
  painting: "paint",
  kidlisp: "kid",
  clock: "clock",
  chat: "chat",
  event: "event",
};

function paintNoProfileState({ api, help, ink, screen, ui }) {
  const retrieving = noprofile === FETCHING;
  const label = noprofile || "No profile.";

  if (!noprofileAction) {
    const text = retrieving
      ? `${label}${ellipsisTicker.text(help.repeat)}`
      : label;
    ink(255).write(text, { center: "xy" }, retrieving ? 64 : "black");
    return;
  }

  noprofileBtn ||= new ui.TextButton(label, { center: "xy", screen });
  noprofileBtn.paint(api);
}

// --- Data loading ---

async function refreshProfile(force = false) {
  if (!visiting || disposed) return;
  if (loading || refreshing) return;

  if (force) refreshing = true;
  else loading = true;

  dataError = null;

  try {
    await loadIdentity();
    if (!profile) return;
    await loadScorecard();
    loadThumbnails(); // fire and forget
  } catch (err) {
    dataError = "Could not load activity.";
    if (debug) console.warn("Profile refresh failed:", err);
  } finally {
    loading = false;
    refreshing = false;
  }
}

async function loadIdentity() {
  if (!visiting || disposed) return;

  try {
    const response = await fetch(`/api/profile/${visiting}`, {
      headers: { Accept: "application/json" },
    });

    if (!response.ok) {
      profile = null;
      noprofile = `No profile found for: ${visiting}`;
      return;
    }

    const data = await response.json();
    profile = {
      handle: visiting,
      sub: data?.sub || null,
      mood: data?.mood?.mood || null,
      moodWhen: data?.mood?.when || null,
    };
    noprofile = null;
  } catch (error) {
    profile = null;
    noprofile = "Error retrieving profile.";
    if (debug) console.warn("Profile lookup failed:", error);
  }
}

async function loadScorecard() {
  const handle = visiting;
  const bare = bareHandle(visiting);

  const [
    paintingRes,
    pieceRes,
    tapeRes,
    kidlispRes,
    clockRes,
    moodRes,
    chatSystemRes,
    chatClockRes,
  ] = await Promise.all([
    fetchJson(
      `/media-collection?for=${encodeURIComponent(`${handle}/painting`)}`,
    ),
    fetchJson(
      `/media-collection?for=${encodeURIComponent(`${handle}/piece`)}`,
    ),
    fetchJson(
      `/media-collection?for=${encodeURIComponent(`${handle}/tape`)}`,
    ),
    fetchJson(
      `/api/store-kidlisp?recent=true&limit=30&handle=${encodeURIComponent(handle)}`,
    ),
    fetchJson(`/api/store-clock?recent=true&limit=120`),
    fetchJson(`/api/mood/all?for=${encodeURIComponent(handle)}`),
    fetchJson(`/api/chat-messages?instance=system&limit=${CHAT_LIMIT}`),
    fetchJson(`/api/chat-messages?instance=clock&limit=${CHAT_LIMIT}`),
  ]);

  const paintingFiles = Array.isArray(paintingRes?.files)
    ? paintingRes.files
    : [];
  const pieceFiles = Array.isArray(pieceRes?.files) ? pieceRes.files : [];
  const tapeFiles = Array.isArray(tapeRes?.files) ? tapeRes.files : [];
  const kidlispRecent = Array.isArray(kidlispRes?.recent)
    ? kidlispRes.recent
    : [];
  const allClocks = Array.isArray(clockRes?.recent) ? clockRes.recent : [];
  const clocksForHandle = allClocks.filter((item) =>
    sameHandle(item?.handle, handle),
  );
  const moodsRaw = Array.isArray(moodRes?.moods) ? moodRes.moods : [];
  const recentMoods = moodsRaw
    .filter((item) => sameHandle(item?.handle, handle))
    .slice(0, MOOD_LIMIT)
    .map((item) => ({
      mood: item?.mood || "",
      when: toTimestamp(item?.when),
    }));
  if (recentMoods.length === 0 && profile?.mood) {
    recentMoods.push({
      mood: profile.mood,
      when: toTimestamp(profile.moodWhen) || Date.now(),
    });
  }
  const systemMessages = Array.isArray(chatSystemRes?.messages)
    ? chatSystemRes.messages
    : [];
  const clockMessages = Array.isArray(chatClockRes?.messages)
    ? chatClockRes.messages
    : [];
  const recentChats = [...systemMessages, ...clockMessages]
    .filter((item) => sameHandle(item?.from, handle))
    .map((item) => ({
      from: item?.from || null,
      text: item?.text || "",
      when: toTimestamp(item?.when),
      hearts: item?.hearts || 0,
    }))
    .sort((a, b) => (b.when || 0) - (a.when || 0))
    .slice(0, CHAT_LIMIT);

  const recentPaintings = paintingFiles
    .map((url) => parsePaintingUrl(url))
    .filter(Boolean)
    .sort((a, b) => {
      if (a.when && b.when) return b.when - a.when;
      if (a.when) return -1;
      if (b.when) return 1;
      return 0;
    })
    .slice(0, 8);

  await Promise.all(
    recentPaintings.slice(0, 6).map(async (painting) => {
      painting.code = await fetchPaintingCode(painting.slug, bare);
    }),
  );

  const recentKidlisp = kidlispRecent.slice(0, 8).map((item) => ({
    code: item.code,
    when: toTimestamp(item.when),
    hits: item.hits || 0,
  }));

  const recentClocks = clocksForHandle.slice(0, 8).map((item) => ({
    code: item.code,
    when: toTimestamp(item.when),
    hits: item.hits || 0,
  }));

  // Build unified activity list.
  const activity = [];

  recentMoods.slice(0, 12).forEach((item) => {
    if (!item?.mood) return;
    activity.push({
      type: "mood",
      when: item.when,
      label: truncate(compact(item.mood), 50),
    });
  });

  recentPaintings.forEach((item) => {
    const label = item.code ? `#${item.code}` : shortSlug(item.slug);
    activity.push({
      type: "painting",
      when: item.when,
      label,
      route: `painting~${visiting}/${item.slug}`,
    });
  });

  recentKidlisp.forEach((item) => {
    activity.push({
      type: "kidlisp",
      when: item.when,
      label: `$${item.code}`,
      route: `$${item.code}`,
    });
  });

  recentClocks.forEach((item) => {
    activity.push({
      type: "clock",
      when: item.when,
      label: `*${item.code}`,
      route: `*${item.code}`,
    });
  });

  recentChats.slice(0, 18).forEach((item) => {
    const text = compact(item.text);
    if (!text) return;
    activity.push({
      type: "chat",
      when: item.when,
      label: truncate(text, 46),
    });
  });

  activity.sort((a, b) => {
    const aWhen = a.when || 0;
    const bWhen = b.when || 0;
    return bWhen - aWhen;
  });

  scorecard = {
    counts: {
      paintings: paintingFiles.length,
      pieces: pieceFiles.length,
      kidlisp: kidlispRecent.length,
      clocks: clocksForHandle.length,
      tapes: tapeFiles.length,
      moods: recentMoods.length,
      chats: recentChats.length,
    },
    recentMedia: {
      paintings: recentPaintings,
      kidlisp: recentKidlisp,
      clocks: recentClocks,
      moods: recentMoods.slice(0, 10),
      chats: recentChats.slice(0, 10),
    },
    activity: activity.slice(0, ACTIVITY_MAX),
    updatedAt: Date.now(),
  };
}

async function loadThumbnails() {
  if (!getApi || thumbsLoading || disposed) return;

  const paintings = scorecard.recentMedia.paintings;
  if (paintings.length === 0) return;

  thumbsLoading = true;

  const toLoad = paintings.slice(0, THUMB_MAX);

  // Preserve already-loaded thumbs that match.
  const existing = new Map(thumbs.map((t) => [t.slug, t]));

  const next = [];
  for (const p of toLoad) {
    if (existing.has(p.slug) && existing.get(p.slug).img) {
      next.push(existing.get(p.slug));
    } else {
      next.push({
        slug: p.slug,
        code: p.code,
        img: null,
        when: p.when,
        btn: null,
        route: null,
      });
    }
  }
  thumbs = next;

  // Load missing images.
  await Promise.all(
    thumbs.map(async (t) => {
      if (t.img || disposed) return;
      try {
        const got = await getApi.painting(t.slug).by(bareHandle(visiting));
        if (!disposed) t.img = got?.img || null;
      } catch (err) {
        if (debug) console.warn("Thumb load failed:", t.slug, err);
      }
    }),
  );

  thumbsLoading = false;
}

// --- WebSocket profile stream ---

function startRefreshLoop() {
  clearInterval(refreshTimer);
  refreshTimer = setInterval(() => {
    if (!disposed) refreshProfile(true);
  }, REFRESH_MS);
}

function connectProfileStream() {
  if (!visiting || disposed) return;
  closeStatusSocket();

  const url = getProfileStreamUrl();
  if (!url) return;

  try {
    statusSocket = new WebSocket(url);
  } catch (err) {
    if (debug) console.warn("Could not open profile stream:", err);
    scheduleReconnect();
    return;
  }

  statusSocket.addEventListener("message", (event) => {
    if (disposed) return;

    try {
      const msg = JSON.parse(event.data);
      if (msg?.type === "profile:snapshot" || msg?.type === "presence:update") {
        applyPresenceSnapshot(msg?.data?.presence);
        return;
      }
      if (msg?.type === "activity:append") {
        appendActivity(msg?.data?.event);
        return;
      }
      if (msg?.type === "counts:update") {
        mergeCounts(msg?.data?.counts);
        return;
      }
      if (msg?.type === "counts:delta") {
        applyCountDelta(msg?.data?.delta);
        return;
      }
      if (msg?.type === "status") {
        const clients = Array.isArray(msg?.data?.clients)
          ? msg.data.clients
          : [];
        applyPresence(clients);
      }
    } catch (err) {
      if (debug) console.warn("Profile stream parse failure:", err);
    }
  });

  statusSocket.addEventListener("close", () => {
    statusSocket = null;
    if (!disposed) scheduleReconnect();
  });

  statusSocket.addEventListener("error", () => {
    try {
      statusSocket?.close();
    } catch (_) {
      // Ignore close errors.
    }
  });
}

function applyPresence(clients) {
  const matched = clients.find((client) =>
    sameHandle(client?.handle, visiting),
  );

  if (!matched) {
    if (presence.online) presence.lastSeenAt = Date.now();
    presence.online = false;
    presence.currentPiece = null;
    presence.worldPiece = null;
    presence.showing = null;
    presence.connections = 0;
    presence.ping = null;
    return;
  }

  const world = matched?.websocket?.worlds?.[0] || null;

  presence.online = true;
  presence.currentPiece = matched.location || null;
  presence.worldPiece = world?.piece || null;
  presence.showing = formatShowing(world?.showing);
  presence.connections = matched?.connectionCount?.total || 1;
  presence.ping = matched?.websocket?.ping || null;
  presence.lastSeenAt = Date.now();
}

function applyPresenceSnapshot(snapshot) {
  if (!snapshot || typeof snapshot !== "object") return;

  const isOnline = !!snapshot.online;
  if (!isOnline && presence.online) {
    presence.lastSeenAt = Date.now();
  }

  presence.online = isOnline;
  presence.currentPiece = snapshot.currentPiece || null;
  presence.worldPiece = snapshot.worldPiece || null;
  presence.showing = formatShowing(snapshot.showing);

  if (snapshot.connections && typeof snapshot.connections === "object") {
    presence.connections =
      snapshot.connections.total ||
      snapshot.connections.websocket ||
      snapshot.connections.udp ||
      0;
  } else {
    presence.connections = snapshot.connections || 0;
  }

  presence.ping = snapshot.pingMs || snapshot.ping || null;
  const fallbackLastSeen = isOnline ? Date.now() : presence.lastSeenAt || null;
  presence.lastSeenAt = snapshot.lastSeenAt || fallbackLastSeen;
}

function appendActivity(event) {
  if (!event || typeof event !== "object") return;
  const label = compact(event.label || event.text || "");
  if (!label) return;

  const when = toTimestamp(event.when) || Date.now();
  scorecard.activity = [
    { type: event.type || "event", when, label },
    ...scorecard.activity,
  ]
    .sort((a, b) => (b.when || 0) - (a.when || 0))
    .slice(0, ACTIVITY_MAX);
}

function mergeCounts(nextCounts) {
  if (!nextCounts || typeof nextCounts !== "object") return;
  scorecard.counts = { ...scorecard.counts, ...nextCounts };
}

function applyCountDelta(delta) {
  if (!delta || typeof delta !== "object") return;

  const next = { ...scorecard.counts };
  for (const [key, value] of Object.entries(delta)) {
    const amount = Number(value);
    if (!Number.isFinite(amount)) continue;
    const previous = Number(next[key] || 0);
    const updated = previous + amount;
    next[key] = updated < 0 ? 0 : updated;
  }

  scorecard.counts = next;
}

// --- Timers & socket ---

function formatShowing(showing) {
  if (!showing) return null;
  if (typeof showing === "string") return truncate(showing, 18);
  if (showing.slug) return truncate(showing.slug, 18);
  if (showing.code) return `#${showing.code}`;
  if (showing.piece) return truncate(showing.piece, 18);
  if (showing.url)
    return truncate(`${showing.url}`.split("/").pop() || "", 18);
  return null;
}

function getProfileStreamUrl() {
  const handle = normalizeHandle(visiting);
  if (!handle) return null;

  const query = `handle=${encodeURIComponent(handle)}`;
  if (typeof location === "undefined") {
    return `wss://session-server.aesthetic.computer/profile-stream?${query}`;
  }

  const host = location.hostname;
  const isLocal =
    host === "localhost" ||
    host === "127.0.0.1" ||
    host.endsWith(".local") ||
    host.endsWith(".localhost");

  if (isLocal) {
    const protocol = location.protocol === "http:" ? "ws" : "wss";
    return `${protocol}://localhost:8889/profile-stream?${query}`;
  }

  return `wss://session-server.aesthetic.computer/profile-stream?${query}`;
}

function scheduleReconnect() {
  clearTimeout(reconnectTimer);
  reconnectTimer = setTimeout(() => {
    if (!disposed) connectProfileStream();
  }, RECONNECT_MS);
}

function closeStatusSocket() {
  if (!statusSocket) return;
  try {
    statusSocket.close();
  } catch (_) {
    // Ignore close errors.
  }
  statusSocket = null;
}

function clearTimersAndSocket() {
  clearInterval(refreshTimer);
  clearTimeout(reconnectTimer);
  refreshTimer = null;
  reconnectTimer = null;
  closeStatusSocket();
}

function resetUiState() {
  noprofileBtn = null;
  paintingsBtn = null;
  activityBtns = [];
  for (const t of thumbs) t.btn = null;
}

// --- Utilities ---

async function fetchJson(url) {
  try {
    const response = await fetch(url, {
      headers: { Accept: "application/json" },
    });
    if (!response.ok) return null;
    return await response.json();
  } catch (err) {
    if (debug) console.warn("Fetch failed:", url, err);
    return null;
  }
}

function parsePaintingUrl(url) {
  if (!url) return null;
  const clean = `${url}`.split("?")[0];
  const parts = clean.split("/");
  const last = parts.pop();
  if (!last) return null;

  const slug = last.replace(/\.(png|zip)$/i, "");
  if (!slug) return null;

  return {
    slug,
    url,
    when: timestampFromSlug(slug),
    code: null,
  };
}

async function fetchPaintingCode(slug, bare) {
  if (!slug || !bare) return null;

  const params = new URLSearchParams({ slug, handle: bare });
  const data = await fetchJson(`/api/painting-metadata?${params.toString()}`);
  if (!data?.code) return null;
  return `${data.code}`.replace(/^#/, "");
}

function timestampFromSlug(slug) {
  if (!slug) return null;
  const base = `${slug}`.split(":")[0];
  if (!/^\d+$/.test(base)) return null;
  const maybe = Number(base);
  if (!Number.isFinite(maybe)) return null;
  if (maybe < 1000000000) return null;
  return maybe;
}

function normalizeHandle(value) {
  if (!value) return null;
  const text = `${value}`.trim();
  if (!text) return null;
  return text.startsWith("@") ? text : `@${text}`;
}

function bareHandle(value) {
  return normalizeHandle(value)?.slice(1) || null;
}

function sameHandle(a, b) {
  const left = normalizeHandle(a);
  const right = normalizeHandle(b);
  if (!left || !right) return false;
  return left.toLowerCase() === right.toLowerCase();
}

function toTimestamp(value) {
  if (!value) return null;
  const ts = new Date(value).getTime();
  if (!Number.isFinite(ts)) return null;
  return ts;
}

function formatTimeAgo(when) {
  const timestamp = typeof when === "number" ? when : toTimestamp(when);
  if (!timestamp) return "";

  const diff = max(0, Date.now() - timestamp);
  const seconds = floor(diff / 1000);
  if (seconds < 60) return `${seconds}s`;

  const minutes = floor(seconds / 60);
  if (minutes < 60) return `${minutes}m`;

  const hours = floor(minutes / 60);
  if (hours < 24) return `${hours}h`;

  const days = floor(hours / 24);
  return `${days}d`;
}

function formatPieceLabel(slug) {
  if (!slug) return null;
  const normalized = `${slug}`.trim();
  if (!normalized) return null;
  const piece = normalized.split("~")[0];
  return truncate(piece, 20);
}

function shortSlug(slug) {
  if (!slug) return "-";
  const text = `${slug}`;
  if (text.length <= 10) return text;
  return `${text.slice(0, 4)}...${text.slice(-4)}`;
}

function compact(value) {
  return `${value || ""}`.replace(/\s+/g, " ").trim();
}

function truncate(value, mx) {
  const text = `${value || ""}`;
  if (text.length <= mx) return text;
  return `${text.slice(0, mx - 3)}...`;
}

export { boot, paint, act, sim, leave, meta };
