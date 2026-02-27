// Profile, 2026.02.27.12.40.00
// Public user scorecard page with live presence + recent activity.

const FETCHING = "Fetching";
const REFRESH_MS = 30000;
const RECONNECT_MS = 3000;
const MOOD_LIMIT = 40;
const CHAT_LIMIT = 120;

let debug;
let visiting;
let profile;
let noprofile = FETCHING;
let noprofileAction;
let noprofileBtn;
let paintingsBtn;
let refreshBtn;
let ellipsisTicker;

let loading = false;
let refreshing = false;
let dataError = null;

let scorecard = makeEmptyScorecard();
let presence = makeOfflinePresence();

let statusSocket = null;
let reconnectTimer = null;
let refreshTimer = null;
let disposed = false;

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
    desc: `Live activity scorecard for ${piece}.`,
  };
}

async function boot({
  params,
  user,
  gizmo,
  handle,
  hud,
  net,
  debug: d,
}) {
  disposed = false;
  debug = d;

  const hand = normalizeHandle(handle());
  visiting = normalizeHandle(params[0] || hand);

  ellipsisTicker = new gizmo.EllipsisTicker();

  resetUiState();
  clearTimersAndSocket();
  scorecard = makeEmptyScorecard();
  presence = makeOfflinePresence();
  profile = null;
  dataError = null;

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

function paint({ api, wipe, help, ink, screen, ui, pen }) {
  if (!pen?.drawing) wipe(98);
  ink(127).line();

  if (!visiting) {
    paintNoProfileState({ api, help, ink, screen, ui });
    return;
  }

  let y = 6;

  ink(255).write(visiting, { x: 4, y }, "black", screen.width - 8);
  y += 10;

  if (!profile && noprofile === FETCHING) {
    ink(255).write(
      `${FETCHING}${ellipsisTicker.text(help.repeat)}`,
      { center: "xy" },
      "black",
    );
    return;
  }

  if (!profile && noprofile && noprofile !== FETCHING) {
    ink(255).write(noprofile, { center: "xy" }, "black");
    return;
  }

  if (profile?.mood) {
    ink(255).write(`Mood: ${profile.mood}`, { x: 4, y }, "black", screen.width - 8);
  } else {
    ink(127).write("Mood: -", { x: 4, y }, "black", screen.width - 8);
  }
  y += 12;

  const counts = scorecard.counts;
  ink(255).write(
    `Paint ${counts.paintings}  Piece ${counts.pieces}  Kid ${counts.kidlisp}  Clock ${counts.clocks}  Tape ${counts.tapes}`,
    { x: 4, y },
    "black",
    screen.width - 8,
  );
  y += 10;
  ink(255).write(
    `Mood ${counts.moods}  Chat ${counts.chats}`,
    { x: 4, y },
    "black",
    screen.width - 8,
  );
  y += 12;

  ink("yellow").write("Live", { x: 4, y }, "black", screen.width - 8);
  y += 10;

  const onlineText = presence.online ? "online" : "offline";
  const currentPiece = formatPieceLabel(presence.currentPiece) || "-";
  const pingText = presence.ping ? ` ${presence.ping}ms` : "";
  ink(255).write(
    `${onlineText}  piece ${currentPiece}${pingText}`,
    { x: 4, y },
    "black",
    screen.width - 8,
  );
  y += 10;

  if (presence.worldPiece || presence.showing) {
    ink(255).write(
      `world ${presence.worldPiece || "-"}  showing ${presence.showing || "-"}`,
      { x: 4, y },
      "black",
      screen.width - 8,
    );
    y += 10;
  }

  if (!presence.online && presence.lastSeenAt) {
    ink(127).write(
      `last seen ${formatTimeAgo(presence.lastSeenAt)}`,
      { x: 4, y },
      "black",
      screen.width - 8,
    );
    y += 10;
  }

  y += 2;
  ink("yellow").write("Recent Activity", { x: 4, y }, "black", screen.width - 8);
  y += 10;

  if ((loading || refreshing) && scorecard.activity.length === 0) {
    ink(255).write(
      `${FETCHING}${ellipsisTicker.text(help.repeat)}`,
      { x: 4, y },
      "black",
      screen.width - 8,
    );
    y += 10;
  } else if (scorecard.activity.length === 0) {
    const emptyText = dataError || "No activity yet.";
    ink(127).write(emptyText, { x: 4, y }, "black", screen.width - 8);
    y += 10;
  } else {
    const maxRows = Math.max(3, Math.floor((screen.height - y - 24) / 10));
    const rows = scorecard.activity.slice(0, maxRows);
    rows.forEach((item) => {
      const when = item.when ? formatTimeAgo(item.when) : "recent";
      ink(255).write(`${when}  ${item.label}`, { x: 4, y }, "black", screen.width - 8);
      y += 10;
    });
  }

  if (refreshing) {
    ink(127).write(
      `Refreshing${ellipsisTicker.text(help.repeat)}`,
      { x: screen.width - 84, y: 2 },
      "black",
      82,
    );
  }

  paintingsBtn ||= new ui.TextButton("Paintings", { x: 4, y: screen.height - 14 });
  refreshBtn ||= new ui.TextButton("Refresh", { x: screen.width - 47, y: screen.height - 14 });

  paintingsBtn.paint(api);
  refreshBtn.paint(api);
}

function act({ event: e, jump, store, user }) {
  paintingsBtn?.act(e, () => {
    if (visiting) jump(`paintings~${visiting}`);
  });

  refreshBtn?.act(e, () => {
    refreshProfile(true);
  });

  if (e.is("keyboard:down:g") && visiting) {
    jump(`paintings~${visiting}`);
  }

  if (e.is("keyboard:down:r")) {
    refreshProfile(true);
  }

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
    refreshBtn = null;
  }
}

function sim() {
  ellipsisTicker?.sim();
}

function leave() {
  disposed = true;
  clearTimersAndSocket();
}

function paintNoProfileState({ api, help, ink, screen, ui }) {
  const retrieving = noprofile === FETCHING;
  const label = noprofile || "No profile.";

  if (!noprofileAction) {
    const text = retrieving ? `${label}${ellipsisTicker.text(help.repeat)}` : label;
    ink(255).write(text, { center: "xy" }, retrieving ? 64 : "black");
    return;
  }

  noprofileBtn ||= new ui.TextButton(label, { center: "xy", screen });
  noprofileBtn.paint(api);
}

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
    const response = await fetch(`/api/profile/${encodeURIComponent(visiting)}`, {
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
    fetchJson(`/media-collection?for=${encodeURIComponent(`${handle}/painting`)}`),
    fetchJson(`/media-collection?for=${encodeURIComponent(`${handle}/piece`)}`),
    fetchJson(`/media-collection?for=${encodeURIComponent(`${handle}/tape`)}`),
    fetchJson(
      `/api/store-kidlisp?recent=true&limit=30&handle=${encodeURIComponent(handle)}`,
    ),
    fetchJson(`/api/store-clock?recent=true&limit=120`),
    fetchJson(`/api/mood/all?for=${encodeURIComponent(handle)}`),
    fetchJson(`/api/chat-messages?instance=system&limit=${CHAT_LIMIT}`),
    fetchJson(`/api/chat-messages?instance=clock&limit=${CHAT_LIMIT}`),
  ]);

  const paintingFiles = Array.isArray(paintingRes?.files) ? paintingRes.files : [];
  const pieceFiles = Array.isArray(pieceRes?.files) ? pieceRes.files : [];
  const tapeFiles = Array.isArray(tapeRes?.files) ? tapeRes.files : [];
  const kidlispRecent = Array.isArray(kidlispRes?.recent) ? kidlispRes.recent : [];
  const allClocks = Array.isArray(clockRes?.recent) ? clockRes.recent : [];
  const clocksForHandle = allClocks.filter((item) => sameHandle(item?.handle, handle));
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
  const systemMessages = Array.isArray(chatSystemRes?.messages) ? chatSystemRes.messages : [];
  const clockMessages = Array.isArray(chatClockRes?.messages) ? chatClockRes.messages : [];
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

  const activity = [];

  recentMoods.slice(0, 12).forEach((item) => {
    if (!item?.mood) return;
    activity.push({
      type: "mood",
      when: item.when,
      label: `Mood: ${truncate(compact(item.mood), 44)}`,
    });
  });

  recentPaintings.forEach((item) => {
    const label = item.code
      ? `Painting #${item.code}`
      : `Painting ${shortSlug(item.slug)}`;
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
      label: `KidLisp $${item.code}`,
      route: `$${item.code}`,
    });
  });

  recentClocks.forEach((item) => {
    activity.push({
      type: "clock",
      when: item.when,
      label: `Clock *${item.code}`,
      route: `*${item.code}`,
    });
  });

  recentChats.slice(0, 18).forEach((item) => {
    const text = compact(item.text);
    if (!text) return;
    activity.push({
      type: "chat",
      when: item.when,
      label: `Chat: ${truncate(text, 40)}`,
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
    activity: activity.slice(0, 20),
    updatedAt: Date.now(),
  };
}

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
        const clients = Array.isArray(msg?.data?.clients) ? msg.data.clients : [];
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
  const matched = clients.find((client) => sameHandle(client?.handle, visiting));

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
  scorecard.activity = [{ type: event.type || "event", when, label }, ...scorecard.activity]
    .sort((a, b) => (b.when || 0) - (a.when || 0))
    .slice(0, 20);
}

function mergeCounts(nextCounts) {
  if (!nextCounts || typeof nextCounts !== "object") return;
  scorecard.counts = {
    ...scorecard.counts,
    ...nextCounts,
  };
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

function formatShowing(showing) {
  if (!showing) return null;
  if (typeof showing === "string") return truncate(showing, 18);
  if (showing.slug) return truncate(showing.slug, 18);
  if (showing.code) return `#${showing.code}`;
  if (showing.piece) return truncate(showing.piece, 18);
  if (showing.url) return truncate(`${showing.url}`.split("/").pop() || "", 18);
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
  refreshBtn = null;
}

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
  if (!timestamp) return "recent";

  const diff = Math.max(0, Date.now() - timestamp);
  const seconds = Math.floor(diff / 1000);
  if (seconds < 60) return `${seconds}s ago`;

  const minutes = Math.floor(seconds / 60);
  if (minutes < 60) return `${minutes}m ago`;

  const hours = Math.floor(minutes / 60);
  if (hours < 24) return `${hours}h ago`;

  const days = Math.floor(hours / 24);
  return `${days}d ago`;
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

function truncate(value, max) {
  const text = `${value || ""}`;
  if (text.length <= max) return text;
  return `${text.slice(0, max - 3)}...`;
}

export { boot, paint, act, sim, leave, meta };
