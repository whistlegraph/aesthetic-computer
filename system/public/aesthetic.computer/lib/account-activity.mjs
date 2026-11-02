import { FEED_ACTIONS, createFeedActivity, feedActivityFields } from "./feed-activity.mjs";
import { activityPiece, accountActivityRoute, SOTCE_ACTIONS } from "./account-activity-model.mjs";
import { LAKLOK_ACTIONS, LAKLOK_PIECES, LAKLOK_FEATURE_VERSION, LAKLOK_PRESENCE } from "./laklok-activity.mjs";
import { automatedVisit, visitProperty, visitSurface, visitReferrer, VISIT_ACTIONS } from "./visit-model.mjs";

// Authenticated activity has its own short-lived session and endpoint. It does
// not add an account field or a join identifier to anonymous network visits.
export function startAccountActivity(win = window, doc = document, {
  getUser = () => win.acUSER,
  getToken = () => win.acTOKEN || win.auth0Client?.getTokenSilently(),
} = {}) {
  if (win.acAccountActivity) return win.acAccountActivity;
  let piece = null, ready = false, owner = null, session = null, generation = 0, ownerGeneration = 0, sequence = 0, stopped = false;
  let sent = new Set(), pending = new Set(), retryAt = 0, inFlight = 0, laklokVisibleMs = 0;
  const allowed = (action, background = false) => !stopped && win === win.top && (background || doc.visibilityState === "visible") &&
    !win.acPACK_MODE && !win.acVisitTrackingDisabled &&
    win.navigator.doNotTrack !== "1" && win.doNotTrack !== "1" && !win.navigator.globalPrivacyControl &&
    visitProperty(win.location.hostname) && accountActivityRoute(win.location.hostname, win.location.pathname, action) &&
    !automatedVisit(win.navigator, win.location.search, win.acAutomation === true);
  function syncOwner() {
    const next = getUser()?.sub || null;
    if (next !== owner) {
      ownerGeneration++; owner = next; session = next ? win.crypto.randomUUID() : null;
      sent = new Set(); pending = new Set(); generation++; sequence = 0; retryAt = 0;
    }
  }
  async function record(action, details = {}) {
    syncOwner();
    const laklok = LAKLOK_ACTIONS.includes(action) || action === LAKLOK_PRESENCE;
    const sotce = SOTCE_ACTIONS.includes(action);
    const feed = FEED_ACTIONS.includes(action);
    if (feed && !feedActivityFields(action, { ...details, piece: "mime" }, visitProperty(win.location.hostname))) return;
    const repeated = laklok || sotce || feed;
    if (sotce && (visitProperty(win.location.hostname) !== "sotce.net" || piece !== "sotce")) return;
    if (laklok && !LAKLOK_PIECES.includes(piece)) return;
    if (!owner || (!feed && (!piece || !ready)) || !allowed(action, feed) || (!repeated && sent.has(action)) ||
        (!repeated && pending.has(action)) || inFlight >= 20 || Date.now() < retryAt) return;
    const epoch = generation, ownerEpoch = ownerGeneration, subject = owner, activeSession = session, activePiece = feed ? "mime" : piece;
    const order = ++sequence;
    const activePending = pending, activeSent = sent;
    activePending.add(action);
    inFlight++;
    let timeout;
    const controller = new AbortController();
    try {
      const expired = new Promise((_, reject) => { timeout = win.setTimeout(() => { controller.abort(); reject(new Error("activity timeout")); }, 10000); });
      const token = await Promise.race([Promise.resolve().then(getToken), expired]);
      if (!token || (feed ? ownerEpoch !== ownerGeneration : epoch !== generation) || subject !== getUser()?.sub || !allowed(action, feed)) return;
      const response = await win.fetch("https://aesthetic.computer/api/account-activity", {
        method: "POST", credentials: "omit", referrerPolicy: "no-referrer", signal: controller.signal, keepalive: true,
        headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}` },
        body: JSON.stringify({ version: 1, id: win.crypto.randomUUID(), session: activeSession,
          sequence: order, piece: activePiece, action, automated: false,
          ...(feed ? details : {}), referrerHost: visitReferrer(doc.referrer),
          ...(LAKLOK_PIECES.includes(activePiece) ? { featureVersion: LAKLOK_FEATURE_VERSION } : {}) }),
      });
      if (response.ok) activeSent.add(action);
      else retryAt = Date.now() + 30000;
    } catch { retryAt = Date.now() + 30000; }
    finally { inFlight--; win.clearTimeout(timeout); activePending.delete(action); }
  }
  const feed = createFeedActivity({
    owner: () => getUser()?.sub || null,
    allowed: () => allowed("mime_piece_viewed", true),
    emit: (action, details) => { void record(action, details); },
    now: () => win.performance?.now() ?? Date.now(),
  });
  const timer = win.setInterval(() => {
    syncOwner();
    if (visitSurface(win.location.pathname) === null) {
      session = owner ? win.crypto.randomUUID() : null;
      sent.clear(); generation++;
      return;
    }
    void record("piece_opened");
    // Count laklok minutes by visible ticks; a sleeping or hidden tab adds none.
    if (!owner || !ready || !LAKLOK_PIECES.includes(piece) || doc.visibilityState !== "visible") return;
    laklokVisibleMs += 2000;
    if (laklokVisibleMs >= 60000) { laklokVisibleMs -= 60000; void record(LAKLOK_PRESENCE); }
  }, 2000);
  const api = {
    feedOpen(entry) { feed.open(entry); },
    feedSample(contentId, active) { feed.sample(contentId, active && doc.visibilityState === "visible"); },
    feedClose(reason) { feed.close(reason); },
    load(path) { feed.close("navigation"); piece = activityPiece(path); ready = false; laklokVisibleMs = 0; sent = new Set(); pending = new Set(); generation++; },
    ready() { ready = true; void record("piece_opened"); },
    action(name) { if (VISIT_ACTIONS.includes(name) || SOTCE_ACTIONS.includes(name) || LAKLOK_ACTIONS.includes(name)) void record(name); },
    stop() { stopped = true; generation++; win.clearInterval(timer); if (win.acAccountActivity === api) delete win.acAccountActivity; },
  };
  win.acAccountActivity = api;
  return api;
}
