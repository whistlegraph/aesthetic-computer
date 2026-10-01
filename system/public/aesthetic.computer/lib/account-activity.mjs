import { activityPiece, accountActivityRoute, SOTCE_ACTIONS } from "./account-activity-model.mjs";
import { LAKLOK_ACTIONS, LAKLOK_PIECES, LAKLOK_FEATURE_VERSION } from "./laklok-activity.mjs";
import { automatedVisit, visitProperty, visitSurface, visitReferrer, VISIT_ACTIONS } from "./visit-model.mjs";

// Authenticated activity has its own short-lived session and endpoint. It does
// not add an account field or a join identifier to anonymous network visits.
export function startAccountActivity(win = window, doc = document, {
  getUser = () => win.acUSER,
  getToken = () => win.acTOKEN || win.auth0Client?.getTokenSilently(),
} = {}) {
  if (win.acAccountActivity) return win.acAccountActivity;
  let piece = null, ready = false, owner = null, session = null, generation = 0, sequence = 0, stopped = false;
  let sent = new Set(), pending = new Set(), retryAt = 0, inFlight = 0;
  const allowed = action => !stopped && win === win.top && doc.visibilityState === "visible" &&
    !win.acPACK_MODE && !win.acVisitTrackingDisabled &&
    win.navigator.doNotTrack !== "1" && win.doNotTrack !== "1" && !win.navigator.globalPrivacyControl &&
    visitProperty(win.location.hostname) && accountActivityRoute(win.location.hostname, win.location.pathname, action) &&
    !automatedVisit(win.navigator, win.location.search, win.acAutomation === true);
  function syncOwner() {
    const next = getUser()?.sub || null;
    if (next !== owner) {
      owner = next; session = next ? win.crypto.randomUUID() : null;
      sent = new Set(); pending = new Set(); generation++; sequence = 0; retryAt = 0;
    }
  }
  async function record(action) {
    syncOwner();
    const laklok = LAKLOK_ACTIONS.includes(action);
    const sotce = SOTCE_ACTIONS.includes(action);
    const repeated = laklok || sotce;
    if (sotce && (visitProperty(win.location.hostname) !== "sotce.net" || piece !== "sotce")) return;
    if (laklok && !LAKLOK_PIECES.includes(piece)) return;
    if (!owner || !piece || !ready || !allowed(action) || (!repeated && sent.has(action)) ||
        (!laklok && pending.has(action)) || inFlight >= 20 || Date.now() < retryAt) return;
    const epoch = generation, subject = owner, activeSession = session, activePiece = piece;
    const order = ++sequence;
    const activePending = pending, activeSent = sent;
    activePending.add(action);
    inFlight++;
    let timeout;
    const controller = new AbortController();
    try {
      const expired = new Promise((_, reject) => { timeout = win.setTimeout(() => { controller.abort(); reject(new Error("activity timeout")); }, 10000); });
      const token = await Promise.race([Promise.resolve().then(getToken), expired]);
      if (!token || epoch !== generation || subject !== getUser()?.sub || !allowed(action)) return;
      const response = await win.fetch("https://aesthetic.computer/api/account-activity", {
        method: "POST", credentials: "omit", referrerPolicy: "no-referrer", signal: controller.signal, keepalive: true,
        headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}` },
        body: JSON.stringify({ version: 1, id: win.crypto.randomUUID(), session: activeSession,
          sequence: order, piece: activePiece, action, automated: false, referrerHost: visitReferrer(doc.referrer),
          ...(LAKLOK_PIECES.includes(activePiece) ? { featureVersion: LAKLOK_FEATURE_VERSION } : {}) }),
      });
      if (response.ok) activeSent.add(action);
      else retryAt = Date.now() + 30000;
    } catch { retryAt = Date.now() + 30000; }
    finally { inFlight--; win.clearTimeout(timeout); activePending.delete(action); }
  }
  const timer = win.setInterval(() => {
    syncOwner();
    if (visitSurface(win.location.pathname) === null) {
      session = owner ? win.crypto.randomUUID() : null;
      sent.clear(); generation++;
      return;
    }
    void record("piece_opened");
  }, 2000);
  const api = {
    load(path) { piece = activityPiece(path); ready = false; sent = new Set(); pending = new Set(); generation++; },
    ready() { ready = true; void record("piece_opened"); },
    action(name) { if (VISIT_ACTIONS.includes(name) || SOTCE_ACTIONS.includes(name) || LAKLOK_ACTIONS.includes(name)) void record(name); },
    stop() { stopped = true; generation++; win.clearInterval(timer); if (win.acAccountActivity === api) delete win.acAccountActivity; },
  };
  win.acAccountActivity = api;
  return api;
}
