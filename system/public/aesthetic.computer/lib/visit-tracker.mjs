import { ACTIVE_BUCKETS, VISIT_ACTIONS, automatedVisit, visitProperty, visitSurface } from "./visit-model.mjs";

const ENDPOINT = "https://aesthetic.computer/api/visit-track";

export function startVisitTracker(win = window, doc = document) {
  if (win.acVisits) return win.acVisits;
  const nav = win.navigator;
  const disabled = () => nav.doNotTrack === "1" || win.doNotTrack === "1" ||
    nav.globalPrivacyControl === true || win.acVisitTrackingDisabled === true;
  if (disabled() || win !== win.top || win.acPACK_MODE ||
      !visitProperty(win.location.hostname) || !win.crypto?.randomUUID) return null;

  let state, path, visibleMs = 0, lastTick = win.performance.now(), lastSent = "";
  let wasVisible = doc.visibilityState === "visible";
  let stopped = false, timer, failures = 0, retryAt = 0;
  const listeners = [];
  const on = (target, event, handler, options) => {
    target.addEventListener(event, handler, options);
    listeners.push(() => target.removeEventListener(event, handler, options));
  };
  const visible = () => doc.visibilityState === "visible";
  const reset = () => {
    path = win.location.pathname;
    const surface = visitSurface(path);
    state = surface === null ? null : {
      version: 1, id: win.crypto.randomUUID(), surface,
      automated: automatedVisit(nav, win.location.search, win.acAutomation === true),
      interacted: false, activeSeconds: 0, inputs: [], actions: [],
    };
    visibleMs = 0; lastSent = ""; lastTick = win.performance.now();
    wasVisible = visible();
  };
  const syncRoute = () => {
    if (path === win.location.pathname) return;
    path = win.location.pathname;
    // Room/round URLs can change automatically. They are one page visit,
    // not new arrivals. Only crossing a private boundary resets measurement.
    const allowed = visitSurface(path) !== null;
    if (allowed !== Boolean(state)) reset();
  };
  const send = () => {
    if (!state || disabled() || stopped || win.performance.now() < retryAt) return;
    // Markers can be installed after the initial module by a render harness.
    state.automated ||= automatedVisit(nav, win.location.search, win.acAutomation === true);
    const body = JSON.stringify(state);
    if (body === lastSent) return;
    lastSent = body;
    // text/plain avoids preflight; omit credentials and referrer across sites.
    // No persistent retry queue or identifiers survive leaving this document.
    const failed = () => {
      if (lastSent !== body) return;
      lastSent = "";
      retryAt = win.performance.now() + Math.min(60000, 1000 * 2 ** Math.min(++failures, 6));
    };
    try {
      Promise.resolve(win.fetch(ENDPOINT, {
        method: "POST", body, credentials: "omit", referrerPolicy: "no-referrer",
        headers: { "Content-Type": "text/plain" }, keepalive: true,
      })).then(response => { if (!response.ok) failed(); else failures = 0; })
        .catch(failed);
    } catch { failed(); }
  };
  const tick = () => {
    if (stopped) return;
    syncRoute();
    const now = win.performance.now();
    // Cap suspension gaps: a sleeping laptop is not engagement time.
    if (wasVisible) visibleMs += Math.min(2000, Math.max(0, now - lastTick));
    lastTick = now; wasVisible = visible();
    if (state) state.activeSeconds = ACTIVE_BUCKETS.filter(n => n * 1000 <= visibleMs).at(-1);
    if (visible() || lastSent) send();
  };
  const interact = input => {
    if (!visible() || disabled()) return;
    tick();
    if (!state) return;
    state.interacted = true;
    if (!state.inputs.includes(input)) state.inputs.push(input);
    send();
  };
  const action = name => {
    if (!VISIT_ACTIONS.includes(name) || !visible() || disabled()) return false;
    syncRoute();
    if (!state?.interacted) return false;
    if (!state.actions.includes(name)) state.actions.push(name);
    send(); return true;
  };
  reset();
  for (const [event, input] of [["pointerdown", "pointer"], ["touchstart", "touch"], ["keydown", "keyboard"], ["wheel", "scroll"]]) {
    on(win, event, e => {
      if (!e.isTrusted) return;
      // Typing in account/contact/editor fields is not collected as interaction.
      if (e.target?.closest?.("input,textarea,select,[contenteditable], [data-ac-no-track]")) return;
      interact(e.pointerType === "touch" ? "touch" : input);
      if (e.target?.tagName === "CANVAS") action("canvas_interacted");
    }, true);
  }
  on(win, "click", e => {
    if (!e.isTrusted || e.target?.closest?.("[data-ac-no-track]")) return;
    const link = e.target?.closest?.("a[href]");
    if (!link) return;
    interact(e.detail === 0 ? "keyboard" : "pointer");
    let url;
    try { url = new URL(link.href, win.location.href); } catch { return; }
    if (!/^https?:$/.test(url.protocol)) return;
    action(link.hasAttribute("download") || /\.(?:dmg|zip|pdf|amxd|exe|apk)$/i.test(url.pathname)
      ? "download_clicked" : "link_followed");
  }, true);
  on(doc, "playing", e => {
    if (["AUDIO", "VIDEO"].includes(e.target?.tagName)) action("media_started");
  }, true);
  on(doc, "visibilitychange", tick);
  on(win, "pagehide", tick);
  // One slow poll covers SPA navigation, visible time and real gamepad presses.
  timer = win.setInterval(() => {
    tick();
    if (!visible() || !doc.hasFocus()) return;
    try {
      if ([...(nav.getGamepads?.() || [])].some(pad =>
        pad?.buttons?.some(button => button.pressed) ||
        pad?.axes?.slice(0, 4).some(axis => Math.abs(axis) > 0.5))) interact("gamepad");
    } catch { /* unavailable in some embedded browsers */ }
  }, 1000);
  const api = { action, stop() { stopped = true; win.clearInterval(timer); listeners.forEach(remove => remove()); if (win.acVisits === api) delete win.acVisits; } };
  win.acVisits = api;
  if (visible()) send();
  return api;
}

if (typeof window !== "undefined") startVisitTracker();
