// Native APNs bridge. Permission belongs to the app's explicit `notifs` command.
// Serialize writes so `nonotifs` always removes an in-flight subscription last.
export function installIOSPushBridge(win) {
  const validToken = (value) => typeof value === "string" && /^[0-9a-f]{32,512}$/i.test(value);
  const read = (key) => { try { return win.localStorage.getItem(key); } catch { return null; } };
  const write = (key, value) => { try { win.localStorage.setItem(key, value); } catch {} };
  const tokenKey = "ac-ios-push-token";
  const disabledKey = "ac-ios-push-disabled";
  const pendingKey = "ac-ios-push-removals";
  let pending;
  try { pending = JSON.parse(read(pendingKey) || "[]"); } catch {}
  pending = new Set(Array.isArray(pending) ? pending.filter(validToken) : []);
  let token = null; // Only native can enable delivery for this page.
  let enabled = false;
  let revision = 0;
  let registeredAs;
  let queue = Promise.resolve();
  const persistPending = () => write(pendingKey, JSON.stringify([...pending]));
  const rememberRemoval = (value) => {
    if (validToken(value)) { pending.add(value); persistPending(); }
  };

  // One retry: a server stall or 5xx shouldn't cost a `notifs` attempt.
  // Registration is an idempotent upsert, so sending twice is harmless.
  async function post(body, headers = {}) {
    for (let attempt = 0; ; attempt++) {
      const controller = new AbortController();
      const timeout = setTimeout(() => controller.abort(), 10000);
      try {
        const response = await win.fetch("/api/register-push-token", {
          method: "POST", signal: controller.signal,
          headers: { "Content-Type": "application/json", ...headers },
          body: JSON.stringify({ kind: "apns", ...body }),
        });
        if (response.ok) return;
        if (response.status < 500 || attempt > 0) throw new Error(`Push registration HTTP ${response.status}`);
      } catch (error) {
        if (attempt > 0 || !(error.name === "AbortError" || error instanceof TypeError)) throw error;
      } finally { clearTimeout(timeout); }
    }
  }

  async function sync() {
    for (const oldToken of [...pending]) {
      await post({ token: oldToken, remove: true });
      if (oldToken === token) registeredAs = undefined;
      pending.delete(oldToken);
      persistPending();
      if (!enabled && read(tokenKey) === oldToken) write(tokenKey, "");
    }
    if (!enabled || !token || read(disabledKey) === "true") return { ok: true, enabled: false };
    const currentRevision = revision;
    const currentToken = token;
    const sub = win.acUSER?.sub || null;
    const identity = JSON.stringify([currentToken, sub]);
    if (registeredAs === identity) return { ok: true, enabled: true };
    const headers = {};
    if (sub) {
      const bearer = await win.auth0Client?.getTokenSilently();
      if (!bearer) throw new Error("Sign-in is still settling");
      headers.Authorization = `Bearer ${bearer}`;
    }
    if (revision !== currentRevision || read(disabledKey) === "true") return { ok: false, reason: "changed" };
    let deviceId = read("ac-push-device-id");
    if (!deviceId) { deviceId = win.crypto.randomUUID(); write("ac-push-device-id", deviceId); }
    await post({
      token: currentToken, platform: "ios", deviceId,
      label: /iPad/.test(win.navigator.userAgent) ? "iPad app" : "iPhone app",
      topics: ["scream", "mood"],
    }, headers);
    if (revision !== currentRevision || read(disabledKey) === "true") {
      // A disable/rotation happened while the server was accepting the token.
      rememberRemoval(currentToken);
      return { ok: false, reason: "changed" };
    }
    registeredAs = identity;
    return { ok: true, enabled: true };
  }

  function schedule() {
    const run = () => win.navigator.locks?.request
      ? win.navigator.locks.request("ac-ios-push", sync) : sync();
    queue = queue.then(run, run).catch(() => ({ ok: false, reason: "network" }));
    return queue;
  }

  win.iOSReceivePushToken = (value) => {
    if (!validToken(value)) return Promise.resolve({ ok: false, reason: "token" });
    const previous = read(tokenKey);
    if (previous !== value) rememberRemoval(previous);
    registeredAs = undefined;
    token = value;
    enabled = true;
    revision++;
    write(tokenKey, value);
    write(disabledKey, "false");
    return schedule();
  };

  win.iOSUnregisterPushToken = (nativeToken) => {
    enabled = false;
    revision++;
    write(disabledKey, "true");
    rememberRemoval(token);
    rememberRemoval(nativeToken);
    rememberRemoval(read(tokenKey));
    token = null;
    registeredAs = undefined;
    return schedule();
  };
  // boot.mjs calls this after sign-in. A signed-in person who hasn't turned
  // notifications off here gets native's one-time offer (native decides).
  win.iOSTryRegisterPushToken = () => {
    if (win.acUSER?.sub && read(disabledKey) !== "true") {
      win.webkit?.messageHandlers?.iOSApp?.postMessage(JSON.stringify({ type: "notifications:offer" }));
    }
    return schedule();
  };
  win.iOSPushBridgeVersion = 2;
  win.addEventListener("online", schedule);
  // Retry removals after a reload, including when the previous page was offline.
  if (pending.size) schedule();
}
