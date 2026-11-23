const $ = id => document.getElementById(id);
const registerUrl = "https://aesthetic.computer/api/register-push-token";
const api = "/api/whistlegraph-chat?action=";
const themeKey = "whistlegraph-theme";
const dark = matchMedia("(prefers-color-scheme: dark)");

function applyTheme(choice) {
  const root = document.documentElement;
  if (choice === "light" || choice === "dark") root.dataset.theme = choice;
  else delete root.dataset.theme;
  $("appearance").value = root.dataset.theme || "auto";
  document.querySelector('meta[name="theme-color"]').content =
    (root.dataset.theme === "dark" || (!root.dataset.theme && dark.matches)) ? "#272332" : "#f5e8ae";
}
applyTheme(document.documentElement.dataset.theme);
$("appearance").onchange = () => {
  const choice = $("appearance").value;
  applyTheme(choice);
  try { localStorage.setItem(themeKey, choice); } catch {}
};
dark.addEventListener("change", () => applyTheme($("appearance").value));
window.addEventListener("storage", event => { if (event.key === themeKey) applyTheme(event.newValue); });

function toggle(button, active) {
  button.textContent = active ? "On" : "Off";
  button.setAttribute("aria-pressed", String(active));
}
const supported = "serviceWorker" in navigator && "PushManager" in window && "Notification" in window;
let registration, subscription, config;
async function webRegister(body) {
  const res = await fetch(registerUrl, {
    method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body),
  });
  if (!res.ok) throw new Error("Could not save web notifications. Try again.");
}

export function setupSettings({ signedIn, login, authorized }) {
  const web = $("web-alerts"), app = $("app-alerts"), status = $("settings-status");
  const webReady = async () => {
    registration = await navigator.serviceWorker.register("/room-sw.js");
    registration = await navigator.serviceWorker.ready;
    subscription = await registration.pushManager.getSubscription();
    toggle(web, !!subscription);
  };
  $("open-settings").onclick = async () => {
    $("settings").showModal();
    status.textContent = "";
    web.disabled = app.disabled = true;
    try {
      const res = await fetch(api + "push-config");
      if (!res.ok) throw new Error("Notifications are temporarily unavailable.");
      config = await res.json();
      if (signedIn()) toggle(app, (await authorized(api + "app-alerts")).enabled);
      else toggle(app, false);
      app.disabled = !config.app;
      if (supported && config.webKey) {
        await webReady();
        web.disabled = false;
      } else if (/iPhone|iPad|iPod/.test(navigator.userAgent) || (navigator.platform === "MacIntel" && navigator.maxTouchPoints > 1)) {
        status.textContent = "For web notifications, add Whistlegraph to your Home Screen and open it there.";
      } else status.textContent = "Web notifications are unavailable in this browser.";
    } catch (err) { status.textContent = err.message; }
  };
  web.onclick = async () => {
    web.disabled = true;
    status.textContent = "";
    try {
      if (subscription) {
        await webRegister({ remove: true, token: subscription.endpoint });
        try { await subscription.unsubscribe(); }
        finally { subscription = null; toggle(web, false); }
      } else {
        if (await Notification.requestPermission() !== "granted") throw new Error("Allow notifications in your browser settings to turn them on.");
        const key = Uint8Array.from(atob(config.webKey.replace(/-/g, "+").replace(/_/g, "/")), c => c.charCodeAt(0));
        const sub = await registration.pushManager.subscribe({ userVisibleOnly: true, applicationServerKey: key });
        try {
          let deviceId = localStorage.getItem("whistlegraph-push-device");
          if (!deviceId) { deviceId = crypto.randomUUID(); localStorage.setItem("whistlegraph-push-device", deviceId); }
          await webRegister({ kind: "webpush", platform: "web", deviceId, label: "Whistlegraph chat", subscription: sub.toJSON(), topics: ["chat-whistlegraph"] });
        } catch (err) { await sub.unsubscribe(); throw err; }
        subscription = sub;
      }
      toggle(web, !!subscription);
    } catch (err) { status.textContent = err.message; }
    finally { web.disabled = false; }
  };
  app.onclick = async () => {
    app.disabled = true;
    status.textContent = "";
    try {
      if (!signedIn()) {
        await login();
        const existing = await authorized(api + "app-alerts");
        if (existing.enabled) { toggle(app, true); return; }
      }
      const enabled = app.getAttribute("aria-pressed") !== "true";
      const result = await authorized(api + "app-alerts", { enabled });
      toggle(app, result.enabled);
      if (result.enabled && !result.hasDevice) status.textContent = "Sign in to the same account in the AC app and enter ‘notifs’ to receive alerts.";
    } catch (err) { status.textContent = err.message; }
    finally { app.disabled = false; }
  };
}
