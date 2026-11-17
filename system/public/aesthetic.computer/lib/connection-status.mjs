// Host reachability only: no navigation, piece reset, or analytics.
// A lost host is asked again by itself, sooner at first and then every
// fifteen seconds, until it answers.
const RETRY_DELAYS = [2000, 4000, 8000, 15000];
export function watchConnection(win, send) {
  if (win.acPACK_MODE) return { retry() {}, setModal() {} };
  let attempt = 0, controller;
  let announcement, lastStatus;
  let failures = 0, retryTimer;
  const scheduleRetry = () => {
    win.clearTimeout(retryTimer);
    retryTimer = win.setTimeout(retry, RETRY_DELAYS[Math.min(failures, RETRY_DELAYS.length - 1)]);
    failures++;
  };
  const report = status => {
    send({ type: "connection:status", content: { status } });
    if (status === lastStatus) return;
    if (win.document?.body && (status === "offline" || announcement)) {
      if (!announcement) {
        announcement = win.document.createElement("div");
        announcement.setAttribute("role", "status");
        announcement.style.cssText = "position:fixed;width:1px;height:1px;overflow:hidden;clip-path:inset(50%);pointer-events:none";
        win.document.body.append(announcement);
      }
      announcement.textContent = status === "offline"
        ? "Connection lost. Retrying."
        : status === "checking" ? "Checking connection." : "Connected.";
    }
    lastStatus = status;
    if (status === "offline") scheduleRetry();
    else if (status === "online") { failures = 0; win.clearTimeout(retryTimer); }
  };
  const offline = () => {
    attempt++;
    controller?.abort();
    report("offline");
  };
  async function retry() {
    controller?.abort();
    const current = ++attempt;
    controller = new AbortController();
    const pending = controller;
    const timeout = win.setTimeout(() => pending.abort(), 5000);
    report("checking");
    try {
      const response = await win.fetch("/aesthetic.computer/boot.mjs", {
        method: "HEAD", cache: "no-store", signal: controller.signal,
      });
      if (current === attempt) report(response.ok ? "online" : "offline");
    } catch {
      if (current === attempt) report("offline");
    } finally { win.clearTimeout(timeout); }
  }
  win.addEventListener("offline", offline);
  win.addEventListener("online", retry);
  win.addEventListener("ac:native-network", () => win.acNativeOnline === false ? offline() : retry());
  if (win.acNativeOnline === false || win.navigator.onLine === false) offline();
  return { retry, setModal() {} }; // setModal: kept for a bios that still calls it.
}
