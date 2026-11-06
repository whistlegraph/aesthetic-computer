// Host reachability only: no navigation, piece reset, or analytics.
export function watchConnection(win, send) {
  if (win.acPACK_MODE) return { retry() {}, setModal() {} };
  let attempt = 0, controller;
  let modalVisible = false;
  let announcement, lastStatus;
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
        ? "Connection lost. Press Enter to retry or Escape to keep playing."
        : status === "checking" ? "Checking connection." : "Connected.";
    }
    lastStatus = status;
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
  // Capture before the prompt's hidden text input can consume Enter or Tab.
  win.addEventListener("keydown", event => {
    if (!modalVisible || event.metaKey || event.ctrlKey) return;
    event.preventDefault();
    event.stopImmediatePropagation();
    const key = event.key === " " ? "space" : event.key.toLowerCase();
    send({ type: "connection:key", content: { key } });
  }, true);
  win.addEventListener("ac:native-network", () => win.acNativeOnline === false ? offline() : retry());
  if (win.acNativeOnline === false || win.navigator.onLine === false) offline();
  return { retry, setModal(visible) { modalVisible = visible; } };
}
