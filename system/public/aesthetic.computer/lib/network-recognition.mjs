import { RECOGNITION_ORIGIN, RECOGNITION_PARENTS, RECOGNITION_TIMEOUT, recognitionEnabled, recognitionPage } from "./network-recognition-model.mjs";

// One bounded check per eligible top-level page. No cookies, persistent ID,
// account data, fishery visit IDs, navigation, prompts or UI status changes.
export function startNetworkRecognition(win = window, doc = document) {
  if (win.acNetworkRecognition) return win.acNetworkRecognition;
  if (win !== win.top || !RECOGNITION_PARENTS.includes(win.location.origin) ||
      !recognitionEnabled(win) || !win.crypto?.randomUUID) return null;
  let started = false, stopped = false, frame, timeout, poll;
  const abort = new AbortController(), id = win.crypto.randomUUID();
  const stop = () => {
    stopped = true; abort.abort(); frame?.remove();
    win.clearTimeout(timeout); win.clearInterval(poll);
    doc.removeEventListener("visibilitychange", begin);
    win.removeEventListener("pagehide", stop); win.removeEventListener("message", receive);
  };
  const receive = event => {
    if (event.source === frame?.contentWindow && event.origin === RECOGNITION_ORIGIN &&
        event.data?.type === "ac-recognition:done" && event.data.id === id) stop();
  };
  async function begin() {
    if (started || stopped || !recognitionPage(win, doc)) return;
    started = true;
    timeout = win.setTimeout(stop, RECOGNITION_TIMEOUT);
    poll = win.setInterval(() => { if (!recognitionPage(win, doc)) stop(); }, 250);
    try {
      const response = await win.fetch(`${RECOGNITION_ORIGIN}/api/network-recognition`, {
        method: "POST", credentials: "omit", referrerPolicy: "no-referrer", signal: abort.signal,
        headers: { "Content-Type": "text/plain" }, body: JSON.stringify({ version: 1, id, phase: "started" }),
      });
      if (!response.ok || stopped || !recognitionPage(win, doc)) { stop(); return; }
      frame = doc.createElement("iframe");
      frame.hidden = true; frame.tabIndex = -1; frame.title = "AC session measurement";
      frame.setAttribute("aria-hidden", "true"); frame.referrerPolicy = "no-referrer";
      frame.src = `${RECOGNITION_ORIGIN}/api/network-recognition-bridge`;
      frame.addEventListener("load", () => {
        if (stopped || !recognitionPage(win, doc)) { stop(); return; }
        frame.contentWindow.postMessage({ type: "ac-recognition:check", id }, RECOGNITION_ORIGIN);
      }, { once: true });
      doc.body.append(frame);
    } catch { stop(); }
  }
  const api = { stop };
  win.acNetworkRecognition = api;
  doc.addEventListener("visibilitychange", begin);
  win.addEventListener("pagehide", stop); win.addEventListener("message", receive);
  void begin();
  return api;
}
