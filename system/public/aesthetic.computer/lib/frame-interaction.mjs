// Observe input without consuming it. The host owns the edge strips; the
// runtime owns the center, including the very first touch and held keys.
export function observeInteraction(win = window, report = data => parent.postMessage(data, "*")) {
  const seen = new WeakSet();
  function watch(target) {
    try {
      const doc = target.document;
      if (!doc || seen.has(doc)) return;
      seen.add(doc);
      const held = new Set();
      const send = () => report({ type: "mime:interaction", phase: held.size ? "hold" : "release" });
      for (const type of ["pointerdown", "pointerup", "pointercancel", "keydown", "keyup"]) {
        target.addEventListener(type, event => {
          if (!event.isTrusted) return;
          const key = type.startsWith("key") ? "key:" + event.code : "pointer:" + event.pointerId;
          if (type === "pointerdown" || type === "keydown") held.add(key);
          else { if (!held.has(key)) return; held.delete(key); }
          send();
        }, { capture: true, passive: true });
      }
      target.addEventListener("wheel", () => report({ type: "mime:interaction", phase: held.size ? "hold" : "pulse" }), { passive: true });
      target.addEventListener("blur", () => { if (!held.size) return; held.clear(); send(); }, { passive: true });
      const frames = () => doc.querySelectorAll("iframe").forEach(frame => {
        if (!frame.dataset.mimeInputObserved) {
          frame.dataset.mimeInputObserved = "true";
          frame.addEventListener("load", () => watch(frame.contentWindow));
        }
        watch(frame.contentWindow);
      });
      frames();
      new target.MutationObserver(frames).observe(doc.documentElement, { childList: true, subtree: true });
    } catch { /* Cross-origin players report their own state changes. */ }
  }
  watch(win);
}
