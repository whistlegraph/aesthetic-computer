// KidLisp console, kidlisp-console.mjs
// The KidLisp-only channel to kidlisp.com's console panel. Lives apart from
// kidlisp.mjs so the main thread can listen for "kidlisp-console-enable" and
// post frame snapshots without loading the evaluator; kidlisp.mjs re-exports it.

import { formatTimestamp } from "./frame-capture.mjs";

// --- KidLisp-only console channel (for kidlisp.com) ---
// This avoids forwarding arbitrary JS console errors into kidlisp.com's console panel.
let __kidlispConsoleEnabled = false;
let __kidlispConsoleInitLogged = false;

function postToParent(content) {
  // NOTE: KidLisp code often runs in the disk worker. Support both window and worker contexts.
  const hasWindow = typeof window !== "undefined";
  const hasWorker = !hasWindow && typeof self !== "undefined" && typeof self.postMessage === "function";

  if (hasWindow) {
    try {
      // In the aesthetic.computer iframe, a BIOS relay listens for
      // { type: 'post-to-parent', content } and forwards it to the embedding page.
      // The BIOS (boot.mjs) and bios.mjs both handle this relay, so we only need
      // to post the wrapped message - no direct window.parent fallback needed.
      // (Having both caused duplicate messages to kidlisp.com)
      window.postMessage({ type: "post-to-parent", content }, "*");
    } catch {
      // ignore
    }

    return;
  }

  if (hasWorker) {
    try {
      // In the disk worker, post to the main thread; the BIOS/disk bridge already understands
      // the { type: 'post-to-parent', content } envelope.
      self.postMessage({ type: "post-to-parent", content });
    } catch {
      // ignore
    }
  }
}

// 📸 Post image snapshot to kidlisp.com console
function postKidlispConsoleImage(imageDataUrl, meta = {}) {
  if (!isKidlispConsoleEnabled()) return;
  const payload = { 
    type: "kidlisp-console-image",
    imageDataUrl,
    frameCount: meta.frameCount,
    timestamp: meta.timestamp || formatTimestamp(),
    dimensions: meta.dimensions,
    pieceCode: meta.pieceCode,       // The $code identifier without $ (e.g., "nece")
    pieceLabel: meta.pieceLabel,     // Full label with $ (e.g., "$nece")
    filename: meta.filename,         // Full filename (e.g., "$nece-@jeffrey-2025-12-17.png")
    userHandle: meta.userHandle,     // User handle (e.g., "@jeffrey")
    embeddedSource: meta.embeddedSource
  };
  postToParent(payload);
}

function enableKidlispConsole() {
  if (__kidlispConsoleEnabled) return;
  __kidlispConsoleEnabled = true;
  if (typeof window !== "undefined") {
    window.__acKidlispConsoleEnabled = true;
  }
  postToParent({ type: "kidlisp-console-enabled" });
  // Avoid spamming status lines into kidlisp.com's Console tab.
  __kidlispConsoleInitLogged = true;
}

function isKidlispConsoleEnabled() {
  if (__kidlispConsoleEnabled) return true;
  try {
    if (typeof window !== "undefined") return window.__acKidlispConsoleEnabled === true;
    if (typeof globalThis !== "undefined") return globalThis.__acKidlispConsoleEnabled === true;
    if (typeof self !== "undefined") return self.__acKidlispConsoleEnabled === true;
    return false;
  } catch {
    return false;
  }
}

if (typeof window !== "undefined" && !window.__acKidlispConsoleListenerInstalled) {
  window.__acKidlispConsoleListenerInstalled = true;
  window.addEventListener("message", (event) => {
    if (event?.data?.type === "kidlisp-console-enable") {
      enableKidlispConsole();
    }
  });
}

if (typeof window === "undefined" && typeof self !== "undefined" && !self.__acKidlispConsoleListenerInstalled) {
  self.__acKidlispConsoleListenerInstalled = true;
  self.addEventListener("message", (event) => {
    if (event?.data?.type === "kidlisp-console-enable") {
      enableKidlispConsole();
    }
  });
}

export {
  postToParent,
  postKidlispConsoleImage,
  enableKidlispConsole,
  isKidlispConsoleEnabled,
};
