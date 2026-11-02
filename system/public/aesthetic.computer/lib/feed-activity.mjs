// Shared authenticated feed protocol. No tokens or anonymous visit IDs cross frames.
export const FEED_ACTIONS = Object.freeze(["mime_opened", "mime_closed", "mime_piece_viewed"]);
export const FEED_ENTRIES = Object.freeze(["prompt_swipe", "prompt_wheel", "prompt_key", "prompt_history", "direct", "already_open"]);
export const FEED_EXITS = Object.freeze(["prompt", "pagehide", "navigation"]);
export const FEED_CONTENT = /^(?:piece|kidlisp)_[a-f0-9]{24}$/;

export function feedActivityFields(action, body, property) {
  if (!["aesthetic.computer", "prompt.ac", "mime.ac"].includes(property) || body.piece !== "mime" ||
      body.activityVersion !== 1 || !FEED_ENTRIES.includes(body.entry)) return null;
  const fields = { activityVersion: 1, entry: body.entry };
  if (action === "mime_piece_viewed") {
    if (!FEED_CONTENT.test(body.contentId || "") || !Number.isSafeInteger(body.focusedMs) ||
        body.focusedMs < 1 || body.focusedMs > 30000) return null;
    Object.assign(fields, { contentId: body.contentId, focusedMs: body.focusedMs });
  }
  if (action === "mime_closed") {
    if (!FEED_EXITS.includes(body.exitReason)) return null;
    fields.exitReason = body.exitReason;
  }
  return fields;
}

// Receives foreground samples, at most once a second while stationary. Delayed
// samples contribute at most two seconds. A different account never inherits time.
export function createFeedActivity({ owner, allowed, emit, now = () => performance.now() }) {
  let open = false, entry = "direct", subject = null, recorded = false;
  let card = null, active = false, at = now(), ms = 0, flushed = at;
  const send = (action, fields = {}) => emit(action, { activityVersion: 1, entry, ...fields });
  function sync() {
    const next = allowed() ? owner() : null;
    if (next !== subject) {
      subject = next; card = null; active = false; ms = 0; at = flushed = now(); recorded = false;
      if (open && subject) { entry = "already_open"; send("mime_opened"); recorded = true; }
    }
  }
  function flush() {
    if (ms && card && subject) send("mime_piece_viewed", { contentId: card, focusedMs: ms });
    ms = 0; flushed = now();
  }
  function accrue() {
    const t = now();
    if (active && subject && allowed()) ms += Math.max(0, Math.min(2000, Math.round(t - at)));
    at = t;
  }
  return {
    open(source) {
      if (open || !FEED_ENTRIES.includes(source)) return;
      sync(); open = true; entry = source; at = flushed = now();
      if (subject) { send("mime_opened"); recorded = true; }
    },
    sample(contentId, foreground) {
      if (!open) return;
      sync(); accrue();
      const next = FEED_CONTENT.test(contentId || "") ? contentId : null;
      if (next !== card || !foreground || now() - flushed >= 15000) flush();
      card = next; active = !!(foreground && next && subject && allowed());
    },
    close(reason = "navigation") {
      if (!open) return;
      sync(); accrue(); flush();
      if (subject && recorded) send("mime_closed", { exitReason: reason });
      open = false; card = null; active = false;
    },
  };
}
