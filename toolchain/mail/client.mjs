import { loadTokens, UA } from "../mcp/ac-token.mjs";

export class MailError extends Error {}

// Fixed first-party origin: never send the shared bearer token to a CLI URL.
export function createClient({ tokens = loadTokens, request = fetch } = {}) {
  let identity;
  async function call(method, body) {
    let session;
    try { session = await tokens(); } catch {
      throw new MailError("Sign in with ac-login, then reopen ac-mail.");
    }
    if (!session.access_token || !session.user?.sub) {
      throw new MailError("Sign in with ac-login, then reopen ac-mail.");
    }
    if (identity && identity !== session.user.sub) {
      throw new MailError("AC account changed. Reopen ac-mail before continuing.");
    }
    identity = session.user.sub;
    let res;
    try {
      res = await request("https://aesthetic.computer/api/mail", {
        method, redirect: "error", signal: AbortSignal.timeout(15000),
        headers: { Authorization: `Bearer ${session.access_token}`, "User-Agent": UA, "Content-Type": "application/json" },
        ...(body ? { body: JSON.stringify(body) } : {}),
      });
    } catch {
      throw new MailError(body?.text
        ? "Delivery unconfirmed. Refresh Sent before resending."
        : "Could not reach AC mail. Press r to retry.");
    }
    if (res.status === 401 || res.status === 403) throw new MailError("Access refused. Check ac-login status or sign in again.");
    if (res.status === 404) throw new MailError("Recipient not found.");
    if (!res.ok) throw new MailError(body?.text
      ? "Delivery unconfirmed. Refresh Sent before resending."
      : `Mail request failed (HTTP ${res.status}).`);
    try { return await res.json(); } catch {
      throw new MailError(body?.text ? "Delivery unconfirmed. Refresh Sent before resending." : "Invalid mail response. Press r to retry.");
    }
  }
  return {
    inbox: () => call("GET"),
    read: (id) => call("POST", { action: "read", id }),
    send: (draft) => {
      const error = validateDraft(draft);
      if (error) throw new MailError(error);
      return call("POST", { to: draft.to.trim(), subject: draft.subject.trim(), text: draft.body.trim() });
    },
  };
}

export function validateDraft({ to, subject, body }) {
  if (!to.trim()) return "Add a recipient.";
  if (!body.trim()) return "Write a letter first.";
  // The current /api/mail applies these limits even to outgoing SMTP letters.
  if (body.trim().length > 500) return "Letters are limited to 500 characters.";
  if (subject.trim().length > 80) return "Subjects are limited to 80 characters.";
  if (/[\r\n\x00-\x1f\x7f]/.test(to + subject)) return "Recipient and subject must be single lines.";
  return null;
}

// All remote text is untrusted terminal input: discard controls and bidi marks.
export function safe(value) {
  return String(value ?? "").replace(/[\x00-\x08\x0b-\x1f\x7f-\x9f\u202a-\u202e\u2066-\u2069]/g, "").replace(/\t/g, "  ");
}
const segments = new Intl.Segmenter(undefined, { granularity: "grapheme" });
export const graphemes = (text) => Array.from(segments.segment(text), (s) => s.segment);
export function width(text) {
  return graphemes(text).reduce((sum, g) => {
    const n = g.codePointAt(0);
    const wide = /\p{Extended_Pictographic}/u.test(g) || (n >= 0x1100 && (
      n <= 0x115f || n === 0x2329 || n === 0x232a || (n >= 0x2e80 && n <= 0xa4cf) ||
      (n >= 0xac00 && n <= 0xd7a3) || (n >= 0xf900 && n <= 0xfaff) ||
      (n >= 0xfe10 && n <= 0xfe6f) || (n >= 0xff01 && n <= 0xff60) || n >= 0x20000));
    return sum + (wide ? 2 : 1);
  }, 0);
}
export function clip(value, columns) {
  let result = "", used = 0;
  for (const g of graphemes(safe(value).replace(/\n/g, " "))) {
    const n = width(g);
    if (used + n > columns) break;
    result += g; used += n;
  }
  return result;
}
export function wrap(value, columns) {
  const lines = [];
  for (const paragraph of safe(value).split("\n")) {
    let line = "";
    for (const g of graphemes(paragraph)) {
      if (width(line + g) > columns) { lines.push(line); line = ""; }
      line += g;
    }
    lines.push(line);
  }
  return lines;
}

export function replyTo(letter) {
  return letter.fromEmail || letter.from || "";
}
