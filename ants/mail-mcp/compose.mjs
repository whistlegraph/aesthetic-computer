// compose — turn a plain-text email into the exact bytes that get sent.
//
// Every outgoing message is multipart/alternative: the plain text as written,
// and an HTML rendering of the same text with the `@handle` signature in the
// handle's per-character colours, linked to the AC profile. Nothing here
// touches the network or the disk — the server hands in colours and
// attachment bytes, and gets back a message plus the HTML used to preview it.

import { createHash, randomBytes } from "node:crypto";

// AC's character palette (nom.mjs) — the fallback when a handle has no saved
// colours or the colour API is unreachable.
const PALETTE = [
  [255, 105, 97], [255, 170, 80], [255, 225, 90], [150, 225, 90], [90, 220, 140],
  [90, 215, 210], [95, 180, 255], [150, 150, 255], [200, 140, 255], [255, 140, 210],
];

function fallbackColor(character) {
  const ch = character.toLowerCase();
  if (ch >= "a" && ch <= "z") return PALETTE[(ch.charCodeAt(0) - 97) % 10];
  if (ch >= "0" && ch <= "9") return PALETTE[Number(ch)];
  return [220, 180, 255];
}

// One [r, g, b] per character of `@handle`. `saved` is the API's colours array
// (which covers the leading @), or anything falsy.
export function handleColors(handle, saved) {
  return Array.from(handle).map((character, i) => {
    const c = saved?.[i];
    if (c && [c.r, c.g, c.b].every((n) => Number.isFinite(n) && n >= 0 && n <= 255)) {
      return [c.r, c.g, c.b].map(Math.round);
    }
    return fallbackColor(character);
  });
}

export const escapeHtml = (text) =>
  String(text)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");

// The inline markup a body may carry, in the order it is matched:
//   [label](https://…) or [label](mailto:…)   a link that reads as its label
//   `code`   **bold**   *italic*   and a bare URL, which links itself.
// Emphasis only opens at a word edge, so "2 * 3 * 4" and "a*b*c" stay as typed.
const LINK = /\[([^\]\n]+)\]\(((?:https?:\/\/|mailto:)[^\s<>)]+)\)/;
const INLINE = new RegExp(
  "(" +
    [
      "\\[[^\\]\\n]+\\]\\((?:https?:\\/\\/|mailto:)[^\\s<>)]+\\)",
      "`[^`\\n]+`",
      "(?<![\\w*])\\*\\*[^*\\s](?:[^*\\n]*[^*\\s])?\\*\\*(?![\\w*])",
      "(?<![\\w*])\\*[^*\\s](?:[^*\\n]*[^*\\s])?\\*(?![\\w*])",
      "https?:\\/\\/[^\\s<>]+",
    ].join("|") +
    ")",
  "g",
);

// Escape a line and render its inline markup. Trailing sentence punctuation
// stays outside a bare link so "see https://x.y/z." doesn't link the full stop.
// A link's label takes emphasis but never another link.
function inlineHtml(line, { links = true } = {}) {
  return line
    .split(INLINE)
    .map((part, i) => {
      if (i % 2 === 0) return escapeHtml(part);
      const link = part.match(LINK);
      if (link && part.startsWith("[")) {
        if (!links) return escapeHtml(part);
        return `<a href="${escapeHtml(link[2])}">${inlineHtml(link[1], { links: false })}</a>`;
      }
      if (part.startsWith("`")) {
        return `<code style="font-family:Menlo,Consolas,'DejaVu Sans Mono',monospace;font-size:0.92em;">${escapeHtml(part.slice(1, -1))}</code>`;
      }
      if (part.startsWith("**")) return `<strong>${inlineHtml(part.slice(2, -2), { links })}</strong>`;
      if (part.startsWith("*")) return `<em>${inlineHtml(part.slice(1, -1), { links })}</em>`;
      if (!links) return escapeHtml(part);
      const trail = part.match(/[.,;:!?)\]]+$/)?.[0] ?? "";
      const url = trail ? part.slice(0, -trail.length) : part;
      return `<a href="${escapeHtml(url)}">${escapeHtml(url)}</a>${escapeHtml(trail)}`;
    })
    .join("");
}

// The same body for the plain-text part, where markup would read as noise:
// a link becomes "label (url)", emphasis loses its asterisks, code keeps its
// backticks. A reader in a text-only client still gets every address.
export function markupToText(text) {
  return text
    .split("\n")
    .map((line) =>
      line
        .split(INLINE)
        .map((part, i) => {
          if (i % 2 === 0) return part;
          const link = part.match(LINK);
          if (link && part.startsWith("[")) {
            const label = markupToText(link[1]);
            const url = link[2].replace(/^mailto:/, "");
            return label === url || label === link[2] ? url : `${label} (${url})`;
          }
          if (part.startsWith("**")) return markupToText(part.slice(2, -2));
          if (part.startsWith("*")) return markupToText(part.slice(1, -1));
          return part;
        })
        .join(""),
    )
    .join("\n");
}

const P = 'style="margin:0 0 1em 0;"';

// Plain text → the HTML body. Blank lines separate blocks; a block whose
// lines all start with "- " is a list, with "> " a quote, anything else a
// paragraph with its line breaks kept.
export function textToHtml(text) {
  return text
    .trim()
    .split(/\n\s*\n/)
    .map((block) => {
      const lines = block.split("\n").map((l) => l.trimEnd());
      if (lines.every((l) => /^[-*] /.test(l))) {
        const items = lines.map((l) => `<li>${inlineHtml(l.slice(2))}</li>`).join("");
        return `<ul style="margin:0 0 1em 0;padding-left:1.4em;">${items}</ul>`;
      }
      if (lines.every((l) => l.startsWith(">"))) {
        const inner = lines.map((l) => inlineHtml(l.replace(/^> ?/, ""))).join("<br>");
        return `<blockquote style="margin:0 0 1em 0;padding-left:0.9em;border-left:3px solid #c8c8d0;color:#666;">${inner}</blockquote>`;
      }
      return `<p ${P}>${lines.map(inlineHtml).join("<br>")}</p>`;
    })
    .join("\n");
}

const toLinear = (v) => (v <= 10.31 ? v / 3294.6 : ((v / 255 + 0.055) / 1.055) ** 2.4);
const toSrgb = (l) =>
  Math.round(255 * (l <= 0.0031308 ? l * 12.92 : 1.055 * l ** (1 / 2.4) - 0.055));

// The handle colours are picked against AC's dark ground, and several are too
// pale to read on a white email. This keeps a colour's hue and darkens it to
// a luminance that reads on white (about 4:1) and still reads on a dark
// client's ground. A colour already that dark is returned as it is.
const INK_LUMINANCE = 0.2;
export function inkColor([r, g, b]) {
  const linear = [r, g, b].map(toLinear);
  const luminance = 0.2126 * linear[0] + 0.7152 * linear[1] + 0.0722 * linear[2];
  if (luminance <= INK_LUMINANCE) return [r, g, b];
  return linear.map((l) => toSrgb((l * INK_LUMINANCE) / luminance));
}

// The sign-off. An `@handle` is set straight on the email's own ground, one
// coloured span per character, each colour darkened by inkColor. `darkCss` is
// the rules that give a dark-mode client the handle's own colours back; it
// belongs in the message's <style>. Anything that isn't a handle stays plain.
export function signatureHtml(signature, { colors, link } = {}) {
  if (!signature) return { html: "", darkCss: "" };
  if (!signature.startsWith("@")) {
    return { html: `<p style="margin:1.4em 0 0 0;">${escapeHtml(signature)}</p>`, darkCss: "" };
  }
  const own = colors ?? handleColors(signature);
  const rgb = (c) => `rgb(${c.join(",")})`;
  const spans = Array.from(signature)
    .map((ch, i) => `<span class="h${i}" style="color:${rgb(inkColor(own[i]))};">${escapeHtml(ch)}</span>`)
    .join("");
  const face =
    "font-family:Menlo,Consolas,'DejaVu Sans Mono',monospace;font-size:14px;font-weight:bold;" +
    "letter-spacing:0.4px;text-decoration:none;";
  const inner = link
    ? `<a href="${escapeHtml(link)}" style="${face}">${spans}</a>`
    : `<span style="${face}">${spans}</span>`;
  return {
    html: `<p style="margin:1.4em 0 0 0;">${inner}</p>`,
    darkCss: own.map((c, i) => `.h${i}{color:${rgb(c)} !important;}`).join(""),
  };
}

const BODY_FONT =
  "font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Helvetica,Arial,sans-serif;" +
  "font-size:15px;line-height:1.5;";

// The HTML part as sent. No text or background colour is set, so a client in
// dark mode keeps its own. `sig` is what signatureHtml returned; a client
// that ignores the dark-mode rules shows the inline colours, which read on
// either ground.
export function wrapHtml(bodyHtml, sig) {
  const head = sig.darkCss
    ? `<head><meta name="color-scheme" content="light dark">` +
      `<style>@media (prefers-color-scheme: dark){${sig.darkCss}}</style></head>`
    : "";
  return (
    `<!doctype html><html>${head}<body style="margin:0;padding:0;">` +
    `<div style="${BODY_FONT}max-width:640px;">\n${bodyHtml}\n${sig.html}\n</div>` +
    `</body></html>`
  );
}

// A page that shows the draft the way a mail client would: header band, then
// the HTML part exactly as it will be sent. Only ever rendered to an image.
export function previewPage({ from, to, cc, bcc, subject, attachments = [] }, html) {
  const row = (label, value) =>
    value
      ? `<tr><td style="color:#888;padding:1px 12px 1px 0;vertical-align:top;">${label}</td><td>${escapeHtml(value)}</td></tr>`
      : "";
  const inner = html.replace(/^.*?<body[^>]*>/s, "").replace(/<\/body>.*$/s, "");
  return (
    `<!doctype html><html><head><meta charset="utf-8"></head>` +
    `<body style="margin:0;background:#fff;color:#111;">` +
    `<div style="background:#f1f1f4;border-bottom:1px solid #d8d8de;padding:14px 22px;${BODY_FONT}font-size:13px;">` +
    `<div style="font-size:11px;letter-spacing:1px;color:#b0306a;font-weight:bold;margin-bottom:6px;">DRAFT — NOT SENT</div>` +
    `<div style="font-size:17px;font-weight:600;margin-bottom:8px;">${escapeHtml(subject)}</div>` +
    `<table style="border-collapse:collapse;font-size:13px;">` +
    row("from", from) + row("to", to) + row("cc", cc) + row("bcc", bcc) +
    row("files", attachments.map((a) => a.name).join(", ")) +
    `</table></div>` +
    `<div style="padding:22px;">${inner}</div>` +
    `</body></html>`
  );
}

// A guess at how tall the preview needs to be, in CSS pixels. Headless Chrome
// screenshots a fixed window; too tall only costs white space.
export function previewHeight({ cc, bcc, attachments = [] }, text) {
  const lines = text.split("\n").filter((l) => l.trim()).reduce((n, l) => n + Math.ceil(l.length / 78), 0);
  const blocks = text.split(/\n\s*\n/).length;
  const headerRows = 2 + (cc ? 1 : 0) + (bcc ? 1 : 0) + (attachments.length ? 1 : 0);
  return Math.round(120 + headerRows * 22 + lines * 23 + blocks * 16 + 70);
}

const b64Lines = (buf) => Buffer.from(buf).toString("base64").replace(/(.{76})/g, "$1\r\n");

// RFC 2047 for a header value that isn't plain ASCII.
function encodeHeader(value) {
  if (/^[\x20-\x7e]*$/.test(value)) return value;
  const words = [];
  let chunk = "";
  for (const ch of value) {
    if (Buffer.byteLength(chunk + ch) > 42) {
      words.push(chunk);
      chunk = "";
    }
    chunk += ch;
  }
  if (chunk) words.push(chunk);
  return words.map((w) => `=?UTF-8?B?${Buffer.from(w).toString("base64")}?=`).join("\r\n ");
}

const boundary = (tag) => `=_${tag}_${randomBytes(9).toString("hex")}`;
const angle = (id) => (id.startsWith("<") ? id : `<${id}>`);

const MIME_BY_EXT = {
  pdf: "application/pdf", png: "image/png", jpg: "image/jpeg", jpeg: "image/jpeg",
  gif: "image/gif", html: "text/html", txt: "text/plain", csv: "text/csv",
  ai: "application/illustrator", svg: "image/svg+xml", zip: "application/zip",
  mp3: "audio/mpeg", wav: "audio/wav", mp4: "video/mp4",
  xlsx: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  docx: "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
};
export const mimeFor = (name) =>
  MIME_BY_EXT[name.split(".").pop()?.toLowerCase()] ?? "application/octet-stream";

// The full RFC 5322 message. `attachments` is [{ name, data: Buffer }].
export function buildMessage({
  from, to, cc, bcc, subject, text, html, messageId, date = new Date(),
  inReplyTo, references, attachments = [],
}) {
  const headers = [
    `Date: ${date.toUTCString().replace("GMT", "+0000")}`,
    `From: ${from}`,
    `To: ${to}`,
  ];
  if (cc) headers.push(`Cc: ${cc}`);
  if (bcc) headers.push(`Bcc: ${bcc}`);
  headers.push(`Subject: ${encodeHeader(subject)}`, `Message-ID: ${angle(messageId)}`);
  if (inReplyTo) headers.push(`In-Reply-To: ${angle(inReplyTo)}`);
  if (references) {
    headers.push(`References: ${references.split(/\s+/).filter(Boolean).map(angle).join(" ")}`);
  }
  headers.push("MIME-Version: 1.0");

  const alt = boundary("alt");
  const altPart = [
    `Content-Type: multipart/alternative; boundary="${alt}"`,
    "",
    `--${alt}`,
    "Content-Type: text/plain; charset=UTF-8",
    "Content-Transfer-Encoding: base64",
    "",
    b64Lines(text),
    `--${alt}`,
    "Content-Type: text/html; charset=UTF-8",
    "Content-Transfer-Encoding: base64",
    "",
    b64Lines(html),
    `--${alt}--`,
  ];

  if (attachments.length === 0) return [...headers, ...altPart, ""].join("\r\n");

  const mixed = boundary("mixed");
  const parts = [
    ...headers,
    `Content-Type: multipart/mixed; boundary="${mixed}"`,
    "",
    `--${mixed}`,
    ...altPart,
  ];
  for (const { name, data } of attachments) {
    parts.push(
      `--${mixed}`,
      `Content-Type: ${mimeFor(name)}; name="${name}"`,
      "Content-Transfer-Encoding: base64",
      `Content-Disposition: attachment; filename="${name}"`,
      "",
      b64Lines(data),
    );
  }
  parts.push(`--${mixed}--`, "");
  return parts.join("\r\n");
}

export const sha256 = (data) => createHash("sha256").update(data).digest("hex");

export const newDraftId = () => `d${Date.now().toString(36)}${randomBytes(2).toString("hex")}`;

const addresses = (list) => (list ?? []).map((a) => a[":email"]).filter(Boolean);

// Who a reply goes to, from mu's JSON record of the message being answered.
// `mine` is every address this seat sends from: none of them is ever a
// recipient, and a reply to one's own sent message goes back to its recipients.
export function planReply(original, { mine, replyAll = true }) {
  // me+tag@… is still me.
  const plain = (a) => a.toLowerCase().replace(/\+[^@]*@/, "@");
  const mineSet = new Set(mine.map(plain));
  const isMine = (a) => mineSet.has(plain(a));
  const from = addresses(original[":from"]);
  const replyTo = addresses(original[":reply-to"]);
  const origTo = addresses(original[":to"]);
  const origCc = addresses(original[":cc"]);

  let to = (replyTo.length ? replyTo : from).filter((a) => !isMine(a));
  if (to.length === 0) to = origTo.filter((a) => !isMine(a));
  // A message between two of this seat's own addresses: answer its sender.
  if (to.length === 0) to = replyTo.length ? replyTo : from;
  const seen = new Set(to.map((a) => a.toLowerCase()));
  const cc = [];
  if (replyAll) {
    for (const a of [...origTo, ...origCc]) {
      if (isMine(a) || seen.has(a.toLowerCase())) continue;
      seen.add(a.toLowerCase());
      cc.push(a);
    }
  }
  const subject = original[":subject"] ?? "";
  const id = original[":message-id"];
  return {
    to: to.join(", "),
    cc: cc.join(", "),
    subject: /^re:/i.test(subject) ? subject : `Re: ${subject}`,
    inReplyTo: id,
    references: [...(original[":references"] ?? []), id].join(" "),
    // The address the original was sent to decides which account answers.
    answeredAs: [...origTo, ...origCc].filter(isMine).map(plain)[0],
  };
}
