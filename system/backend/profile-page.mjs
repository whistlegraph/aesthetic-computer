import { normalizeProfileHandle } from "./profile-card.mjs";

// A profile is a document. Direct links need neither the canvas runtime nor
// server-side database work before the browser can paint the handle.
export function profilePage(requestPath) {
  let decoded;
  try { decoded = decodeURIComponent(requestPath); } catch { return null; }
  const match = /^\/@([^/]+)\/?$/.exec(decoded);
  const handle = match && normalizeProfileHandle(match[1]);
  if (!handle) return null;
  return `<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
<meta name="color-scheme" content="light dark">
<title>@${handle} · Aesthetic Computer</title>
<link rel="icon" href="/favicon.ico">
<link rel="canonical" href="https://aesthetic.computer/@${handle}">
<style>body{margin:0;background:#f6f5ef;color:#232821;font-family:system-ui,sans-serif}main{max-width:440px;margin:auto;padding:24px 16px}nav{max-width:440px;margin:auto;padding:20px 16px 0}a{color:inherit}ac-profile{display:block}@media(prefers-color-scheme:dark){body{background:#101a16;color:#f3f3e9}}</style>
<script type="module" src="/aesthetic.computer/lib/profile-card.mjs"></script></head>
<body><nav><a href="/prompt">Aesthetic Computer</a></nav>
<main><ac-profile handle="@${handle}"><h1>@${handle}</h1><p>Loading profile…</p></ac-profile></main>
</body></html>`;
}
