// oskiewar-dump, 26.08.07
// Renders a crash dump that a console encoded into its own error-screen QR.
// The payload travels in the link, so a dump stays shareable even when the
// console never reached the reporting endpoint.

import { respond } from "../../backend/http.mjs";

const MAX_PAYLOAD = 8192;

const escape = (value) => String(value)
  .replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
  .replace(/"/g, "&quot;");

export function decodeDump(encoded) {
  const raw = String(encoded || "");
  if (!raw || raw.length > MAX_PAYLOAD) return null;
  if (!/^[A-Za-z0-9_-]+$/.test(raw)) return null;
  const padded = raw.replace(/-/g, "+").replace(/_/g, "/")
    .padEnd(Math.ceil(raw.length / 4) * 4, "=");
  let dump;
  try {
    dump = JSON.parse(Buffer.from(padded, "base64").toString("utf8"));
  } catch (_) {
    return null;
  }
  return dump && typeof dump === "object" && !Array.isArray(dump) &&
    dump.v === 1 ? dump : null;
}

export function dumpRows(dump) {
  const state = dump.s && typeof dump.s === "object" ? dump.s : {};
  const round = state.round || {};
  const camera = state.camera || {};
  const rows = [
    ["phase", dump.p],
    ["error", [dump.n, dump.m].filter(Boolean).join(": ")],
    ["source", dump.src
      ? `${dump.src.file}:${dump.src.line}:${dump.src.column}` : ""],
    ["stack", dump.k],
    ["build", state.build],
    ["mode", state.shell],
    ["round", [round.id, round.result, round.elapsedMs && `${round.elapsedMs}ms`]
      .filter(Boolean).join("  ")],
    ["camera", camera.width
      ? `${camera.x},${camera.y}  width ${camera.width}  aspect ${camera.aspect}`
      : ""],
  ];
  for (const player of Array.isArray(state.players) ? state.players : [])
    rows.push([player.handle || "player",
      `${player.stance} ${player.alive ? "alive" : "down"}  ` +
      `pos ${player.x},${player.y},${player.z}  vel ${player.vx},${player.vy}` +
      (player.removed?.length ? `  lost ${player.removed.join(", ")}` : "")]);
  const balls = (Array.isArray(state.balls) ? state.balls : [])
    .map((ball) => `${ball.type}@${ball.x},${ball.y} v${ball.vx},${ball.vy}`);
  if (balls.length) rows.push(["balls", balls.join("   ")]);
  return rows.filter(([, value]) => value !== undefined && value !== "");
}

function page(dump) {
  const rows = dump ? dumpRows(dump) : [];
  const body = dump
    ? rows.map(([label, value]) =>
      `<tr><th>${escape(label)}</th><td>${escape(value)}</td></tr>`).join("")
    : `<tr><td class="empty">No dump in this link. Scan the QR on the error
       screen, or open the code it produced in full.</td></tr>`;
  const json = dump ? escape(JSON.stringify(dump, null, 2)) : "";
  return `<!doctype html>
<html lang="en"><head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="robots" content="noindex">
<title>oskiewar crash dump</title>
<style>
  :root { color-scheme: dark; }
  body { margin: 0; padding: 24px; background: #07090f; color: #e8eefc;
    font: 15px/1.5 ui-monospace, "Berkeley Mono", "SF Mono", Menlo, monospace; }
  h1 { margin: 0 0 4px; font-size: 20px; color: #ff5c74; font-weight: 600; }
  p.lede { margin: 0 0 20px; color: #8d9bb8; }
  table { width: 100%; border-collapse: collapse; }
  th { width: 96px; text-align: left; vertical-align: top; padding: 6px 12px 6px 0;
    color: #74eab8; font-weight: 400; }
  td { padding: 6px 0; vertical-align: top; word-break: break-word; }
  td.empty { color: #ffcd4a; }
  details { margin-top: 24px; }
  summary { cursor: pointer; color: #70eab8; }
  pre { overflow-x: auto; padding: 12px; background: #10141f; border-radius: 6px;
    color: #bccce6; }
</style>
</head><body>
<h1>aesthetic.computer error</h1>
<p class="lede">oskiewar crash dump, carried in this link.</p>
<table><tbody>${body}</tbody></table>
${dump ? `<details><summary>raw</summary><pre>${json}</pre></details>` : ""}
</body></html>`;
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");
  if (event.httpMethod !== "GET")
    return respond(405, { error: "Method not allowed" });
  const params = event.queryStringParameters || {};
  const dump = decodeDump(params.d);
  if (params.format === "json")
    return dump ? respond(200, { dump }) : respond(400, { error: "Invalid dump" });
  return respond(dump ? 200 : 400, page(dump), {
    "Content-Type": "text/html; charset=utf-8",
    // The payload is the URL, so a decoded dump never changes.
    "Cache-Control": dump
      ? "public, max-age=31536000, immutable" : "no-store",
  });
}
