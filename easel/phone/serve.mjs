// A static server for the phone client, rooted at the repository so the page
// can import `easel/src/*.mjs` and fetch `easel/context/*.md` at the very paths
// the bridge builds internally. Nothing is copied or bundled — the phone runs
// the same files the desktop does.
//
//   node easel/phone/serve.mjs [--port 8770] [--token]
//
// `--token` mints a dev endpoint that hands the page the access token out of
// ~/.ac-token, so a phone on the LAN can sign in before the real
// ASWebAuthenticationSession flow exists. It is off by default and refuses any
// request that is not from a private address, because it is a bearer token.

import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { existsSync, readFileSync } from "node:fs";
import { join, normalize, extname } from "node:path";
import { homedir, networkInterfaces } from "node:os";

const ROOT = new URL("../..", import.meta.url).pathname.replace(/\/$/, "");

const args = process.argv.slice(2);
const flag = (name, fallback) => {
  const at = args.indexOf(`--${name}`);
  if (at === -1) return fallback;
  const value = args[at + 1];
  return value && !value.startsWith("--") ? value : true;
};

const PORT = Number(flag("port", 8770));
const SHARE_TOKEN = Boolean(flag("token", false));

const TYPES = {
  ".html": "text/html; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".md": "text/markdown; charset=utf-8",
  ".png": "image/png",
  ".svg": "image/svg+xml",
  ".woff": "font/woff",
  ".woff2": "font/woff2",
};

// Only these trees are reachable. The server sits on the repository root so the
// unmodified bridge resolves, which is exactly the kind of convenience that
// serves a vault by accident if it is not fenced.
const ALLOWED = ["/easel/phone/", "/easel/src/", "/easel/context/"];

function isPrivate(address = "") {
  const host = address.replace(/^::ffff:/, "");
  return (
    host === "127.0.0.1" ||
    host === "::1" ||
    host.startsWith("10.") ||
    host.startsWith("192.168.") ||
    host.startsWith("100.") || // tailnet
    /^172\.(1[6-9]|2\d|3[01])\./.test(host)
  );
}

function localAddresses() {
  const out = [];
  for (const entries of Object.values(networkInterfaces())) {
    for (const entry of entries ?? []) {
      if (entry.family === "IPv4" && !entry.internal) out.push(entry.address);
    }
  }
  return out;
}

const server = createServer(async (request, response) => {
  const url = new URL(request.url, `http://${request.headers.host ?? "localhost"}`);
  let path = decodeURIComponent(url.pathname);
  if (path === "/") path = "/easel/phone/index.html";

  if (path === "/dev-token") {
    if (!SHARE_TOKEN) return send(response, 404, "text/plain", "dev token sharing is off");
    if (!isPrivate(request.socket.remoteAddress)) {
      return send(response, 403, "text/plain", "refused: not a private address");
    }
    const file = join(homedir(), ".ac-token");
    if (!existsSync(file)) return send(response, 404, "text/plain", "no ~/.ac-token — run ac-login");
    try {
      const record = JSON.parse(readFileSync(file, "utf8"));
      if (!record.access_token) throw new Error("no access_token in ~/.ac-token");
      return send(
        response,
        200,
        TYPES[".json"],
        JSON.stringify({ access_token: record.access_token }),
      );
    } catch (error) {
      return send(response, 500, "text/plain", error.message);
    }
  }

  const safe = normalize(path);
  if (!ALLOWED.some((prefix) => safe.startsWith(prefix))) {
    return send(response, 403, "text/plain", "forbidden");
  }

  try {
    const body = await readFile(join(ROOT, safe));
    send(response, 200, TYPES[extname(safe)] ?? "application/octet-stream", body);
  } catch {
    send(response, 404, "text/plain", `not found: ${safe}`);
  }
});

function send(response, status, type, body) {
  response.writeHead(status, {
    "Content-Type": type,
    "Cache-Control": "no-store",
    // The page calls aesthetic.computer, not the other way round, so this is
    // only here so a phone can be pointed at a laptop without a proxy.
    "Access-Control-Allow-Origin": "*",
  });
  response.end(body);
}

server.listen(PORT, "0.0.0.0", () => {
  console.log(`aesel phone → http://localhost:${PORT}/`);
  for (const address of localAddresses()) console.log(`             → http://${address}:${PORT}/`);
  if (SHARE_TOKEN) console.log("  /dev-token is live (private addresses only)");
});
