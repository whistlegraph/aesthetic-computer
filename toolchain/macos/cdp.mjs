#!/usr/bin/env node
// cdp.mjs — drive an already-running visible Chrome over the DevTools Protocol.
//
// Launch Chrome yourself first, e.g.:
//   "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
//     --remote-debugging-port=9222 --user-data-dir=$HOME/.some-profile \
//     --new-window "https://example.com"
//
// Then drive it (Node 22+ has a global WebSocket — no deps):
//   node toolchain/macos/cdp.mjs targets                 # list page targets
//   node toolchain/macos/cdp.mjs eval '<js expression>'  # eval in active page, prints JSON result
//   node toolchain/macos/cdp.mjs evalfile <path.js>      # eval the file's contents as an expression
//   node toolchain/macos/cdp.mjs nav <url>               # navigate active page
//   node toolchain/macos/cdp.mjs shot <out.png>          # screenshot active page
//
// Target selection: by default the most-recently-active http(s) page. Override
// with CDP_TARGET=<targetId substring or url substring>. PORT via CDP_PORT.

const PORT = process.env.CDP_PORT || "9222";
const BASE = `http://127.0.0.1:${PORT}`;

async function listTargets() {
  const r = await fetch(`${BASE}/json`);
  const all = await r.json();
  return all.filter((t) => t.type === "page");
}

function pickTarget(targets) {
  const sel = process.env.CDP_TARGET;
  if (sel) {
    const m = targets.find(
      (t) => t.id.includes(sel) || (t.url || "").includes(sel),
    );
    if (m) return m;
  }
  // prefer http(s) pages over devtools:// / about:blank
  const http = targets.filter((t) => /^https?:/.test(t.url || ""));
  return (http[0] || targets[0]);
}

let _id = 0;
function send(ws, method, params = {}) {
  const id = ++_id;
  return new Promise((resolve, reject) => {
    const onMsg = (ev) => {
      let msg;
      try {
        msg = JSON.parse(ev.data);
      } catch {
        return;
      }
      if (msg.id === id) {
        ws.removeEventListener("message", onMsg);
        if (msg.error) reject(new Error(JSON.stringify(msg.error)));
        else resolve(msg.result);
      }
    };
    ws.addEventListener("message", onMsg);
    ws.send(JSON.stringify({ id, method, params }));
  });
}

async function withPage(fn) {
  const targets = await listTargets();
  if (!targets.length) throw new Error("no page targets — is Chrome running with --remote-debugging-port?");
  const t = pickTarget(targets);
  const ws = new WebSocket(t.webSocketDebuggerUrl);
  await new Promise((res, rej) => {
    ws.addEventListener("open", res, { once: true });
    ws.addEventListener("error", rej, { once: true });
  });
  try {
    await send(ws, "Runtime.enable");
    await send(ws, "Page.enable");
    return await fn(ws, t);
  } finally {
    ws.close();
  }
}

async function evalExpr(ws, expr) {
  const r = await send(ws, "Runtime.evaluate", {
    expression: expr,
    returnByValue: true,
    awaitPromise: true,
    userGesture: true,
  });
  if (r.exceptionDetails) {
    throw new Error(
      r.exceptionDetails.exception?.description ||
        JSON.stringify(r.exceptionDetails),
    );
  }
  return r.result.value;
}

const [cmd, ...rest] = process.argv.slice(2);

if (cmd === "targets") {
  const targets = await listTargets();
  for (const t of targets) console.log(`${t.id}\t${t.url}\n\t${t.title || ""}`);
} else if (cmd === "eval") {
  const out = await withPage((ws) => evalExpr(ws, rest.join(" ")));
  console.log(typeof out === "string" ? out : JSON.stringify(out, null, 2));
} else if (cmd === "evalfile") {
  const fs = await import("node:fs");
  const code = fs.readFileSync(rest[0], "utf8");
  const out = await withPage((ws) => evalExpr(ws, code));
  console.log(typeof out === "string" ? out : JSON.stringify(out, null, 2));
} else if (cmd === "nav") {
  await withPage(async (ws) => {
    await send(ws, "Page.navigate", { url: rest[0] });
  });
  console.log("navigated:", rest[0]);
} else if (cmd === "click") {
  // click <x> <y> — real mouse click at viewport coords
  const x = parseFloat(rest[0]), y = parseFloat(rest[1]);
  await withPage(async (ws) => {
    await send(ws, "Input.dispatchMouseEvent", { type: "mousePressed", x, y, button: "left", clickCount: 1 });
    await send(ws, "Input.dispatchMouseEvent", { type: "mouseReleased", x, y, button: "left", clickCount: 1 });
  });
  console.log("clicked", x, y);
} else if (cmd === "shot") {
  // shot <out.png> [WxH] — optional WxH sets the emulated viewport first
  const fs = await import("node:fs");
  const out = rest[0] || "/tmp/cdp-shot.png";
  const wh = rest[1] && /^\d+x\d+$/.test(rest[1]) ? rest[1].split("x").map(Number) : null;
  await withPage(async (ws) => {
    if (wh) {
      await send(ws, "Emulation.setDeviceMetricsOverride", {
        width: wh[0], height: wh[1], deviceScaleFactor: 2, mobile: false,
      });
    }
    const { data } = await send(ws, "Page.captureScreenshot", { format: "png" });
    fs.writeFileSync(out, Buffer.from(data, "base64"));
    if (wh) await send(ws, "Emulation.clearDeviceMetricsOverride");
  });
  console.log("wrote", out, wh ? `@${rest[1]}` : "");
} else {
  console.log("usage: cdp.mjs <targets|eval <js>|evalfile <path>|nav <url>|shot <out.png>>");
  process.exit(1);
}
