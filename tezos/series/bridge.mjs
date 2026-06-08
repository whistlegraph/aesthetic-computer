#!/usr/bin/env node
// ─────────────────────────────────────────────────────────────────────────
// tezos/series/bridge.mjs
//
// generator → Electron app (production AC, logged in as @jeffrey) → $codes.
//
// Drives the ALREADY-RUNNING Electron app over the Chrome DevTools Protocol
// (port 9222, which every ac-electron start script enables). For each piece in
// manifest.json it:
//   1. posts  { type:"kidlisp-reload", code:<source>, createCode:true }  into
//      the aesthetic.computer webview (handled by boot.mjs:1728),
//   2. the runtime runs it and — because the webview session is authenticated —
//      auto-caches the source under @jeffrey via /api/store-kidlisp,
//   3. the page posts `kidlisp-code-created` back; we capture the $code,
//   4. write it into manifest.json.
//
// No Electron code changes. No CLI token. The DB association rides the
// logged-in webview session. After this, the $codes are ready for
// `node keeps.mjs mint $code`.
//
// PREREQUISITES (manual, once):
//   • Launch the desktop app on the PRODUCTION url and log in as @jeffrey:
//       cd ac-electron && npm run start:prod
//     (or: open "/Applications/Aesthetic Computer.app")
//   • Make sure you're logged in (handle shows @jeffrey at the prompt).
//
// Usage:
//   node tezos/series/bridge.mjs                 # all uncoded pieces
//   node tezos/series/bridge.mjs --series clock  # one series only
//   node tezos/series/bridge.mjs --force         # re-run even if code exists
//   node tezos/series/bridge.mjs --preview       # render each, don't capture codes
//   node tezos/series/bridge.mjs --port 9222 --timeout 25000
//   node tezos/series/bridge.mjs --allow-anon    # don't require a logged-in user
// ─────────────────────────────────────────────────────────────────────────

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const MANIFEST = path.join(__dirname, "manifest.json");

// ── args ──────────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const getArg = (k, d) => {
  const i = argv.indexOf(`--${k}`);
  return i >= 0 ? argv[i + 1] : d;
};
const PORT = parseInt(getArg("port", "9222"), 10);
const TIMEOUT = parseInt(getArg("timeout", "25000"), 10);
const ONLY = getArg("series", null);
const FORCE = argv.includes("--force");
const PREVIEW = argv.includes("--preview");
const ALLOW_ANON = argv.includes("--allow-anon");
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// ── minimal CDP client (same wire pattern as ac-electron/testing/cdp-latency) ─
class Cdp {
  constructor(ws) {
    this.ws = ws;
    this.id = 1;
    this.pending = new Map();
    ws.addEventListener("message", (m) => {
      const msg = JSON.parse(m.data);
      if (msg.id && this.pending.has(msg.id)) {
        const { resolve, reject } = this.pending.get(msg.id);
        this.pending.delete(msg.id);
        msg.error ? reject(new Error(`${msg.error.code}: ${msg.error.message}`)) : resolve(msg.result);
      }
    });
  }
  static async connect(url) {
    const ws = new WebSocket(url);
    await new Promise((res, rej) => {
      ws.addEventListener("open", res, { once: true });
      ws.addEventListener("error", rej, { once: true });
    });
    return new Cdp(ws);
  }
  send(method, params = {}) {
    const id = this.id++;
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }
  // Evaluate an expression in the page; await promises, return the value.
  async evalJs(expression) {
    const r = await this.send("Runtime.evaluate", {
      expression,
      awaitPromise: true,
      returnByValue: true,
    });
    if (r.exceptionDetails) {
      throw new Error(r.exceptionDetails.exception?.description || r.exceptionDetails.text || "eval error");
    }
    return r.result?.value;
  }
  close() { this.ws.close(); }
}

async function listTargets(port) {
  const r = await fetch(`http://127.0.0.1:${port}/json`);
  if (!r.ok) throw new Error(`CDP /json → ${r.status}. Is the app running with --remote-debugging-port=${port}?`);
  return r.json();
}

// AC content runs inside a <webview> (type "webview"); the outer window is a
// file:// "page". Prefer production aesthetic.computer; warn if only localhost.
function findAcTarget(targets) {
  const isAc = (t) => /aesthetic\.computer/.test(t.url || "") && (t.type === "webview" || t.type === "page");
  const prod = targets.find((t) => isAc(t) && !/localhost|127\.0\.0\.1/.test(t.url || ""));
  return prod || targets.find(isAc) || null;
}

// ── load manifest ───────────────────────────────────────────────────────────
if (!fs.existsSync(MANIFEST)) {
  console.error(`❌ ${MANIFEST} missing — run: node tezos/series/generate.mjs`);
  process.exit(1);
}
const manifest = JSON.parse(fs.readFileSync(MANIFEST, "utf8"));
const seriesNames = ONLY ? [ONLY] : Object.keys(manifest.series);
for (const n of seriesNames) {
  if (!manifest.series[n]) {
    console.error(`❌ unknown series "${n}". have: ${Object.keys(manifest.series).join(", ")}`);
    process.exit(1);
  }
}

const saveManifest = () => fs.writeFileSync(MANIFEST, JSON.stringify(manifest, null, 2));

// ── main ──────────────────────────────────────────────────────────────────
async function main() {
  console.log(`🔌 CDP :${PORT} …`);
  let targets;
  try {
    targets = await listTargets(PORT);
  } catch (e) {
    console.error(`❌ ${e.message}`);
    console.error(`   Launch it first:  cd ac-electron && npm run start:prod`);
    process.exit(1);
  }
  const target = findAcTarget(targets);
  if (!target) {
    console.error(`❌ No aesthetic.computer webview found on :${PORT}.`);
    console.error(`   Open the app and make sure an AC pane (e.g. /prompt) is showing.`);
    process.exit(1);
  }
  if (/localhost|127\.0\.0\.1/.test(target.url)) {
    console.warn(`⚠️  Target is a LOCAL dev url (${target.url}). You asked for production —`);
    console.warn(`   relaunch with  npm run start:prod  so $codes save to the live DB.`);
  }
  console.log(`→ attaching: ${target.url}`);
  const cdp = await Cdp.connect(target.webSocketDebuggerUrl);
  await cdp.send("Runtime.enable");
  await cdp.send("Page.enable").catch(() => {});

  // Wait for the runtime bridge (acSEND) to be live.
  const bootDeadline = Date.now() + 15000;
  let ready = false;
  while (Date.now() < bootDeadline) {
    const ok = await cdp.evalJs(`typeof window.acSEND === 'function'`).catch(() => false);
    if (ok) { ready = true; break; }
    await sleep(400);
  }
  if (!ready) {
    console.error(`❌ window.acSEND never appeared — is an AC piece actually loaded in the pane?`);
    process.exit(1);
  }

  // Identity check.
  const who = await cdp.evalJs(`JSON.stringify(window.acUSER || null)`).catch(() => null);
  const user = who ? JSON.parse(who) : null;
  const ident = user ? (user.handle || user.name || user.sub || "?") : null;
  if (!user) {
    if (!ALLOW_ANON) {
      console.error(`❌ Not logged in (window.acUSER is null). Pieces would cache anonymously.`);
      console.error(`   Log in as @jeffrey in the app, or pass --allow-anon to override.`);
      process.exit(1);
    }
    console.warn(`⚠️  Not logged in — caching ANONYMOUSLY (--allow-anon).`);
  } else {
    console.log(`👤 logged in as: ${ident}`);
    if (!/jeffrey/i.test(JSON.stringify(user)) && !ALLOW_ANON) {
      console.warn(`⚠️  Session does not look like @jeffrey — continuing anyway.`);
    }
  }

  // Per-piece: inject kidlisp-reload + createCode, await kidlisp-code-created.
  const runPiece = async (source) => {
    const expr = `new Promise((resolve) => {
      const SRC = ${JSON.stringify(source)};
      let done = false;
      const onMsg = (e) => {
        const d = e && e.data; if (!d) return;
        if (d.type === 'kidlisp-code-created' || d.type === 'setCode') {
          if (done) return; done = true;
          window.removeEventListener('message', onMsg);
          resolve(d.code || d.value || null);
        }
      };
      window.addEventListener('message', onMsg);
      window.postMessage({ type: 'kidlisp-reload', code: SRC, createCode: ${PREVIEW ? "false" : "true"} }, '*');
      setTimeout(() => { if (!done){ done = true; window.removeEventListener('message', onMsg); resolve(null); } }, ${TIMEOUT});
    })`;
    return cdp.evalJs(expr);
  };

  let ran = 0, got = 0, failed = 0;
  for (const name of seriesNames) {
    const list = manifest.series[name];
    console.log(`\n══ ${name} (${list.length}) ══`);
    for (const it of list) {
      if (it.code && !FORCE && !PREVIEW) {
        console.log(`  • ${name}/${it.title}  ↩︎ already $${it.code}`);
        continue;
      }
      process.stdout.write(`  • ${name}/${it.title}  … `);
      try {
        const code = await runPiece(it.source);
        ran++;
        if (PREVIEW) {
          console.log(`rendered (preview)`);
          await sleep(1500); // linger so you can eyeball it
        } else if (code) {
          it.code = code;
          got++;
          saveManifest();
          console.log(`$${code} ✅`);
          await sleep(900);
        } else {
          failed++;
          console.log(`no code returned ⚠️  (rendered, but createCode message not seen)`);
          await sleep(500);
        }
      } catch (e) {
        failed++;
        console.log(`error: ${e.message} ❌`);
      }
    }
  }

  cdp.close();
  console.log(`\n──────── done ────────`);
  console.log(`ran ${ran} · captured ${got} · issues ${failed}`);
  if (!PREVIEW && got) {
    console.log(`\nCaptured $codes:`);
    for (const name of seriesNames) {
      for (const it of manifest.series[name]) {
        if (it.code) console.log(`  ${name}/${it.title.padEnd(10)} $${it.code}   https://aesthetic.computer/$${it.code}`);
      }
    }
    console.log(`\nMint as keeps (per piece):  node keeps.mjs mint $<code>`);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
