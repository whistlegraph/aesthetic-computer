#!/usr/bin/env node
// design-loop — look at Aesel's own furniture, change it, look again.
//
// Aesel does not draw all of itself. The QR you scan, the little live card of
// the piece, the stone that carries the session's status: those are Slab
// menubar overlays parked on top of the terminal, and they are the first thing
// anyone sees. Adjusting them used to be guesswork, for two reasons that
// compounded:
//
//   1. `frame` deliberately filters Slab's own overlays out of every capture —
//      the usual job is reading the machine's real content *beneath* them — so
//      a screenshot taken to check the card's padding showed the terminal
//      where the card is. Changing a number and looking at the result meant
//      looking at the wrong picture. `--overlays` is the opt-out, and this
//      tool always passes it.
//   2. The overlays are placed once, when a session's window appears. Editing
//      the placement and reinstalling the menubar does not move the surfaces
//      already on screen in any way you can trust, so the only honest check is
//      a *fresh* session. That is the restart this tool performs.
//
// So: close the Aesel session, open a new one, wait for its overlays to exist,
// point the camera at them. One command per iteration, and the picture that
// comes back is the thing being designed.
//
//   node easel/bin/design-loop.mjs              # restart Aesel, then shoot
//   node easel/bin/design-loop.mjs --shot       # shoot what is already open
//   node easel/bin/design-loop.mjs --out ~/x.jpg
//
// After an overlay edit the order is: install.sh in slab/menubar-swift, then
// this. Reinstalling without restarting the session measures the old layout.

import { spawn } from "node:child_process";
import { readFile, readdir, mkdir } from "node:fs/promises";
import { homedir, tmpdir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const FRAME = join(REPO, "slab", "bin", "frame.mjs");
const MARKERS = join(homedir(), ".local", "share", "slab", "state", "active-prompts");
const LEDGER = join(homedir(), ".config", "slab", "ledger", "local.json");

// The resident prox daemon, shared by every Claude session on this machine.
// Going through it rather than reimplementing close/launch means this tool
// inherits prox's own refusals — it will not close the session calling it, and
// it will not launch anything outside the allowlist.
const PROX = "http://127.0.0.1:7773";

const argv = process.argv.slice(2);
const flag = (name) => argv.includes(name);
const opt = (name) => { const i = argv.indexOf(name); return i === -1 ? null : argv[i + 1]; };

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function prox(name, args = {}) {
  const response = await fetch(`${PROX}/`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0", id: Date.now(),
      method: "tools/call", params: { name, arguments: args },
    }),
    signal: AbortSignal.timeout(30_000),
  }).catch((e) => {
    throw new Error(`prox daemon unreachable on ${PROX} — is the menubar's MCP up? (${e.message})`);
  });
  const body = await response.json();
  const text = (body?.result?.content || []).map((c) => c.text || "").join("\n");
  if (body?.result?.isError) throw new Error(`${name}: ${text}`);
  return text;
}

/// Every live Aesel session on this machine, newest marker first. Read from
/// the marker files rather than asked of prox: this runs between a close and a
/// launch, when the ledger cache is the one thing guaranteed to be stale.
async function easelSessions() {
  let names = [];
  try { names = await readdir(MARKERS); } catch { return []; }
  const out = [];
  for (const name of names) {
    try {
      const marker = JSON.parse(await readFile(join(MARKERS, name), "utf8"));
      if (marker.agent_type !== "easel") continue;
      out.push({ ...marker, id: marker.session_id || name });
    } catch { /* a marker mid-write is not an error, it is a retry */ }
  }
  return out.sort((a, b) => String(b.updated || "").localeCompare(String(a.updated || "")));
}

/// Wait for an Aesel session that is not one we already knew about, and that
/// has got far enough to own a terminal and a piece. `scan_url` is the signal
/// that matters: it is set at the moment the session has an address to encode,
/// which is the moment the QR and the preview card come into existence. Waiting
/// on the marker's mere existence would photograph the overlays mid-placement.
async function waitForFreshSession(known, timeoutMs = 45_000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    for (const session of await easelSessions()) {
      if (known.has(session.id)) continue;
      if (session.tty && session.scan_url) return session;
    }
    await sleep(500);
  }
  return null;
}

/// Bring a session's Terminal window to the front by its tty, because the
/// window id is not in the marker and the tty is. A frame of an unfocused
/// window still captures pixels, but the overlays belong to whichever window
/// is on top — photographing a buried one photographs somebody else's rock.
async function focusTty(tty) {
  const script = `
    tell application "Terminal"
      activate
      repeat with w in windows
        repeat with t in tabs of w
          if tty of t is "/dev/${tty}" then
            set selected tab of w to t
            set index of w to 1
            return "ok"
          end if
        end repeat
      end repeat
      return "missing"
    end tell`;
  const result = await run("osascript", ["-e", script]);
  return result.stdout.trim() === "ok";
}

function run(command, args) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, { stdio: ["ignore", "pipe", "pipe"] });
    let stdout = "", stderr = "";
    child.stdout.on("data", (d) => { stdout += d; });
    child.stderr.on("data", (d) => { stderr += d; });
    child.on("error", reject);
    child.on("close", (code) => {
      if (code === 0) resolve({ stdout, stderr });
      else reject(new Error(`${command} exited ${code}: ${stderr.trim().slice(0, 400)}`));
    });
  });
}

/// The capture itself. `--overlays` is the whole point; `--quiet-overlay` keeps
/// OCR from drawing its own boxes over the thing being judged, and `--no-ocr`
/// skips the text pass entirely, because this picture is read by an eye.
async function shoot(out) {
  await mkdir(dirname(out), { recursive: true }).catch(() => {});
  const { stdout } = await run(process.execPath, [
    FRAME, "local", "--overlays", "--no-ocr", "--quiet-overlay", "--json", "--out", out,
  ]);
  let envelope = {};
  try { envelope = JSON.parse(stdout); } catch { /* the file is what matters */ }
  return envelope;
}

async function selfHost() {
  try { return JSON.parse(await readFile(LEDGER, "utf8")).host; } catch { return null; }
}

const out = opt("--out") || join(tmpdir(), `easel-design-${Date.now()}.jpg`);

if (flag("--help") || flag("-h")) {
  console.log([
    "design-loop — restart Aesel and photograph its overlays",
    "",
    "  node easel/bin/design-loop.mjs [--shot] [--out file.jpg] [--cwd dir]",
    "",
    "      --shot: skip the restart and shoot the session already open",
    "      --out:  where the JPEG lands (default: a temp file, path printed)",
    "      --cwd:  working directory for the new session (default: this repo)",
    "      --settle: seconds to let the overlays land first (default 4)",
    "",
    "  Edit an overlay, run slab/menubar-swift/install.sh, then run this.",
  ].join("\n"));
  process.exit(0);
}

let session = (await easelSessions())[0] || null;

if (!flag("--shot")) {
  const host = await selfHost();
  if (!host) throw new Error(`no local ledger at ${LEDGER} — is the Slab menubar running?`);
  const known = new Set((await easelSessions()).map((s) => s.id));

  if (session) {
    console.log(`⟲ closing ${host}:easel (${session.piece || session.id.slice(0, 8)})`);
    await prox("prox_close", { handle: session.id });
  } else {
    console.log("⟲ no Aesel session open — launching a first one");
  }

  console.log("⟳ launching a fresh Aesel");
  await prox("prox_launch", { host, agent: "easel", cwd: opt("--cwd") || REPO, by: "easel:design-loop" });

  session = await waitForFreshSession(known);
  if (!session) throw new Error("the new Aesel never reported a scan URL — nothing to photograph.");
  // The marker is written the moment the address exists; the overlays are
  // placed on the menubar's next walk of the window list, which is a separate
  // clock this tool cannot read. Measured at about three seconds on blueberry —
  // a shot at 1.5s caught the terminal with no card and no QR on it, which is
  // the same wrong picture the `--overlays` flag exists to prevent. Four is
  // that with room. If the overlays still have not landed, `--shot` takes
  // another look without paying for another session.
  await sleep(Number(opt("--settle")) * 1000 || 4000);
}

if (!session) throw new Error("no Aesel session is open — drop --shot to launch one.");
if (!(await focusTty(session.tty))) {
  console.log(`! could not focus /dev/${session.tty} — shooting whatever is frontmost`);
}

const envelope = await shoot(out);
console.log(`📷 ${out}  (${envelope.capture_scope || "?"})`);
console.log(`   ${session.handle ? "@" + session.handle + "/" : ""}${session.piece || "?"} · ${session.scan_url || "no scan url"}`);
