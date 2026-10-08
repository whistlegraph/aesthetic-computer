// safari-puppet.mjs — puppet's `browser: "safari"` backend. Drives the REAL
// desktop Safari (your own windows, cookies and logins) on any registered
// machine, here or over ssh, through the same verbs the Chrome/CDP daemon
// answers. Safari offers no CDP and safaridriver only drives a separate,
// signed-out automation window, so this rides two older doors instead:
//
//   • AppleScript `do JavaScript` for page reads, nav, fill and snapshots
//     (needs Safari ▸ Settings ▸ Developer ▸ "Allow JavaScript from Apple
//     Events", once per machine), and
//   • native CGEvent input at global screen points for trusted clicks, drags
//     and keys — exactly frame's coordinate space, via macos.mjs, under the
//     same per-machine input lease frame and puppet use.
//
// Page coordinates stay browser CSS pixels (puppet's contract); we convert
// them with the tab's window geometry. Assumes 100% page zoom.
//
// Target IDs are `safari:<windowId>:<tabIndex>`. The window id is Safari's
// AppleScript id, which is also its CoreGraphics window number, so shots use
// `screencapture -l` and work even when the window is covered.
import { randomUUID } from "node:crypto";
import { withMachineLease } from "./computer-use-lease.mjs";
import { clickPointAsync, sendKeysAsync, shAsync } from "../bin/macos.mjs";
import { evalCall, pageCall, pendingCall } from "./safari-page.mjs";
import { localFrame } from "./frame-local.mjs";
import { homedir } from "node:os";
import { join } from "node:path";

const ID_RE = /^safari:(\d+):(\d+)$/;
const sleep = ms => new Promise(r => setTimeout(r, ms));

const ENABLE_HINT = "Safari refuses scripted JavaScript on this machine. Turn on Safari ▸ Settings ▸ Advanced ▸ \"Show features for web developers\", then Developer ▸ \"Allow JavaScript from Apple Events\".";

function jxa(spec, script) {
  return shAsync(spec, "osascript -l JavaScript -", { stdin: script }).catch(error => {
    const msg = String(error.stderr || error.message || error);
    if (/Allow JavaScript from Apple Events/i.test(msg)) throw new Error(ENABLE_HINT);
    if (/not allowed to send Apple events|-1743/.test(msg)) throw new Error("macOS blocked Apple Events to Safari from this process. Approve it under System Settings ▸ Privacy & Security ▸ Automation.");
    throw new Error(msg.replace(/^Command failed:[^\n]*\n?/, "").trim() || "osascript failed");
  });
}

// JXA prelude: resolve a target to {win, tab}. `exact` demands a target ID.
function prelude(target, { exact = false } = {}) {
  return `const S = Application("Safari");
function wins() { const out = []; for (const w of S.windows()) { try { if (w.tabs.length) out.push(w); } catch (e) {} } return out; }
function pick() {
  const target = ${JSON.stringify(target ?? null)};
  const m = target && target.match(${ID_RE});
  if (m) {
    let win, tab;
    try { win = S.windows.byId(Number(m[1])); tab = win.tabs[Number(m[2]) - 1]; tab.url(); }
    catch (e) { throw new Error("Safari target is gone or does not match exactly; no action sent"); }
    return { win, tab, id: target };
  }
  ${exact ? `throw new Error("An exact Safari target ID is required (puppet_list browser:safari), e.g. safari:75:1");` : ""}
  for (const win of wins()) {
    const tabs = win.tabs();
    for (let i = 0; i < tabs.length; i++) {
      const t = tabs[i];
      const hit = target ? ((t.url() || "").includes(target) || (t.name() || "").includes(target)) : t.index() === win.currentTab().index();
      if (hit) return { win, tab: t, id: "safari:" + win.id() + ":" + (i + 1) };
    }
  }
  throw new Error(target ? "No Safari tab matches " + JSON.stringify(target) : "Safari has no open window");
}
`;
}

// Run a page script in the target tab and parse its JSON answer.
async function inTab(spec, target, js, opts) {
  const out = await jxa(spec, `${prelude(target, opts)}
const p = pick();
const r = S.doJavaScript(${JSON.stringify(js)}, { in: p.tab });
JSON.stringify({ id: p.id, r: r === undefined ? null : r });`);
  const { id, r } = JSON.parse(out);
  if (r === null) throw new Error("Safari returned nothing — the page may still be loading, or the script has a syntax error");
  const parsed = typeof r === "string" ? JSON.parse(r) : { value: r };
  if (parsed.error) throw new Error(parsed.error);
  return { id, ...parsed };
}
const page = async (spec, target, call, opts) => (await inTab(spec, target, pageCall(call), opts)).ok;

// Bring the tab's window to the front so native input lands on it.
async function focus(spec, target, opts) {
  const out = await jxa(spec, `${prelude(target, opts)}
const p = pick();
p.win.currentTab = p.tab; p.win.index = 1; S.activate(); delay(0.2);
p.id;`);
  return out;
}

// CSS-pixel point in the tab → global screen point (frame's space).
function toScreen(g, x, y) {
  return [g.sx + (g.ow - g.iw) + x, g.sy + (g.oh - g.ih) + y];
}

// ── verbs ───────────────────────────────────────────────────────────────────
export async function list(spec, machine) {
  const out = await jxa(spec, `const S = Application("Safari");
const rows = [];
if (S.running()) for (const w of S.windows()) {
  let tabs = []; try { tabs = w.tabs(); } catch (e) { continue; }
  let cur = -1; try { cur = w.currentTab().index(); } catch (e) {}
  tabs.forEach((t, i) => rows.push({ id: "safari:" + w.id() + ":" + (i + 1), title: t.name() || "", url: t.url() || "", current: t.index() === cur, window: w.index() }));
}
JSON.stringify({ running: S.running(), pages: rows });`);
  const { running, pages } = JSON.parse(out);
  return { machine, browser: "safari", running, pages };
}

export async function evaluate(spec, { js, target }, { timeout = 20000 } = {}) {
  let res;
  try { res = await inTab(spec, target, evalCall(js)); }
  catch (error) {
    // Statements (not one expression) break the wrapper; fall back to Safari's
    // own completion value, which AppleScript converts as best it can.
    if (!/returned nothing/.test(error.message)) throw error;
    const out = await jxa(spec, `${prelude(target)}
const p = pick(); const r = S.doJavaScript(${JSON.stringify(js)}, { in: p.tab }); JSON.stringify(r === undefined ? null : r);`);
    return JSON.parse(out);
  }
  const deadline = Date.now() + timeout;
  while (res.pending) {
    if (Date.now() > deadline) throw new Error("promise did not settle before timeout");
    await sleep(100);
    res = await inTab(spec, res.id, pendingCall(res.pending));
  }
  return res.undef ? undefined : res.value;
}

export async function waitFor(spec, { js, target, timeout = 15000, interval = 150 }) {
  const deadline = Date.now() + timeout;
  for (;;) {
    let v; try { v = await evaluate(spec, { js, target }, { timeout: Math.max(1, deadline - Date.now()) }); } catch (e) { v = undefined; }
    if (v) return v;
    if (Date.now() > deadline) throw new Error(`waitFor timeout after ${timeout}ms`);
    await sleep(Math.max(100, interval));
  }
}

export async function nav(spec, { url, target }) {
  return jxa(spec, `${prelude(target)}
const p = pick(); p.tab.url = ${JSON.stringify(url)}; "navigated " + p.id;`);
}

export async function reload(spec, { target }) {
  await inTab(spec, target, "(location.reload(),JSON.stringify({value:true}))");
  return "reloaded (Safari's scripted reload uses the cache; Option-Cmd-R is the origin reload)";
}

export async function shot(spec, { target, format = "jpeg" }) {
  // Make the tab current (a window only shows its current tab), then capture
  // the window by number and crop away the toolbar.
  const id = await jxa(spec, `${prelude(target)}
const p = pick(); p.win.currentTab = p.tab; p.id;`);
  await sleep(150);
  const g = await page(spec, id, "P.geometry()", { exact: true });
  const wid = Number(id.match(ID_RE)[1]);
  const ext = format === "png" ? "png" : "jpg";
  const top = Math.max(0, Math.round(g.oh - g.ih)), ow = Math.max(1, Math.round(g.ow)), left = Math.max(0, Math.round(g.ow - g.iw));
  const b64 = await shAsync(spec, `f=$(mktemp -t puppetshot).${ext}; screencapture -x -o -l${wid} -t ${ext} "$f" || exit 1
pw=$(sips -g pixelWidth "$f" | awk '/pixelWidth/{print $2}'); ph=$(sips -g pixelHeight "$f" | awk '/pixelHeight/{print $2}')
t=$(( pw * ${top} / ${ow} )); l=$(( pw * ${left} / ${ow} ))
sips -c $(( ph - t )) $(( pw - l )) --cropOffset $t $l "$f" >/dev/null 2>&1
base64 -i "$f"; rm -f "$f"`).catch(error => {
    // The puppet launch agent usually has no Screen Recording grant of its
    // own. Locally, fall back to frame's native capture (SlabMenubar holds
    // that grant): bring the tab forward, then capture its page rectangle.
    if (spec.local && /could not create image/.test(error.message)) return null;
    throw error;
  });
  if (b64 === null) return frameShot(spec, id, g);
  if (!b64) throw new Error("screencapture returned nothing — grant Screen Recording to the process running puppet");
  return b64.replace(/\s+/g, "");
}

async function frameShot(spec, id) {
  return withMachineLease(spec, async () => {
    await focus(spec, id, { exact: true });
    const g = await page(spec, id, "P.geometry()", { exact: true });
    const [x, y] = toScreen(g, 0, 0);
    const crop = [x, y, g.iw, g.ih].map(Math.round).join(",");
    const frame = await localFrame(join(homedir(), ".local", "share", "slab", "state"), `window noocr novisual fast crop=${crop}`);
    if (!frame?.jpg?.length) throw new Error("frame capture returned no image");
    return Buffer.from(frame.jpg).toString("base64");
  });
}

// Native pointer stream through CSS-pixel points. One point is a click.
async function strokeScreen(spec, pts) {
  if (pts.length === 1) return clickPointAsync(spec, pts[0][0], pts[0][1]);
  const path = JSON.stringify(pts.map(([x, y]) => [Math.round(x), Math.round(y)]));
  return jxa(spec, `ObjC.import("CoreGraphics");
const pts = ${path};
const ev = (type, p) => $.CGEventPost($.kCGHIDEventTap, $.CGEventCreateMouseEvent(null, type, $.CGPointMake(p[0], p[1]), $.kCGMouseButtonLeft));
let last = pts[0];
try {
  ev($.kCGEventLeftMouseDown, last); delay(0.04);
  for (let i = 1; i < pts.length; i++) {
    const [a, b] = [pts[i - 1], pts[i]];
    const steps = Math.max(1, Math.ceil(Math.hypot(b[0] - a[0], b[1] - a[1]) / 6));
    for (let s = 1; s <= steps; s++) { last = [a[0] + (b[0] - a[0]) * s / steps, a[1] + (b[1] - a[1]) * s / steps]; ev($.kCGEventLeftMouseDragged, last); delay(0.008); }
  }
} finally { ev($.kCGEventLeftMouseUp, last); }
"ok";`);
}

export async function stroke(spec, { points, target }) {
  if (!Array.isArray(points) || !points.length || !points.every(p => Array.isArray(p) && p.length === 2 && p.every(Number.isFinite)))
    throw new Error("points must be [[x,y],...] in page CSS pixels");
  return withMachineLease(spec, async () => {
    const id = await focus(spec, target);
    const g = await page(spec, id, "P.geometry()", { exact: true });
    await strokeScreen(spec, points.map(([x, y]) => toScreen(g, x, y)));
    return points.length;
  });
}

// Human-ish drag: eased progress along a bent path with a little jitter.
export async function gesture(spec, { from, to, opts = {}, target }) {
  const { speed = 1, bend = 0.15, wobble = 1.5 } = opts;
  const dist = Math.hypot(to[0] - from[0], to[1] - from[1]);
  const steps = Math.max(8, Math.round(dist / (8 * Math.max(0.2, speed))));
  const nx = -(to[1] - from[1]) / (dist || 1), ny = (to[0] - from[0]) / (dist || 1);
  const pts = [];
  for (let i = 0; i <= steps; i++) {
    const t = i / steps, e = t < 0.5 ? 2 * t * t : 1 - (-2 * t + 2) ** 2 / 2;
    const arc = Math.sin(Math.PI * e) * bend * dist, j = i && i < steps ? (Math.random() - 0.5) * wobble : 0;
    pts.push([from[0] + (to[0] - from[0]) * e + nx * arc + j, from[1] + (to[1] - from[1]) * e + ny * arc + j]);
  }
  await stroke(spec, { points: pts, target });
  return pts.length;
}

const DOM_KEYS = { Enter: "enter", Tab: "tab", Escape: "escape", Backspace: "delete", " ": "space",
  ArrowUp: "up", ArrowDown: "down", ArrowLeft: "left", ArrowRight: "right" };
export async function key(spec, { key: k, modifiers = 0, target }) {
  const mods = [[1, "alt"], [2, "ctrl"], [4, "cmd"], [8, "shift"]].filter(([b]) => modifiers & b).map(([, m]) => m);
  const name = DOM_KEYS[k] || (String(k).length === 1 ? k : null);
  if (!name) throw new Error(`unsupported key for Safari: ${k} (use a DOM name like Enter/Tab/Escape/Backspace/ArrowLeft or one character)`);
  return withMachineLease(spec, async () => {
    await focus(spec, target);
    await sendKeysAsync(spec, name, mods);
    return k;
  });
}

export async function cursor(spec, { x, y, target }) {
  return page(spec, target, `P.cursor(${Number(x)},${Number(y)})`);
}

export async function upload() {
  throw new Error("Safari exposes no way to set a file input from automation. Use frame_drag to drop a Finder file on the page, or browser:\"chrome\".");
}

// ── semantic verbs (snapshot / click / fill / wait) ─────────────────────────
function budget(value = 5000) {
  if (!Number.isFinite(value) || value < 1 || value > 10000) throw new Error("timeout must be 1..10000 ms");
  return value;
}
const STATES = ["visible", "hidden", "attached", "detached"];

async function observe(spec, target, { image = false } = {}) {
  const snap = await page(spec, target, "P.snapshot(24000)", { exact: true });
  const g = await page(spec, target, "P.geometry()", { exact: true });
  return {
    observation: { id: randomUUID(), capturedAt: new Date().toISOString(), target, url: g.url, coordinateSpace: "browser-css-pixels" },
    tree: snap.tree, truncated: snap.truncated,
    ...(image ? { image: await shot(spec, { target }) } : {}),
  };
}

async function waitUntil(spec, target, locator, state, deadline) {
  const call = `P.satisfied(${JSON.stringify(locator)},${JSON.stringify(state)})`;
  for (;;) {
    if (await page(spec, target, call, { exact: true })) return true;
    if (Date.now() >= deadline) throw new Error(`Timed out waiting for locator to be ${state}`);
    await sleep(100);
  }
}

const queues = new Map();
export function semantic(spec, action, args) {
  if (action === "choose") throw new Error("puppet_choose reads Chrome's accessibility tree and is Chrome-only for now. On Safari, use puppet_snapshot and pick the locator yourself.");
  if (["snapshot", "wait"].includes(action)) return perform(spec, action, args);
  const key = `${args.machine}|${args.target}`;
  const previous = queues.get(key) || Promise.resolve();
  const op = previous.catch(() => {}).then(() => perform(spec, action, args));
  queues.set(key, op);
  const cleanup = () => { if (queues.get(key) === op) queues.delete(key); };
  op.then(cleanup, cleanup);
  return op;
}

async function perform(spec, action, args) {
  const ms = budget(args.timeout), deadline = Date.now() + ms;
  const { target } = args;
  if (!ID_RE.test(target || "")) throw new Error("An exact Safari target ID is required (puppet_list browser:safari), e.g. safari:75:1");
  if (action === "snapshot") return observe(spec, target, args);
  const locator = args.locator;
  if (!locator || typeof locator !== "object") throw new Error("locator is required");
  if (action === "wait") {
    const state = args.state || "visible";
    if (!STATES.includes(state)) throw new Error("Invalid wait state");
    await waitUntil(spec, target, locator, state, deadline);
    return { verified: true, state, target };
  }
  if (!["click", "fill"].includes(action)) throw new Error("Unknown semantic action");
  if (action === "fill" && typeof args.value !== "string") throw new Error("fill requires a string value");
  const afterState = args.after?.state || "visible";
  if (args.after && !STATES.includes(afterState)) throw new Error("Invalid postcondition state");

  // Wait for actionability before any input; strict violations fail fast.
  const editable = action === "fill";
  let ready;
  for (;;) {
    ready = await page(spec, target, `P.actionable(${JSON.stringify(locator)},{editable:${editable}})`, { exact: true });
    if (ready.ready || ready.strict) break;
    if (Date.now() >= deadline) break;
    await sleep(100);
  }
  if (!ready.ready) throw new Error(`${ready.reason}; no action sent`);

  try {
    if (action === "fill") {
      const r = await page(spec, target, `P.fill(${JSON.stringify(locator)},${JSON.stringify(args.value)})`, { exact: true });
      if (!r.filled) throw new Error(`${r.reason}; no action sent`);
    } else {
      await withMachineLease(spec, async () => {
        await focus(spec, target, { exact: true });
        // Re-measure after focusing: the window may have scrolled or moved.
        const again = await page(spec, target, `P.actionable(${JSON.stringify(locator)},{})`, { exact: true });
        if (!again.ready) throw Object.assign(new Error(`${again.reason}; no action sent`), { noInput: true });
        const g = await page(spec, target, "P.geometry()", { exact: true });
        const [sx, sy] = toScreen(g, again.x, again.y);
        await clickPointAsync(spec, sx, sy);
      });
    }
  } catch (error) {
    if (error.noInput || /no action sent/.test(error.message)) throw error;
    return { action, performed: "unknown", target, verification: { ok: false, error: error.message } };
  }
  const result = { action, performed: true, target, verification: { ok: null } };
  try {
    if (args.after) { await waitUntil(spec, target, args.after.locator, afterState, deadline); result.verification = { ok: true, state: afterState }; }
    Object.assign(result, await observe(spec, target, args));
  } catch (error) {
    result.verification = { ok: false, error: error.message };
  }
  return result;
}
