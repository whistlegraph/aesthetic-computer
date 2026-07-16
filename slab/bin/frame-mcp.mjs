#!/usr/bin/env node
// frame-mcp.mjs — a thin MCP server over `frame` (frame.mjs), so any agent can
// SEE a fleet Mac's screen without knowing the CLI exists. It wraps the sibling
// `frame` observe pipeline (pixels + OCR + Accessibility tree) and hands the
// JPEG back as an inline image block plus a text digest of the OCR/AX/window
// state — no screencapture→Read shuffle, discoverable by name from any session.
//
// Native click/key exploration deliberately returns a fresh frame in the same
// response. Broader mutation (typing, shell, browser navigation) stays in
// puppet, keeping this loop narrow and visually self-verifying.
//
// Hand-rolled JSON-RPC over stdio (newline-delimited), matching the house style
// of artery/emacs-mcp.mjs and ants/mail-mcp — no SDK, only node builtins. It
// shells out to the sibling frame.mjs, so it needs no PATH setup and travels
// with the repo. The machine registry still lives in the untracked
// ~/.config/slab/puppet.json that `frame` already reads.
import { execFile } from "node:child_process";
import { readFileSync } from "node:fs";
import { readFile, unlink } from "node:fs/promises";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";
import { clickPoint, hoverPoint, sendKeys } from "./macos.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const FRAME = join(HERE, "frame.mjs");
const CONFIG_PATH = process.env.SLAB_PUPPET_CONFIG || join(process.env.HOME, ".config", "slab", "puppet.json");

function machineSpec(name) {
  if (!name) throw new Error("`machine` is required");
  let cfg;
  try { cfg = JSON.parse(readFileSync(CONFIG_PATH, "utf8")); } catch {}
  const spec = cfg?.machines?.[name];
  if (spec) return spec;
  // frame.mjs synthesizes the controller's hostname when it is absent from
  // puppet.json; `local` is the explicit portable spelling for that host.
  if (name === "local") return { local: true };
  throw new Error(`unknown machine: ${name} — see frame_list`);
}

const settle = (ms = 180) => new Promise((resolve) => setTimeout(resolve, ms));

// Run frame.mjs with args; resolve { stdout, stderr }. frame prints its JSON
// envelope (with --json) to stdout and writes the JPEG to --out, so a capture
// never base64s through this pipe — we read the file instead.
function runFrame(args, { timeoutMs = 30000 } = {}) {
  return new Promise((resolve, reject) => {
    execFile(
      process.execPath,
      [FRAME, ...args],
      { timeout: timeoutMs, maxBuffer: 64 * 1024 * 1024, encoding: "utf8" },
      (err, stdout, stderr) => {
        // frame exits non-zero for unreachable/unknown machines; surface stderr
        // as the error message rather than a bare exit code.
        if (err && !stdout) return reject(new Error((stderr || err.message).trim()));
        resolve({ stdout, stderr });
      },
    );
  });
}

// Fold the rich envelope into a compact, agent-legible digest. Keeps the pieces
// an agent acts on — capture status, frontmost app, screen size, and every OCR
// region + AX element with its CENTER coords (what `puppet` would click) — and
// drops the raw geometry noise.
function digest(env) {
  const L = [];
  L.push(`capture: ${env.capture}`);
  const m = env.meta || {};
  if (m.frontmost) L.push(`frontmost: ${m.frontmost.app} (${m.frontmost.bundle})`);
  if (m.screen) L.push(`screen: ${m.screen.w}×${m.screen.h} @${m.screen.scale}x`);
  if (Array.isArray(m.windows)) L.push(`windows: ${m.windows.length}`);

  const ocr = env.ocr || [];
  L.push(`\nOCR (${ocr.length} regions) — «text» @(cx,cy):`);
  for (const o of ocr) L.push(`  «${o.t}» @(${o.cx},${o.cy})`);

  const ax = env.ax?.elements || [];
  L.push(`\nAX (${ax.length} elements, trusted=${env.ax?.trusted}) — role «title» @(cx,cy) actions:`);
  for (const e of ax) {
    const title = (e.title || "").replace(/\s+/g, " ").slice(0, 60);
    const acts = (e.actions || []).join(",");
    L.push(`  ${e.role} «${title}» @(${e.cx},${e.cy})${acts ? ` [${acts}]` : ""}`);
  }
  const visual = env.visual || [];
  L.push(`\nVISUAL (${visual.length} compact controls) — kind @(cx,cy), distance from hover:`);
  for (const v of visual) L.push(`  ${v.kind} @(${v.cx},${v.cy}) d=${v.distance}`);
  const changes = env.diff || [];
  L.push(`\nDIFF (${changes.length} changed regions) — @(cx,cy), changed grid cells:`);
  for (const d of changes) L.push(`  change @(${d.cx},${d.cy}) cells=${d.cells}`);
  return L.join("\n");
}

// ── the capture tool: frame a machine, return image + digest ────────────────
async function toolFrame({ machine, ocr = true, fast = false, cursor = true, cursorAt, crop, baseline = false, diff = false } = {}) {
  if (!machine) throw new Error("`machine` is required (see frame_list)");
  const out = join(tmpdir(), `frame-mcp-${machine}-${process.pid}.jpg`);
  const args = [machine, "--json", "--out", out];
  if (!ocr) args.push("--no-ocr");
  if (fast) args.push("--fast");
  if (cursorAt) args.push("--cursor-at", `${cursorAt[0]},${cursorAt[1]}`);
  else if (cursor) args.push("--cursor");
  if (crop) args.push("--crop", crop.join(","));
  if (baseline) args.push("--baseline");
  if (diff) args.push("--diff");

  const { stdout } = await runFrame(args);
  let env;
  try {
    env = JSON.parse(stdout);
  } catch {
    throw new Error(`frame ${machine} returned no envelope — is SlabMenubar running there? (frame_doctor)`);
  }

  const content = [{ type: "text", text: digest(env) }];
  if (env.capture === "permission_needed") {
    content.push({
      type: "text",
      text: `\n⚠️  Screen Recording not granted to SlabMenubar on ${machine} — pixels + OCR are blocked (AX + window meta still captured). Run the frame_setup tool for ${machine} to fix, then re-frame.`,
    });
  } else {
    try {
      const jpg = await readFile(out);
      content.unshift({ type: "image", data: jpg.toString("base64"), mimeType: "image/jpeg" });
    } catch {
      /* no pixels on disk — digest already carries the text state */
    }
  }
  try { await unlink(out); } catch {}
  return content;
}

async function toolHover({ machine, x, y, width = 720, height = 520, ocr = true, fast = true }) {
  x = Number(x); y = Number(y);
  const crop = [Math.round(x - width / 2), Math.round(y - height / 2), Math.round(width), Math.round(height)];
  await toolFrame({ machine, ocr: false, cursor: false, crop, baseline: true });
  hoverPoint(machineSpec(machine), x, y);
  await settle(350);
  return toolFrame({ machine, ocr, fast, cursorAt: [x, y], crop, diff: true });
}

// Native exploration primitives return the post-action frame in the SAME MCP
// response. Agents need one tool round-trip, not act → wait → call frame again.
async function toolClick({ machine, x, y, count = 1, ocr = true, fast = true }) {
  clickPoint(machineSpec(machine), Number(x), Number(y), { count });
  await settle();
  return toolFrame({ machine, ocr, fast, cursorAt: [Number(x), Number(y)] });
}

async function toolKey({ machine, key, mod, ocr = true, fast = true }) {
  const mods = Array.isArray(mod) ? mod : (mod ? String(mod).split(",").filter(Boolean) : []);
  sendKeys(machineSpec(machine), key, mods);
  await settle();
  return toolFrame({ machine, ocr, fast, cursor: true });
}

async function toolList() {
  const { stdout } = await runFrame(["list"]);
  return [{ type: "text", text: stdout.trim() || "(no machines registered)" }];
}

async function toolDoctor({ machine } = {}) {
  const { stdout } = await runFrame(machine ? ["doctor", machine] : ["doctor"], { timeoutMs: 20000 });
  return [{ type: "text", text: stdout.trim() }];
}

async function toolSetup({ machine }) {
  if (!machine) throw new Error("`machine` is required");
  const { stdout } = await runFrame(["setup", machine], { timeoutMs: 30000 });
  return [{ type: "text", text: stdout.trim() }];
}

const TOOLS = [
  {
    name: "frame",
    description:
      "Capture a rich frame of a fleet Mac's screen — a downscaled JPEG (returned inline) plus a text digest: capture status, frontmost app, screen size, and every OCR text region and Accessibility element with its center (cx,cy) coordinates. Use this to VISUALLY verify anything on a remote Mac (menu-bar icons, app windows, UI state) instead of guessing over SSH. Machine names come from the frame_list tool. Requires SlabMenubar running + Screen Recording granted on the target (frame_doctor / frame_setup).",
    inputSchema: {
      type: "object",
      properties: {
        machine: { type: "string", description: "Machine name (e.g. neo, blueberry, local). See frame_list." },
        ocr: { type: "boolean", description: "Run OCR (default true). Set false for a faster pixels+AX-only frame." },
        fast: { type: "boolean", description: "Use Vision .fast OCR — lower latency, less accurate on small text (default false)." },
        cursor: { type: "boolean", description: "Draw a high-contrast virtual cursor at the current mouse position (default true)." },
      },
      required: ["machine"],
    },
  },
  {
    name: "frame_hover",
    description: "OBSERVES CONTEXT: move the real pointer without clicking, wait for hover-only controls/tooltips, then return a cheaper cropped reframe around that point. Lesson 1: when an element may reveal options, hover and reframe before clicking. Coordinates remain global and click-ready.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, x: { type: "number" }, y: { type: "number" }, width: { type: "number", description: "Crop width (default 720)." }, height: { type: "number", description: "Crop height (default 520)." }, ocr: { type: "boolean" }, fast: { type: "boolean" } }, required: ["machine", "x", "y"] },
  },
  {
    name: "frame_click",
    description: "ACTS + OBSERVES: click a native macOS screen coordinate from frame OCR/AX, then immediately return a fresh frame with a virtual marker at the click. Use for low-risk UI exploration; inspect labels and avoid destructive controls.",
    inputSchema: {
      type: "object",
      properties: {
        machine: { type: "string" }, x: { type: "number" }, y: { type: "number" },
        count: { type: "number", minimum: 1, maximum: 3, description: "Click count (default 1)." },
        ocr: { type: "boolean", description: "Include OCR in the returned frame (default true)." },
        fast: { type: "boolean", description: "Use fast OCR for the returned frame (default true)." },
      },
      required: ["machine", "x", "y"],
    },
  },
  {
    name: "frame_key",
    description: "ACTS + OBSERVES: send one navigation key/chord to the frontmost native app, then immediately return a fresh frame. Intended for reversible exploration such as tab, escape, arrows, space, and enter.",
    inputSchema: {
      type: "object",
      properties: {
        machine: { type: "string" }, key: { type: "string" },
        mod: { type: "string", description: "Optional comma-separated modifiers: cmd,shift,opt,ctrl." },
        ocr: { type: "boolean" }, fast: { type: "boolean" },
      },
      required: ["machine", "key"],
    },
  },
  {
    name: "frame_list",
    description: "List the fleet Macs registered for framing (from ~/.config/slab/puppet.json), with their ssh hosts.",
    inputSchema: { type: "object", properties: {} },
  },
  {
    name: "frame_doctor",
    description: "Report per-machine health for framing: whether SlabMenubar is running and whether Screen Recording is granted. Omit machine to sweep all.",
    inputSchema: {
      type: "object",
      properties: { machine: { type: "string", description: "Machine to check (optional; omit to check all)." } },
    },
  },
  {
    name: "frame_setup",
    description: "Trigger and guide the one-time Screen Recording grant for SlabMenubar on a machine (needed before frames include pixels + OCR). Returns the on-screen steps.",
    inputSchema: {
      type: "object",
      properties: { machine: { type: "string", description: "Machine to set up." } },
      required: ["machine"],
    },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "frame": return toolFrame(args || {});
    case "frame_hover": return toolHover(args || {});
    case "frame_click": return toolClick(args || {});
    case "frame_key": return toolKey(args || {});
    case "frame_list": return toolList();
    case "frame_doctor": return toolDoctor(args || {});
    case "frame_setup": return toolSetup(args || {});
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "frame-mcp", version: "1.0.0" },
          },
        };
      case "initialized":
      case "notifications/initialized":
        return null; // notification — no response
      case "ping":
        return { jsonrpc: "2.0", id, result: {} };
      case "tools/list":
        return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default:
        return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    // Tool failures come back as an error-flagged result (visible to the model)
    // rather than a protocol error, so the agent can read and react to them.
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

// stdio by default (Claude spawns one process per session), or `--http [port]`
// for one resident daemon every session shares — installed by
// toolchain/mcp/install-daemons.sh. Each capture shells out fresh, so there is
// no per-session state to keep.
const port = httpPort(process.argv, 7767);
if (port) serveHttp({ handleMessage, port, banner: "🖼  frame-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🖼  frame-mcp started (observe + native click/key exploration)" });
