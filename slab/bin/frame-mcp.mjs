#!/usr/bin/env node
// frame-mcp.mjs — a thin MCP server over `frame` (frame.mjs), so any agent can
// SEE a fleet Mac's focused window without knowing the CLI exists. It wraps the sibling
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
import { randomUUID } from "node:crypto";
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

// Mutations such as Fuser replica creation can take ~9 seconds before emitting
// a short-lived toast. Keep watching beyond that boundary so "between action"
// state cannot disappear before the next agent observation.
const ACTION_SAMPLE_TIMES = [120, 500, 1100, 2200, 4000, 6500, 9000, 12000, 15000];
const MAX_TRAIL_TEXT = 24;

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
  L.push(`scope: ${env.capture_scope || "screen"}`);
  if (env.crop) L.push(`frame region: ${env.crop.w}×${env.crop.h} @(${env.crop.x},${env.crop.y}) — coordinates below are global`);
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
const stagedClicks = new Map();
const recentActionTrails = new Map();

async function captureFrame({ machine, ocr = true, fast = false, screen = false, cursor = true, cursorAt, targetAt, pressAt, pressCount = 1, pressTitle, actionOnly = false, clearTarget = false, crop, baseline = false, diff = false } = {}) {
  if (!machine) throw new Error("`machine` is required (see frame_list)");
  // A unique path matters now that an action trail may capture while another
  // session asks for a normal frame of the same machine.
  const out = join(tmpdir(), `frame-mcp-${machine}-${process.pid}-${randomUUID()}.jpg`);
  const args = [machine, "--json", "--out", out];
  if (screen) args.push("--screen");
  if (!ocr) args.push("--no-ocr");
  if (fast) args.push("--fast");
  if (cursorAt) args.push("--cursor-at", `${cursorAt[0]},${cursorAt[1]}`);
  else if (cursor) args.push("--cursor");
  if (targetAt) args.push("--target-at", `${targetAt[0]},${targetAt[1]}`);
  if (pressAt) args.push("--press-at", `${pressAt[0]},${pressAt[1]}`, "--press-count", String(pressCount));
  if (pressTitle) args.push("--press-title", String(pressTitle));
  if (actionOnly) args.push("--action-only");
  if (clearTarget) args.push("--clear-target");
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

  let jpg;
  if (env.capture === "permission_needed") {
    // The envelope remains useful: Accessibility can still preserve a DOM-like
    // state even when screen pixels are unavailable.
  } else {
    try {
      jpg = await readFile(out);
    } catch {
      /* no pixels on disk — the envelope still carries text state */
    }
  }
  try { await unlink(out); } catch {}
  return { env, jpg };
}

function frameContent({ env, jpg }, machine) {
  const content = [{ type: "text", text: digest(env) }];
  if (env.capture === "permission_needed") {
    content.push({
      type: "text",
      text: `\n⚠️  Screen Recording not granted to SlabMenubar on ${machine} — pixels + OCR are blocked (AX + window meta still captured). Run the frame_setup tool for ${machine} to fix, then re-frame.`,
    });
  } else if (jpg) {
    content.unshift({ type: "image", data: jpg.toString("base64"), mimeType: "image/jpeg" });
  }
  return content;
}

async function toolFrame(options = {}) {
  return frameContent(await captureFrame(options), options.machine);
}

function visibleText(env) {
  const values = [];
  for (const item of env.ocr || []) values.push(item.t);
  for (const item of env.ax?.elements || []) values.push(item.title);
  const byKey = new Map();
  for (const raw of values) {
    const text = String(raw || "").replace(/\s+/g, " ").trim();
    if (text.length < 2) continue;
    const key = text.toLocaleLowerCase();
    if (!byKey.has(key)) byKey.set(key, text);
  }
  return byKey;
}

function clickTargetPrediction(env, x, y) {
  let best;
  const interactiveRoles = new Set(["AXButton", "AXLink", "AXCheckBox", "AXRadioButton", "AXPopUpButton"]);
  for (const item of env.ax?.elements || []) {
    const title = String(item.title || "").replace(/\s+/g, " ").trim();
    const rect = Array.isArray(item.r) && item.r.length === 4 ? item.r.map(Number) : null;
    const validRect = rect?.every(Number.isFinite);
    const contains = validRect && x >= rect[0] && x <= rect[0] + rect[2] &&
      y >= rect[1] && y <= rect[1] + rect[3];
    const edgeDistance = validRect ? Math.hypot(
      Math.max(rect[0] - x, 0, x - rect[0] - rect[2]),
      Math.max(rect[1] - y, 0, y - rect[1] - rect[3]),
    ) : Infinity;
    const centerDistance = Math.hypot(Number(item.cx) - x, Number(item.cy) - y);
    const interactive = interactiveRoles.has(item.role) || (item.actions || []).includes("AXPress");
    let probability = 0;
    let basis = "nearby accessibility element";
    if (contains && interactive) { probability = 0.98; basis = "point inside an actionable AX control"; }
    else if (contains) { probability = 0.82; basis = "point inside a bounded AX element"; }
    else if (edgeDistance <= 8 && interactive) { probability = 0.9; basis = "point beside an actionable AX control"; }
    else if (centerDistance <= 120 && interactive) { probability = 0.68; basis = "nearby actionable AX control"; }
    else if (centerDistance <= 120 && title) { probability = 0.48; }
    if (!best || probability > best.probability ||
        (probability === best.probability && centerDistance < best.distance)) {
      best = { title, role: item.role, probability, basis, distance: centerDistance };
    }
  }
  if ((best?.probability || 0) > 0) {
    return { ...best, title: best.title.slice(0, 80) || null };
  }
  for (const item of env.ocr || []) {
    const rect = Array.isArray(item.r) && item.r.length === 4 ? item.r.map(Number) : null;
    if (!rect?.every(Number.isFinite)) continue;
    if (x >= rect[0] && x <= rect[0] + rect[2] && y >= rect[1] && y <= rect[1] + rect[3]) {
      return {
        title: String(item.t || "").replace(/\s+/g, " ").trim().slice(0, 80) || null,
        role: "OCRText", probability: 0.45, basis: "point inside OCR text",
      };
    }
  }
  return { title: null, role: null, probability: 0.2, basis: "unresolved point fallback" };
}

function choiceBox(label, probability) {
  const question = `${label.replace(/[?\s]+$/g, "")}?`;
  const confidence = `Target confidence ${Math.round(probability * 100)}% · p=${probability.toFixed(2)}`;
  let innerWidth = Math.max(29, Math.min(59, Math.max(question.length, confidence.length) + 14));
  // Matching parity lets the title have exactly equal padding on both sides.
  if ((innerWidth - question.length) % 2 !== 0) innerWidth += 1;
  const centered = (text) => {
    const room = Math.max(0, innerWidth - text.length);
    const left = Math.floor(room / 2);
    return `██${" ".repeat(left)}${text}${" ".repeat(room - left)}██`;
  };
  const choices = Array(innerWidth).fill(" ");
  const place = (text, center) => {
    const start = Math.round(center - text.length / 2);
    for (let i = 0; i < text.length; i += 1) choices[start + i] = text[i];
  };
  place("Yes", innerWidth * 0.27);
  place("No", innerWidth * 0.73);
  const edge = "█".repeat(innerWidth + 4);
  return [
    edge,
    centered(""),
    centered(""),
    centered(question),
    centered(""),
    centered(confidence),
    centered(""),
    `██${choices.join("")}██`,
    centered(""),
    edge,
  ].join("\n");
}

function trailText(trail) {
  const lines = [`ACTION TRAIL — ${trail.label}`];
  for (const sample of trail.samples) {
    const changes = [];
    for (const text of sample.added.slice(0, MAX_TRAIL_TEXT)) changes.push(`+ ${text}`);
    for (const text of sample.removed.slice(0, 8)) changes.push(`− ${text}`);
    if (!changes.length) continue;
    lines.push(`\n${sample.atMs}ms`);
    lines.push(...changes.map((line) => `  ${line}`));
  }
  if (lines.length === 1) lines.push("\nNo visible text/accessibility changes were detected.");
  lines.push(`\nRecorded ${trail.samples.length} states over ${trail.durationMs}ms. + appeared, − disappeared.`);
  return lines.join("\n");
}

function sampleScore(sample) {
  const changed = [...sample.added, ...sample.removed].join(" ");
  const important = /error|failed|failure|invalid|unable|denied|exception|warning/i.test(changed);
  return sample.added.length + sample.removed.length + (important ? 1000 : 0);
}

async function recordActionTrail({ machine, label, baselineText, cursorAt, clearTarget = false, ocr = true, fast = true }) {
  const started = Date.now();
  let previous = baselineText || new Map();
  const samples = [];
  let representative;
  for (const targetMs of ACTION_SAMPLE_TIMES) {
    await settle(Math.max(0, targetMs - (Date.now() - started)));
    const capture = await captureFrame({
      machine, ocr, fast, cursorAt,
      clearTarget: clearTarget && samples.length === 0,
    });
    const current = visibleText(capture.env);
    const added = [...current].filter(([key]) => !previous.has(key)).map(([, text]) => text);
    const removed = [...previous].filter(([key]) => !current.has(key)).map(([, text]) => text);
    const sample = { atMs: Date.now() - started, added, removed, capture };
    samples.push(sample);
    if (!representative || sampleScore(sample) > sampleScore(representative)) representative = sample;
    previous = current;
    if ([...added, ...removed].some((text) => /replica (created|failed)|error|failure|exception/i.test(text))) break;
  }
  const trail = {
    machine,
    label,
    recordedAt: new Date().toISOString(),
    durationMs: Date.now() - started,
    samples,
    representative,
  };
  recentActionTrails.set(machine, trail);
  return trail;
}

function actionTrailContent(trail) {
  const content = [{ type: "text", text: trailText(trail) }];
  const capture = trail.representative?.capture;
  if (capture?.jpg) content.unshift({ type: "image", data: capture.jpg.toString("base64"), mimeType: "image/jpeg" });
  if (capture?.env) content.push({ type: "text", text: `\nREPRESENTATIVE STATE @ ${trail.representative.atMs}ms\n${digest(capture.env)}` });
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

async function toolStageClick({ machine, x, y, count = 1, label, ocr = true, fast = true }) {
  x = Number(x); y = Number(y); count = Number(count);
  if (!Number.isFinite(x) || !Number.isFinite(y)) throw new Error("x and y must be finite numbers");
  if (![1, 2, 3].includes(count)) throw new Error("count must be 1, 2, or 3");
  const approvalId = randomUUID();
  hoverPoint(machineSpec(machine), x, y);
  await settle(80);
  const capture = await captureFrame({ machine, ocr, fast, cursorAt: [x, y], targetAt: [x, y] });
  const target = clickTargetPrediction(capture.env, x, y);
  label = String(label || target.title || "Confirm click").trim();
  stagedClicks.set(machine, {
    approvalId, x, y, count, label,
    targetTitle: target.title,
    targetConfidence: target.probability,
    targetBasis: target.basis,
    baselineText: visibleText(capture.env),
  });
  const content = frameContent(capture, machine);
  content.push({ type: "text", text: `\n\`\`\`text\n${choiceBox(label, target.probability)}\n\`\`\`\napproval_id: ${approvalId}\ntarget_confidence: ${target.probability.toFixed(2)} (${Math.round(target.probability * 100)}%)\ntarget_basis: ${target.basis}` });
  return content;
}

async function toolCommitClick({ machine, approvalId, ocr = true, fast = true }) {
  const pending = stagedClicks.get(machine);
  if (!pending || pending.approvalId !== approvalId) {
    throw new Error("No matching staged click. Stage it again so the human can inspect the current target.");
  }
  // Run the approved action inside the Accessibility-trusted native process.
  // It uses AXPress for semantic controls and a physical click fallback for
  // canvases/custom surfaces, after synchronously removing its own overlay.
  await captureFrame({
    machine,
    ocr: false,
    fast: true,
    cursor: false,
    clearTarget: true,
    pressAt: [pending.x, pending.y],
    pressCount: pending.count,
    pressTitle: pending.targetTitle,
    actionOnly: true,
  });
  stagedClicks.delete(machine);
  const trail = await recordActionTrail({
    machine,
    label: pending.label,
    baselineText: pending.baselineText,
    cursorAt: [pending.x, pending.y],
    clearTarget: false,
    ocr,
    fast,
  });
  return actionTrailContent(trail);
}

async function toolActionTrail({ machine }) {
  const trail = recentActionTrails.get(machine);
  if (!trail) throw new Error(`No recorded action trail for ${machine}.`);
  return actionTrailContent(trail);
}

async function toolRejectClick({ machine, approvalId, ocr = true, fast = true }) {
  const pending = stagedClicks.get(machine);
  if (!pending || pending.approvalId !== approvalId) {
    throw new Error("No matching staged click to reject.");
  }
  stagedClicks.delete(machine);
  const content = await toolFrame({ machine, ocr, fast, cursor: false, clearTarget: true });
  content.push({ type: "text", text: `\nREJECTED CLICK ${approvalId} — no click occurred.` });
  return content;
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
      "Capture a rich frame of a fleet Mac's focused/frontmost window by default — an isolated JPEG (returned inline) plus a text digest with OCR and Accessibility centers in global, click-ready screen coordinates. Set screen=true only when the complete display or menu bar context is needed. Machine names come from frame_list. Requires SlabMenubar running + Screen Recording granted on the target (frame_doctor / frame_setup).",
    inputSchema: {
      type: "object",
      properties: {
        machine: { type: "string", description: "Machine name (e.g. neo, blueberry, local). See frame_list." },
        screen: { type: "boolean", description: "Capture the complete display instead of the focused window (default false)." },
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
    name: "frame_stage_click",
    description: "HUMAN-IN-THE-LOOP: stage, but do not perform, a native click. Dims the display and outlines the full target control (point fallback), returning an approval_id. Ask the human for y to click or n to cancel before calling frame_commit_click or frame_reject_click.",
    inputSchema: {
      type: "object",
      properties: {
        machine: { type: "string" }, x: { type: "number" }, y: { type: "number" },
        count: { type: "number", minimum: 1, maximum: 3, description: "Click count to perform only after approval (default 1)." },
        label: { type: "string", description: "Short human-facing action label, e.g. Create replica." },
        ocr: { type: "boolean" }, fast: { type: "boolean" },
      },
      required: ["machine", "x", "y"],
    },
  },
  {
    name: "frame_commit_click",
    description: "APPROVE a staged click. Performs only the bound click, then records a short OCR + Accessibility timeline so transient toasts and errors survive between actions.",
    inputSchema: {
      type: "object",
      properties: { machine: { type: "string" }, approvalId: { type: "string" }, ocr: { type: "boolean" }, fast: { type: "boolean" } },
      required: ["machine", "approvalId"],
    },
  },
  {
    name: "frame_action_trail",
    description: "Replay the most recent approved-click recording for a machine: changed OCR/Accessibility text over time plus the most informative captured frame.",
    inputSchema: {
      type: "object",
      properties: { machine: { type: "string" } },
      required: ["machine"],
    },
  },
  {
    name: "frame_reject_click",
    description: "REJECT a previously staged click. Clears the blinking marker and guarantees that no click occurs.",
    inputSchema: {
      type: "object",
      properties: { machine: { type: "string" }, approvalId: { type: "string" }, ocr: { type: "boolean" }, fast: { type: "boolean" } },
      required: ["machine", "approvalId"],
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
    case "frame_stage_click": return toolStageClick(args || {});
    case "frame_commit_click": return toolCommitClick(args || {});
    case "frame_reject_click": return toolRejectClick(args || {});
    case "frame_action_trail": return toolActionTrail(args || {});
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
