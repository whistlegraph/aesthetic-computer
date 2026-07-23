// Bounded KidLisp-to-native-QuickJS bridge for the Xbox Native BIOS.
//
// This is intentionally a small compiler, not a second KidLisp runtime. It
// accepts only the drawing vocabulary the native host can faithfully execute
// and emits calls to the existing sandboxed QuickJS bindings. Published source
// is fetched from one fixed Aesthetic Computer endpoint; generated pieces gain
// no fetch, filesystem, DOM, process, or WinRT primitive.

import { extractAst } from "../../system/backend/kidlisp-ast.mjs";

const DEFAULT_ORIGIN = "https://aesthetic.computer";
const MAX_SOURCE_BYTES = 50_000;
const CODE_PATTERN = /^[0-9A-Za-z]{3,12}$/;
const PAINTING_PATTERN = /^#[0-9A-Za-z]{1,8}$/;

// Start with the CSS/KidLisp colors needed by the native proof. Extending this
// table is safe and mechanical; unsupported colors fail closed.
const COLORS = Object.freeze({
  black: [0, 0, 0],
  white: [255, 255, 255],
  red: [255, 0, 0],
  green: [0, 128, 0],
  blue: [0, 0, 255],
  lightblue: [173, 216, 230],
});

function normalizeCode(value) {
  const code = String(value || "").trim().replace(/^\$/, "");
  if (!CODE_PATTERN.test(code)) throw new Error("KidLisp code must be 3-12 alphanumeric characters");
  return code;
}

function tree(nodes) {
  const byParent = new Map();
  for (const node of nodes) {
    const key = node.parent ?? -1;
    if (!byParent.has(key)) byParent.set(key, []);
    byParent.get(key).push(node);
  }
  for (const children of byParent.values())
    children.sort((left, right) => left.position - right.position);
  const build = (node) => ({ ...node, children: (byParent.get(node.id) || []).map(build) });
  return (byParent.get(-1) || []).map(build);
}

function atom(node, label) {
  if (!node || node.kind === "call" || typeof node.literal !== "string")
    throw new Error(`${label} must be a literal`);
  return node.literal;
}

function number(node, label) {
  const value = Number(atom(node, label));
  if (!Number.isFinite(value)) throw new Error(`${label} must be a finite number`);
  return value;
}

function color(name) {
  const value = COLORS[String(name).toLowerCase()];
  if (!value) throw new Error(`unsupported native KidLisp color: ${name}`);
  return value;
}

function compileForm(form, paintings) {
  if (form.kind !== "call" || !form.op) throw new Error("native KidLisp expects top-level calls");
  const op = form.op.toLowerCase();
  const args = form.children;

  if (Object.hasOwn(COLORS, op) && args.length === 0) {
    const [r, g, b] = color(op);
    return `wipe(${r},${g},${b});`;
  }

  if (op === "wipe") {
    if (args.length === 1) {
      const [r, g, b] = color(atom(args[0], "wipe color"));
      return `wipe(${r},${g},${b});`;
    }
    if (args.length === 3) {
      const channels = args.map((item, index) => number(item, `wipe channel ${index + 1}`));
      if (channels.some((item) => item < 0 || item > 255))
        throw new Error("wipe channels must be between 0 and 255");
      return `wipe(${channels.map((item) => Math.round(item)).join(",")});`;
    }
    throw new Error("native wipe accepts a color or three RGB channels");
  }

  if (op === "stamp") {
    if (args.length < 1 || args.length > 4) throw new Error("native stamp accepts #code [x y scale]");
    const painting = atom(args[0], "stamp painting");
    if (!PAINTING_PATTERN.test(painting))
      throw new Error("native stamp accepts only a short #painting code");
    paintings.add(painting.slice(1));
    const x = args[1] ? number(args[1], "stamp x") : null;
    const y = args[2] ? number(args[2], "stamp y") : null;
    const scale = args[3] ? number(args[3], "stamp scale") : 1;
    if (Math.abs(scale) > 8) throw new Error("native stamp scale is limited to +/-8");
    // KidLisp chooses a fresh random center when x/y are omitted.
    return `stampPainting(${JSON.stringify(painting)},${x ?? "Math.random()*runtime().width"},${y ?? "Math.random()*runtime().height"},${scale});`;
  }

  if (op === "blur") {
    if (args.length !== 1) throw new Error("native blur accepts one strength");
    const strength = number(args[0], "blur strength");
    if (strength < 0 || strength > 48)
      throw new Error("native blur strength must be between 0 and 48");
    // Browser graph.blur scales KidLisp strength by three before choosing its
    // separable kernel. Preserve that visible radius for the native box pass.
    const radius = strength <= 0.1 ? 0 : Math.max(1, Math.floor(strength / 3));
    return `blur(${radius});`;
  }

  throw new Error(`unsupported native KidLisp form: ${form.op}`);
}

export function compileKidLispSource(source, { code = "local" } = {}) {
  if (typeof source !== "string" || !source.trim()) throw new Error("KidLisp source is empty");
  if (Buffer.byteLength(source, "utf8") > MAX_SOURCE_BYTES)
    throw new Error(`KidLisp source exceeds ${MAX_SOURCE_BYTES} bytes`);
  const forms = tree(extractAst(source));
  if (forms.length === 0 || forms.length > 256) throw new Error("native KidLisp requires 1-256 forms");
  const paintings = new Set();
  const body = forms.map((form) => compileForm(form, paintings));
  const safeCode = String(code).replace(/[^0-9A-Za-z_-]/g, "").slice(0, 24) || "local";
  const generated = [
    `// Generated from KidLisp $${safeCode}; native subset v1.`,
    `function boot(){telemetry("KIDLISP_BOOT",${JSON.stringify(`$${safeCode} forms=${forms.length} paintings=${[...paintings].join(",")}`)});}`,
    "function sim(){}",
    `function paint(){${body.join("")}}`,
    "function act(button){telemetry(\"KIDLISP_BUTTON\",button);}",
    "function leave(){telemetry(\"KIDLISP_LEAVE\",\"ok\");}",
    "",
  ].join("\n");
  return { code: safeCode, source, generated, formCount: forms.length, paintings: [...paintings] };
}

export async function fetchPublishedKidLisp(value, { fetchImpl = globalThis.fetch } = {}) {
  if (typeof fetchImpl !== "function") throw new Error("fetch is unavailable");
  const code = normalizeCode(value);
  const url = new URL("/api/store-kidlisp", DEFAULT_ORIGIN);
  url.searchParams.set("code", code);
  const response = await fetchImpl(url, {
    headers: { accept: "application/json", "user-agent": "AC-Xbox-KidLisp-Bridge/1" },
    redirect: "error",
  });
  if (!response.ok) throw new Error(`KidLisp $${code} fetch failed with HTTP ${response.status}`);
  const declared = Number(response.headers.get("content-length") || 0);
  if (declared > MAX_SOURCE_BYTES * 2) throw new Error("KidLisp response is too large");
  const text = await response.text();
  if (Buffer.byteLength(text, "utf8") > MAX_SOURCE_BYTES * 2)
    throw new Error("KidLisp response is too large");
  const payload = JSON.parse(text);
  if (!payload || typeof payload.source !== "string") throw new Error(`KidLisp $${code} has no source`);
  return { code, source: payload.source, handle: payload.handle || "anon", when: payload.when || null };
}

export async function compilePublishedKidLisp(value, options = {}) {
  const published = await fetchPublishedKidLisp(value, options);
  return { ...published, ...compileKidLispSource(published.source, { code: published.code }) };
}

export const __testing = { COLORS, MAX_SOURCE_BYTES, normalizeCode };
