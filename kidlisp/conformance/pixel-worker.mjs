// One isolate per replay: graph.mjs owns mutable renderer state.
import { parentPort, workerData } from "node:worker_threads";
import { KidLisp } from "../../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { KidLispExecution, KidLispExecutionError } from "../../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";
import { numericOperationNames } from "../../system/public/aesthetic.computer/lib/kidlisp-ops.mjs";
import { cssColors } from "../../system/public/aesthetic.computer/lib/num.mjs";
import * as graph from "../../system/public/aesthetic.computer/lib/graph.mjs";
import { validateReplayForms } from "./replay.mjs";

const operations = new Set([...numericOperationNames(), "wipe", "ink", "line", "box", "circle", "point", "clock", "random", "?", "repeat", "def", "tap", "draw", "lift", "if", "once", "frame", "width", "height", "w", "h", "no", "yes", "=", ">", "<", "...", "fill", "outline"]);
const fail = message => { throw new KidLispExecutionError("PIXEL_CAPABILITY", message); };
const diagnostics = [];
// The evaluator catches some failures. A warning/error must still fail the run.
console.warn = console.error = (...args) => diagnostics.push(args.map(String).join(" ").slice(0, 1000));
console.log = console.info = console.debug = () => {};

function run(fixture) {
  const { source, seed, epochMs, stepMs, width, height, frames, events, maxSteps, maxDepth, monitor } = fixture;
  const execution = new KidLispExecution({ seed, epochMs, stepMs, maxSteps, maxDepth });
  const lisp = new KidLisp({ execution });
  const ast = lisp.parse(source);
  validateReplayForms(ast, { operations, profile: "pixels-v1" });
  const values = new Set([...operations, "penx", "peny", "down", "erase", "fill", "outline", ...Object.keys(cssColors)]);
  const bindings = form => {
    if (!Array.isArray(form)) return;
    if (form[0] === "def" && typeof form[1] === "string") values.add(form[1]);
    if (form[0] === "repeat" && form.length >= 4 && typeof form[2] === "string") values.add(form[2]);
    form.forEach(bindings);
  };
  ast.forEach(bindings);
  // These values can reach live clocks or unseeded randomness before a host
  // drawing call. Keep them outside this profile, including quoted spellings.
  const validateValue = value => {
    if (Array.isArray(value)) return value.forEach(validateValue);
    if (typeof value !== "string") return;
    const token = value.replace(/^"|"$/g, "");
    if (/^(?:fade:|rainbow|zebra|p\d+$|c\d+$)/i.test(token)) fail(`Unsupported pixel color: ${token}`);
    if (!values.has(token) && !/^\d*\.?\d+s(?:!|\.{2,3})?$/.test(token) && !/^(?:#|0x)?[\da-f]{6}$/i.test(token)) fail(`Unsupported pixel value: ${token}`);
  };
  ast.forEach(validateValue);
  const piece = lisp.module(source, true);
  if (!piece || !lisp.ast || lisp.lastValidationErrors?.length) fail("Pixel replay requires valid source");
  lisp.cacheInitiated = true; // No publishing, source lookup, HUD, or network host.
  lisp.perf.enabled = monitor;
  const display = { width, height, pixels: new Uint8ClampedArray(width * height * 4) };
  const commands = [];
  graph.twoD(commands);

  function color(args) {
    while (args.length === 1 && Array.isArray(args[0])) args = args[0];
    args = [...args];
    // The browser's unspecified ink uses host randomness. This profile routes
    // that same choice through the instance seed; never patch Math.random.
    if (!args.length || args[0] === undefined) {
      const alpha = args[1] ?? 255;
      args = [0, 0, 0].map(() => Math.floor(lisp.seededRandom() * 256)).concat(alpha);
    }
    if (typeof args[0] === "string") {
      const name = args[0];
      if (!(Object.hasOwn(cssColors, name) || name === "erase" || /^(?:#|0x)?[\da-f]{6}$/i.test(name))) fail(`Unsupported pixel color: ${name}`);
      if (args.length > 2) fail("Named colors accept only an optional alpha");
    } else if (typeof args[0] !== "number" && typeof args[0] !== "boolean") fail("Invalid pixel color");
    if (args.length > 4 || args.some((v, i) => (i > 0 || typeof v === "number") && !Number.isFinite(v))) fail("Color channels must be finite numbers");
    const resolved = graph.findColor(...args);
    if (resolved.length !== 4 || !resolved.every(Number.isFinite)) fail("Color did not resolve to RGBA");
    graph.color(...resolved);
  }
  const coordinates = (name, values, count) => {
    if (values.length < count || values.slice(0, count).some(n => !Number.isFinite(n) || Math.abs(n) > 8192)) fail(`${name} needs finite coordinates within ±8192`);
  };
  const api = {
    screen: { ...display }, params: [], colon: [], clock: execution.clock,
    system: { fps: 60 }, sound: {},
    fps() {}, needsPaint() {}, toggleHUD() {},
    page(buffer) { Object.assign(api.screen, buffer); graph.setBuffer(buffer); },
    ink(...args) { color(args); return api; },
    inkrn: () => graph.color(),
    wipe(...args) {
      const saved = graph.color();
      try { color(args.length ? args : [255]); graph.clear(); }
      finally { graph.color(...saved); }
      return api;
    },
    backgroundFill(...args) { return api.wipe(...args); },
    blend(mode) {
      if (!["blend", "erase"].includes(mode)) fail(`Unsupported blend: ${mode}`);
      return graph.blendMode(mode);
    },
    unmask: graph.unmask,
    paste: graph.paste,
    line(...args) {
      coordinates("line", args, 4);
      if (args.length > 5 || (args[4] !== undefined && (!Number.isFinite(args[4]) || args[4] < 0 || args[4] > 1024))) fail("Invalid line thickness");
      return graph.line(...args);
    },
    box(...args) {
      coordinates("box", args, 4);
      if (args.length > 5 || ![undefined, "fill", "outline"].includes(args[4])) fail("Unsupported box mode");
      return graph.box(...args);
    },
    circle(...args) {
      coordinates("circle", args, 3);
      if (args[2] < 0 || args[2] > 1024 || args.length > 4 || ![undefined, true, false, "fill", "outline"].includes(args[3])) fail("Unsupported circle radius or mode");
      return graph.circle(...args);
    },
    point(...args) { coordinates("point", args, 2); if (args.length !== 2) fail("Invalid point"); return graph.point(...args); },
    plot(...args) { coordinates("plot", args, 2); if (args.length !== 2) fail("Invalid plot"); return graph.plot(...args); },
    send() { fail("Pixel replay has no messaging host"); },
  };
  api.page(display);
  lisp.setAPI(api);
  const output = [];
  const check = () => {
    if (execution.state.error) throw execution.state.error;
    if (diagnostics.length) throw new KidLispExecutionError("PIXEL_RENDER", diagnostics.join("\n"));
  };
  try {
    execution.beginFrame(0);
    piece.boot(execution.bindApi(api));
    check();
    for (let frame = 0; frame < frames; frame++) {
      execution.beginFrame(frame);
      commands.length = 0;
      graph.writeBuffer.length = 0;
      api.page(display);
      const bound = execution.bindApi(api);
      for (const event of events.filter(e => e.frame === frame)) {
        piece.act({ api: bound, event: { ...event, delta: { x: event.dx, y: event.dy }, is: type => event.type === type } });
      }
      // One simulation step, then one paint. The module's frame counter starts
      // at one; the fixture and its clock use zero-based frame indices.
      piece.sim(bound);
      piece.paint(bound);
      check();
      if (api.screen.pixels !== display.pixels) fail("Paint did not restore the display buffer");
      output.push({ frame, timeMs: execution.timeMs, rgba: display.pixels.slice() });
    }
  } finally { piece.leave?.(); }
  return { version: 1, profile: "pixels-v1", width, height, seed, epochMs, stepMs, frames: output };
}

try {
  const result = run(workerData);
  parentPort.postMessage({ result }, result.frames.map(frame => frame.rgba.buffer));
} catch (error) {
  parentPort.postMessage({ error: { name: error.name, message: error.message, code: error.code, frame: error.frame, steps: error.steps } });
}
