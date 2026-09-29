// fiapup's web shell: the console's bindings, in a browser, plus the ones
// the console doesn't have — `touches()` and `haptic()`. The game draws
// through its own copy of the frame interpreter into `triangle3d` (the
// depth-tested WebGL scene oskiewar uses) and `write`; it reads `gamepad(0)`
// and `touches()`. With fur on (`?fur`, or start({ fur: true })) the shell
// takes the frame program itself (`frame`), draws the yard flat and the pup
// in fur (fur.mjs), and combs the fur where a finger strokes it.
//
// The stage is 1080 units tall and as wide as the window's shape: a phone
// held upright is about 500 × 1080, a laptop 1920 × 1080. The game frames
// itself by that shape and keeps its HUD out of the safe-area insets.
import WebGLOskiewarScene3D from "./live/scene3d-webgl.mjs";
import { createFrameVm } from "./live/frame-vm.mjs";
import { createFur, furry, octa, pickPrim, sketchPrims, surface } from "./fur.mjs";

export async function start(options = {}) {
  const H = 1080;
  let W = 1920;
  const frameEl = document.querySelector("#frame");
  const sceneCanvas = document.querySelector("#scene");
  const textCanvas = document.querySelector("#text");
  const ctx = textCanvas.getContext("2d");
  const scene = new WebGLOskiewarScene3D(sceneCanvas);
  let clear = [0, 0, 0, 1];
  const safe = { top: 0, right: 0, bottom: 0, left: 0 };

  // A keyboard legend only where there's likely a keyboard.
  const touchFirst = navigator.maxTouchPoints > 0 || matchMedia("(pointer: coarse)").matches;
  document.body.classList.toggle("keys", !touchFirst && !new URLSearchParams(location.search).has("nokeys"));

  function fit() {
    const rect = frameEl.getBoundingClientRect();
    W = Math.max(480, Math.min(2880, Math.round(H * rect.width / Math.max(1, rect.height))));
    scene.setLogicalSize(W, H);
    const dpr = Math.min(2, devicePixelRatio || 1);
    const w = Math.max(1, Math.round(rect.width * dpr)), h = Math.max(1, Math.round(rect.height * dpr));
    scene.resize(w, h);
    if (textCanvas.width !== w) textCanvas.width = w;
    if (textCanvas.height !== h) textCanvas.height = h;
    const probe = getComputedStyle(document.querySelector("#safe")), k = H / Math.max(1, rect.height);
    for (const edge of ["top", "right", "bottom", "left"])
      safe[edge] = (parseFloat(probe["padding-" + edge]) || 0) * k;
  }
  addEventListener("resize", fit);
  addEventListener("orientationchange", () => setTimeout(fit, 50));
  fit();

  // ——— the bindings ———
  const t0 = performance.now();
  const host = {
    runtime: () => ({ width: W, height: H, safe, perPoint: H / Math.max(1, frameEl.clientHeight), monotonicUs: Math.round((performance.now() - t0) * 1000) }),
    triangle3d: (...a) => scene.triangle3d(...a),
    wipe: (r, g, b) => { clear = [r / 255, g / 255, b / 255, 1]; },
    box: (x, y, w, h, r, g, b) => { ctx.fillStyle = `rgb(${r},${g},${b})`; ctx.fillRect(x, y, w, h); },
    line: () => {},
    write: (s, x, y, size = 32, r = 255, g = 255, b = 255) => {
      ctx.fillStyle = `rgb(${r},${g},${b})`;
      ctx.font = `600 ${size}px ui-rounded, "Comic Sans MS", system-ui, sans-serif`;
      ctx.textBaseline = "top";
      ctx.fillText(s, x, y);
    },
    synth, gamepad, touches, haptic,
  };

  // ——— sound: one sine with a quick decay, like the console's synth ———
  let audio = null;
  const unlock = () => { audio ||= new AudioContext(); if (audio.state !== "running") audio.resume?.(); };
  for (const type of ["keydown", "pointerdown", "touchend"]) addEventListener(type, unlock);
  function synth(frequency, duration = .1) {
    if (!audio || audio.state !== "running") return;
    const at = audio.currentTime, osc = audio.createOscillator(), gain = audio.createGain();
    osc.frequency.value = frequency;
    gain.gain.setValueAtTime(.18, at);
    gain.gain.exponentialRampToValueAtTime(.0001, at + duration + .05);
    osc.connect(gain).connect(audio.destination);
    osc.start(at); osc.stop(at + duration + .06);
  }

  // ——— haptics: the iOS app's bridge, a vibration elsewhere, or nothing ———
  function haptic(kind) {
    const bridge = globalThis.webkit?.messageHandlers?.haptic;
    if (bridge) bridge.postMessage(kind);
    else if (kind !== "soft") navigator.vibrate?.(kind === "medium" ? 14 : 8);
  }

  // ——— touches: every finger (or the mouse, while pressed), in stage units ———
  const fingers = new Map();
  function stagePoint(e) {
    const rect = frameEl.getBoundingClientRect();
    return { x: (e.clientX - rect.left) * W / rect.width, y: (e.clientY - rect.top) * H / rect.height };
  }
  frameEl.addEventListener("pointerdown", (e) => {
    frameEl.setPointerCapture?.(e.pointerId);
    fingers.set(e.pointerId, { id: e.pointerId, ...stagePoint(e) });
    e.preventDefault();
  });
  frameEl.addEventListener("pointermove", (e) => {
    if (fingers.has(e.pointerId)) fingers.set(e.pointerId, { id: e.pointerId, ...stagePoint(e) });
  });
  for (const type of ["pointerup", "pointercancel", "lostpointercapture"])
    frameEl.addEventListener(type, (e) => fingers.delete(e.pointerId));
  addEventListener("contextmenu", (e) => e.preventDefault());
  addEventListener("gesturestart", (e) => e.preventDefault());   // Safari's pinch
  function touches() { return [...fingers.values()]; }

  // ——— the pad: an Xbox controller if there is one, else the keyboard ———
  const keys = new Set();
  const keyMap = { KeyW: "ArrowUp", ArrowUp: "ArrowUp", KeyS: "ArrowDown", ArrowDown: "ArrowDown",
    KeyA: "ArrowLeft", ArrowLeft: "ArrowLeft", KeyD: "ArrowRight", ArrowRight: "ArrowRight",
    Space: "A", Enter: "A", KeyC: "B", KeyT: "X", KeyR: "Y", Escape: "Menu" };
  addEventListener("keydown", (e) => { const k = keyMap[e.code]; if (k) { keys.add(k); e.preventDefault(); } });
  addEventListener("keyup", (e) => { const k = keyMap[e.code]; if (k) keys.delete(k); });
  addEventListener("blur", () => { keys.clear(); fingers.clear(); });
  const padNames = ["A", "B", "X", "Y", "LeftShoulder", "RightShoulder", "LeftTrigger", "RightTrigger",
    "View", "Menu", "LeftThumb", "RightThumb", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight"];
  function gamepad() {
    const down = new Set(keys);
    const pad = [...(navigator.getGamepads?.() || [])].find((p) => p?.connected);
    pad?.buttons.forEach((b, i) => { if (b.pressed && padNames[i]) down.add(padNames[i]); });
    const stick = (v) => (Math.abs(v || 0) > .2 ? v : 0);
    return { connected: true, down: [...down], leftX: stick(pad?.axes[0]), leftY: -stick(pad?.axes[1]) };
  }

  // ——— fur: the pup's SKETCH ops drawn as fur, the rest flat (fur.mjs) ———
  const params = new URLSearchParams(location.search);
  const useFur = options.fur ?? params.has("fur");
  let fur = null, furPrims = [], lastCamera = null, furTiles = new Map(), game = null;
  const shapes = new Map(), springs = new Map();
  let ruffle = 0, lastT = performance.now();
  if (useFur) {
    const flat = createFrameVm({ triangle3d: host.triangle3d, box: host.box, line: host.line, wipe: host.wipe,
      write: host.write });
    // Quality: ?fur=low (8 shells), ?fur=high (24), else 16; it also steps
    // down by itself (16 → 12 → 8) when frames come slower than ~50 fps.
    const quality = { low: 8, high: 24 }[params.get("fur")] || 16;
    fur = createFur(scene.gl, { shells: Number(params.get("shells")) || options.shells || quality });
    host.frame = (program, length, strings) => {
      // Walk the ops: keep SHAPES records, take the pup's all-ball-and-limb
      // sketches for fur (their handle is blanked so the flat pass skips them).
      const pup = new Set(game?.fiapup.pupHandles() || []);
      furPrims = [];
      let at = 0;
      for (const { op, args } of flat.decode(program, length, strings)) {
        if (op === 9) lastCamera = args.slice(0, 24);
        if (op === 18) shapes.set(args[0], { count: args[1], records: Float64Array.from(args.slice(3)) });
        if (op === 19 && pup.has(args[0])) {
          const sk = shapes.get(args[0]);
          if (sk && furry(sk.records, sk.count)) {
            const start = furPrims.length;
            sketchPrims(sk.records, sk.count, args.slice(1, 13), null, furPrims);
            for (let i = start; i < furPrims.length; i++) {
              const key = `${args[0]}:${i - start}`;
              if (!furTiles.has(key)) furTiles.set(key, furTiles.size);
              furPrims[i].tile = furTiles.get(key);
              furPrims[i].key = key;
            }
            program[at + 1] = -1;
          }
        }
        at += args.length + 1;
      }
      surface(furPrims);
      flat.run(program, length, strings);
    };
  }

  // The comb follows the finger over the fur: the ray picks a primitive and a
  // point on it; consecutive points on the same primitive give the direction.
  let combing = null;
  function combAt(e) {
    if (!fur || !lastCamera || !frameEl.hasPointerCapture?.(e.pointerId)) return;
    const at = stagePoint(e), hit = pickPrim(furPrims, lastCamera, at.x, at.y);
    if (!hit) { combing = null; return; }
    const o = octa(hit.local), tile = furPrims[hit.index].tile;
    if (combing && combing.tile === tile) {
      const dir = [o[0] - combing.o[0], o[1] - combing.o[1]];
      if (Math.hypot(...dir) > .002) fur.comb(tile, o, dir, .45 + .5 * (e.pressure || .5));
    }
    combing = { tile, o };
  }
  frameEl.addEventListener("pointermove", combAt);
  frameEl.addEventListener("pointerup", () => { combing = null; });

  // Motion: each part's tips lag behind it on a damped spring; running ruffles.
  function furMotion(dt) {
    const pup = game.fiapup.world.pup;
    const wild = pup.speed > 170 || pup.state === "zoomies" ? 1 : pup.state === "stretch" || pup.state === "wiggle" ? .5 : 0;
    ruffle = Math.max(wild, ruffle * Math.exp(-dt / 6));
    for (const q of furPrims) {
      q.ruffle = ruffle;
      let s = springs.get(q.key);
      if (!s) springs.set(q.key, s = { p: [...q.a], v: [0, 0, 0] });
      for (let k = 0; k < 3; k++) {
        const acc = (q.a[k] - s.p[k]) * 160 - s.v[k] * 18;
        s.v[k] += acc * dt; s.p[k] += s.v[k] * dt;
      }
      const lag = [s.p[0] - q.a[0], s.p[1] - q.a[1], s.p[2] - q.a[2]], m = Math.hypot(...lag), cap = q.len * 1.5 + .01;
      q.lag = m > cap ? lag.map((x) => x * cap / m) : lag;
    }
  }

  // ——— the game ———
  const source = await (await fetch("./fiapup.js", { cache: "no-store" })).text();
  const names = Object.keys(host);
  game = new Function(...names, `${source}\nreturn { boot, sim, paint, act, leave, fiapup };`)(
    ...names.map((n) => host[n]));
  game.boot();
  if (params.get("stage")) game.fiapup.stage(params.get("stage"), Number(params.get("seconds")) || 2.5,
    params.has("touch"));
  if (params.has("touch")) game.fiapup.world.touch.mode = true;
  if (params.has("portrait")) game.fiapup.portrait();
  const paused = params.has("pause") || params.has("portrait");
  globalThis.__fiapup = game.fiapup;

  const frameTimes = [], gaps = [];
  let lastFrame = performance.now(), lastGap = 0;
  globalThis.__fiapupFrameMs = () => [...frameTimes].sort((a, b) => a - b)[frameTimes.length >> 1] || 0;
  globalThis.__fiapupFur = () => fur;
  function frame() {
    const frameStart = performance.now();
    const s = textCanvas.width / W;
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, textCanvas.width, textCanvas.height);
    ctx.setTransform(s, 0, 0, s, 0, 0);
    scene.beginFrame();
    if (!paused) game.sim();
    game.paint();
    scene.present({ clear });
    if (fur && furPrims.length && lastCamera) {
      const now = performance.now(), dt = Math.min(.05, (now - lastT) / 1000);
      lastT = now;
      furMotion(dt);
      fur.draw(furPrims, lastCamera, W, H, dt);
    }
    frameTimes.push(performance.now() - frameStart);
    if (frameTimes.length > 120) frameTimes.shift();
    gaps.push(frameStart - lastFrame); lastFrame = frameStart;
    if (gaps.length >= 120) {
      lastGap = gaps.sort((a, b) => a - b)[60];
      gaps.length = 0;
      if (fur && !params.get("shells") && lastGap > 20 && fur.state.shells > 8) fur.setShells(fur.state.shells - 4);
    }
    if (params.has("stats")) {
      ctx.fillStyle = "#fff"; ctx.font = "600 26px system-ui"; ctx.textBaseline = "top";
      ctx.fillText(`${lastGap.toFixed(1)} ms/frame · js ${globalThis.__fiapupFrameMs().toFixed(1)} ms · ${fur ? fur.state.shells + " shells" : "flat"}`,
        24 + safe.left, H - 64 - safe.bottom);
    }
    globalThis.__fiapupFrames = (globalThis.__fiapupFrames || 0) + 1;
    // Safe-area insets settle after the first layout in a WKWebView without a
    // resize to say so; look again now and then.
    if (globalThis.__fiapupFrames % 30 === 1) fit();
    requestAnimationFrame(frame);
  }
  requestAnimationFrame(frame);
  return game;
}
