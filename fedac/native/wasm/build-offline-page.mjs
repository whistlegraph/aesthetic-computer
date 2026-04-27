import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import wabtFactory from "wabt";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const nativeRoot = path.resolve(__dirname, "..");
const repoRoot = path.resolve(nativeRoot, "..", "..");
const outputDir = path.join(nativeRoot, "build");
const publicOutputDir = path.resolve(repoRoot, "system", "public", "ac-native-wasm");

const pieceArg = (process.argv[2] || process.env.AC_NATIVE_WASM_PIECE || "prompt").toLowerCase();

const PIECES = {
  prompt: {
    title: "AC Native Prompt • WASM Offline Prototype",
    hint: "<strong>Type</strong> in the canvas. <strong>Esc</strong> clears. <strong>Tab</strong> completes. <strong>Backspace</strong> returns from stub pieces.",
    sourcePath: path.join(nativeRoot, "pieces", "prompt.mjs"),
    inlineModules: [],
    bootSpec: "prompt",
    outputBaseName: "ac-native-prompt-offline.html",
    publicFileName: "index.html",
  },
  notepat: {
    title: "AC Native Notepat • WASM Offline Prototype",
    hint: "<strong>Click canvas</strong> to focus. Click anywhere first to enable audio.",
    sourcePath: path.join(nativeRoot, "pieces", "notepat.mjs"),
    inlineModules: [
      {
        importPath: "/lib/percussion.mjs",
        sourcePath: path.join(repoRoot, "system", "public", "aesthetic.computer", "lib", "percussion.mjs"),
      },
    ],
    bootSpec: "notepat",
    outputBaseName: "ac-native-notepat-offline.html",
    publicFileName: "notepat.html",
    inlineQR: true,
  },
};

const piece = PIECES[pieceArg];
if (!piece) {
  console.error(`Unknown piece: ${pieceArg}. Known: ${Object.keys(PIECES).join(", ")}`);
  process.exit(1);
}

let pieceSource = await fs.readFile(piece.sourcePath, "utf8");
// Optional QR library — inlined when the piece needs globalThis.qr().
const qrLibPath = path.join(repoRoot, "utilities", "ffos-build", "overlays", "launcher-ui", "js", "qrcode.min.js");
const qrLibSource = piece.inlineQR ? await fs.readFile(qrLibPath, "utf8") : "";

// AudioWorklet source — same speaker-bundled.mjs that ac-web's bios.mjs
// loads. Exposes registerProcessor("speaker-processor", ...) plus the
// inlined Synth/Bubble/Fart/etc. dependencies. Runs as a Blob URL added
// via audioContext.audioWorklet.addModule().
const speakerWorkletPath = path.join(repoRoot, "system", "public", "aesthetic.computer", "lib", "speaker-bundled.mjs");
const speakerWorkletSource = await fs.readFile(speakerWorkletPath, "utf8");

// ---------- Bitmap fonts ----------
// Parse the same ac-native C headers (font-matrix-chunky8.h, font-6x10.h)
// into JSON glyph tables. The browser blitter renders these pixel-for-pixel
// via wasm.fill_rect so the prototype matches the bare-metal look.
const matrixHeader = await fs.readFile(path.join(nativeRoot, "src", "font-matrix-chunky8.h"), "utf8");
const sixTenHeader = await fs.readFile(path.join(nativeRoot, "src", "font-6x10.h"), "utf8");

function parseMatrixFont(src) {
  // Each entry: { w, h, xoff, yoff, dwidth, {row, row, ...} }
  // The struct has 5 leading scalar fields then a brace-enclosed row list.
  const re = /\{\s*(\d+)\s*,\s*(\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(\d+)\s*,\s*\{([^}]*)\}\s*\}/g;
  const glyphs = [];
  let m;
  while ((m = re.exec(src)) && glyphs.length < 95) {
    const rows = m[6].split(",").map(s => s.trim()).filter(Boolean).map(s => parseInt(s, 16) || 0);
    glyphs.push({ w: +m[1], h: +m[2], xoff: +m[3], yoff: +m[4], dwidth: +m[5], rows });
  }
  return { kind: "matrix", ascent: 8, glyphs };
}
function parse6x10Font(src) {
  // 95 entries, each { 10 hex bytes }.
  const re = /\{\s*((?:0x[0-9a-fA-F]+\s*,?\s*){10})\}/g;
  const glyphs = [];
  let m;
  while ((m = re.exec(src)) && glyphs.length < 95) {
    const rows = m[1].split(",").map(s => s.trim()).filter(Boolean).map(s => parseInt(s, 16) || 0);
    glyphs.push({ w: 6, h: 10, xoff: 0, yoff: 0, dwidth: 6, rows });
  }
  return { kind: "fixed", ascent: 0, glyphs };
}
const FONT_TABLES = {
  matrix: parseMatrixFont(matrixHeader),
  "6x10": parse6x10Font(sixTenHeader)
};
if (FONT_TABLES.matrix.glyphs.length !== 95) {
  console.warn("matrix font parse: got", FONT_TABLES.matrix.glyphs.length, "glyphs (expected 95)");
}
if (FONT_TABLES["6x10"].glyphs.length !== 95) {
  console.warn("6x10 font parse: got", FONT_TABLES["6x10"].glyphs.length, "glyphs (expected 95)");
}

const inlineModules = [];
for (let i = 0; i < piece.inlineModules.length; i++) {
  const mod = piece.inlineModules[i];
  const source = await fs.readFile(mod.sourcePath, "utf8");
  const placeholder = `__INLINE_MODULE_${i}_PLACEHOLDER__`;
  // Replace both quoted forms of the import path in the piece source with
  // an opaque placeholder. The browser swaps these for blob URLs at runtime.
  const single = `'${mod.importPath}'`;
  const dbl = `"${mod.importPath}"`;
  pieceSource = pieceSource.split(dbl).join(`"${placeholder}"`);
  pieceSource = pieceSource.split(single).join(`'${placeholder}'`);
  inlineModules.push({ importPath: mod.importPath, source, placeholder });
}

const wabt = await wabtFactory();

const watSource = String.raw`(module
  (memory (export "memory") 256)
  (global $width (mut i32) (i32.const 0))
  (global $height (mut i32) (i32.const 0))
  (global $ink_r (mut i32) (i32.const 255))
  (global $ink_g (mut i32) (i32.const 255))
  (global $ink_b (mut i32) (i32.const 255))
  (global $ink_a (mut i32) (i32.const 255))

  (func (export "init") (param $w i32) (param $h i32) (result i32)
    local.get $w
    global.set $width
    local.get $h
    global.set $height
    i32.const 0
  )

  (func (export "set_ink") (param $r i32) (param $g i32) (param $b i32) (param $a i32)
    local.get $r
    global.set $ink_r
    local.get $g
    global.set $ink_g
    local.get $b
    global.set $ink_b
    local.get $a
    global.set $ink_a
  )

  (func $pixel_offset (param $x i32) (param $y i32) (result i32)
    local.get $y
    global.get $width
    i32.mul
    local.get $x
    i32.add
    i32.const 4
    i32.mul
  )

  (func $blend (param $dst i32) (param $src i32) (param $a i32) (result i32)
    local.get $src
    local.get $a
    i32.mul
    local.get $dst
    i32.const 255
    local.get $a
    i32.sub
    i32.mul
    i32.add
    i32.const 127
    i32.add
    i32.const 255
    i32.div_u
  )

  (func (export "clear")
    (local $i i32)
    (local $total i32)
    (local $addr i32)

    global.get $width
    global.get $height
    i32.mul
    local.set $total

    block $done
      loop $loop
        local.get $i
        local.get $total
        i32.ge_u
        br_if $done

        local.get $i
        i32.const 4
        i32.mul
        local.set $addr

        local.get $addr
        global.get $ink_r
        i32.store8

        local.get $addr
        i32.const 1
        i32.add
        global.get $ink_g
        i32.store8

        local.get $addr
        i32.const 2
        i32.add
        global.get $ink_b
        i32.store8

        local.get $addr
        i32.const 3
        i32.add
        i32.const 255
        i32.store8

        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $loop
      end
    end
  )

  (func (export "fill_rect") (param $x i32) (param $y i32) (param $w i32) (param $h i32)
    (local $yy i32)
    (local $xx i32)
    (local $px i32)
    (local $py i32)
    (local $addr i32)
    (local $dr i32)
    (local $dg i32)
    (local $db i32)

    block $outer_done
      loop $outer
        local.get $yy
        local.get $h
        i32.ge_u
        br_if $outer_done

        i32.const 0
        local.set $xx

        block $inner_done
          loop $inner
            local.get $xx
            local.get $w
            i32.ge_u
            br_if $inner_done

            local.get $x
            local.get $xx
            i32.add
            local.set $px

            local.get $y
            local.get $yy
            i32.add
            local.set $py

            local.get $px
            local.get $py
            call $pixel_offset
            local.set $addr

            global.get $ink_a
            i32.const 255
            i32.eq
            if
              local.get $addr
              global.get $ink_r
              i32.store8

              local.get $addr
              i32.const 1
              i32.add
              global.get $ink_g
              i32.store8

              local.get $addr
              i32.const 2
              i32.add
              global.get $ink_b
              i32.store8

              local.get $addr
              i32.const 3
              i32.add
              i32.const 255
              i32.store8
            else
              local.get $addr
              i32.load8_u
              local.set $dr

              local.get $addr
              i32.const 1
              i32.add
              i32.load8_u
              local.set $dg

              local.get $addr
              i32.const 2
              i32.add
              i32.load8_u
              local.set $db

              local.get $addr
              local.get $dr
              global.get $ink_r
              global.get $ink_a
              call $blend
              i32.store8

              local.get $addr
              i32.const 1
              i32.add
              local.get $dg
              global.get $ink_g
              global.get $ink_a
              call $blend
              i32.store8

              local.get $addr
              i32.const 2
              i32.add
              local.get $db
              global.get $ink_b
              global.get $ink_a
              call $blend
              i32.store8

              local.get $addr
              i32.const 3
              i32.add
              i32.const 255
              i32.store8
            end

            local.get $xx
            i32.const 1
            i32.add
            local.set $xx
            br $inner
          end
        end

        local.get $yy
        i32.const 1
        i32.add
        local.set $yy
        br $outer
      end
    end
  )
)`;

const wasmModule = wabt.parseWat("raster.wat", watSource);
const { buffer } = wasmModule.toBinary({ log: false, write_debug_names: true });
const wasmBase64 = Buffer.from(buffer).toString("base64");

const html = `<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>${piece.title}</title>
  <style>
    * { box-sizing: border-box; }
    html, body { margin: 0; padding: 0; width: 100vw; height: 100vh; overflow: hidden; background: #000; }
    canvas {
      position: fixed;
      inset: 0;
      width: 100vw;
      height: 100vh;
      display: block;
      image-rendering: pixelated;
      image-rendering: crisp-edges;
      touch-action: none;
      outline: none;
    }
  </style>
</head>
<body>
  <canvas id="screen" width="960" height="640" tabindex="0" aria-label="${piece.title}"></canvas>
  ${qrLibSource ? `<script>\n${qrLibSource}\n</script>` : ""}
  <script type="module">
    const WASM_BASE64 = ${JSON.stringify(wasmBase64)};
    const PIECE_NAME = ${JSON.stringify(piece.bootSpec)};
    const PIECE_SOURCE = ${JSON.stringify(pieceSource)};
    const INLINE_MODULES = ${JSON.stringify(inlineModules)};
    const FONT_TABLES = ${JSON.stringify(FONT_TABLES)};
    const SPEAKER_WORKLET_SOURCE = ${JSON.stringify(speakerWorkletSource)};

    const screen = document.getElementById("screen");
    const ctx = screen.getContext("2d", { alpha: false });
    ctx.imageSmoothingEnabled = false;

    function bytesFromBase64(base64) {
      const text = atob(base64);
      const bytes = new Uint8Array(text.length);
      for (let i = 0; i < text.length; i++) bytes[i] = text.charCodeAt(i);
      return bytes;
    }

    function clone(value) {
      return value == null ? value : JSON.parse(JSON.stringify(value));
    }

    const wasmBytes = bytesFromBase64(WASM_BASE64);
    const { instance } = await WebAssembly.instantiate(wasmBytes, {});
    const wasm = instance.exports;

    // Pixel density — how many physical pixels per logical pixel in the
    // backing buffer. density=1 → backing buffer matches viewport; higher
    // values give chunkier pixels. Ctrl+= / Ctrl+- adjusts at runtime.
    let density = 1;
    let WIDTH = screen.width;
    let HEIGHT = screen.height;
    let pixelView = null;
    let imageData = null;
    function resizeFramebuffer(newW, newH) {
      newW = Math.max(64, Math.min(4096, Math.floor(newW)));
      newH = Math.max(64, Math.min(4096, Math.floor(newH)));
      // 256 pages × 64KB = 16.7 MB max → 4M pixels at 4 bytes; cap pixels.
      const maxPixels = 256 * 65536 / 4;
      if (newW * newH > maxPixels) {
        const scale = Math.sqrt(maxPixels / (newW * newH));
        newW = Math.floor(newW * scale);
        newH = Math.floor(newH * scale);
      }
      WIDTH = newW; HEIGHT = newH;
      screen.width = WIDTH; screen.height = HEIGHT;
      wasm.init(WIDTH, HEIGHT);
      pixelView = new Uint8ClampedArray(wasm.memory.buffer, 0, WIDTH * HEIGHT * 4);
      imageData = new ImageData(pixelView, WIDTH, HEIGHT);
      // hostApi.screen is updated by tick() each frame — no need to reach
      // into it here, which avoids a TDZ ReferenceError on initial sizing.
    }
    function applyDensity() {
      const vpW = window.innerWidth || 960;
      const vpH = window.innerHeight || 640;
      resizeFramebuffer(vpW / density, vpH / density);
    }
    resizeFramebuffer(WIDTH, HEIGHT);

    // Build blob URLs for inline modules and substitute their placeholders
    // (created at build time) with the real URLs so imports resolve.
    let rewrittenSource = PIECE_SOURCE;
    for (const mod of INLINE_MODULES) {
      const blob = new Blob([mod.source], { type: "text/javascript" });
      const url = URL.createObjectURL(blob);
      rewrittenSource = rewrittenSource.split(mod.placeholder).join(url);
    }

    globalThis.__theme = (function() {
      function getLAOffset() {
        const d = new Date();
        const m = d.getUTCMonth();
        const y = d.getUTCFullYear();
        if (m > 2 && m < 10) return 7;
        if (m < 2 || m > 10) return 8;
        if (m === 2) {
          const mar1 = new Date(y, 2, 1);
          const ss = 8 + (7 - mar1.getDay()) % 7;
          return d.getUTCDate() > ss || (d.getUTCDate() === ss && d.getUTCHours() >= 10) ? 7 : 8;
        }
        const nov1 = new Date(y, 10, 1);
        const fs = 1 + (7 - nov1.getDay()) % 7;
        return d.getUTCDate() < fs || (d.getUTCDate() === fs && d.getUTCHours() < 9) ? 7 : 8;
      }
      function getLAHour() {
        return (new Date().getUTCHours() - getLAOffset() + 24) % 24;
      }
      const t = { dark: true, _lastCheck: 0, _overrideId: null, _override: null };
      t.presets = {};
      t.apply = function(id) {
        t._overrideId = id || null;
        t._lastCheck = 0;
        return t.update();
      };
      t.update = function() {
        const now = Date.now();
        if (now - t._lastCheck < 5000) return t;
        t._lastCheck = now;
        const h = getLAHour();
        t.dark = (t._forceDark !== undefined) ? !!t._forceDark : (h >= 20 || h < 7);
        t.hour = h;
        t.bg = t.dark ? [20, 20, 25] : [240, 238, 232];
        t.bgAlt = t.dark ? [28, 28, 30] : [250, 248, 244];
        t.bgDim = t.dark ? [15, 15, 18] : [230, 228, 222];
        t.fg = t.dark ? 220 : 40;
        t.fgDim = t.dark ? 140 : 100;
        t.fgMute = t.dark ? 80 : 150;
        t.bar = t.dark ? [35, 20, 30] : [225, 220, 215];
        t.border = t.dark ? [55, 35, 45] : [200, 195, 190];
        t.accent = t.dark ? [200, 100, 140] : [180, 60, 120];
        t.ok = t.dark ? [80, 255, 120] : [30, 160, 60];
        t.err = t.dark ? [255, 85, 85] : [200, 40, 40];
        t.warn = t.dark ? [255, 200, 60] : [180, 120, 20];
        t.link = t.dark ? [120, 200, 255] : [40, 100, 200];
        t.pad = t.dark ? [28, 28, 30] : [250, 248, 244];
        t.padSharp = t.dark ? [18, 18, 20] : [235, 232, 228];
        t.padLine = t.dark ? [50, 50, 55] : [210, 205, 200];
        t.cursor = t.dark ? [220, 80, 140] : [180, 50, 110];
        return t;
      };
      t.update();
      return t;
    })();

    function makeStorage() {
      const fallback = new Map();
      return {
        get(key) {
          try { return localStorage.getItem(key); }
          catch (_) { return fallback.has(key) ? fallback.get(key) : null; }
        },
        set(key, value) {
          try { localStorage.setItem(key, value); }
          catch (_) { fallback.set(key, value); }
        }
      };
    }

    const storage = makeStorage();
    const storageKey = "ac-native-wasm:" + PIECE_NAME + ":files";

    function loadFiles() {
      const raw = storage.get(storageKey);
      const defaults = {
        "/mnt/config.json": JSON.stringify({ piece: PIECE_NAME, darkMode: "auto" }, null, 2),
        "/mnt/wifi_creds.json": "[]"
      };
      if (!raw) return defaults;
      try { return JSON.parse(raw); } catch (_) { return defaults; }
    }

    const files = loadFiles();
    function persistFiles() { storage.set(storageKey, JSON.stringify(files)); }

    function readConfig() {
      const raw = files["/mnt/config.json"];
      if (!raw) return {};
      try { return JSON.parse(raw); } catch (_) { return {}; }
    }

    const state = {
      currentPieceName: PIECE_NAME,
      currentPiece: null,
      currentPieceSpec: PIECE_NAME,
      currentParams: [],
      currentColon: [],
      paintCount: 0,
      jumpMessage: "",
      lastPost: "",
      statusText: "booting native " + PIECE_NAME,
      events: [],
      focused: false,
      pointerDown: false,
      pointerX: 0,
      pointerY: 0
    };

    const wifi = {
      connected: false,
      state: 0,
      networks: [],
      iface: "wlan0",
      ip: null,
      _connectTimer: null,
      status() { return wifi.connected ? "connected" : "offline"; },
      scan() {
        wifi.state = 3;
        setTimeout(() => {
          wifi.networks = [
            { ssid: "aesthetic.computer", signal: 92, encrypted: true },
            { ssid: "offline.lab", signal: 44, encrypted: false }
          ];
          wifi.state = 0;
        }, 120);
      },
      connect(ssid, pass) {
        wifi.state = 4;
        clearTimeout(wifi._connectTimer);
        wifi._connectTimer = setTimeout(() => {
          wifi.connected = true;
          wifi.state = 0;
          wifi.ip = "10.0.0.42";
          wifi.networks = [{ ssid, signal: 96, encrypted: !!pass }];
        }, 280);
      },
      disconnect() {
        clearTimeout(wifi._connectTimer);
        wifi.connected = false;
        wifi.ip = null;
        wifi.state = 0;
      }
    };

    function normalizeKey(event) {
      const key = event.key;
      if (key === " ") return "space";
      if (key === "Enter") return "enter";
      if (key === "Backspace") return "backspace";
      if (key === "Delete") return "delete";
      if (key === "Escape") return "escape";
      if (key === "Tab") return "tab";
      if (key === "Shift") return "shift";
      if (key === "Meta") return "meta";
      if (key === "Control") return "control";
      if (key === "Alt") return "alt";
      if (key === "ArrowLeft") return "arrowleft";
      if (key === "ArrowRight") return "arrowright";
      if (key === "ArrowUp") return "arrowup";
      if (key === "ArrowDown") return "arrowdown";
      if (key === "Home") return "home";
      if (key === "End") return "end";
      return typeof key === "string" ? key.toLowerCase() : "";
    }

    function makeKeyEvent(type, key) {
      return { key, type, is(match) { return type === match; } };
    }

    function makePointerEvent(type, x, y, pointerId, pressure) {
      return {
        type,
        x, y,
        pointer: { x, y, id: pointerId, pressure },
        pressure,
        is(match) { return type === match; }
      };
    }

    const blockedKeys = new Set([
      "space", "enter", "backspace", "delete", "escape", "tab",
      "arrowleft", "arrowright", "arrowup", "arrowdown", "home", "end"
    ]);

    screen.addEventListener("click", () => screen.focus());
    screen.addEventListener("focus", () => { state.focused = true; });
    screen.addEventListener("blur", () => { state.focused = false; });

    window.addEventListener("keydown", (event) => {
      const key = normalizeKey(event);
      if (!key) return;
      // Density / zoom shortcuts belong to the host, not the piece.
      if ((event.ctrlKey || event.metaKey) && (key === "=" || key === "+" || key === "-" || key === "_" || key === "0")) {
        return;
      }
      if (blockedKeys.has(key) || key.length === 1 || key === "shift" || key === "meta" || key === "control" || key === "alt") {
        event.preventDefault();
      }
      const type = (key === "shift" || key === "meta" || key === "control" || key === "alt")
        ? "keyboard:down:" + key
        : "keyboard:down";
      state.events.push(makeKeyEvent(type, key));
      if (type !== "keyboard:down") {
        // Pieces also commonly listen for plain keyboard:down on modifiers
        state.events.push(makeKeyEvent("keyboard:down", key));
      }
    });
    window.addEventListener("keyup", (event) => {
      const key = normalizeKey(event);
      if (!key) return;
      const type = (key === "shift" || key === "meta" || key === "control" || key === "alt")
        ? "keyboard:up:" + key
        : "keyboard:up";
      if (type !== "keyboard:up") event.preventDefault();
      state.events.push(makeKeyEvent(type, key));
      if (type !== "keyboard:up") {
        state.events.push(makeKeyEvent("keyboard:up", key));
      }
    });

    function canvasCoords(event) {
      const rect = screen.getBoundingClientRect();
      const sx = WIDTH / rect.width;
      const sy = HEIGHT / rect.height;
      return [
        Math.round((event.clientX - rect.left) * sx),
        Math.round((event.clientY - rect.top) * sy)
      ];
    }

    screen.addEventListener("pointerdown", (event) => {
      event.preventDefault();
      screen.setPointerCapture?.(event.pointerId);
      const [x, y] = canvasCoords(event);
      state.pointerDown = true;
      state.pointerX = x; state.pointerY = y;
      state.events.push(makePointerEvent("touch", x, y, event.pointerId, event.pressure ?? 1));
    });
    screen.addEventListener("pointermove", (event) => {
      const [x, y] = canvasCoords(event);
      state.pointerX = x; state.pointerY = y;
      if (state.pointerDown) {
        state.events.push(makePointerEvent("draw", x, y, event.pointerId, event.pressure ?? 1));
      }
    });
    function pointerUp(event) {
      const [x, y] = canvasCoords(event);
      if (state.pointerDown) {
        state.pointerDown = false;
        state.events.push(makePointerEvent("lift", x, y, event.pointerId, 0));
      }
    }
    screen.addEventListener("pointerup", pointerUp);
    screen.addEventListener("pointercancel", pointerUp);

    // ---------- raster + canvas-overlay drawing ----------
    let currentInk = [255, 255, 255, 255];
    const lineQueue = [];

    function setInk(r = 255, g = 255, b = 255, a = 255) {
      currentInk = [r | 0, g | 0, b | 0, a == null ? 255 : a | 0];
      wasm.set_ink(currentInk[0], currentInk[1], currentInk[2], currentInk[3]);
    }
    function clampRect(x, y, w, h) {
      let rx = Math.trunc(x), ry = Math.trunc(y);
      let rw = Math.trunc(w), rh = Math.trunc(h);
      if (rw < 0) { rx += rw; rw = -rw; }
      if (rh < 0) { ry += rh; rh = -rh; }
      if (rx < 0) { rw += rx; rx = 0; }
      if (ry < 0) { rh += ry; ry = 0; }
      if (rx + rw > WIDTH) rw = WIDTH - rx;
      if (ry + rh > HEIGHT) rh = HEIGHT - ry;
      return rw > 0 && rh > 0 ? [rx, ry, rw, rh] : null;
    }
    function rasterWipe(r, g, b, a = 255) { setInk(r, g, b, a); wasm.clear(); }

    // ---------- bitmap-font blitter ----------
    // Mirrors ac-native's font.c: 6x10 is fixed-grid, matrix-chunky8 has
    // per-glyph w/h/xoff/yoff/dwidth and uses ascent for vertical alignment.
    // Lit pixels go straight to the WASM framebuffer via fill_rect.
    function drawBitmapText(text, x, y, scale, fontKey) {
      const font = FONT_TABLES[fontKey] || FONT_TABLES["6x10"];
      if (!font) return;
      const fixed = font.kind === "fixed";
      const ascent = font.ascent | 0;
      const startX = x;
      let cx = x;
      let cy = y;
      for (let i = 0; i < text.length; i++) {
        const code = text.charCodeAt(i);
        if (code === 10) { // \n
          cx = startX;
          cy += (fixed ? 10 : ascent) * scale;
          continue;
        }
        let cp = (code >= 32 && code <= 126) ? code : 63; // '?'
        const glyph = font.glyphs[cp - 32] || font.glyphs[63 - 32];
        if (!glyph) continue;
        const gw = glyph.w | 0, gh = glyph.h | 0;
        const advance = (glyph.dwidth || gw + 1) * scale;
        // Vertical placement: matrix glyphs use baseline math; 6x10 plots from top-left.
        const gx = cx + (fixed ? 0 : (glyph.xoff | 0) * scale);
        const gy = cy + (fixed ? 0 : (ascent - (glyph.yoff | 0) - gh) * scale);
        const rows = glyph.rows;
        for (let row = 0; row < gh; row++) {
          const bits = rows[row] | 0;
          if (!bits) continue;
          for (let col = 0; col < gw; col++) {
            if (bits & (0x80 >> col)) {
              const px = gx + col * scale;
              const py = gy + row * scale;
              if (px + scale <= 0 || py + scale <= 0 || px >= WIDTH || py >= HEIGHT) continue;
              wasm.fill_rect(px, py, scale, scale);
            }
          }
        }
        cx += advance;
      }
    }
    function rasterBox(x, y, w, h, filled = true) {
      const rect = clampRect(x, y, w, h);
      if (!rect) return;
      if (filled) { wasm.fill_rect(rect[0], rect[1], rect[2], rect[3]); return; }
      wasm.fill_rect(rect[0], rect[1], rect[2], 1);
      wasm.fill_rect(rect[0], rect[1] + rect[3] - 1, rect[2], 1);
      wasm.fill_rect(rect[0], rect[1], 1, rect[3]);
      wasm.fill_rect(rect[0] + rect[2] - 1, rect[1], 1, rect[3]);
    }

    const hostApi = {
      wipe(r, g, b, a = 255) {
        if (typeof r === "object" && r !== null) {
          // wipe([r,g,b]) form
          const arr = r;
          rasterWipe(arr[0]|0, arr[1]|0, arr[2]|0, (arr[3] ?? 255)|0);
          return;
        }
        rasterWipe(r|0, g|0, b|0, (a ?? 255)|0);
      },
      ink(r, g, b, a = 255) {
        if (typeof r === "object" && r !== null) {
          const arr = r;
          setInk(arr[0]|0, arr[1]|0, arr[2]|0, (arr[3] ?? 255)|0);
          return;
        }
        setInk(r|0, g|0, b|0, (a ?? 255)|0);
      },
      box: (x, y, w, h, filled) => rasterBox(x, y, w, h, filled !== false),
      line(x1, y1, x2, y2) {
        lineQueue.push({
          x1: Math.trunc(x1), y1: Math.trunc(y1),
          x2: Math.trunc(x2), y2: Math.trunc(y2),
          color: clone(currentInk)
        });
      },
      write(text, options = {}) {
        // Render directly into the WASM framebuffer so glyphs land on the
        // same pixel layer as box/line — matches ac-native's font.c order.
        const requested = String(options.font || "6x10").toLowerCase();
        const fontKey = (requested === "matrix" || requested === "matrixchunky8")
          ? "matrix"
          : (requested === "font_1" ? "6x10" : (FONT_TABLES[requested] ? requested : "6x10"));
        drawBitmapText(String(text),
                       Math.trunc(options.x ?? 0),
                       Math.trunc(options.y ?? 0),
                       Math.max(1, options.size | 0 || 1),
                       fontKey);
      },
      screen: { width: WIDTH, height: HEIGHT },
      paintCount: 0,
      wifi,
      sound: makeSound(),
      trackpad: { dx: 0, dy: 0 },
      pressures: {}
    };

    // ---------- Web Audio (AudioWorklet path) ----------
    // The audio backend is the same speaker-bundled.mjs that ac-web's
    // bios.mjs loads — registered as an "speaker-processor" AudioWorklet.
    // Voices live INSIDE the worklet, so synth() / kill() / replay /
    // sample / room / glitch / volume are all postMessages. This gives us
    // the real harp (Karplus-Strong) and whistle (Cook/STK waveguide)
    // models, plus room reverb and glitch bitcrush, identical to the web
    // runtime. Browsers require a user gesture to start audio — we create
    // the AudioContext on the first pointerdown/keydown and resume() it
    // there. addModule() is async; messages sent before it resolves are
    // queued and flushed once the worklet is ready.
    let audioCtx = null;
    let speakerWorklet = null;
    let outputGain = null;       // post-worklet master volume → destination
    let workletReady = false;
    let workletLoading = false;
    const pendingWorkletMsgs = [];
    function postWorklet(msg) {
      if (workletReady && speakerWorklet) {
        speakerWorklet.port.postMessage(msg);
      } else {
        pendingWorkletMsgs.push(msg);
      }
    }
    // Drive/wobble FX intensity — kept in state so the slider feels
    // responsive, but the worklet doesn't have these (matches ac-web).
    const fxState = { drive: 0, bitcrush: 0, wobble: 0, echo: 0, master: 1 };
    let pendingMasterVolume = 0.6;

    // Master-output ring buffer — captures the last RECENT_BUFFER_SECONDS
    // of mixed audio so speaker.getRecentBuffer() and speaker.drawStrip()
    // can read it. Filled by a ScriptProcessorNode tapped off outputGain.
    const RECENT_BUFFER_SECONDS = 12;
    let recentBuf = null;
    let recentHead = 0;
    let recentFilled = 0;

    // Voice ID generator — must be BigInt because the inlined Synth class
    // does Number(id) * 2654435761 style arithmetic on it for whistle seed.
    let nextSoundId = 0n;
    function makeId() { const id = nextSoundId; nextSoundId += 1n; return id; }

    function applyFx() {
      // The speaker worklet has room (echo) + glitch (bitcrush) + volume.
      // drive/wobble don't exist there (matches ac-web).
      const echoMix = fxState.echo * fxState.master;
      postWorklet({
        type: "room:set",
        data: { enabled: echoMix > 0.001, mix: echoMix, feedback: 0.6 }
      });
      const crushMix = fxState.bitcrush * fxState.master;
      postWorklet({
        type: "glitch:set",
        data: {
          enabled: crushMix > 0.001,
          mix: crushMix,
          // mix=0 → 12 bits (clean), mix=1 → 2 bits (heavy crush)
          crush: Math.max(2, Math.round(12 - crushMix * 10)),
          rate: 1600,
          jitter: 0.15
        }
      });
    }

    function ensureAudio() {
      if (audioCtx) return audioCtx;
      const Ctor = window.AudioContext || window.webkitAudioContext;
      if (!Ctor) { console.warn("[ac-native-wasm] no AudioContext available"); return null; }
      audioCtx = new Ctor();
      outputGain = audioCtx.createGain();
      outputGain.gain.value = pendingMasterVolume;
      outputGain.connect(audioCtx.destination);

      // Master-output tap (post-worklet). ScriptProcessor is deprecated but
      // it's the simplest way to feed a ring buffer without yet another
      // AudioWorklet module file.
      recentBuf = new Float32Array(audioCtx.sampleRate * RECENT_BUFFER_SECONDS);
      recentHead = 0;
      recentFilled = 0;
      const PROC_FRAMES = 4096;
      const tap = audioCtx.createScriptProcessor(PROC_FRAMES, 1, 1);
      tap.onaudioprocess = (e) => {
        const input = e.inputBuffer.getChannelData(0);
        const buf = recentBuf;
        let head = recentHead;
        for (let i = 0; i < input.length; i++) {
          buf[head] = input[i];
          head = head + 1; if (head >= buf.length) head = 0;
        }
        recentHead = head;
        if (recentFilled < buf.length) recentFilled = Math.min(buf.length, recentFilled + input.length);
      };
      const muteSink = audioCtx.createGain();
      muteSink.gain.value = 0;
      outputGain.connect(tap);
      tap.connect(muteSink).connect(audioCtx.destination);

      // Load the speaker worklet (same speaker-bundled.mjs ac-web uses).
      // addModule() is async — voices spawned before it resolves are
      // queued in pendingWorkletMsgs and flushed on ready.
      //
      // URL strategy: under file:// (e.g. Desktop double-click) Chrome
      // refuses to load blob: URLs as worklet modules — "Not allowed to
      // load local resource". We prefer data: URLs there. Under http(s):
      // blob: is faster and avoids the percent-encoding overhead, so we
      // try blob: first and fall back to data: on rejection.
      if (!workletLoading) {
        workletLoading = true;
        const isFileOrigin = window.location.protocol === "file:";
        const dataUrl = "data:application/javascript;charset=utf-8," + encodeURIComponent(SPEAKER_WORKLET_SOURCE);
        const blob = isFileOrigin ? null : new Blob([SPEAKER_WORKLET_SOURCE], { type: "application/javascript" });
        const blobUrl = blob ? URL.createObjectURL(blob) : null;
        const finalize = () => {
          try {
            speakerWorklet = new AudioWorkletNode(audioCtx, "speaker-processor", {
              outputChannelCount: [2],
              processorOptions: { bpm: 120, debug: false }
            });
            speakerWorklet.connect(outputGain);
            workletReady = true;
            // Drain queued messages in submission order.
            while (pendingWorkletMsgs.length) {
              speakerWorklet.port.postMessage(pendingWorkletMsgs.shift());
            }
            applyFx();
            speakerWorklet.port.postMessage({ type: "volume", data: pendingMasterVolume });
          } catch (err) {
            console.error("[ac-native-wasm] failed to instantiate speaker worklet:", err);
          }
        };
        const tryUrl = (url) => audioCtx.audioWorklet.addModule(url);
        const primary = isFileOrigin ? dataUrl : blobUrl;
        const secondary = isFileOrigin ? null : dataUrl;
        tryUrl(primary)
          .then(finalize)
          .catch((err) => {
            console.warn("[ac-native-wasm] primary worklet URL rejected:", err.message);
            if (!secondary) {
              console.error("[ac-native-wasm] no fallback worklet URL available");
              return;
            }
            tryUrl(secondary)
              .then(finalize)
              .catch((err2) => {
                console.error("[ac-native-wasm] worklet addModule failed (fallback also rejected):", err2);
              });
          })
          .finally(() => {
            if (blobUrl) URL.revokeObjectURL(blobUrl);
          });
      }

      return audioCtx;
    }
    function unlockAudio() {
      const ctx = ensureAudio();
      if (ctx && ctx.state !== "running") {
        ctx.resume().catch(() => {});
      }
    }
    // Capture-phase listeners so we run before any pointerdown/keydown
    // hits the canvas — the gesture window closes once any default action
    // settles, so creating + resuming the context here is what matters.
    window.addEventListener("pointerdown", unlockAudio, { capture: true });
    window.addEventListener("keydown", unlockAudio, { capture: true });
    window.addEventListener("touchstart", unlockAudio, { capture: true, passive: true });

    function makeSound() {
      // Voice handle that talks to the worklet via postMessage. Matches
      // the shape ac-web's $sound.synth returns: { id, kill, update }.
      function makeHandle(id) {
        let killed = false;
        return {
          id,
          kill(fade = 0.02) {
            if (killed) return;
            killed = true;
            postWorklet({
              type: "kill",
              data: { id, fade: Math.max(0.005, Number(fade) || 0.02) }
            });
          },
          update(properties) {
            if (killed || !properties) return;
            postWorklet({ type: "update", data: { id, properties } });
          },
          progress() { /* ac-web stub */ }
        };
      }

      function synth(opts = {}) {
        ensureAudio();
        const id = makeId();
        // beats = duration_seconds * bpm / 60 — the worklet's internal
        // duration unit. duration: Infinity → Infinity beats (sustained).
        const bpm = 120;
        const durRaw = opts.duration;
        const isInfinite = durRaw === Infinity || durRaw === "infinite";
        const duration = isInfinite ? Infinity : Math.max(0.001, Number(durRaw) || 0.2);
        const beats = isInfinite ? Infinity : (duration * bpm) / 60;
        const data = {
          id,
          type: opts.type || "sine",
          tone: Number(opts.tone) || 440,
          beats,
          attack: Math.max(0, Number(opts.attack) || 0.01),
          decay: Math.min(0.999, Math.max(0, Number(opts.decay) || 0.9)),
          volume: Math.max(0, Math.min(1, opts.volume == null ? 1 : Number(opts.volume))),
          pan: Math.max(-1, Math.min(1, Number(opts.pan) || 0))
        };
        postWorklet({ type: "sound", data });
        return makeHandle(id);
      }

      function kill(handle, fade) {
        try { handle?.kill?.(fade); } catch (_) {}
      }

      // Player factory shared by replay + sample. Stores the most recent
      // PCM buffer; play() spawns a "sample" voice in the worklet with
      // speed = tone / base for pitch-shifted playback.
      function makeBufferPlayer(label) {
        let activeBuffer = null;     // Float32Array
        let activeRate = 0;
        let bufferLabel = "";
        let labelCounter = 0;
        function loadData(data, rate) {
          if (!data || !data.length) return;
          // Copy so the worklet can hold the buffer independently of any
          // mic chunks that may keep getting overwritten.
          activeBuffer = new Float32Array(data);
          activeRate = Number(rate) || 48000;
          bufferLabel = label + "-" + (++labelCounter);
        }
        function play(opts = {}) {
          ensureAudio();
          if (!activeBuffer) return makeHandle(makeId());
          const id = makeId();
          const tone = Number(opts.tone) || 261.63;
          const base = Number(opts.base) || 261.63;
          const speed = Math.max(0.0001, tone / base);
          const data = {
            id,
            type: "sample",
            options: {
              buffer: {
                channels: [activeBuffer],
                sampleRate: activeRate,
                length: activeBuffer.length
              },
              label: bufferLabel,
              speed,
              loop: !!opts.loop,
              from: 0,
              to: 1
            },
            beats: opts.loop ? Infinity : (activeBuffer.length / activeRate / Math.abs(speed)) * 2,
            attack: Math.max(0, Number(opts.attack) || 0.001),
            decay: Math.min(0.999, Math.max(0, Number(opts.decay) || 0.001)),
            volume: Math.max(0, Math.min(1, opts.volume == null ? 1 : Number(opts.volume))),
            pan: Math.max(-1, Math.min(1, Number(opts.pan) || 0))
          };
          postWorklet({ type: "sound", data });
          return makeHandle(id);
        }
        function getData() {
          return activeBuffer ? new Float32Array(activeBuffer) : null;
        }
        return { loadData, play, getData, kill: (h, fade) => { try { h?.kill?.(fade); } catch (_) {} } };
      }

      // ---- speaker tap ----
      function getRecentBuffer(seconds) {
        if (!audioCtx || !recentBuf || recentFilled <= 0) return null;
        const sr = audioCtx.sampleRate;
        const want = Math.min(recentFilled, Math.max(1, Math.floor(Number(seconds) * sr)));
        if (want <= 0) return null;
        const out = new Float32Array(want);
        let read = recentHead - want; if (read < 0) read += recentBuf.length;
        for (let i = 0; i < want; i++) {
          out[i] = recentBuf[read];
          read = read + 1; if (read >= recentBuf.length) read = 0;
        }
        return { data: out, rate: sr };
      }

      function drawStrip(x, y, w, h, totalSec, anchor, offsetSec) {
        const px = Math.trunc(x), py = Math.trunc(y);
        const pw = Math.trunc(w), ph = Math.trunc(h);
        if (pw <= 0 || ph <= 0) return;
        // Background
        const T = globalThis.__theme.update();
        const prevInk = clone(currentInk);
        setInk(T.padSharp[0], T.padSharp[1], T.padSharp[2], 255);
        rasterBox(px, py, pw, ph, true);

        // No audio yet — just draw the empty rail with a center line.
        if (!audioCtx || recentFilled <= 0) {
          setInk(T.padLine[0], T.padLine[1], T.padLine[2], 255);
          rasterBox(px, py + Math.floor(ph / 2), pw, 1, true);
          setInk(prevInk[0], prevInk[1], prevInk[2], prevInk[3]);
          return;
        }
        const sr = audioCtx.sampleRate;
        const offSec = Math.max(0, Number(offsetSec) || 0);
        const totSec = Math.max(0.05, Number(totalSec) || 4);
        const totalSamples = Math.floor(totSec * sr);
        const offSamples = Math.floor(offSec * sr);
        // Pull the visible window from the ring buffer.
        const out = new Float32Array(totalSamples);
        let read = recentHead - offSamples - totalSamples;
        // wrap into [0, recentBuf.length)
        const L = recentBuf.length;
        read = ((read % L) + L) % L;
        for (let i = 0; i < totalSamples; i++) {
          out[i] = recentBuf[read];
          read = read + 1; if (read >= L) read = 0;
        }
        // Per-pixel min/max → vertical bar
        const halfH = Math.floor(ph / 2);
        const baseY = py + halfH;
        setInk(T.accent[0], T.accent[1], T.accent[2], 255);
        for (let col = 0; col < pw; col++) {
          const s0 = Math.floor((col / pw) * totalSamples);
          const s1 = Math.floor(((col + 1) / pw) * totalSamples);
          let lo = 0, hi = 0;
          for (let i = s0; i < s1; i++) {
            const v = out[i];
            if (v < lo) lo = v;
            if (v > hi) hi = v;
          }
          const top = Math.max(py, baseY + Math.round(lo * halfH));
          const bot = Math.min(py + ph - 1, baseY + Math.round(hi * halfH));
          const barH = Math.max(1, bot - top + 1);
          rasterBox(px + col, top, 1, barH, true);
        }
        // Center rail
        setInk(T.padLine[0], T.padLine[1], T.padLine[2], 180);
        rasterBox(px, baseY, pw, 1, true);
        // Anchor needle (red)
        const a = Math.max(0, Math.min(1, Number(anchor) || 0.5));
        const needleX = px + Math.floor(pw * a);
        setInk(220, 60, 60, 255);
        rasterBox(needleX, py, 1, ph, true);
        setInk(prevInk[0], prevInk[1], prevInk[2], prevInk[3]);
      }

      const replayPlayer = makeBufferPlayer("ac-wasm-replay");
      const samplePlayer = makeBufferPlayer("ac-wasm-sample");
      // FX setMix wrappers — store intent in fxState. applyFx() forwards
      // echo→room and bitcrush→glitch to the worklet. drive/wobble are
      // intentional no-ops here (matches ac-web — speaker.mjs has no
      // dedicated drive/wobble effect).
      const room = { setMix: (v) => { fxState.echo = +v || 0; applyFx(); } };
      const glitch = { setMix: (v) => { fxState.bitcrush = +v || 0; applyFx(); } };
      const drive = { setMix: (v) => { fxState.drive = +v || 0; /* worklet has no drive */ } };
      const wobble = { setMix: (v) => { fxState.wobble = +v || 0; /* worklet has no wobble */ } };
      const fx = { setMix: (v) => { fxState.master = Math.max(0, Math.min(1, +v || 0)); applyFx(); } };
      const volume = {
        setMix: (v) => {
          // notepat sends mix*2 (0..2). Worklet's volume control accepts 0..1.
          // Apply to BOTH outputGain (instant gate) and worklet (post-FX).
          const raw = Math.max(0, Math.min(2, Number(v) || 0));
          const g = raw * 0.6; // map to outputGain
          pendingMasterVolume = g;
          if (outputGain) outputGain.gain.setTargetAtTime(g, audioCtx.currentTime, 0.02);
          postWorklet({ type: "volume", data: Math.min(1, raw) });
        }
      };
      // ---------- microphone capture ----------
      // notepat calls sound.microphone.rec() to start sampling and
      // sound.microphone.cut() to stop. cut() must dump the captured
      // PCM into the sample slot so a subsequent sound.sample.getData()
      // returns it.
      let micStream = null;
      let micSrc = null;
      let micProc = null;
      let micRecording = false;
      let micChunks = [];
      let micCapturedLen = 0;
      let micLastError = "";
      // Dedupe in-flight getUserMedia. Without this, every press of REC
      // before the user resolves the first permission prompt fires another
      // getUserMedia, generating duplicate prompts and AbortError races.
      let micPendingPromise = null;

      function ensureMicStream() {
        if (micStream) return Promise.resolve(micStream);
        if (micPendingPromise) return micPendingPromise;
        if (!navigator.mediaDevices?.getUserMedia) {
          micLastError = "no-getUserMedia";
          return Promise.resolve(null);
        }
        micPendingPromise = (async () => {
          try {
            const stream = await navigator.mediaDevices.getUserMedia({
              audio: { echoCancellation: false, noiseSuppression: false, autoGainControl: false }
            });
            ensureAudio();
            if (!audioCtx) return null;
            micSrc = audioCtx.createMediaStreamSource(stream);
            micProc = audioCtx.createScriptProcessor(4096, 1, 1);
            micProc.onaudioprocess = (e) => {
              if (!micRecording) return;
              const input = e.inputBuffer.getChannelData(0);
              // Cap at MAX_REC_SECS = 10 s.
              const max = audioCtx.sampleRate * 10;
              if (micCapturedLen >= max) return;
              const remain = max - micCapturedLen;
              const slice = input.length <= remain ? new Float32Array(input) : new Float32Array(input.subarray(0, remain));
              micChunks.push(slice);
              micCapturedLen += slice.length;
            };
            const muteSink = audioCtx.createGain();
            muteSink.gain.value = 0;
            micSrc.connect(micProc).connect(muteSink).connect(audioCtx.destination);
            micStream = stream;
            micLastError = "";
            return stream;
          } catch (err) {
            micLastError = String(err?.name || err?.message || err);
            console.warn("[ac-native-wasm] mic permission denied:", micLastError);
            return null;
          } finally {
            // Clear so a future call can retry after a denial.
            micPendingPromise = null;
          }
        })();
        return micPendingPromise;
      }

      function micRec() {
        ensureAudio();
        if (!audioCtx) { micLastError = "no-audio-ctx"; return false; }
        // Reset capture state and arm. If the stream isn't ready yet, the
        // promise resolution will install the processor and chunks will
        // start flowing then. Optimistic true return keeps notepat's UI
        // happy; if permission is denied micChunks stays empty and cut()
        // returns 0.
        micChunks = [];
        micCapturedLen = 0;
        micRecording = true;
        ensureMicStream();
        return true;
      }

      function micCut() {
        if (!micRecording) return 0;
        micRecording = false;
        if (micCapturedLen <= 0) return 0;
        const buf = new Float32Array(micCapturedLen);
        let off = 0;
        for (const c of micChunks) { buf.set(c, off); off += c.length; }
        micChunks = [];
        const len = micCapturedLen;
        micCapturedLen = 0;
        // Push into sample player so sound.sample.getData() returns it.
        samplePlayer.loadData(buf, audioCtx.sampleRate);
        return len;
      }

      return {
        synth,
        kill,
        get microphone() {
          return {
            rec: micRec,
            cut: micCut,
            sampleRate: audioCtx?.sampleRate || 48000,
            sampleLength: micCapturedLen,
            connected: !!micStream,
            device: micStream ? "default" : "",
            lastError: micLastError,
            level: 0
          };
        },
        deck: { play: () => {}, setSpeed: () => {}, kill: () => {}, stop: () => {} },
        replay: replayPlayer,
        sample: samplePlayer,
        speaker: { drawStrip, getRecentBuffer },
        room, glitch, drive, wobble, fx, volume
      };
    }

    // ---------- QR ----------
    // Cache: text → Uint8Array bit matrix flattened row-major, plus size.
    const qrCache = new Map();
    function buildQRMatrix(text) {
      if (qrCache.has(text)) return qrCache.get(text);
      if (typeof QRCode === "undefined") return null;
      // QRCode.js requires a DOM container. We give it a hidden one and
      // pull the bit matrix out via the inner _oQRCode. Lower CorrectLevel
      // = fewer modules = bigger pixels at the same on-screen footprint.
      const div = document.createElement("div");
      div.style.cssText = "position:absolute;visibility:hidden;width:0;height:0;overflow:hidden";
      document.body.appendChild(div);
      try {
        const inst = new QRCode(div, {
          text: String(text),
          width: 64, height: 64,
          correctLevel: QRCode.CorrectLevel.M
        });
        const inner = inst._oQRCode;
        const size = inner.getModuleCount();
        const bits = new Uint8Array(size * size);
        for (let r = 0; r < size; r++) {
          for (let c = 0; c < size; c++) {
            bits[r * size + c] = inner.isDark(r, c) ? 1 : 0;
          }
        }
        const result = { size, bits };
        qrCache.set(text, result);
        return result;
      } catch (err) {
        console.warn("[ac-native-wasm] qr encode failed:", err);
        qrCache.set(text, null);
        return null;
      } finally {
        document.body.removeChild(div);
      }
    }
    globalThis.qr = (text, x, y, scale) => {
      const m = buildQRMatrix(text);
      const s = Math.max(1, Math.trunc(scale || 1));
      const prevInk = clone(currentInk);
      // 1-module quiet zone, white background
      const quiet = 1;
      const matrixSize = m ? m.size : 21; // fallback placeholder
      const total = (matrixSize + quiet * 2) * s;
      setInk(255, 255, 255, 255);
      rasterBox(x, y, total, total, true);
      if (m) {
        setInk(0, 0, 0, 255);
        const ox = x + quiet * s, oy = y + quiet * s;
        for (let r = 0; r < m.size; r++) {
          for (let c = 0; c < m.size; c++) {
            if (m.bits[r * m.size + c]) {
              rasterBox(ox + c * s, oy + r * s, s, s, true);
            }
          }
        }
      } else {
        setInk(180, 60, 60, 255);
        rasterBox(x, y, total, total, false);
      }
      setInk(prevInk[0], prevInk[1], prevInk[2], prevInk[3]);
    };

    const pieces = {};
    function makeStubPiece(title, describe) {
      let lines = [];
      return {
        boot({ system, params, colon }) {
          lines = [
            title,
            describe({ params, colon, system }),
            "This prototype is running the real native " + PIECE_NAME + " piece,",
            "but other native pieces are still browser stubs.",
            "Press Backspace or Escape to return."
          ];
        },
        act({ event, system }) {
          if (event.is("keyboard:down") && (event.key === "backspace" || event.key === "escape")) {
            system.jump(PIECE_NAME);
          }
        },
        sim() {},
        paint({ wipe, ink, box, write, screen }) {
          const T = globalThis.__theme.update();
          wipe(T.bg[0], T.bg[1], T.bg[2]);
          ink(T.accent[0], T.accent[1], T.accent[2]);
          box(36, 36, screen.width - 72, screen.height - 72, true);
          ink(T.bg[0], T.bg[1], T.bg[2]);
          box(40, 40, screen.width - 80, screen.height - 80, true);
          ink(T.fg, T.fg, T.fg + 10);
          lines.forEach((line, index) => {
            write(line, { x: 56, y: 64 + index * 16, size: 1, font: "6x10" });
          });
        }
      };
    }

    pieces.lisp = makeStubPiece("lisp", () => "KidLisp eval is not wired into the browser host yet.");
    pieces.list = makeStubPiece("list", ({ system }) => "Available offline pieces: " + system.listPieces().join(", "));
    pieces.claude = makeStubPiece("claude", () => "Claude integration is native-only for now.");
    pieces.link = makeStubPiece("link", ({ params }) => params[0] ? "Link code: " + params[0] : "No link code provided.");
    pieces.login = pieces.link;

    const pieceUrl = URL.createObjectURL(new Blob([rewrittenSource], { type: "text/javascript" }));
    const loadedPieceModule = await import(pieceUrl);
    pieces[PIECE_NAME] = loadedPieceModule;

    function normalizeJumpTarget(spec) {
      const raw = String(spec || PIECE_NAME).trim() || PIECE_NAME;
      const colonIndex = raw.indexOf(":");
      const baseSpec = colonIndex >= 0 ? raw.slice(0, colonIndex) : raw;
      const colon = colonIndex >= 0 ? raw.slice(colonIndex + 1).split(":").filter(Boolean) : [];
      const spaceIndex = baseSpec.indexOf(" ");
      const pieceName = (spaceIndex >= 0 ? baseSpec.slice(0, spaceIndex) : baseSpec).toLowerCase();
      const params = spaceIndex >= 0 ? baseSpec.slice(spaceIndex + 1).split(" ").filter(Boolean) : [];
      return { pieceName, params, colon, raw };
    }

    const systemApi = {
      version: "ac-native wasm " + PIECE_NAME + " prototype",
      aesthetic: { handle: null, sub: null },
      bootDevice: "browser",
      hasHdmi: false,
      hdmi: { connected: false },
      tabletMode: false,
      get sshStarted() { return false; },
      // Pending fetch state — notepat polls these each frame
      fetchPending: false,
      fetchResult: null,
      fetchError: null,
      // Binary fetch state (firmware updates etc.)
      fetchBinary: () => false,
      fetchBinaryDone: false,
      fetchBinaryOk: false,
      fetchBinaryProgress: 0,
      // Flash state (firmware flashing)
      flashUpdate: () => false,
      flashDone: false,
      flashOk: false,
      flashPhase: "idle",
      flashTargets: [],
      flashVerifiedBytes: 0,
      // UDP/WS — stubbed
      udp: { send: () => {}, on: () => {} },
      // ws stub — notepat polls connect/connecting/connected/messages every
      // frame for the chat overlay. Real WS isn't wired offline.
      ws: {
        connected: false,
        connecting: false,
        messages: [],
        connect: () => {},
        disconnect: () => {},
        send: () => {},
        on: () => {}
      },
      listPieces() { return Object.keys(pieces).sort(); },
      readFile(filePath) {
        return Object.prototype.hasOwnProperty.call(files, filePath) ? files[filePath] : "";
      },
      writeFile(filePath, value) {
        files[filePath] = String(value);
        persistFiles();
        return true;
      },
      saveConfig(key, value) {
        const config = readConfig();
        config[key] = value;
        files["/mnt/config.json"] = JSON.stringify(config, null, 2);
        persistFiles();
      },
      reboot() { state.statusText = "reboot requested (stubbed in browser)"; },
      poweroff() { state.statusText = "poweroff requested (stubbed in browser)"; },
      startSSH() { state.statusText = "ssh requested (stubbed in browser)"; },
      fetch(url, opts) {
        systemApi.fetchPending = true;
        systemApi.fetchResult = null;
        systemApi.fetchError = null;
        fetch(url, opts).then(async (r) => {
          systemApi.fetchPending = false;
          systemApi.fetchResult = await r.text();
        }).catch((err) => {
          systemApi.fetchPending = false;
          systemApi.fetchError = String(err);
        });
        return true;
      },
      fetchCancel() { systemApi.fetchPending = false; },
      fetchPost(url, body, headers) {
        state.lastPost = "POST " + url;
        console.log("[ac-native-wasm] fetchPost", { url, body, headers });
      },
      usbMidi: {
        status() { return { enabled: false, active: false, reason: "browser-stub" }; },
        enable() { return { enabled: false, active: false, reason: "browser-stub" }; },
        disable() { return { enabled: false, active: false, reason: "browser-stub" }; },
        refresh() { return { enabled: false, active: false, reason: "browser-stub" }; }
      },
      jump(spec) { loadPiece(spec); }
    };

    async function loadPiece(spec) {
      const target = normalizeJumpTarget(spec);
      state.currentPieceSpec = target.raw;
      state.currentPieceName = pieces[target.pieceName] ? target.pieceName : PIECE_NAME;
      state.currentParams = target.params;
      state.currentColon = target.colon;
      state.currentPiece = pieces[state.currentPieceName];
      state.paintCount = 0;
      state.jumpMessage = target.raw;
      state.statusText = "piece -> " + state.currentPieceName;
      if (typeof state.currentPiece.boot === "function") {
        try {
          state.currentPiece.boot({
            ...hostApi,
            system: systemApi,
            params: clone(state.currentParams),
            colon: clone(state.currentColon)
          });
        } catch (err) {
          state.statusText = "boot error: " + (err?.message || err);
          console.error(err);
        }
      }
    }

    function flushFrame() {
      ctx.putImageData(imageData, 0, 0);
      // Lines (drawn after raster, before text)
      if (lineQueue.length > 0) {
        ctx.lineWidth = 1;
        for (const ln of lineQueue) {
          const [r, g, b, a] = ln.color;
          ctx.strokeStyle = "rgba(" + r + ", " + g + ", " + b + ", " + (a / 255) + ")";
          ctx.beginPath();
          ctx.moveTo(ln.x1 + 0.5, ln.y1 + 0.5);
          ctx.lineTo(ln.x2 + 0.5, ln.y2 + 0.5);
          ctx.stroke();
        }
        lineQueue.length = 0;
      }
      // Bitmap text was rendered into the framebuffer during paint(), so
      // there's no canvas-overlay text pass anymore.
    }

    function tick() {
      hostApi.paintCount = state.paintCount;
      // Pick up density / resize changes — the framebuffer may have been
      // resized since the previous frame, so re-publish current dimensions.
      hostApi.screen = { width: WIDTH, height: HEIGHT };
      // Flush any setTimeout queue the piece installed (notepat polyfill).
      try { state.currentPiece?.__tickPendingTimeouts?.(); } catch (_) {}
      // Some pieces export a pending-timeout ticker on globalThis instead.
      try { globalThis.__tickPendingTimeouts?.(); } catch (_) {}

      const baseArgs = () => ({
        ...hostApi,
        system: systemApi,
        params: clone(state.currentParams),
        colon: clone(state.currentColon)
      });

      if (typeof state.currentPiece?.sim === "function") {
        try { state.currentPiece.sim(baseArgs()); }
        catch (err) { state.statusText = "sim error: " + err.message; console.error(err); }
      }

      while (state.events.length > 0) {
        const event = state.events.shift();
        if (typeof state.currentPiece?.act === "function") {
          try { state.currentPiece.act({ ...baseArgs(), event }); }
          catch (err) { state.statusText = "act error: " + err.message; console.error(err); }
        }
      }

      if (typeof state.currentPiece?.paint === "function") {
        try { state.currentPiece.paint(baseArgs()); }
        catch (err) {
          state.statusText = "paint error: " + err.message;
          console.error(err);
          // Show error on screen too
          ctx.fillStyle = "#000";
          ctx.fillRect(0, 0, WIDTH, HEIGHT);
          ctx.fillStyle = "#ff6b9d";
          ctx.font = "14px 'Iosevka Term', monospace";
          ctx.fillText("paint error: " + err.message, 12, 20);
          const stack = String(err.stack || "").split("\\n").slice(0, 8);
          stack.forEach((line, i) => ctx.fillText(line, 12, 44 + i * 16));
          requestAnimationFrame(tick);
          return;
        }
      }

      flushFrame();
      state.paintCount += 1;
      requestAnimationFrame(tick);
    }

    // Match the framebuffer to the viewport so the chunky pixels align
    // with the real screen, and follow viewport changes.
    applyDensity();
    let resizeTimer = 0;
    window.addEventListener("resize", () => {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(applyDensity, 80);
    });

    // Ctrl+= / Ctrl++ → bigger pixels (density up). Ctrl+- → smaller.
    window.addEventListener("keydown", (e) => {
      if (!(e.ctrlKey || e.metaKey)) return;
      if (e.key === "=" || e.key === "+") {
        e.preventDefault();
        density = Math.min(8, density + 1);
        applyDensity();
      } else if (e.key === "-" || e.key === "_") {
        e.preventDefault();
        density = Math.max(1, density - 1);
        applyDensity();
      } else if (e.key === "0") {
        e.preventDefault();
        density = 1;
        applyDensity();
      }
    }, { capture: true });

    await loadPiece(PIECE_NAME);
    screen.focus();
    requestAnimationFrame(tick);
  </script>
</body>
</html>
`;

await fs.mkdir(outputDir, { recursive: true });
const outputPath = path.join(outputDir, piece.outputBaseName);
await fs.writeFile(outputPath, html, "utf8");
await fs.mkdir(publicOutputDir, { recursive: true });
const publicOutputPath = path.join(publicOutputDir, piece.publicFileName);
await fs.writeFile(publicOutputPath, html, "utf8");

console.log(`Built offline artifact for piece "${piece.bootSpec}":`);
console.log(outputPath);
console.log("Built production route:");
console.log(publicOutputPath);
