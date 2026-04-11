import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import wabtFactory from "wabt";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const nativeRoot = path.resolve(__dirname, "..");
const promptPath = path.join(nativeRoot, "pieces", "prompt.mjs");
const outputDir = path.join(nativeRoot, "build");
const outputPath = path.join(outputDir, "ac-native-prompt-offline.html");
const publicOutputDir = path.resolve(nativeRoot, "..", "..", "system", "public", "ac-native-wasm");
const publicOutputPath = path.join(publicOutputDir, "index.html");

const promptSource = await fs.readFile(promptPath, "utf8");
const wabt = await wabtFactory();

const watSource = String.raw`(module
  (memory (export "memory") 80)
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
  <title>AC Native Prompt WASM Prototype</title>
  <style>
    :root {
      --bg: #111013;
      --panel: rgba(255, 255, 255, 0.06);
      --border: rgba(255, 255, 255, 0.14);
      --text: #f3e9ef;
      --muted: #baa9b5;
      --accent: #ff6b9d;
    }
    * { box-sizing: border-box; }
    html, body { height: 100%; }
    body {
      margin: 0;
      display: grid;
      place-items: center;
      background:
        radial-gradient(circle at top, rgba(255, 107, 157, 0.18), transparent 34%),
        radial-gradient(circle at bottom right, rgba(93, 209, 196, 0.16), transparent 28%),
        linear-gradient(180deg, #17131a 0%, #0e0c10 100%);
      color: var(--text);
      font-family: "Iosevka Aile", "IBM Plex Sans", ui-sans-serif, sans-serif;
    }
    .shell {
      width: min(96vw, 1120px);
      padding: 18px;
      border: 1px solid var(--border);
      border-radius: 20px;
      background: rgba(10, 8, 12, 0.82);
      box-shadow: 0 32px 90px rgba(0, 0, 0, 0.45);
      backdrop-filter: blur(18px);
    }
    .bar {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 16px;
      margin-bottom: 14px;
    }
    .title {
      font-size: 14px;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      color: var(--muted);
    }
    .hint {
      font-size: 13px;
      color: var(--muted);
    }
    .hint strong { color: var(--text); font-weight: 600; }
    .stage {
      position: relative;
      width: 100%;
      aspect-ratio: 3 / 2;
      border-radius: 14px;
      overflow: hidden;
      border: 1px solid rgba(255, 255, 255, 0.08);
      background: #0e0d11;
    }
    canvas {
      width: 100%;
      height: 100%;
      display: block;
      image-rendering: pixelated;
      image-rendering: crisp-edges;
    }
    .status {
      display: flex;
      justify-content: space-between;
      gap: 16px;
      margin-top: 12px;
      font-size: 12px;
      color: var(--muted);
    }
    .status strong { color: var(--accent); }
  </style>
</head>
<body>
  <div class="shell">
    <div class="bar">
      <div class="title">AC Native Prompt • WASM Offline Prototype</div>
      <div class="hint"><strong>Type</strong> in the canvas. <strong>Esc</strong> clears. <strong>Tab</strong> completes. <strong>Backspace</strong> returns from stub pieces.</div>
    </div>
    <div class="stage">
      <canvas id="screen" width="960" height="640" tabindex="0" aria-label="AC Native Prompt WASM Prototype"></canvas>
    </div>
    <div class="status">
      <div id="status-left">booting...</div>
      <div id="status-right">single-file offline html</div>
    </div>
  </div>
  <script type="module">
    const WASM_BASE64 = ${JSON.stringify(wasmBase64)};
    const PROMPT_SOURCE = ${JSON.stringify(promptSource)};
    const screen = document.getElementById("screen");
    const ctx = screen.getContext("2d", { alpha: false });
    ctx.imageSmoothingEnabled = false;

    const statusLeft = document.getElementById("status-left");
    const statusRight = document.getElementById("status-right");

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

    const WIDTH = screen.width;
    const HEIGHT = screen.height;
    const bufferPtr = wasm.init(WIDTH, HEIGHT);
    const pixelView = new Uint8ClampedArray(wasm.memory.buffer, bufferPtr, WIDTH * HEIGHT * 4);
    const imageData = new ImageData(pixelView, WIDTH, HEIGHT);

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
      t.presets = {
        serious: {
          label: "serious", desc: "black & white",
          dark: { bg:[0,0,0], bgAlt:[10,10,10], bgDim:[0,0,0], fg:255, fgDim:160, fgMute:90, bar:[15,15,15], border:[60,60,60], accent:[128,128,128], ok:[200,200,200], err:[255,100,100], warn:[200,200,100], link:[180,180,255], pad:[10,10,10], padSharp:[5,5,5], padLine:[40,40,40], cursor:[255,255,255] },
          light: { bg:[255,255,255], bgAlt:[245,245,245], bgDim:[235,235,235], fg:0, fgDim:80, fgMute:160, bar:[240,240,240], border:[180,180,180], accent:[100,100,100], ok:[40,40,40], err:[180,40,40], warn:[120,100,20], link:[40,40,180], pad:[245,245,245], padSharp:[230,230,230], padLine:[200,200,200], cursor:[0,0,0] }
        },
        neo: {
          label: "neo", desc: "lime & black",
          dark: { bg:[0,0,0], bgAlt:[5,10,5], bgDim:[0,0,0], fg:200, fgDim:120, fgMute:60, bar:[5,15,5], border:[0,80,0], accent:[0,200,80], ok:[0,255,0], err:[255,50,50], warn:[200,255,0], link:[0,180,255], pad:[5,10,5], padSharp:[0,5,0], padLine:[0,50,0], cursor:[0,255,80] },
          light: { bg:[220,255,220], bgAlt:[230,255,230], bgDim:[200,240,200], fg:10, fgDim:60, fgMute:120, bar:[200,240,200], border:[100,180,100], accent:[0,140,60], ok:[0,120,40], err:[180,30,30], warn:[120,140,0], link:[0,80,180], pad:[210,245,210], padSharp:[190,230,190], padLine:[140,200,140], cursor:[0,120,40] }
        },
        ember: {
          label: "ember", desc: "warm amber",
          dark: { bg:[20,12,8], bgAlt:[28,18,12], bgDim:[14,8,5], fg:220, fgDim:150, fgMute:90, bar:[35,20,12], border:[60,35,20], accent:[255,140,40], ok:[120,220,80], err:[255,70,50], warn:[255,200,60], link:[255,180,100], pad:[28,18,12], padSharp:[18,10,6], padLine:[55,35,22], cursor:[255,120,30] },
          light: { bg:[255,245,230], bgAlt:[255,250,240], bgDim:[245,235,218], fg:40, fgDim:90, fgMute:140, bar:[245,232,215], border:[210,190,160], accent:[200,100,20], ok:[40,140,50], err:[190,40,30], warn:[180,120,20], link:[180,90,20], pad:[250,240,225], padSharp:[238,225,208], padLine:[220,200,175], cursor:[200,90,15] }
        }
      };
      t.apply = function(id) {
        if (!id || id === "default") {
          t._overrideId = null;
          t._override = null;
        } else if (t.presets[id]) {
          t._overrideId = id;
          t._override = t.presets[id];
        }
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
        if (t._override) {
          const mode = t.dark ? t._override.dark : t._override.light;
          if (mode) {
            for (const key of Object.keys(mode)) t[key] = mode[key];
          }
        }
        return t;
      };
      t.update();
      return t;
    })();

    function makeStorage() {
      const fallback = new Map();
      return {
        get(key) {
          try {
            return localStorage.getItem(key);
          } catch (_) {
            return fallback.has(key) ? fallback.get(key) : null;
          }
        },
        set(key, value) {
          try {
            localStorage.setItem(key, value);
          } catch (_) {
            fallback.set(key, value);
          }
        }
      };
    }

    const storage = makeStorage();
    const storageKey = "ac-native-wasm:files";

    function loadFiles() {
      const raw = storage.get(storageKey);
      if (!raw) {
        return {
          "/mnt/config.json": JSON.stringify({ piece: "prompt", darkMode: "auto" }, null, 2),
          "/mnt/wifi_creds.json": "[]"
        };
      }
      try {
        return JSON.parse(raw);
      } catch (_) {
        return {
          "/mnt/config.json": JSON.stringify({ piece: "prompt", darkMode: "auto" }, null, 2),
          "/mnt/wifi_creds.json": "[]"
        };
      }
    }

    const files = loadFiles();
    function persistFiles() {
      storage.set(storageKey, JSON.stringify(files));
    }

    const state = {
      currentPieceName: "prompt",
      currentPiece: null,
      currentPieceSpec: "prompt",
      currentParams: [],
      currentColon: [],
      paintCount: 0,
      jumpMessage: "",
      lastPost: "",
      statusText: "booting native prompt",
      events: [],
      focused: false
    };

    const wifi = {
      connected: false,
      state: 0,
      networks: [],
      _connectTimer: null,
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
          wifi.networks = [{ ssid, signal: 96, encrypted: !!pass }];
        }, 280);
      },
      disconnect() {
        clearTimeout(wifi._connectTimer);
        wifi.connected = false;
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
      if (key === "ArrowLeft") return "arrowleft";
      if (key === "ArrowRight") return "arrowright";
      if (key === "ArrowUp") return "arrowup";
      if (key === "ArrowDown") return "arrowdown";
      if (key === "Home") return "home";
      if (key === "End") return "end";
      return typeof key === "string" ? key.toLowerCase() : "";
    }

    function makeEvent(type, key) {
      return {
        key,
        type,
        is(match) {
          return type === match;
        }
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
      if (blockedKeys.has(key) || key.length === 1 || key === "shift") event.preventDefault();
      state.events.push(makeEvent(key === "shift" ? "keyboard:down:shift" : "keyboard:down", key));
    });
    window.addEventListener("keyup", (event) => {
      const key = normalizeKey(event);
      if (key === "shift") {
        event.preventDefault();
        state.events.push(makeEvent("keyboard:up:shift", key));
      }
    });

    const rasterApi = (() => {
      let ink = [255, 255, 255, 255];
      function setInk(r = 255, g = 255, b = 255, a = 255) {
        ink = [r | 0, g | 0, b | 0, a == null ? 255 : a | 0];
        wasm.set_ink(ink[0], ink[1], ink[2], ink[3]);
      }
      function clampRect(x, y, w, h) {
        let rx = Math.trunc(x);
        let ry = Math.trunc(y);
        let rw = Math.trunc(w);
        let rh = Math.trunc(h);
        if (rw < 0) { rx += rw; rw = -rw; }
        if (rh < 0) { ry += rh; rh = -rh; }
        if (rx < 0) { rw += rx; rx = 0; }
        if (ry < 0) { rh += ry; ry = 0; }
        if (rx + rw > WIDTH) rw = WIDTH - rx;
        if (ry + rh > HEIGHT) rh = HEIGHT - ry;
        return rw > 0 && rh > 0 ? [rx, ry, rw, rh] : null;
      }
      return {
        wipe(r, g, b, a = 255) {
          setInk(r, g, b, a);
          wasm.clear();
        },
        ink(r, g, b, a = 255) {
          setInk(r, g, b, a);
        },
        box(x, y, w, h, filled = true) {
          const rect = clampRect(x, y, w, h);
          if (!rect) return;
          if (filled) {
            wasm.fill_rect(rect[0], rect[1], rect[2], rect[3]);
            return;
          }
          wasm.fill_rect(rect[0], rect[1], rect[2], 1);
          wasm.fill_rect(rect[0], rect[1] + rect[3] - 1, rect[2], 1);
          wasm.fill_rect(rect[0], rect[1], 1, rect[3]);
          wasm.fill_rect(rect[0] + rect[2] - 1, rect[1], 1, rect[3]);
        }
      };
    })();

    const textQueue = [];
    function drawText(text, options = {}) {
      textQueue.push({
        text: String(text),
        x: Math.trunc(options.x ?? 0),
        y: Math.trunc(options.y ?? 0),
        size: options.size ?? 1,
        font: options.font ?? "6x10",
        color: clone(currentInk)
      });
    }

    let currentInk = [255, 255, 255, 255];
    const hostApi = {
      wipe(r, g, b, a = 255) {
        currentInk = [r | 0, g | 0, b | 0, a | 0];
        rasterApi.wipe(r, g, b, a);
      },
      ink(r, g, b, a = 255) {
        currentInk = [r | 0, g | 0, b | 0, a | 0];
        rasterApi.ink(r, g, b, a);
      },
      box: rasterApi.box,
      write: drawText,
      screen: { width: WIDTH, height: HEIGHT },
      paintCount: 0,
      wifi,
      sound: {
        speakCached() {},
        speak() {}
      }
    };

    function readConfig() {
      const raw = files["/mnt/config.json"];
      if (!raw) return {};
      try {
        return JSON.parse(raw);
      } catch (_) {
        return {};
      }
    }

    const pieces = {};
    function makeStubPiece(title, describe) {
      let lines = [];
      return {
        boot({ system, params, colon }) {
          lines = [
            title,
            describe({ params, colon, system }),
            "This prototype is running the real native prompt piece,",
            "but other native pieces are still browser stubs.",
            "Press Backspace or Escape to return."
          ];
        },
        act({ event, system }) {
          if (event.is("keyboard:down") && (event.key === "backspace" || event.key === "escape")) {
            system.jump("prompt");
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
    pieces.login = makeStubPiece("login", ({ params }) => params[0] ? "Login code: " + params[0] : "No login code provided.");

    const promptUrl = URL.createObjectURL(new Blob([PROMPT_SOURCE], { type: "text/javascript" }));
    const promptPiece = await import(promptUrl);
    pieces.prompt = promptPiece;

    function normalizeJumpTarget(spec) {
      const raw = String(spec || "prompt").trim() || "prompt";
      const colonIndex = raw.indexOf(":");
      const baseSpec = colonIndex >= 0 ? raw.slice(0, colonIndex) : raw;
      const colon = colonIndex >= 0 ? raw.slice(colonIndex + 1).split(":").filter(Boolean) : [];
      const spaceIndex = baseSpec.indexOf(" ");
      const pieceName = (spaceIndex >= 0 ? baseSpec.slice(0, spaceIndex) : baseSpec).toLowerCase();
      const params = spaceIndex >= 0 ? baseSpec.slice(spaceIndex + 1).split(" ").filter(Boolean) : [];
      return { pieceName, params, colon, raw };
    }

    const systemApi = {
      version: "ac-native wasm prompt prototype",
      get sshStarted() {
        return false;
      },
      listPieces() {
        return Object.keys(pieces).sort();
      },
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
      reboot() {
        state.statusText = "reboot requested (stubbed in browser)";
      },
      poweroff() {
        state.statusText = "poweroff requested (stubbed in browser)";
      },
      startSSH() {
        state.statusText = "ssh requested (stubbed in browser)";
      },
      fetchPost(url, body, headers) {
        state.lastPost = "POST " + url;
        console.log("[ac-native-wasm] fetchPost", { url, body, headers });
      },
      usbMidi: {
        status() {
          return { enabled: false, active: false, reason: "browser-stub" };
        },
        enable() {
          return { enabled: false, active: false, reason: "browser-stub" };
        },
        disable() {
          return { enabled: false, active: false, reason: "browser-stub" };
        },
        refresh() {
          return { enabled: false, active: false, reason: "browser-stub" };
        }
      },
      jump(spec) {
        loadPiece(spec);
      }
    };

    async function loadPiece(spec) {
      const target = normalizeJumpTarget(spec);
      state.currentPieceSpec = target.raw;
      state.currentPieceName = pieces[target.pieceName] ? target.pieceName : "lisp";
      state.currentParams = target.params;
      state.currentColon = target.colon;
      state.currentPiece = pieces[state.currentPieceName];
      state.paintCount = 0;
      state.jumpMessage = target.raw;
      state.statusText = "piece -> " + state.currentPieceName;
      if (typeof state.currentPiece.boot === "function") {
        state.currentPiece.boot({
          ...hostApi,
          system: systemApi,
          params: clone(state.currentParams),
          colon: clone(state.currentColon)
        });
      }
    }

    function flushFrame() {
      ctx.putImageData(imageData, 0, 0);
      ctx.textBaseline = "top";
      ctx.font = "10px 'Iosevka Term', 'IBM Plex Mono', monospace";
      textQueue.forEach((entry) => {
        const [r, g, b, a] = entry.color;
        ctx.fillStyle = "rgba(" + r + ", " + g + ", " + b + ", " + (a / 255) + ")";
        ctx.fillText(entry.text, entry.x, entry.y);
      });
      textQueue.length = 0;
    }

    function tick() {
      hostApi.paintCount = state.paintCount;
      if (typeof state.currentPiece?.sim === "function") {
        state.currentPiece.sim({
          ...hostApi,
          system: systemApi,
          params: clone(state.currentParams),
          colon: clone(state.currentColon)
        });
      }

      while (state.events.length > 0) {
        const event = state.events.shift();
        if (typeof state.currentPiece?.act === "function") {
          state.currentPiece.act({
            ...hostApi,
            event,
            system: systemApi,
            params: clone(state.currentParams),
            colon: clone(state.currentColon)
          });
        }
      }

      if (typeof state.currentPiece?.paint === "function") {
        state.currentPiece.paint({
          ...hostApi,
          system: systemApi,
          params: clone(state.currentParams),
          colon: clone(state.currentColon)
        });
      }

      flushFrame();
      state.paintCount += 1;
      const config = readConfig();
      statusLeft.textContent = state.statusText;
      statusRight.textContent = [
        wifi.connected ? "wifi: connected" : "wifi: offline",
        config.darkMode ? "mode: " + config.darkMode : "mode: auto",
        state.focused ? "kbd: focused" : "kbd: click to focus"
      ].join(" • ");
      requestAnimationFrame(tick);
    }

    await loadPiece("prompt");
    statusLeft.textContent = "running prompt.mjs";
    statusRight.textContent = "click canvas to focus keyboard";
    screen.focus();
    requestAnimationFrame(tick);
  </script>
</body>
</html>
`;

await fs.mkdir(outputDir, { recursive: true });
await fs.writeFile(outputPath, html, "utf8");
await fs.mkdir(publicOutputDir, { recursive: true });
await fs.writeFile(publicOutputPath, html, "utf8");

console.log("Built offline artifact:");
console.log(outputPath);
console.log("Built production route:");
console.log(publicOutputPath);
