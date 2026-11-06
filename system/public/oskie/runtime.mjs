// Both functions are serialized into an opaque-origin iframe. User modules
// execute only in its dedicated worker, never in the signed-in AC window.
export function gameWorker() {
  let canvas,
    ctx,
    game,
    mod,
    view = { width: 1920, height: 1080 },
    pointer = { x: 0, y: 0, down: false, active: false },
    keys = [],
    pads = [],
    time = 0,
    running = false,
    api;
  const urls = new Map(),
    pending = new Set();
  const color = (r = 255, g = r, b = r) => `rgb(${r},${g},${b})`;
  const fault = (e) => {
    running = false;
    postMessage({
      type: "error",
      error: String(e?.message || e).slice(0, 2000),
    });
  };
  self.addEventListener("unhandledrejection", (e) => {
    e.preventDefault();
    fault(e.reason);
  });
  const fileURL = (path) => {
    if (urls.has(path)) return urls.get(path);
    if (pending.has(path))
      throw Error("Cyclic imports are not supported: " + path);
    const file = game.files[path];
    if (!file) throw Error("Missing bundled file: " + path);
    pending.add(path);
    let body =
      file.text ?? Uint8Array.from(atob(file.base64), (c) => c.charCodeAt(0));
    if (/\.m?js$/.test(path))
      body = body.replace(
        /(\bfrom\s*|\bimport\s*\(?\s*)(["'])(\.\.?\/[^"']+)\2/g,
        (match, lead, quote, spec) => {
          const resolved = new URL(
            spec,
            "https://bundle.invalid/" + path,
          ).pathname.slice(1);
          return lead + quote + fileURL(resolved) + quote;
        },
      );
    const url = URL.createObjectURL(
      new Blob([body], {
        type: /\.m?js$/.test(path) ? "text/javascript" : file.type,
      }),
    );
    pending.delete(path);
    urls.set(path, url);
    return url;
  };
  const call = (name) => {
    if (typeof mod?.[name] === "function") mod[name](api);
  };
  let previous = 0,
    accumulator = 0;
  function frame() {
    if (!running) return;
    const stamp = performance.now();
    accumulator += Math.min(0.1, (stamp - previous) / 1000);
    previous = stamp;
    try {
      while (accumulator >= 1 / 60) {
        time += 1 / 60;
        call("sim");
        accumulator -= 1 / 60;
      }
      call("paint");
    } catch (e) {
      fault(e);
      return;
    }
    setTimeout(frame, 1000 / 60);
  }
  self.onmessage = async ({ data: d }) => {
    try {
      if (d.type === "input") {
        pointer = d.pointer;
        keys = d.keys;
        pads = d.pads;
        return;
      }
      if (d.type === "size") {
        view = { width: (1080 * d.width) / d.height, height: 1080 };
        canvas.width = Math.min(1920, d.width);
        canvas.height = Math.round((canvas.width * d.height) / d.width);
        ctx.setTransform(
          canvas.width / view.width,
          0,
          0,
          canvas.height / 1080,
          0,
          0,
        );
        return;
      }
      if (d.type === "capture") {
        if (canvas && running) {
          const blob = await canvas.convertToBlob({ type: "image/png" });
          if (blob.size < 650000)
            postMessage({ type: "frame", bytes: await blob.arrayBuffer() });
        }
        return;
      }
      if (d.type !== "start" || canvas) return;
      canvas = d.canvas;
      ctx = canvas.getContext("2d");
      game = d.bundle;
      view = { width: (1080 * canvas.width) / canvas.height, height: 1080 };
      ctx.setTransform(
        canvas.width / view.width,
        0,
        0,
        canvas.height / 1080,
        0,
        0,
      );
      api = Object.freeze({
        manifest: game.manifest,
        author: game.manifest.author,
        engine: "ac-game-1",
        view: () => ({ ...view }),
        now: () => time,
        pointer: () => ({ ...pointer }),
        keys: () => [...keys],
        cursor: (name) => postMessage({ type: "cursor", name: String(name) }),
        pad: (i = 0) =>
          pads[i] || {
            connected: keys.length > 0,
            down: keys.map(
              (k) =>
                ({
                  KeyW: "ArrowUp",
                  KeyA: "ArrowLeft",
                  KeyS: "ArrowDown",
                  KeyD: "ArrowRight",
                  KeyJ: "X",
                  KeyK: "A",
                  Space: "B",
                  Enter: "A",
                })[k] || k,
            ),
            leftX: 0,
            leftY: 0,
            rightX: 0,
            rightY: 0,
          },
        wipe: (r = 0, g = r, b = r) => {
          ctx.fillStyle = color(r, g, b);
          ctx.fillRect(0, 0, view.width, 1080);
        },
        box: (x, y, w, h, r, g, b) => {
          ctx.fillStyle = color(r, g, b);
          ctx.fillRect(x, y, w, h);
        },
        circle: (x, y, radius, r, g, b) => {
          ctx.fillStyle = color(r, g, b);
          ctx.beginPath();
          ctx.arc(x, y, Math.max(0, radius), 0, Math.PI * 2);
          ctx.fill();
        },
        line: (x, y, x2, y2, width = 2, r, g, b) => {
          ctx.strokeStyle = color(r, g, b);
          ctx.lineWidth = width;
          ctx.beginPath();
          ctx.moveTo(x, y);
          ctx.lineTo(x2, y2);
          ctx.stroke();
        },
        triangle: (x, y, x2, y2, x3, y3, r, g, b) => {
          ctx.fillStyle = color(r, g, b);
          ctx.beginPath();
          ctx.moveTo(x, y);
          ctx.lineTo(x2, y2);
          ctx.lineTo(x3, y3);
          ctx.closePath();
          ctx.fill();
        },
        write: (text, x, y, size = 32, r, g, b) => {
          ctx.fillStyle = color(r, g, b);
          ctx.font = `${size}px monospace`;
          ctx.textBaseline = "top";
          ctx.fillText(String(text), x, y);
        },
        textWidth: (text, size = 32) => {
          ctx.font = `${size}px monospace`;
          return ctx.measureText(String(text)).width;
        },
        tone: (hz, seconds = 0.12) =>
          postMessage({
            type: "tone",
            hz: Number(hz),
            seconds: Number(seconds),
          }),
        asset: fileURL,
        screen: () => "game",
        players: () => [],
      });
      mod = await import(fileURL(game.manifest.entry));
      call("boot");
      running = true;
      postMessage({ type: "ready" });
      previous = performance.now();
      frame();
    } catch (e) {
      fault(e);
    }
  };
}
export function frameBootstrap(workerSource) {
  const canvas = document.createElement("canvas");
  canvas.style.cssText =
    "width:100%;height:100%;display:block;touch-action:none";
  document.body.append(canvas);
  document.body.style.cssText = "margin:0;overflow:hidden;background:#230f3c";
  let worker,
    port,
    audio,
    keys = new Set(),
    pointer = { x: 0, y: 0, down: false, active: false };
  const sendInput = () => {
    let pads = [];
    try {
      pads = [...navigator.getGamepads()]
        .filter(Boolean)
        .map((p) => ({
          connected: true,
          down: p.buttons.flatMap((b, i) =>
            b.pressed
              ? [
                  [
                    "A",
                    "B",
                    "X",
                    "Y",
                    "LeftShoulder",
                    "RightShoulder",
                    "LeftTrigger",
                    "RightTrigger",
                    "View",
                    "Menu",
                    "LeftStick",
                    "RightStick",
                    "ArrowUp",
                    "ArrowDown",
                    "ArrowLeft",
                    "ArrowRight",
                  ][i],
                ]
              : [],
          ),
          leftX: p.axes[0] || 0,
          leftY: -(p.axes[1] || 0),
          rightX: p.axes[2] || 0,
          rightY: -(p.axes[3] || 0),
        }));
    } catch {}
    worker?.postMessage({ type: "input", pointer, keys: [...keys], pads });
  };
  const point = (e) => {
    pointer = {
      x: (e.clientX / innerHeight) * 1080,
      y: (e.clientY / innerHeight) * 1080,
      active: true,
      down:
        e.type === "pointerdown"
          ? true
          : e.type === "pointerup" || e.type === "pointercancel"
            ? false
            : pointer.down,
    };
    sendInput();
  };
  for (const type of [
    "pointerdown",
    "pointerup",
    "pointermove",
    "pointercancel",
  ])
    canvas.addEventListener(type, point);
  canvas.addEventListener("pointerdown", (e) => {
    canvas.setPointerCapture(e.pointerId);
    if (!audio) audio = new AudioContext();
    void audio.resume();
  });
  addEventListener("keydown", (e) => {
    e.preventDefault();
    keys.add(e.code);
    sendInput();
  });
  addEventListener("keyup", (e) => {
    keys.delete(e.code);
    sendInput();
  });
  addEventListener("blur", () => {
    keys.clear();
    pointer.down = false;
    sendInput();
  });
  addEventListener("resize", () =>
    worker?.postMessage({
      type: "size",
      width: Math.max(1, innerWidth),
      height: Math.max(1, innerHeight),
    }),
  );
  addEventListener("message", (e) => {
    if (e.source !== parent || e.data?.type !== "start" || port || !e.ports[0])
      return;
    port = e.ports[0];
    worker = new Worker(
      URL.createObjectURL(
        new Blob([`(${workerSource})()`], { type: "text/javascript" }),
      ),
      { name: "AC .oskie game" },
    );
    worker.onmessage = ({ data: d }) => {
      if (["ready", "frame", "error"].includes(d.type)) port.postMessage(d);
      if (
        d.type === "cursor" &&
        ["default", "none", "crosshair", "pointer"].includes(d.name)
      )
        canvas.style.cursor = d.name;
      if (
        d.type === "tone" &&
        audio &&
        Number.isFinite(d.hz) &&
        Number.isFinite(d.seconds)
      ) {
        const osc = audio.createOscillator(),
          gain = audio.createGain();
        osc.frequency.value = Math.max(20, Math.min(20000, d.hz));
        gain.gain.setValueAtTime(0.05, audio.currentTime);
        gain.gain.exponentialRampToValueAtTime(
          0.001,
          audio.currentTime + Math.max(0.01, Math.min(2, d.seconds)),
        );
        osc.connect(gain).connect(audio.destination);
        osc.start();
        osc.stop(audio.currentTime + Math.max(0.01, Math.min(2, d.seconds)));
      }
    };
    worker.onerror = (e) =>
      port.postMessage({
        type: "error",
        error: e.message || "Game worker could not start.",
      });
    port.onmessage = ({ data: d }) => {
      if (d.type === "capture") worker.postMessage(d);
    };
    canvas.width = Math.min(1280, Math.max(1, innerWidth));
    canvas.height = Math.round(
      (canvas.width * Math.max(1, innerHeight)) / Math.max(1, innerWidth),
    );
    const offscreen = canvas.transferControlToOffscreen();
    worker.postMessage(
      { type: "start", canvas: offscreen, bundle: e.data.bundle },
      [offscreen],
    );
    setInterval(sendInput, 100);
  });
}
export function sandboxDocument(nonce) {
  const source =
    `(${frameBootstrap.toString()})(${JSON.stringify(gameWorker.toString())})`.replace(
      /<\//g,
      "<\\/",
    );
  return `<!doctype html><html><head><meta charset="utf-8"><meta http-equiv="Content-Security-Policy" content="default-src 'none'; script-src 'nonce-${nonce}' blob:; worker-src blob:; img-src blob: data:; media-src blob:; style-src 'unsafe-inline'; connect-src 'none'; base-uri 'none'; form-action 'none'"></head><body><script nonce="${nonce}">${source}</script></body></html>`;
}
