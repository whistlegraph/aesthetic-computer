// see, 26.04.23
// Free image generation via NVIDIA NIM FLUX.1 schnell, with two AC style
// presets baked into the proxy at /api/flux. Drop a prompt and a bitmap
// shows up — the model does the rest.
//
// Usage:
//   see                              — show usage
//   see a happy frog                 — generate with default kidlisp preset
//   see:warm a happy frog            — soft pastel mascot preset
//   see:raw photorealistic frog      — no AC style suffix, raw FLUX
//
// Tap to roll a new seed. Backspace to clear and re-prompt.

const { floor, min, max } = Math;

let state = "empty"; // "empty" | "loading" | "ready" | "error"
let promptText = "";
let presetName = "kidlisp";
let bitmap = null;        // { width, height, pixels: Uint8ClampedArray }
let errorMsg = "";
let seedNum = null;       // null = let server roll
let elapsedMs = 0;
let ellipsis = 0;
let frame = 0;
let abortController = null;

function boot({ params, colon, hud }) {
  hud.label("see");
  if (colon[0]) presetName = colon[0];
  promptText = (params || []).join(" ").trim();
  if (promptText) generate();
}

function meta() {
  return {
    title: "see",
    desc: "Free FLUX image generation in your AC palette.",
  };
}

async function generate() {
  if (!promptText) return;
  state = "loading";
  bitmap = null;
  errorMsg = "";
  ellipsis = 0;

  abortController?.abort();
  abortController = new AbortController();

  try {
    const res = await fetch("/api/flux", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        prompt: promptText,
        preset: presetName,
        ...(seedNum !== null ? { seed: seedNum } : {}),
      }),
      signal: abortController.signal,
    });

    const data = await res.json();
    if (!data.ok) {
      state = "error";
      errorMsg =
        data.reason === "filtered"
          ? "blocked by safety filter — try different wording"
          : data.reason === "no_key"
          ? "server has no NVIDIA_API_KEY"
          : data.reason === "timeout"
          ? "timed out — NVIDIA may be slow, tap to retry"
          : data.reason === "upstream"
          ? "NVIDIA error — tap to retry"
          : `error: ${data.reason || "unknown"}`;
      return;
    }

    elapsedMs = data.elapsed_ms;
    seedNum = parseInt(data.seed, 10);
    bitmap = await dataUrlToBitmap(data.png);
    state = "ready";
  } catch (err) {
    if (err.name === "AbortError") return;
    state = "error";
    errorMsg = err.message;
  }
}

// Decode a data:image/jpeg;base64,... URL into an AC-paste-able bitmap.
function dataUrlToBitmap(dataUrl) {
  return new Promise((resolve, reject) => {
    const img = new Image();
    img.onload = () => {
      const canvas = document.createElement("canvas");
      canvas.width = img.width;
      canvas.height = img.height;
      const ctx = canvas.getContext("2d");
      ctx.drawImage(img, 0, 0);
      const id = ctx.getImageData(0, 0, img.width, img.height);
      resolve({ width: img.width, height: img.height, pixels: id.data });
    };
    img.onerror = (e) => reject(new Error("decode failed"));
    img.src = dataUrl;
  });
}

function paint({ wipe, ink, paste, write, screen }) {
  frame++;
  const w = screen.width;
  const h = screen.height;

  // Black background — matches the kidlisp preset's own background, looks
  // intentional regardless of preset.
  wipe(0);

  if (state === "ready" && bitmap) {
    // Center, scale-to-fit with integer scale (preserves pixel crispness).
    const scale = max(1, floor(min(w / bitmap.width, h / bitmap.height)));
    const drawW = bitmap.width * scale;
    const drawH = bitmap.height * scale;
    const x = floor((w - drawW) / 2);
    const y = floor((h - drawH) / 2);
    paste(bitmap, x, y, { scale });

    // Subtle status footer
    const footer = `${elapsedMs}ms · seed ${seedNum} · ${presetName}`;
    ink(80).write(footer, { x: 6, y: h - 14 });
    ink(180).write("tap to roll", { x: w - 70, y: h - 14 });
    return;
  }

  if (state === "loading") {
    if (frame % 20 === 0) ellipsis = (ellipsis + 1) % 4;
    const dots = ".".repeat(ellipsis);
    ink(0, 255, 200).write(`generating${dots}`, { center: "xy" });
    ink(80).write(promptText, { center: "x", y: floor(h / 2) + 18 });
    return;
  }

  if (state === "error") {
    ink(255, 80, 120).write("✗", { center: "x", y: floor(h / 2) - 20 });
    ink(255, 200, 200).write(errorMsg, { center: "xy" }, undefined, w - 20);
    ink(120).write("tap to retry", { center: "x", y: floor(h / 2) + 24 });
    return;
  }

  // empty — show usage
  const lines = [
    "type a subject to see it",
    "",
    "see a happy frog",
    "see:warm a coffee mug",
    "see:raw a misty forest",
  ];
  let yy = floor(h / 2) - (lines.length * 14) / 2;
  for (const line of lines) {
    ink(line.startsWith("see") ? [0, 255, 200] : 200).write(line, {
      center: "x",
      y: yy,
    });
    yy += 14;
  }
}

function act({ event: e, sound }) {
  if (state === "loading") return;

  if (e.is("touch")) {
    if (state === "error") {
      // retry with same seed
      generate();
    } else if (state === "ready") {
      // roll a new seed
      seedNum = null;
      sound?.synth?.({ type: "sine", tone: 660, duration: 0.04, volume: 0.3 });
      generate();
    }
  }

  if (e.is("keyboard:down:backspace") || e.is("keyboard:down:escape")) {
    state = "empty";
    bitmap = null;
    errorMsg = "";
    abortController?.abort();
  }
}

function leave() {
  abortController?.abort();
  bitmap = null;
}

export { boot, paint, act, leave, meta };
