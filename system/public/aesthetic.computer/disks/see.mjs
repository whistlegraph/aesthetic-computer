// see, 26.04.23
// Budgeted Cloudflare FLUX.1 schnell image generation via /api/flux.
//
// Usage:
//   see                              — show usage
//   see a happy frog                 — generate with default kidlisp preset
//   see:warm a happy frog            — soft pastel mascot preset
//   see:raw photorealistic frog      — no AC style suffix, raw FLUX
//
// Tap to generate another variation. Backspace to clear and re-prompt.

const { floor, min, max } = Math;

let state = "empty"; // "empty" | "loading" | "ready" | "error"
let promptText = "";
let presetName = "kidlisp";
let bitmap = null; // { width, height, pixels: Uint8ClampedArray }
let errorMsg = "";
let seedNum = null; // null = let server roll
let elapsedMs = 0;
let providerName = "";
let ellipsis = 0;
let frame = 0;
let abortController = null;
let retryAt = 0;

function boot({ params, colon, hud }) {
  hud.label("see");
  if (colon[0]) presetName = colon[0];
  promptText = (params || []).join(" ").trim();
  if (promptText) generate();
}

function meta() {
  return {
    title: "see",
    desc: "Generate an image in your AC palette.",
  };
}

async function generate() {
  if (!promptText || Date.now() < retryAt) return;
  state = "loading";
  bitmap = null;
  errorMsg = "";
  ellipsis = 0;

  abortController?.abort();
  const requestController = new AbortController();
  abortController = requestController;

  try {
    const res = await fetch("/api/flux", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        prompt: promptText,
        preset: presetName,
        ...(seedNum !== null ? { seed: seedNum } : {}),
      }),
      signal: requestController.signal,
    });

    const data = await res.json();
    if (requestController.signal.aborted) return;
    if (!data.ok) {
      const retrySeconds = Number(
        res.headers.get("Retry-After") || data.retry_after,
      );
      retryAt =
        Number.isFinite(retrySeconds) && retrySeconds > 0
          ? Date.now() + retrySeconds * 1000
          : 0;
      state = "error";
      errorMsg =
        data.reason === "filtered"
          ? "blocked by safety filter — try different wording"
          : data.reason === "image_budget_exhausted"
            ? "image allowance is used up"
            : data.reason === "busy"
              ? "image generation is busy"
              : "image generation is temporarily unavailable";
      return;
    }

    elapsedMs = data.elapsed_ms;
    const nextSeed = parseInt(data.seed, 10);
    seedNum = Number.isInteger(nextSeed) ? nextSeed : null;
    providerName = data.provider || "cloudflare";
    const decoded = await dataUrlToBitmap(data.png, requestController.signal);
    if (requestController.signal.aborted) return;
    bitmap = decoded;
    state = "ready";
  } catch (err) {
    if (requestController.signal.aborted || err.name === "AbortError") return;
    state = "error";
    errorMsg = err.message;
  }
}

// Pieces run in a worker: decode without DOM Image/document APIs.
async function dataUrlToBitmap(dataUrl, signal) {
  const response = await fetch(dataUrl, { signal });
  const image = await createImageBitmap(await response.blob());
  try {
    const canvas = new OffscreenCanvas(image.width, image.height);
    const context = canvas.getContext("2d");
    context.drawImage(image, 0, 0);
    const pixels = context.getImageData(0, 0, image.width, image.height).data;
    return { width: image.width, height: image.height, pixels };
  } finally {
    image.close();
  }
}

function paint({ wipe, ink, paste, write, screen }) {
  frame++;
  const w = screen.width;
  const h = screen.height;

  // Black background — matches the kidlisp preset's own background, looks
  // intentional regardless of preset.
  wipe(0);

  if (state === "ready" && bitmap) {
    // Fit the entire image on small screens; retain crisp integer upscaling.
    const fit = min(w / bitmap.width, h / bitmap.height);
    const scale = fit >= 1 ? floor(fit) : fit;
    const drawW = bitmap.width * scale;
    const drawH = bitmap.height * scale;
    const x = floor((w - drawW) / 2);
    const y = floor((h - drawH) / 2);
    paste(bitmap, x, y, scale);

    // Subtle status footer
    const seedLabel = seedNum === null ? "" : ` · seed ${seedNum}`;
    const footer = `${elapsedMs}ms · ${providerName}${seedLabel} · ${presetName}`;
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
    const retrySeconds = Math.max(0, Math.ceil((retryAt - Date.now()) / 1000));
    ink(120).write(
      retrySeconds ? `retry in ${retrySeconds}s` : "tap to retry",
      {
        center: "x",
        y: floor(h / 2) + 24,
      },
    );
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
      // retry the current subject
      generate();
    } else if (state === "ready") {
      // generate another variation
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
