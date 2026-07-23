// Terrarium Dev, 26.07.23.12.10
// Hidden development seam for authenticated Mediorgan visits on loopback.

const ORGANS = ["sensory", "spatial", "drive", "memory", "action", "voice"];

let endpoint = "http://127.0.0.1:8787";
let token = null;
let verifiedHandle = null;
let status = "connecting";
let lastResponse = "";
let hudRef = null;
let needsPaintRef = null;

function endpointFor(colon = []) {
  const candidate = Number(colon[0]);
  const port = Number.isInteger(candidate) && candidate >= 1024 && candidate <= 65535 ? candidate : 8787;
  return `http://127.0.0.1:${port}`;
}

async function request(path, options = {}) {
  const headers = { ...(options.headers || {}), Authorization: `Bearer ${token}` };
  const response = await fetch(`${endpoint}${path}`, { ...options, headers, cache: "no-store" });
  const body = await response.json().catch(() => ({}));
  if (!response.ok) throw new Error(body.error || `request failed (${response.status})`);
  return body;
}

async function boot({ authorize, colon, hud, needsPaint }) {
  endpoint = endpointFor(colon);
  hudRef = hud;
  needsPaintRef = needsPaint;
  hudRef.label("terrarium dev", "yellow");
  try {
    token = await authorize();
    if (!token) throw new Error("log in first");
    const result = await request("/api/state");
    verifiedHandle = result.handle;
    status = "connected";
    lastResponse = `${result.state.entities.length} lives · tick ${result.state.tick}`;
    hudRef.label("terrarium dev", "lime");
  } catch (error) {
    status = "unavailable";
    lastResponse = error.message;
    hudRef.label("terrarium dev", "orange");
  }
  needsPaintRef?.();
}

async function prod(target, modality = "gesture") {
  if (!token || status !== "connected") return;
  status = `prodding ${target}`;
  needsPaintRef?.();
  try {
    const result = await request("/api/prod", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ target, modality, stimulus: "terrarium-dev-piece" }),
    });
    lastResponse = `${target} answered · event ${result.eventSeq}`;
    status = "connected";
  } catch (error) {
    status = "unavailable";
    lastResponse = error.message;
  }
  needsPaintRef?.();
}

function paint({ ink, screen }) {
  ink(9, 12, 20).box(0, 0, screen.width, screen.height);
  ink(235, 120, 190).write("MEDIORGAN", { center: "x", y: 22 }, screen);
  ink(190).write(verifiedHandle || "unverified visitor", { center: "x", y: 43 }, screen);
  ink(status === "connected" ? 120 : 230, status === "connected" ? 230 : 170, 150)
    .write(status, { center: "x", y: 61 }, screen);
  ink(150).write("keys 1–6 prod an organ", { center: "x", y: 87 }, screen);
  ORGANS.forEach((organ, index) => {
    ink(110 + index * 18, 180, 220).write(`${index + 1}  ${organ}`, { center: "x", y: 108 + index * 15 }, screen);
  });
  if (lastResponse) ink(125).write(lastResponse.slice(0, 54), { center: "x", y: screen.height - 22 }, screen);
}

function act({ event: e }) {
  if (!e.is("keyboard:down")) return;
  const index = Number(e.key) - 1;
  if (index >= 0 && index < ORGANS.length) void prod(ORGANS[index]);
}

function leave() {
  token = null;
  verifiedHandle = null;
  status = "left";
  lastResponse = "";
}

// Deliberately no meta() export: this development piece stays out of list and
// autocomplete while remaining directly reachable as `terrarium-dev`.
export { boot, paint, act, leave };
