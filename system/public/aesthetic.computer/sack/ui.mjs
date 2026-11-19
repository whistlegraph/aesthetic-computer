// Sack UI, 2026.11.18
// The main-thread half of the `sack` piece. It draws nothing: the piece paints
// the whole screen from the state signalled here. This half does what only the
// main thread can — open a file dialog, catch a drop, send files to storage in
// parts, play audio, start a download — and talks to /api/sack.
//
//   piece → here   sack:zone { zone, width }   where the drop zone is drawn
//                  sack:get { url }  sack:copy  sack:play { id }
//                  sack:remove { id }  sack:delete
//   here → piece   { type: "sack:state", code, sack, mine, yours, uploads,
//                    note, over, playing, loaded, missing, gone, origin, seq }

const API = "/api/sack";
const PARALLEL = 3; // parts in flight per file
const TRIES = 4; // per part

const root = document.querySelector("#sack");
const state = { code: root?.dataset.code || "", sack: null, mine: null, uploads: [], note: "",
  over: false, playing: null, loaded: false, missing: false, gone: false };
let authToken, seq = 0, queued = false;

// Tell the piece, at most once a frame.
function push() {
  if (queued) return;
  queued = true;
  requestAnimationFrame(() => {
    queued = false;
    if (!root.isConnected) return;
    window.signal({
      type: "sack:state", seq: seq++, origin: location.origin,
      code: state.code, sack: state.sack, mine: state.mine, yours: yours(),
      uploads: state.uploads.map(({ name, file, sent, status, error }) => ({ name, bytes: file.size, sent, status, error })),
      note: state.note, over: state.over, playing: state.playing,
      loaded: state.loaded, missing: state.missing, gone: state.gone,
    });
  });
}

async function token() {
  if (authToken) return authToken;
  try { authToken = window.acTOKEN || await window.auth0Client?.getTokenSilently(); }
  catch { authToken = null; }
  return authToken;
}

async function api(method, { query = "", body } = {}) {
  const auth = await token();
  const res = await fetch(API + query, {
    method,
    headers: { ...(auth ? { Authorization: `Bearer ${auth}` } : {}), ...(body ? { "Content-Type": "application/json" } : {}) },
    body: body ? JSON.stringify(body) : undefined,
  });
  const data = await res.json().catch(() => ({}));
  if (!res.ok) throw Object.assign(new Error(data.error || `sack answered ${res.status}`), { status: res.status, data });
  return data;
}

const yours = () => !!(state.sack && state.mine && state.sack.handle === state.mine.handle);
const say = (text) => { state.note = text; push(); };

export function size(n) {
  const units = ["B", "KB", "MB", "GB", "TB"];
  let u = 0;
  while (n >= 1000 && u < units.length - 1) { n /= 1000; u++; }
  return (n >= 100 || u === 0 ? Math.round(n) : n.toFixed(1).replace(/\.0$/, "")) + " " + units[u];
}

// ── Uploading ───────────────────────────────────────────────────────────────
function putPart(url, blob, onProgress) {
  return new Promise((resolve, reject) => {
    const xhr = new XMLHttpRequest();
    xhr.open("PUT", url);
    xhr.upload.onprogress = (e) => onProgress(e.loaded);
    xhr.onload = () => xhr.status >= 200 && xhr.status < 300 ? resolve() : reject(new Error(`storage answered ${xhr.status}`));
    xhr.onerror = () => reject(new Error("connection lost"));
    xhr.send(blob);
  });
}

async function sendFile(upload, partBytes) {
  const sent = new Array(upload.parts.length).fill(0);
  const tick = () => { upload.sent = sent.reduce((a, b) => a + b, 0); push(); };
  let next = 0;
  const lane = async () => {
    while (next < upload.parts.length) {
      const n = next++;
      const blob = upload.file.slice(n * partBytes, (n + 1) * partBytes);
      for (let attempt = 1; ; attempt++) {
        try {
          await putPart(upload.parts[n], blob, (loaded) => { sent[n] = loaded; tick(); });
          sent[n] = blob.size; tick();
          break;
        } catch (error) {
          sent[n] = 0; tick();
          if (attempt >= TRIES) throw error;
          await new Promise((wait) => setTimeout(wait, attempt * 1500));
        }
      }
    }
  };
  await Promise.all(Array.from({ length: Math.min(PARALLEL, upload.parts.length) }, lane));
}

async function drop(fileList) {
  const files = [...fileList].filter((file) => file.size > 0);
  if (!files.length) return;
  if (!(await token())) return say("log in to keep a sack");
  if (state.code && !yours()) return say("this sack is not yours: open `sack` to start your own");
  say("");
  const name = files.length === 1 ? files[0].name : `${files[0].name} + ${files.length - 1} more`;
  let begun;
  try {
    begun = await api("POST", { body: { action: "begin", code: state.code || undefined, name,
      files: files.map((file) => ({ name: file.name, bytes: file.size, type: file.type })) } });
  } catch (error) {
    const d = error.data;
    return say(error.status === 413 && d
      ? `not enough space: you use ${size(d.used)} of ${size(d.limit)} and this drop is ${size(d.adding)}` : error.message);
  }
  state.code = begun.code;
  const batch = begun.uploads.map((upload, i) => ({ ...upload, file: files[i], sent: 0, status: "waiting" }));
  state.uploads.push(...batch);
  push();
  for (const upload of batch) {
    upload.status = "sending"; push();
    try {
      await sendFile(upload, begun.partBytes);
      upload.status = "saving"; push();
      const done = await api("POST", { body: { action: "commit", code: begun.code, ids: [upload.id] } });
      if (done.failed.includes(upload.id)) throw new Error("storage did not keep it");
      state.sack = done.sack;
      state.uploads = state.uploads.filter((each) => each !== upload);
    } catch (error) {
      upload.status = "failed"; upload.error = error.message;
    }
    push();
  }
  await load();
}

async function load() {
  const [sack, mine] = await Promise.all([
    state.code ? api("GET", { query: `?code=${encodeURIComponent(state.code)}` }).then((d) => d.sack)
      .catch((error) => { state.missing = error.status === 404; if (!state.missing) state.note = error.message; return null; }) : null,
    token().then((auth) => auth ? api("GET").catch(() => null) : null),
  ]);
  if (sack || !state.code) state.sack = sack;
  state.mine = mine;
  state.loaded = true;
  push();
}

// ── Mount ───────────────────────────────────────────────────────────────────
if (root) {
  // The only element: an invisible file picker the piece keeps over its zone.
  const style = document.createElement("style");
  style.textContent = `#content { pointer-events: none; }
    #sack label { position: absolute; display: none; pointer-events: auto; cursor: pointer; opacity: 0; }`;
  const input = Object.assign(document.createElement("input"), { type: "file", multiple: true, hidden: true });
  input.addEventListener("change", () => { drop(input.files); input.value = ""; });
  const picker = document.createElement("label");
  picker.append(input);
  const audio = new Audio();
  audio.addEventListener("ended", () => { state.playing = null; push(); });
  root.append(style, picker);

  // Canvas pixels to page pixels: the content frame covers the canvas.
  window.when("sack:zone", ({ zone, width }) => {
    if (!zone) return void (picker.style.display = "none");
    const k = root.parentElement.clientWidth / width;
    Object.assign(picker.style, { display: "block", left: zone.x * k + "px", top: zone.y * k + "px",
      width: zone.w * k + "px", height: zone.h * k + "px" });
  });
  window.when("sack:get", ({ url }) => { location.href = url; }); // an attachment: the page stays
  window.when("sack:copy", async () => {
    const url = `${location.origin}/sack~${state.code}`;
    try { await navigator.clipboard.writeText(url); } catch { say(url); }
  });
  window.when("sack:play", async ({ id }) => {
    const file = state.sack?.files.find((each) => each.id === id);
    audio.pause();
    if (!file || state.playing === id) { state.playing = null; return push(); }
    audio.src = file.inline;
    try { await audio.play(); state.playing = id; }
    catch { state.playing = null; location.href = file.download; } // no autoplay here: download instead
    push();
  });
  window.when("sack:remove", async ({ id }) => {
    try { state.sack = (await api("DELETE", { query: `?code=${state.code}&file=${id}` })).sack; await load(); }
    catch (error) { say(error.message); }
  });
  window.when("sack:delete", async () => {
    try { await api("DELETE", { query: `?code=${state.code}` }); audio.pause(); state.gone = true; push(); }
    catch (error) { say(error.message); }
  });

  // Catch every drop on the page before the runtime's own handler, which
  // would decode a dropped .wav into memory.
  const hasFiles = (e) => [...(e.dataTransfer?.types || [])].includes("Files");
  const listen = (type, fn) => {
    const handler = (e) => {
      if (!root.isConnected) { audio.pause(); return window.removeEventListener(type, handler, true); }
      if (!hasFiles(e)) return;
      e.preventDefault(); e.stopPropagation();
      fn(e);
    };
    window.addEventListener(type, handler, true);
  };
  const setOver = (on) => { if (state.over !== on) { state.over = on; push(); } };
  listen("dragover", (e) => { e.dataTransfer.dropEffect = "copy"; setOver(true); });
  listen("dragleave", () => setOver(false));
  listen("drop", (e) => { setOver(false); drop(e.dataTransfer.files); });
  window.addEventListener("beforeunload", (e) => {
    if (root.isConnected && state.uploads.some((upload) => ["waiting", "sending", "saving"].includes(upload.status))) e.preventDefault();
  });
  push();
  load();
}
