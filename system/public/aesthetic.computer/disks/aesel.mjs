// The AC prompt edition of Aesel shares the hosted agent loop and write_piece
// tool with AC OS. Each visit owns its working file and conversation.
//
// It boots the way the terminal Aesel does: the letterhead donkey at his easel
// with the version, engine, model and file beside him, the conversation
// growing up from the typing line, and a status line with the handle and the
// braincells. That screen is drawn on the hd() layer in a monospace face, at
// the display's own resolution; previews and icons, which have no hd layer,
// get the pixel edition underneath.
import { AcBridge } from "../lib/aesel-os/aesel-ac.mjs";
import { randomSlug, wrapRows } from "../lib/aesel-os/aesel-bridge.mjs";

let desk;

// The letterhead donkey, as aesel/src/mascot.mjs draws him (TILE, TILE_INKS):
// each letter is 2x2 pixels, X his fur, E his eye, K the canvas on his easel.
const TILE = [
  "..............X.X.....KKKKKKKKKK..",
  "..............X.X.....KKKKKKKKKK..",
  "..............XXXXXX..KKKKKKKKKK..",
  "..............XEXXXX..KKKKKKKKKK..",
  "...XXXXXXXXXXXXXX.....KKKKKKKKKK..",
  "...XXXXXXXXXXXXXX.....KKKKKKKKKK..",
  "..X.XXXXXXXXXXXXX.......K....K....",
  "....XXXXXXXXXXXXX......K......K...",
  "....X.X.......X.X......K......K...",
  "..................................",
];
const TILE_INKS = { X: [120, 112, 165], E: [0, 0, 0], K: [200, 30, 100] };

// The terminal's palette (aesel/src/render.mjs), the AC prompt's dark scheme.
const PALETTE = {
  ground: [70, 50, 100], text: [255, 255, 255], handle: [255, 100, 255],
  soft: [220, 180, 255], muted: [170, 150, 205], you: [255, 90, 160],
  edit: [130, 255, 130], highlight: [255, 100, 0], error: [255, 90, 90],
};
const FONT = "15px ui-monospace, Menlo, Monaco, Consolas, monospace";
const BOLD = "bold " + FONT;
const ROW = 20; // CSS pixels a line
const PAD = 16;
const rgb = ([r, g, b]) => `rgb(${r},${g},${b})`;

function transport() {
  let request, result;
  return {
    start(body, headers) {
      this.cancel();
      const controller = new AbortController();
      request = controller;
      result = { text: "", done: false };
      const output = result;
      (async () => {
        try {
          const response = await fetch("https://aesthetic.computer/api/easel-inference", {
            method: "POST", headers: { ...headers, "Content-Type": "application/json" },
            body, signal: controller.signal,
          });
          if (!response.ok) {
            const failure = await response.json().catch(() => ({}));
            throw Error(failure.error?.message || failure.message || "Aesel returned HTTP " + response.status);
          }
          const reader = response.body.getReader(), decoder = new TextDecoder();
          for (;;) {
            const { done, value } = await reader.read();
            if (done) break;
            output.text += decoder.decode(value, { stream: true });
          }
          output.text += decoder.decode();
        } catch (error) { output.error = error.message; }
        finally { output.done = true; }
      })();
      return true;
    },
    poll: () => result,
    cancel() { request?.abort(); request = null; },
  };
}

// The version line reads what prompt.ac serves to installers.
async function readVersion(state) {
  try {
    const { version } = await (await fetch("https://aesthetic.computer/aesel.json")).json();
    if (desk === state && version) state.version = version;
  } catch {}
}

// What is left of today's free braincells plus what was bought.
async function readBraincells(api, state) {
  if (!api.user?.sub) return;
  try {
    const token = await api.authorize();
    if (!token || desk !== state) return;
    const response = await fetch("https://aesthetic.computer/api/easel-credits", { headers: { Authorization: `Bearer ${token}` } });
    if (!response.ok) return;
    const body = await response.json();
    const total = (Number(body.remaining) || 0) + (Number(body.purchased) || 0);
    if (desk === state && Number.isFinite(total)) state.braincells = total;
  } catch {}
}

function boot(api) {
  const name = randomSlug() + ".mjs";
  const state = desk = { source: "", name, lines: [], busy: false, turn: 0, status: "", offset: 0, version: "", braincells: null };
  api.hud.label("Aesel TUI");
  state.bridge = new AcBridge({
    transport: transport(), file: name, readPiece: () => state.source,
    writePiece(source) { state.source = source; return true; },
    instructions: "You are Aesel, making a JavaScript piece for the Aesthetic Computer web runtime. Use write_piece for every revision. Send the complete module with exported paint($) and optionally boot, act or sim. Use $.wipe, $.ink, $.box and $.line. Aesthetic Computer runs the module as a piece. The user can save the finished .mjs file.",
  });
  state.bridge.launch();
  state.model = String(state.bridge.model || "").split("/").pop();
  state.fields = new api.ui.TextFields(api, [{name:"line", label:">", placeholder:"Describe a piece…"}], ({line}) => submit(api, state, line));
  state.fields.input.mute = false;
  state.status = api.user?.sub ? "" : "Sign in with AC to start a thread.";
  readVersion(state);
  readBraincells(api, state);
}

async function submit(api, state, line) {
  const text = line.trim();
  if (!text || state.busy || !api.user?.sub) return;
  const turn = ++state.turn;
  state.busy = true;
  state.status = "Connecting…";
  try {
    const token = await api.authorize();
    if (desk !== state || state.turn !== turn) return;
    if (!token) throw Error("Sign in with AC, then reopen Aesel.");
    state.bridge.token = token;
    state.bridge.handshake();
    state.fields.reset(api);
    state.lines.push({ kind: "you", text });
    state.offset = 0;
    state.reply = null;
    state.bridge.say(text);
    state.status = "Aesel is working…";
  } catch (error) { if (desk === state && state.turn === turn) { state.status = error.message; state.busy = false; } }
}

function sim(api) {
  if (!desk) return;
  const s = desk;
  s.bridge.tick();
  for (const e of s.bridge.take()) {
    if (e.type === "delta") {
      if (s.reply === null) { s.reply = s.lines.length; s.lines.push({ kind: "reply", text: "" }); }
      s.lines[s.reply].text += e.text;
    }
    if (e.type === "tool" && e.status === "completed") s.lines.push({ kind: "tool", text: "Wrote " + s.name });
    if (e.type === "turn" && e.status !== "started") {
      s.busy = false;
      s.bridge.token = "";
      s.status = e.error || (e.status === "interrupted" ? "Stopped." : "");
      readBraincells(api, s);
    }
  }
}

// 🖼️ The terminal's screen, on the hd layer.
function paintTerminal(api, layer, s) {
  const { screen, display } = api;
  const css = Number(display?.subdivisions) || 1; // CSS pixels per screen pixel
  const W = screen.width * css, H = screen.height * css;
  const ctx = layer.ctx;
  ctx.save();
  ctx.scale(1 / css, 1 / css);
  ctx.fillStyle = rgb(PALETTE.ground);
  ctx.fillRect(0, 0, W, H);
  ctx.font = FONT;
  ctx.textBaseline = "middle";
  const cw = ctx.measureText("M").width;
  const cols = Math.max(8, Math.floor((W - PAD * 2) / cw));
  const say = (text, x, y, ink, font = FONT) => { ctx.font = font; ctx.fillStyle = rgb(ink); ctx.fillText(text, x, y); return x + ctx.measureText(text).width; };
  const clip = (text, n) => (text.length > n ? text.slice(0, Math.max(0, n - 1)) + "…" : text);

  // The bottom: the status line, and the typing line above it.
  const statusY = H - PAD - ROW / 2;
  const inputY = statusY - ROW * 1.6;
  s.inputY = inputY / css;

  // The conversation, bottom-anchored above the typing line, the letterhead
  // first so it scrolls away with the history, as in the terminal.
  const tileCells = TILE[0].length / 2;
  const facts = [
    null,
    [["Aesel", PALETTE.text, BOLD], [s.version ? ` v${s.version}` : "", PALETTE.muted]],
    [[`aesthetic · ${s.model || ""}`, PALETTE.soft]],
    [[`aesthetic.computer/aesel · ${s.name}`, PALETTE.muted]],
    null,
  ];
  const rows = facts.map((fact, i) => ({ tile: i, fact }));
  rows.push({ blank: true });
  const inks = { you: PALETTE.you, reply: PALETTE.text, tool: PALETTE.edit };
  for (const line of s.lines) {
    const prefix = line.kind === "you" ? "› " : "";
    for (const row of wrapRows(prefix + line.text, cols)) rows.push({ text: row.text, ink: inks[line.kind] || PALETTE.text });
    if (line.kind === "you") rows.push({ blank: true });
  }
  if (s.status) rows.push({ text: s.status, ink: /fail|error|HTTP|not /i.test(s.status) ? PALETTE.error : PALETTE.muted });

  const visible = Math.max(1, Math.floor((inputY - ROW - PAD) / ROW));
  s.offset = Math.min(s.offset, Math.max(0, rows.length - visible));
  s.overflow = rows.length > visible;
  const shown = rows.slice(Math.max(0, rows.length - visible - s.offset), rows.length - s.offset);
  shown.forEach((row, i) => {
    const top = inputY - ROW * 1.2 - (shown.length - i) * ROW;
    if (row.tile !== undefined) {
      const half = cw / 2, tall = ROW / 2;
      for (const [dy, line] of [TILE[row.tile * 2], TILE[row.tile * 2 + 1]].entries()) {
        for (let x = 0; x < line.length; x++) {
          const ink = TILE_INKS[line[x]];
          if (!ink) continue;
          ctx.fillStyle = rgb(ink);
          ctx.fillRect(Math.floor(PAD + x * half), Math.floor(top + dy * tall), Math.ceil(half), Math.ceil(tall));
        }
      }
      if (row.fact && cols > tileCells + 12) {
        let x = PAD + (tileCells + 2) * cw;
        for (const [text, ink, font] of row.fact) x = say(clip(text, cols - tileCells - 2), x, top + ROW / 2, ink, font);
      }
    } else if (!row.blank) say(row.text, PAD, top + ROW / 2, row.ink);
  });

  // The typing line: what has been typed, then a hollow caret, as idle in
  // the terminal; solid while there is text.
  const typed = s.fields.input?.text || "";
  const tail = typed.slice(-Math.max(1, cols - 2));
  const end = say(tail, PAD, inputY, PALETTE.text);
  ctx.strokeStyle = ctx.fillStyle = rgb(PALETTE.muted);
  ctx.lineWidth = 1.5;
  if (typed) ctx.fillRect(end + 1, inputY - ROW / 2 + 2, cw * 0.9, ROW - 4);
  else ctx.strokeRect(end + 1.5, inputY - ROW / 2 + 2.5, cw * 0.9 - 1, ROW - 5);

  // The status line: who, the engine and the model on the left, what can be
  // done now, and the braincells on the right. Each word is a place to tap.
  s.hits = [];
  let x = PAD;
  const word = (text, ink, action, font) => {
    const from = x;
    x = say(text, x, statusY, ink, font);
    if (action) s.hits.push({ action, from: from / css, to: x / css });
  };
  const handle = api.handle?.();
  if (api.user?.sub && handle) word(handle, PALETTE.handle);
  else if (api.user?.sub) word("signed in", PALETTE.soft);
  else word("sign in", PALETTE.highlight, "login");
  word(" · aesthetic · ", PALETTE.muted);
  word(s.model || "", PALETTE.muted);
  if (s.busy) { word("  ", PALETTE.muted); word("stop", PALETTE.highlight, "stop"); }
  if (s.source) { word("  ", PALETTE.muted); word("save .mjs", PALETTE.highlight, "save"); }
  if (s.overflow) { word("  ", PALETTE.muted); word("older", PALETTE.soft, "older"); word(" ", PALETTE.muted); word("newer", PALETTE.soft, "newer"); }
  const right = s.busy ? "working…" : Number.isFinite(s.braincells) ? `${Math.round(s.braincells).toLocaleString("en-US")} 🧠` : "";
  if (right) {
    ctx.font = FONT;
    const at = W - PAD - ctx.measureText(right).width;
    if (at > x + cw) say(right, at, statusY, s.busy ? PALETTE.highlight : PALETTE.muted);
  }
  s.statusTop = (statusY - ROW / 2) / css;
  ctx.restore();
}

// The pixel edition, for previews and icons (no hd layer).
function paintPixels(api, s) {
  const { screen, ink } = api;
  const cols = Math.max(8, Math.floor((screen.width - 10) / 6)), visible = Math.max(1, Math.floor((screen.height - 65) / 10));
  const rows = s.lines.flatMap((line) => wrapRows((line.kind === "you" ? "> " : "") + line.text, cols).map((row) => ({ text: row.text, you: line.kind === "you" })));
  rows.slice(-visible).forEach((row, i) => ink(row.you ? "pink" : "white").write(row.text, { x: 5, y: 15 + i * 10 }));
  for (let y = 0; y < TILE.length; y++) for (let x = 0; x < TILE[y].length; x++) {
    const color = TILE_INKS[TILE[y][x]];
    if (color) ink(...color).box(5 + x, screen.height - 60 + y, 1, 1);
  }
  ink("wheat").write((s.status || "Describe a piece, then press Enter.").slice(0, cols), { x: 5, y: screen.height - 45 });
}

function paint(api) {
  if (!desk) return;
  const { screen, wipe } = api, s = desk;
  wipe(...PALETTE.ground);
  const layer = api.hd?.();
  // The text field stays in the pixel buffer so a tap still reaches it (it
  // opens the keyboard on a phone); the hd layer covers it with the
  // terminal's own typing line.
  const fieldY = layer && s.inputY !== undefined ? Math.round(s.inputY - 8) : screen.height - 32;
  s.fields.paint(api, { x: 2, y: fieldY, width: screen.width - 4, height: 16 });
  if (layer) paintTerminal(api, layer, s);
  else { s.hits = []; s.statusTop = undefined; paintPixels(api, s); }
}

function act(api) {
  const s = desk, e = api.event;
  if (!s) return;
  if (e.is("touch") && s.statusTop !== undefined && e.y >= s.statusTop) {
    const hit = s.hits?.find((h) => e.x >= h.from && e.x <= h.to);
    if (hit?.action === "login") api.jump("prompt~login~!autorun");
    if (hit?.action === "stop") { s.turn++; s.bridge.interrupt(); s.bridge.token = ""; s.busy = false; s.status = "Stopped."; }
    if (hit?.action === "save") api.download(s.name, s.source, { type: "text/javascript" });
    if (hit?.action === "older") s.offset += 5;
    if (hit?.action === "newer") s.offset = Math.max(0, s.offset - 5);
    return;
  }
  if (e.is("keyboard:down:pageup")) s.offset += 5;
  if (e.is("keyboard:down:pagedown")) s.offset = Math.max(0, s.offset - 5);
  if (api.user?.sub) s.fields.act(api);
  else if (e.is("touch")) api.jump("prompt~login~!autorun");
}

function leave() {
  if (desk) { desk.bridge.interrupt(); desk.bridge.token = ""; desk = null; }
}
function meta() { return { title: "Aesel TUI", desc: "Make a piece with Aesel in the AC prompt." }; }
export { boot, sim, paint, act, leave, meta };
