// printer-serve — a printer that shows up in the Print dialog.
//
//   ./smiley/smiley --serve
//
// The system's own IPP printer (ippeveprinter, part of CUPS) announces the
// printer on the local network and hands each job to the launcher. The page is
// the harness's image input; what the harness makes is the print. This window
// is the printer's front panel: each job as it arrives, then what came out.
//
// Two processes: the window (serve) and one per job (printJob), started by
// ippeveprinter. A job cannot write to the window, so it appends events to
// spool/events.jsonl and the window reads them.
import { spawn, spawnSync } from "node:child_process";
import { appendFileSync, copyFileSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import net from "node:net";
import { homedir, tmpdir, userInfo } from "node:os";
import path from "node:path";
import process from "node:process";
import { ACSession, SITE, USER_AGENT } from "./ac-session.mjs";
import { OUTPUTS, runHarness } from "./harness-factory.mjs";
import { HOSTED_MODEL_INFO, HOSTED_MODELS } from "./open-models.mjs";
import { codeFor } from "./printer.mjs";
import { hit, panel } from "./printer-panel.mjs";

const SERVER = "ippeveprinter";
const queueName = (name) => `aesel_${name.replace(/[^a-z0-9]+/gi, "_")}`;
const emit = (file, event) => appendFileSync(file, JSON.stringify(event) + "\n");

const freePort = () => new Promise((resolve, reject) => {
  const probe = net.createServer().once("error", reject).listen(0, () => { const { port } = probe.address(); probe.close(() => resolve(port)); });
});
const listening = (port) => new Promise((resolve) => {
  const socket = net.connect(port, "localhost").once("connect", () => { socket.destroy(); resolve(true); }).once("error", () => resolve(false));
});

// A day's free allowance fills the cartridge; bought braincells keep it full.
export const FULL_INK = 200000;
export const inkLevel = (braincells) => Math.max(0, Math.min(100, Math.round((braincells / FULL_INK) * 100)));

/** A picture as half-block rows for a terminal, or "" when it cannot be read. */
export function preview(file, side = 40) {
  if (process.platform !== "darwin") return "";
  const scratch = mkdtempSync(path.join(tmpdir(), "printer-"));
  try {
    const bmp = path.join(scratch, "p.bmp");
    if (spawnSync("sips", ["-s", "format", "bmp", "-Z", String(side), file, "--out", bmp]).status !== 0 || !existsSync(bmp)) return "";
    const data = readFileSync(bmp);
    const offset = data.readUInt32LE(10), width = data.readInt32LE(18), tall = data.readInt32LE(22), bytes = data.readUInt16LE(28) / 8;
    const height = Math.abs(tall), stride = Math.ceil((width * bytes) / 4) * 4;
    if (bytes < 3) return "";
    // Transparent pixels (an SVG has no paper of its own) are laid on white.
    const pixel = (x, y) => {
      if (y >= height) return [255, 255, 255];
      const at = offset + (tall > 0 ? height - 1 - y : y) * stride + x * bytes;
      const alpha = bytes === 4 ? data[at + 3] / 255 : 1;
      return [2, 1, 0].map((c) => Math.round(data[at + c] * alpha + 255 * (1 - alpha)));
    };
    const rows = [];
    for (let y = 0; y < height; y += 2) {
      let row = "  ";
      for (let x = 0; x < width; x++) {
        const [r, g, b] = pixel(x, y), [r2, g2, b2] = pixel(x, y + 1);
        row += `\x1b[38;2;${r};${g};${b}m\x1b[48;2;${r2};${g2};${b2}m▀`;
      }
      rows.push(row + "\x1b[0m");
    }
    return rows.join("\n");
  } catch { return ""; } finally { rmSync(scratch, { recursive: true, force: true }); }
}

// What the Print dialog was set to, as the harness should hear it. The dialog's
// own controls are the printer's knobs:
//   copies         that many variations (four at most)
//   black & white  a print with no colour in it
//   quality        draft is a quick rough one, best a careful one on the
//                  strongest model that can see
//   paper          the output takes the shape of the page that was sent
const QUALITY = { draft: "draft", 3: "draft", high: "best", 5: "best" };
const QUALITY_MODEL = { draft: HOSTED_MODELS.sonnet, best: HOSTED_MODELS.opus };
const COST = Object.fromEntries(Object.values(HOSTED_MODEL_INFO).map((info) => [info.id, info.cost]));

// A page is a picture, so only a model that sees can print. Of the hosted
// ones that is Claude; a text-only model answers a picture with a 404.
const sees = (model) => /^anthropic\//.test(model);

/** The model a quality means. A curated menu (a class's, say) is kept to. */
export function qualityModel(quality, menu = []) {
  if (quality === "normal") return "";
  const able = (menu.length ? menu : Object.values(QUALITY_MODEL)).filter(sees).sort((a, b) => (COST[a] ?? 3) - (COST[b] ?? 3));
  return (quality === "draft" ? able[0] : able.at(-1)) || "";
}
export const MAX_COPIES = 4;

function pageShape(page) {
  const sizes = spawnSync("sips", ["-g", "pixelWidth", "-g", "pixelHeight", page], { encoding: "utf8" }).stdout || "";
  const width = Number(/pixelWidth: (\d+)/.exec(sizes)?.[1]), height = Number(/pixelHeight: (\d+)/.exec(sizes)?.[1]);
  if (!width || !height) return null;
  const ratio = width / height;
  return { width, height, name: ratio > 1.1 ? "landscape" : ratio < 0.9 ? "portrait" : "square" };
}

// What a printed page says about itself, read on this machine and never sent
// anywhere but into the job: its text, title, the app it came from, its links.
// PDFKit does the reading, through JavaScript for Automation (macOS only).
const FACTS = `ObjC.import('PDFKit');
function run(argv) {
  const doc = $.PDFDocument.alloc.initWithURL($.NSURL.fileURLWithPath(argv[0]));
  if (doc.isNil()) return "{}";
  const attrs = doc.documentAttributes;
  const get = (key) => { const value = attrs.objectForKey(key); return value && !value.isNil() ? String(ObjC.unwrap(value)) : ""; };
  const links = [];
  for (let i = 0; i < Math.min(doc.pageCount, 8); i++) {
    const notes = doc.pageAtIndex(i).annotations;
    for (let j = 0; j < notes.count; j++) { const url = notes.objectAtIndex(j).URL; if (url && !url.isNil()) links.push(ObjC.unwrap(url.absoluteString)); }
  }
  return JSON.stringify({ pages: Number(doc.pageCount), title: get("Title"), app: get("Creator"), text: ObjC.unwrap(doc.string) || "", links: [...new Set(links)].slice(0, 40) });
}`;
const MAX_PAGE_TEXT = 24000;

/** The page's own text and facts, or {} for a picture or off macOS. */
export function pageFacts(page) {
  if (process.platform !== "darwin" || !/\.pdf$/i.test(page)) return {};
  try { return JSON.parse(spawnSync("osascript", ["-l", "JavaScript", "-e", FACTS, page], { encoding: "utf8", timeout: 20000 }).stdout || "{}"); } catch { return {}; }
}

/** The job sheet: what the model is told about the page beside its picture. */
export function jobSheet({ name = "", from = "" } = {}, facts = {}) {
  const text = String(facts.text || "").trim();
  return [
    `Print job${name ? ` “${name}”` : ""}${from ? `, sent by ${from}` : ""}${facts.pages ? `, ${facts.pages} page${facts.pages === 1 ? "" : "s"}` : ""}${facts.app ? `, printed from ${facts.app}` : ""}.`,
    facts.title && facts.title !== name ? `Its title: ${facts.title}` : "",
    facts.links?.length ? `Links on it:\n${facts.links.map((link) => `- ${link}`).join("\n")}` : "",
    text ? `The picture is its first page. Its text, read from the file:\n\n${text.slice(0, MAX_PAGE_TEXT)}${text.length > MAX_PAGE_TEXT ? "\n[…cut]" : ""}` : "The picture is the page; no text could be read from the file.",
  ].filter(Boolean).join("\n\n");
}

/** The settings of one job, read from the IPP_ variables and the page itself. */
export function jobSettings(asked = {}, page = "", menu = []) {
  const copies = Math.max(1, Math.min(MAX_COPIES, Number(asked.copies) || 1));
  const quality = QUALITY[asked.print_quality] || "normal";
  return { copies, mono: asked.print_color_mode === "monochrome", quality, model: qualityModel(quality, menu), shape: page ? pageShape(page) : null };
}

/** The settings as lines the model reads after the harness's own prompt. */
export function settingsBrief({ mono, shape, quality }, copy = 1, copies = 1) {
  const lines = [];
  if (quality === "draft") lines.push("It was sent at draft quality: a quick, rough one. Few parts, no fine detail, as short an answer as will do.");
  if (quality === "best") lines.push("It was sent at best quality: take care over it, and give it the detail and finish a keeper deserves.");
  if (mono) lines.push("It was sent as black and white: use black, white and greys only, no colour at all.");
  if (shape) lines.push(`The page is ${shape.name} (${shape.width} by ${shape.height}): give what you make that same shape.`);
  if (copies > 1) lines.push(`This is copy ${copy} of ${copies}. Each copy is a different take on the same page, so do not make the obvious one unless you are copy 1.`);
  return lines.length ? `\n\n## This print job\n\n${lines.map((line) => `- ${line}`).join("\n")}` : "";
}

/** One print job, run by ippeveprinter with the spooled page as its argument. */
export async function printJob(harness, page, { hosted, events = process.env.AESEL_PRINT_JOB } = {}) {
  const say = (event) => emit(events, { job: process.env.IPP_JOB_ID || String(process.pid), ...event });
  // What the Print dialog was set to (copies, colour, paper…) arrives as IPP_ variables.
  const asked = Object.fromEntries(Object.entries(process.env).filter(([key]) => key.startsWith("IPP_")).map(([key, value]) => [key.slice(4).toLowerCase(), value]));
  let menu = [];
  try { menu = JSON.parse(readFileSync(path.join(path.dirname(events), "menu.json"), "utf8")); } catch { /* no curated menu */ }
  const settings = jobSettings(asked, page, menu);
  const job = { name: asked.job_name || path.basename(page), from: asked.job_originating_user_name || "" };
  const facts = pageFacts(page);
  say({ type: "job", ...job, settings, facts: { pages: facts.pages, app: facts.app, title: facts.title, characters: (facts.text || "").length, links: facts.links?.length || 0 } });
  const sheet = jobSheet(job, facts);
  writeFileSync(`${page}.sheet.txt`, sheet + "\n");
  const services = hosted((text) => say({ type: "step", text: text.replace(/^[\s…]+/, "") }));
  try {
    if (!services.session.signedIn) throw new Error("sign in first: ac login");
    // A page is a picture and a job sheet, whatever the harness was written to take.
    const inputs = [...new Set([...harness.inputs, "image", "text"])];
    const started = Date.now();
    const copies = await Promise.allSettled(Array.from({ length: settings.copies }, async (_, index) => {
      const taking = { ...harness, inputs, model: settings.model || harness.model, prompt: harness.prompt + settingsBrief(settings, index + 1, settings.copies) };
      const label = settings.copies > 1 ? `copy ${index + 1}: ` : "";
      // Each copy keeps its own runs/ folder, which is named to the second.
      const run = await runHarness(taking, { texts: [sheet], images: [page], files: [] },
        { ...services, now: new Date(started + index * 1000), onstep: (text) => say({ type: "step", text: label + text }) });
      if (run.problems.length) throw new Error(label + run.problems[0]);
      say({ type: "done", file: run.file, rounds: run.rounds, copy: index + 1, copies: settings.copies });
    }));
    const failed = copies.find((copy) => copy.status === "rejected");
    if (failed) throw failed.reason;
  } catch (error) {
    say({ type: "error", message: error?.message || String(error) });
    process.exitCode = 1;
  }
}

// A print's name, the way AC names a saved painting: who, what, when.
//   @jeffrey-diagram-2026.11.22.10.32.10.123.svg
export function stamp(date = new Date()) {
  const two = (n, digits = 2) => String(n).padStart(digits, "0");
  return [date.getFullYear(), two(date.getMonth() + 1), two(date.getDate()), two(date.getHours()), two(date.getMinutes()), two(date.getSeconds()), two(date.getMilliseconds(), 3)].join(".");
}
export const printName = (sender, printer, ext, date = new Date()) => `${sender}-${printer}-${stamp(date)}.${ext}`;

/** Switch the printer on in this window and stay until it is switched off. */
export async function serve(harness, launcher, { out = process.stdout, input = process.stdin } = {}) {
  if (spawnSync("which", [SERVER]).status !== 0) throw new Error(`${SERVER} is not on this machine; a printer in the Print dialog needs CUPS (macOS has it)`);
  const session = new ACSession();
  // Whose printer it is shows wherever it is chosen: a room has many.
  const handle = session.handle, owner = handle ? `@${handle}` : "";
  const code = codeFor(harness.dir);
  const title = `${owner ? `${owner}’s ` : ""}${harness.name} printer`, queue = queueName(harness.name);
  const spool = path.join(harness.dir, "spool"), events = path.join(spool, "events.jsonl");
  const tray = path.join(homedir(), "Documents", "Aesel Prints", harness.name);
  const goal = OUTPUTS[harness.output];
  mkdirSync(spool, { recursive: true });
  writeFileSync(events, "");
  const port = await freePort();
  // The printer's picture in the dialog is the one it was made from.
  const seed = ["seed.svg", "seed.png"].map((name) => path.join(harness.dir, name)).find((file) => existsSync(file)) || "";
  const icon = path.join(spool, "icon.png");
  const pictured = seed && spawnSync("sips", ["-s", "format", "png", "-z", "512", "512", seed, "--out", icon]).status === 0;
  // _universal and image/urf are what a phone's AirPrint looks for; "-s 10,5"
  // makes it a colour printer, so the dialog offers black and white.
  const server = spawn(SERVER, ["-c", launcher, "-f", "application/pdf,image/jpeg,image/png,image/urf", "-r", "_print,_universal", "-s", "10,5",
    ...(owner ? ["-l", `${owner}${code ? ` · ${code}` : ""}`] : []), ...(pictured ? ["-i", icon] : []), "-d", spool, "-k", "-p", String(port), "-M", "Aesel", "-m", title, title],
    { stdio: "ignore", env: { ...process.env, AESEL_PRINT_JOB: events } });

  const tty = Boolean(out.isTTY && input.isTTY);
  const state = { name: harness.name, owner, code, goal: harness.output, ink: null, level: 0, prints: [], job: null, error: "", tick: 0, on: "switching on…" };
  let zones = [], off = false;
  const draw = () => {
    if (!tty || off) return;
    const frame = panel(state, { columns: out.columns || 80, rows: out.rows || 24 });
    zones = frame.zones;
    out.write("\x1b[H" + frame.rows.map((row) => row + "\x1b[K").join("\n") + "\x1b[J");
  };
  const log = (text) => { if (!tty) out.write(text + "\n"); };
  const stop = () => {
    if (off) return; off = true;
    server.kill();
    spawnSync("lpadmin", ["-x", queue]);
    if (tty) { input.setRawMode(false); out.write("\x1b[?1000l\x1b[?1006l\x1b[?25h\x1b[?1049l"); }
    out.write(`${title} is off.\n`);
    process.exit(0);
  };
  process.on("SIGINT", stop); process.on("SIGTERM", stop);
  server.once("exit", (status) => { if (!off) { log(`the print server stopped (${status})`); stop(); } });
  if (tty) { out.write(`\x1b]0;${title}${code ? ` · ${code}` : ""}\x07\x1b[?1049h\x1b[?25l\x1b[?1000h\x1b[?1006h`); draw(); }

  for (let tries = 0; tries < 40 && !(await listening(port)); tries++) await new Promise((resolve) => setTimeout(resolve, 250));
  const added = spawnSync("lpadmin", ["-p", queue, "-E", "-v", `ipp://localhost:${port}/ipp/print`, "-m", "everywhere", "-D", title], { encoding: "utf8" });
  state.on = added.status === 0
    ? `on. Print from any app and choose “${title}”. Prints land in ${tray.replace(homedir(), "~")}/`
    : `on the network as “${title}”; add it in Printers & Scanners (${(added.stderr || "").trim() || "lpadmin failed"})`;
  log(state.on); draw();

  // Ink is braincells: the four inks show what is left of the day's allowance,
  // the waste tank what was spent. The print server takes levels on /supplies.
  const ink = async () => {
    try {
      const response = await fetch(`${SITE}/api/easel-credits`, { headers: { Authorization: `Bearer ${await session.token()}`, "User-Agent": USER_AGENT }, signal: AbortSignal.timeout(40000) });
      if (!response.ok) return;
      const body = await response.json();
      writeFileSync(path.join(spool, "menu.json"), JSON.stringify(body.curation?.models || []));
      state.ink = (Number(body.remaining) || 0) + (Number(body.purchased) || 0);
      state.level = inkLevel(state.ink);
      draw(); log(`ink: ${state.ink.toLocaleString("en-US")} braincells (${state.level}%)`);
      await fetch(`http://localhost:${port}/supplies?supply0=${100 - state.level}${[1, 2, 3, 4].map((n) => `&supply${n}=${state.level}`).join("")}`, { signal: AbortSignal.timeout(5000) });
    } catch { /* the meter keeps its last reading */ }
  };
  ink(); // in the background: a slow network must not hold the printer up

  const me = userInfo().username;
  const clock = () => new Date().toLocaleTimeString([], { hour: "2-digit", minute: "2-digit", second: "2-digit", hour12: false });
  let started = 0, printed = 0, sender = owner || "someone";
  const show = (event) => {
    if (event.type === "job") {
      started = Date.now(); printed = 0; state.error = "";
      // The owner printing to their own printer is known by handle; anyone
      // else by the name their machine gave, which is all a print job carries.
      sender = event.from === me && owner ? owner : (event.from || "someone").replace(/[\/:\0\s]+/g, "-");
      const set = event.settings || {}, facts = event.facts || {};
      state.job = { name: event.name || "print", from: sender, seconds: 0, step: "",
        read: [facts.app && `from ${facts.app}`, facts.pages && `${facts.pages} page${facts.pages === 1 ? "" : "s"}`, facts.characters && `${facts.characters.toLocaleString("en-US")} characters read`, facts.links && `${facts.links} links`].filter(Boolean).join(" · "),
        knobs: [set.copies > 1 && `${set.copies} copies`, set.mono && "black & white", set.quality && set.quality !== "normal" && `${set.quality} quality`, set.shape?.name].filter(Boolean).join(" · ") };
      log(`▸ ${clock()} ${state.job.name} from ${sender}`);
    }
    if (event.type === "step" && state.job) { state.job.step = event.text; log(`  … ${event.text}`); }
    if (event.type === "error") { state.error = event.message; state.job = null; log(`  ✗ ${event.message}`); ink(); }
    if (event.type === "done") {
      // The owner's copy lands where prints are looked for: Documents, a
      // folder for each sender, named who-what-when.
      let kept = event.file;
      try {
        const folder = path.join(tray, sender);
        mkdirSync(folder, { recursive: true });
        kept = path.join(folder, printName(sender, harness.name, goal.ext, new Date(Date.now() + (event.copy || 1) - 1)));
        copyFileSync(event.file, kept);
      } catch { kept = event.file; }
      // A sound is heard as it comes out: there is nothing to look at.
      if (goal.as === "score" && process.platform === "darwin") spawn("afplay", [kept], { stdio: "ignore" });
      state.prints.push({ n: state.prints.length + 1, at: clock(), name: state.job?.name || "print", from: sender, file: path.basename(kept), path: kept });
      log(`  ✓ ${kept.replace(homedir(), "~")} (${Math.round((Date.now() - started) / 1000)} s)`);
      if (++printed >= (event.copies || 1)) { state.job = null; ink(); }
    }
    draw();
  };
  let read = 0;
  setInterval(() => {
    const text = readFileSync(events, "utf8");
    const whole = text.slice(read, text.lastIndexOf("\n") + 1);
    read += whole.length;
    for (const line of whole.split("\n").filter(Boolean)) { try { show(JSON.parse(line)); } catch { /* a torn line is skipped */ } }
    if (state.job) { state.job.seconds = Math.round((Date.now() - started) / 1000); state.tick++; draw(); }
  }, 400);

  if (tty) {
    const act = (action) => {
      if (!action) return;
      if (action === "off") return stop();
      if (action === "folder") { mkdirSync(tray, { recursive: true }); return spawn("open", [tray], { stdio: "ignore" }); }
      if (action === "prompt") return spawn("open", ["-t", path.join(harness.dir, "prompt.md")], { stdio: "ignore" });
      const print = state.prints.find((entry) => entry.n === action.open);
      if (print) spawn("open", [print.path], { stdio: "ignore" });
    };
    input.setRawMode(true); input.resume();
    input.on("data", (data) => {
      const text = String(data);
      if (text === "\x03" || text === "q") return stop();
      const click = /\x1b\[<0;(\d+);(\d+)M/.exec(text);
      if (click) return act(hit(zones, Number(click[1]) - 1, Number(click[2]) - 1));
      if (/^\d$/.test(text)) act({ open: Number(text) });
    });
    out.on("resize", draw);
  }
  return new Promise(() => {});
}
