// harness-factory — make your own small harness: inputs in, one MIME out.
//
// Aesel is a harness around one idea: you are always making a kind of media,
// and the interface says which. A factory harness is that idea cut down to
// something a person can write in an afternoon. It is a folder:
//
//   harness.json   { "name", "inputs": ["text","image","file"], "output": "image/svg+xml", "model"? }
//   prompt.md      what the harness knows and how it behaves — the part you write
//   check.mjs      optional: export default (output) => [problems], for a repair round
//   runs/          every run: what went in, what the model said, what came out
//
// The output MIME type is the goal. It picks the file extension, the glyph the
// terminal shows, the format contract appended after prompt.md, the checks a
// result must pass, and how the model's answer becomes bytes: most types are
// the answer itself, image/png is an art-directed prompt sent to an image model,
// audio/wav is a score this machine synthesizes. Inference is AC hosted, paid
// in braincells by the @handle running it; nothing else needs installing.
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync, cpSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { decode, encode } from "../media/picture/png.mjs";
import { sourceChecks } from "./edit-contract.mjs";
import { renderScore, scoreProblems } from "./wav-score.mjs";

export const STARTERS = fileURLToPath(new URL("../workshop/starters/", import.meta.url));
export const DEFAULT_MODEL = "anthropic/claude-sonnet-5.5";
export const MAX_TOKENS = 8000;
const MAX_TEXT_FILE = 64 * 1024;
const MAX_IMAGE_SIDE = 768;

// The forms a harness can take in. The hosted engine reads words and bounded
// PNG images; anything else becomes one of those before it leaves.
export const INPUTS = {
  text: { glyph: "✎", about: "words typed on the command line" },
  image: { glyph: "🖼", about: "a photo, drawing or screenshot (png, jpg, heic…), sent as a PNG up to 768 px" },
  file: { glyph: "📄", about: "a text file (txt, md, csv, json, code), up to 64 KB, read in as words" },
};

const fence = (lang) => new RegExp("```" + lang + "[^\\n]*\\n([\\s\\S]*?)```");

// The goals. `as` is how the answer becomes the file: "text" keeps the fenced
// block, "image" sends it to an image model, "score" synthesizes it.
export const OUTPUTS = {
  "text/plain": { ext: "txt", glyph: "✎", kind: "words", as: "text", lang: "text",
    contract: "Answer with the finished text only, inside one ```text block." },
  "text/markdown": { ext: "md", glyph: "📝", kind: "document", as: "text", lang: "markdown",
    contract: "Answer with one Markdown document inside one ```markdown block." },
  "text/html": { ext: "html", glyph: "🌐", kind: "page", as: "text", lang: "html",
    contract: "Answer with one complete, self-contained HTML page (inline CSS and JS, no external files) inside one ```html block. It must work when opened straight from disk." },
  "image/svg+xml": { ext: "svg", glyph: "🖼", kind: "picture", as: "text", lang: "svg",
    contract: "Answer with one standalone SVG document (with xmlns and a viewBox) inside one ```svg block. No external images or fonts." },
  "application/json": { ext: "json", glyph: "🧾", kind: "data", as: "text", lang: "json",
    contract: "Answer with valid JSON only, inside one ```json block." },
  "text/csv": { ext: "csv", glyph: "📊", kind: "table", as: "text", lang: "csv",
    contract: "Answer with a CSV table with a header row inside one ```csv block." },
  "text/x-lisp": { ext: "lisp", glyph: "🎨", kind: "KidLisp piece", as: "text", lang: "lisp",
    contract: "Answer with one KidLisp program inside one ```lisp block. KidLisp is Aesthetic Computer's Lisp for generative art: each line is an expression like (wipe \"navy\"), (ink \"yellow\"), (circle 128 128 64), (box x y w h), (line x1 y1 x2 y2); width and height are the screen size; the program runs every frame." },
  "text/javascript": { ext: "mjs", glyph: "🎮", kind: "AC piece", as: "text", lang: "(?:js|javascript|mjs)",
    contract: "Answer with one Aesthetic Computer piece inside one ```js block: an ES module with no imports that exports `paint` (and optionally `boot`, `sim`, `act`). Each function receives AC's API object, e.g. function paint({ wipe, ink, screen }) { wipe(\"navy\"); ink(\"yellow\").circle(screen.width / 2, screen.height / 2, 40); }." },
  "image/png": { ext: "png", glyph: "🖼", kind: "picture", as: "image", lang: "prompt",
    contract: "You are the art director; an image model paints. Answer with the image prompt inside one ```prompt block: subject, composition, medium, palette, light and mood in concrete words, under 1,500 characters. Do not describe text you want legible unless it is short." },
  "audio/wav": { ext: "wav", glyph: "🔊", kind: "sound", as: "score", lang: "json",
    contract: "You are the composer; this machine plays your score. Answer with one JSON score inside one ```json block: {\"bpm\": 96, \"notes\": [{\"at\": 0, \"beats\": 1, \"pitch\": \"C4\", \"wave\": \"sine\", \"gain\": 0.6}]}. `at` and `beats` count beats; pitch is a note name like C4 or F#3, or \"noise\" for a drum hit; wave is sine, square, saw, triangle or noise. Overlapping notes make chords. Keep it under 60 seconds." },
};

const ALIASES = { svg: "image/svg+xml", png: "image/png", image: "image/png", wav: "audio/wav", audio: "audio/wav", sound: "audio/wav",
  html: "text/html", md: "text/markdown", markdown: "text/markdown", txt: "text/plain", text: "text/plain", json: "application/json",
  csv: "text/csv", lisp: "text/x-lisp", kidlisp: "text/x-lisp", js: "text/javascript", mjs: "text/javascript", piece: "text/javascript" };

export function outputType(name) {
  const wanted = String(name || "").trim().toLowerCase();
  const mime = OUTPUTS[wanted] ? wanted : ALIASES[wanted];
  if (!mime) throw new Error(`no output goal "${name}" — try one of: ${Object.keys(OUTPUTS).join(", ")}`);
  return mime;
}

// harness.json, checked, with what a missing field means.
export function validateManifest(value) {
  if (!value || typeof value !== "object" || Array.isArray(value)) throw new Error("harness.json must be an object");
  const name = String(value.name || "").trim();
  if (!/^[a-z0-9][a-z0-9-]{0,39}$/.test(name)) throw new Error("harness.json: name is lowercase letters, digits and dashes");
  const inputs = Array.isArray(value.inputs) && value.inputs.length ? value.inputs : ["text"];
  for (const input of inputs) if (!INPUTS[input]) throw new Error(`harness.json: unknown input "${input}" — use ${Object.keys(INPUTS).join(", ")}`);
  const output = outputType(value.output);
  const model = value.model === undefined ? DEFAULT_MODEL : String(value.model);
  const image = { provider: "openai", model: "gpt-image-2", quality: "low", size: "1024x1024", ...(value.image || {}) };
  return { name, description: String(value.description || ""), inputs: [...new Set(inputs)], output, model, image };
}

export function readHarness(dir) {
  const root = path.resolve(dir);
  const file = path.join(root, "harness.json");
  if (!existsSync(file)) throw new Error(`no harness.json in ${dir} — make one with: ac harness new ${dir}`);
  let manifest;
  try { manifest = JSON.parse(readFileSync(file, "utf8")); } catch (error) { throw new Error(`harness.json is not valid JSON: ${error.message}`); }
  const promptFile = path.join(root, "prompt.md");
  const prompt = existsSync(promptFile) ? readFileSync(promptFile, "utf8").trim() : "";
  if (!prompt) throw new Error(`${path.join(dir, "prompt.md")} is empty — that is where the harness is written`);
  return { dir: root, ...validateManifest(manifest), prompt, check: existsSync(path.join(root, "check.mjs")) ? path.join(root, "check.mjs") : null };
}

// Box-filter a decoded RGBA picture down so its longer side fits.
export function fit({ width, height, data }, side = MAX_IMAGE_SIDE) {
  const scale = Math.min(1, side / Math.max(width, height));
  if (scale === 1) return { width, height, data };
  const w = Math.max(1, Math.round(width * scale)), h = Math.max(1, Math.round(height * scale));
  const out = Buffer.alloc(w * h * 4);
  for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) {
    const x0 = Math.floor((x * width) / w), x1 = Math.max(x0 + 1, Math.floor(((x + 1) * width) / w));
    const y0 = Math.floor((y * height) / h), y1 = Math.max(y0 + 1, Math.floor(((y + 1) * height) / h));
    const sum = [0, 0, 0, 0];
    for (let yy = y0; yy < y1; yy++) for (let xx = x0; xx < x1; xx++) for (let c = 0; c < 4; c++) sum[c] += data[(yy * width + xx) * 4 + c];
    const n = (x1 - x0) * (y1 - y0);
    for (let c = 0; c < 4; c++) out[(y * w + x) * 4 + c] = Math.round(sum[c] / n);
  }
  return { width: w, height: h, data: out };
}

// Any picture file as a PNG the hosted engine accepts. Plain 8-bit PNGs are
// decoded here; anything else (jpg, heic, gif, webp, 16-bit or palette PNGs)
// goes through macOS's sips first.
export function imagePng(file, { platform = process.platform } = {}) {
  try { return bounded(decode(readFileSync(file))); } catch (error) {
    if (platform !== "darwin") throw new Error(`${path.basename(file)} must be an 8-bit PNG here; re-export it as PNG (${error.message})`);
  }
  const scratch = mkdtempSync(path.join(tmpdir(), "harness-"));
  try {
    const png = path.join(scratch, "in.png");
    const result = spawnSync("sips", ["-s", "format", "png", "-Z", String(MAX_IMAGE_SIDE), file, "--out", png], { encoding: "utf8" });
    if (result.status !== 0 || !existsSync(png)) throw new Error(`could not read ${path.basename(file)} as a picture`);
    return bounded(decode(readFileSync(png)));
  } finally { rmSync(scratch, { recursive: true, force: true }); }
}

// The hosted engine also caps the bytes (700,000 base64 characters), and a
// textured photo at 768 px can be over it; shrink until it fits.
const MAX_IMAGE_BYTES = 500000;
function bounded(picture) {
  for (let side = MAX_IMAGE_SIDE; ; side = Math.floor(side * 0.8)) {
    const png = encode(fit(picture, side));
    if (png.length <= MAX_IMAGE_BYTES || side < 64) return png;
  }
}

// The command line's inputs as message content, refusing forms the harness
// did not ask for so a run means what harness.json says.
export function inputContent(harness, { texts = [], images = [], files = [] }, { cwd = process.cwd(), readImage = imagePng } = {}) {
  const content = [];
  const allow = (form, given) => { if (given.length && !harness.inputs.includes(form)) throw new Error(`${harness.name} does not take ${form} input (harness.json inputs: ${harness.inputs.join(", ")})`); };
  allow("text", texts); allow("image", images); allow("file", files);
  for (const file of files) {
    const absolute = path.resolve(cwd, file);
    if (statSync(absolute).size > MAX_TEXT_FILE) throw new Error(`${file} is over 64 KB`);
    content.push({ type: "text", text: `File ${path.basename(file)}:\n\n${readFileSync(absolute, "utf8")}` });
  }
  for (const file of images) {
    content.push({ type: "text", text: `Image ${path.basename(file)}:` });
    content.push({ type: "image", source: { type: "base64", media_type: "image/png", data: readImage(path.resolve(cwd, file)).toString("base64") } });
  }
  if (texts.length) content.push({ type: "text", text: texts.join(" ") });
  if (!content.length) throw new Error(`give ${harness.name} something: ${harness.inputs.map((i) => (i === "text" ? '"words"' : `--${i} <path>`)).join(" ")}`);
  return content;
}

// The system prompt: the person's harness, then the format contract its goal needs.
export function systemPrompt(harness) {
  const goal = OUTPUTS[harness.output];
  return `${harness.prompt}\n\n---\n\nOutput goal: ${harness.output} (${goal.kind}).\n${goal.contract}`;
}

export function harnessRequest(harness, content, { previous = "", problems = [] } = {}) {
  const messages = [{ role: "user", content }];
  if (previous && problems.length) {
    messages.push({ role: "assistant", content: [{ type: "text", text: previous }] });
    messages.push({ role: "user", content: [{ type: "text", text: ["That did not pass the checks:", ...problems.map((p) => `- ${p}`), "Send the whole corrected answer again."].join("\n") }] });
  }
  return { model: harness.model, max_tokens: MAX_TOKENS, system: systemPrompt(harness), messages };
}

// The deliverable out of the model's answer: its fenced block, or the whole
// answer when the model forgot the fence.
export function extract(mime, answer) {
  const text = String(answer || "");
  const match = fence(OUTPUTS[mime].lang).exec(text) || /```[^\n]*\n([\s\S]*?)```/.exec(text);
  return (match ? match[1] : text).trim();
}

// The checks every result of a type must pass before it becomes a file.
export function builtInProblems(mime, body) {
  if (!body) return ["The answer was empty."];
  if (mime === "application/json") { try { JSON.parse(body); } catch (error) { return [`The JSON does not parse: ${error.message}`]; } }
  if (mime === "audio/wav") { try { return scoreProblems(JSON.parse(body)); } catch (error) { return [`The score is not valid JSON: ${error.message}`]; } }
  if (mime === "image/svg+xml" && !/<svg[\s>][\s\S]*<\/svg>\s*$/.test(body)) return ["Send one complete <svg>…</svg> document."];
  if (mime === "text/html" && !/<html[\s>]/i.test(body)) return ["Send one complete HTML page with an <html> element."];
  if (mime === "text/x-lisp") {
    let depth = 0;
    for (const ch of body.replace(/"(?:\\.|[^"\\])*"/g, "").replace(/;.*$/gm, "")) { depth += ch === "(" ? 1 : ch === ")" ? -1 : 0; if (depth < 0) break; }
    if (depth !== 0) return ["The parentheses do not balance."];
  }
  if (mime === "text/javascript") {
    const problems = sourceChecks(body).map((finding) => finding.message);
    if (/^\s*import\s/m.test(body)) problems.push("No import statements; everything comes from the lifecycle arguments.");
    if (!/export\s*(?:\{[^}]*\bpaint\b|function\s+paint\b)/.test(body)) problems.push("Export `paint`.");
    return problems;
  }
  if (mime === "image/png" && body.length > 16000) return ["Keep the image prompt under 16,000 characters."];
  return [];
}

// Run the harness's own check.mjs, if it has one. Its problems join the built-in ones.
export async function ownProblems(harness, output) {
  if (!harness.check) return [];
  const module = await import(`${pathToFileURL(harness.check).href}?t=${Date.now()}`);
  const found = await (module.default || module.check)?.(output);
  return Array.isArray(found) ? found.map(String).filter(Boolean) : [];
}

// Hosted inference streams Anthropic-style server-sent events; keep the text.
export async function streamText(response) {
  const reader = response.body.getReader(), decoder = new TextDecoder();
  let tail = "", text = "", usage = {};
  for (;;) {
    const { value, done } = await reader.read();
    if (done) break;
    tail += decoder.decode(value, { stream: true });
    const lines = tail.split("\n");
    tail = lines.pop();
    for (const line of lines) {
      if (!line.startsWith("data:")) continue;
      let event;
      try { event = JSON.parse(line.slice(5)); } catch { continue; }
      if (event.type === "content_block_delta" && event.delta?.type === "text_delta") text += event.delta.text;
      if (event.type === "message_start") Object.assign(usage, event.message?.usage);
      if (event.type === "message_delta") Object.assign(usage, event.usage);
      if (event.type === "error") throw new Error(event.error?.message || "inference failed");
    }
  }
  return { text, usage };
}

const stamp = (date = new Date()) => date.toISOString().replace(/[:.]/g, "-").slice(0, 19);

// One run: ask, check, repair once, make the file, write it all down.
// `infer(request) → {text, usage}` and `paint(plan) → {bytes}` are injected so
// tests and dry runs spend nothing.
export async function runHarness(harness, inputs, { infer, paint, cwd = process.cwd(), onstep = () => {}, now = new Date() } = {}) {
  const goal = OUTPUTS[harness.output];
  const content = inputContent(harness, inputs, { cwd });
  const dir = path.join(harness.dir, "runs", stamp(now));
  mkdirSync(dir, { recursive: true });
  let answer = "", body = "", problems = [], rounds = 0;
  const usage = [];
  for (; rounds < 2; rounds++) {
    onstep(rounds ? `repairing: ${problems[0]}` : `asking ${harness.model}`);
    const reply = await infer(harnessRequest(harness, content, { previous: answer, problems }));
    answer = reply.text; usage.push(reply.usage || {});
    body = extract(harness.output, answer);
    problems = builtInProblems(harness.output, body);
    if (!problems.length) problems = await ownProblems(harness, { mime: harness.output, text: body, inputs });
    if (!problems.length) break;
  }
  writeFileSync(path.join(dir, "answer.md"), answer);
  let output = null;
  if (!problems.length) {
    let bytes = Buffer.from(body + (goal.as === "text" ? "\n" : ""));
    if (goal.as === "score") bytes = renderScore(JSON.parse(body));
    if (goal.as === "image") {
      onstep(`painting with ${harness.image.model}`);
      const reference = inputs.images?.length && harness.image.provider === "openai" ? imagePng(path.resolve(cwd, inputs.images[0])) : null;
      ({ bytes } = await paint({ ...harness.image, prompt: body, reference }));
      writeFileSync(path.join(dir, "prompt.txt"), body + "\n");
    }
    if (goal.as === "score") writeFileSync(path.join(dir, "score.json"), body + "\n");
    output = path.join(dir, `output.${goal.ext}`);
    writeFileSync(output, bytes);
  }
  const record = { harness: harness.name, output: harness.output, model: harness.model, at: now.toISOString(),
    inputs: { texts: inputs.texts || [], images: inputs.images || [], files: inputs.files || [] },
    rounds: rounds + (problems.length ? 0 : 1), problems, usage, file: output && path.relative(harness.dir, output) };
  writeFileSync(path.join(dir, "run.json"), JSON.stringify(record, null, 2) + "\n");
  return { ...record, dir, file: output };
}

// The newest run that made a file, or the one named.
export function latestRun(harness, name) {
  const runs = path.join(harness.dir, "runs");
  const names = existsSync(runs) ? readdirSync(runs).sort().reverse() : [];
  for (const run of name ? [name] : names) {
    const file = path.join(runs, run, "run.json");
    if (!existsSync(file)) continue;
    const record = JSON.parse(readFileSync(file, "utf8"));
    if (record.file) return { ...record, dir: path.join(runs, run), file: path.join(harness.dir, record.file) };
  }
  throw new Error(name ? `run ${name} made no file` : `${harness.name} has no finished runs yet: ac harness run ${path.relative(process.cwd(), harness.dir) || "."} …`);
}

export function starters() {
  if (!existsSync(STARTERS)) return [];
  return readdirSync(STARTERS).filter((name) => existsSync(path.join(STARTERS, name, "harness.json"))).sort()
    .map((name) => ({ id: name, ...validateManifest(JSON.parse(readFileSync(path.join(STARTERS, name, "harness.json"), "utf8"))) }));
}

const PROMPT = (inputs, output) => `# What this harness is

You are a small, specific tool. Describe here who you are, what you make, and
how you decide. Be concrete: a harness is mostly taste, written down.

# What comes in

${inputs.map((input) => `- ${input}: ${INPUTS[input].about}`).join("\n")}

# What goes out

One ${OUTPUTS[output].kind} (${output}). Say what makes a good one, and what to avoid.
`;

// A new harness folder, from a starter or from the inputs and goal named.
export function newHarness(dir, { from, inputs, output, cwd = process.cwd() } = {}) {
  const root = path.resolve(cwd, dir);
  if (existsSync(path.join(root, "harness.json"))) throw new Error(`${dir} already has a harness.json`);
  const name = path.basename(root).toLowerCase().replace(/[^a-z0-9-]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 40) || "harness";
  if (from) {
    const source = path.join(STARTERS, from);
    if (!existsSync(path.join(source, "harness.json"))) throw new Error(`no starter "${from}" — see: ac harness list`);
    cpSync(source, root, { recursive: true, filter: (file) => !file.includes(`${path.sep}runs`) });
    const manifest = JSON.parse(readFileSync(path.join(root, "harness.json"), "utf8"));
    writeFileSync(path.join(root, "harness.json"), JSON.stringify({ ...manifest, name, from }, null, 2) + "\n");
  } else {
    const manifest = validateManifest({ name, inputs: inputs?.length ? inputs : ["text"], output: output || "text/plain" });
    mkdirSync(root, { recursive: true });
    writeFileSync(path.join(root, "harness.json"), JSON.stringify({ name, inputs: manifest.inputs, output: manifest.output }, null, 2) + "\n");
    writeFileSync(path.join(root, "prompt.md"), PROMPT(manifest.inputs, manifest.output));
  }
  return readHarness(root);
}
