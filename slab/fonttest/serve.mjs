// slab-font test | sheet | results — serve the font pages on localhost.
//
// test:    opens the tiny text test, waits for the run, prints and saves it.
// sheet:   opens the contact sheet, then exits once the page has its fonts.
// results: prints the last saved run.
//
// Fonts are read from the Slab seed and ~/Library/Fonts at request time, so
// the same pages work on any Mac with whichever candidates it has. Extra font
// files named on the command line join the comparison.
import { createServer } from "node:http";
import { readFile, writeFile, mkdir, readdir, access } from "node:fs/promises";
import { execFile } from "node:child_process";
import { basename, dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const here = dirname(fileURLToPath(import.meta.url));
const repo = process.env.SLAB_REPO || resolve(here, "..");
const slabHome = process.env.SLAB_HOME || join(homedir(), ".local/share/slab");
const resultsDir = join(slabHome, "font-test/results");
const userFonts = join(homedir(), "Library/Fonts");
const wanted = process.env.SLAB_FONT_WANTED || "";
const [mode = "test", ...extra] = process.argv.slice(2);

const CANDIDATES = [
  { id: "tight", label: "Atkinson Mono Tight", ps: "AtkinsonHyperlegibleMonoTight-Regular", file: join(repo, "seed/fonts/AtkinsonHyperlegibleMonoTight-Regular.ttf") },
  { id: "atkinson", label: "Atkinson Mono", ps: "AtkinsonHyperlegibleMono-Regular", file: join(userFonts, "AtkinsonHyperlegibleMono[wght].ttf"), weight: "200 800" },
  { id: "monaspace", label: "Monaspace Argon", ps: "MonaspaceArgon-Regular", file: join(repo, "seed/fonts/MonaspaceArgon-Regular.otf") },
  { id: "jetbrains", label: "JetBrains Mono", ps: "JetBrainsMono-Regular", file: join(userFonts, "JetBrainsMono-Regular.ttf") },
  { id: "intelone", label: "Intel One Mono", ps: "IntelOneMono-Regular", file: join(userFonts, "IntelOneMono-Regular.otf") },
  { id: "menlo", label: "Menlo", ps: "Menlo-Regular", local: "Menlo-Regular" }, // a system font: the browser has it
  ...extra.map((path, i) => ({ id: `extra${i}`, label: basename(path, extname(path)), ps: "", file: resolve(path) })),
];

async function fonts() {
  const found = [];
  for (const c of CANDIDATES) {
    if (c.file) { try { await access(c.file); } catch { continue; } }
    found.push({ id: c.id, label: c.label + (c.ps && c.ps === wanted ? " (now)" : ""), weight: c.weight || "400",
                 src: c.local ? `local("${c.local}")` : `url(/font/${c.id})` });
  }
  return found;
}

function table(run) {
  const pad = (s, n) => String(s).padEnd(n);
  const lines = [`${pad("font", 26)} ${pad("smallest", 9)} ${pad("cell", 9)} ${pad("cell there", 11)} ${pad("columns", 8)} missed`];
  const best = Math.min(...run.rows.map((r) => r.cell));
  for (const r of run.rows) {
    lines.push(`${pad(r.label, 26)} ${pad(r.threshold.toFixed(1) + " pt", 9)} ${pad(r.advance.toFixed(3) + " em", 9)} ` +
      `${pad(r.cell.toFixed(2) + " px", 11)} ${pad(Math.round((100 * best) / r.cell) + "%", 8)} ${r.confusions}`);
  }
  return `tiny text test, ${run.at}\n${lines.join("\n")}\n` +
    `"smallest" is the size read right about half the time; "columns" compares text per line at that limit.`;
}

async function lastRun() {
  const names = (await readdir(resultsDir).catch(() => [])).filter((n) => n.endsWith(".json")).sort();
  return names.length ? JSON.parse(await readFile(join(resultsDir, names.at(-1)), "utf8")) : null;
}

if (mode === "results") {
  const run = await lastRun();
  console.log(run ? table(run) : "No runs yet. Start one with: slab-font test");
  process.exit(0);
}

const types = { ".html": "text/html; charset=utf-8", ".ttf": "font/ttf", ".otf": "font/otf", ".ttc": "font/collection" };
let lastHit = 0, served = false;

const server = createServer(async (req, res) => {
  lastHit = Date.now();
  try {
    const path = new URL(req.url, "http://localhost").pathname;
    if (req.method === "POST" && path === "/result") {
      let body = "";
      for await (const chunk of req) { body += chunk; if (body.length > 2e6) throw new Error("too large"); }
      const run = JSON.parse(body);
      await mkdir(resultsDir, { recursive: true });
      const name = `run-${new Date().toISOString().replace(/[:.]/g, "-")}.json`;
      await writeFile(join(resultsDir, name), JSON.stringify(run, null, 1));
      res.end(join(resultsDir, name));
      console.log(table(run));
      console.log(`saved ${join(resultsDir, name)}`);
      return void setTimeout(() => process.exit(0), 300);
    }
    if (path === "/fonts.json") {
      res.writeHead(200, { "content-type": "application/json", "cache-control": "no-store" });
      return void res.end(JSON.stringify(await fonts()));
    }
    if (path.startsWith("/font/")) {
      const c = CANDIDATES.find((x) => x.id === path.slice(6) && x.file);
      if (!c) throw new Error("no such font");
      res.writeHead(200, { "content-type": types[extname(c.file)] || "font/ttf", "cache-control": "no-store" });
      return void res.end(await readFile(c.file));
    }
    const page = path === "/" ? (mode === "sheet" ? "sheet.html" : "index.html") : null;
    if (!page) throw new Error("not found");
    served = true;
    res.writeHead(200, { "content-type": types[".html"], "cache-control": "no-store" });
    res.end(await readFile(join(here, page)));
  } catch (e) {
    res.writeHead(404); res.end(String(e.message));
  }
});

server.listen(0, "127.0.0.1", () => {
  const url = `http://127.0.0.1:${server.address().port}/`;
  console.log(mode === "sheet" ? `contact sheet: ${url}` : `tiny text test: ${url}\nwaiting for the run to finish (Ctrl-C to stop)…`);
  if (!process.env.SLAB_FONT_NO_OPEN) execFile("/usr/bin/open", [url]);
});

// The sheet needs the server only until its fonts load. The test waits for its
// result, but not forever.
const started = Date.now();
setInterval(() => {
  if (mode === "sheet" && served && Date.now() - lastHit > 8000) process.exit(0);
  if (Date.now() - started > 45 * 60 * 1000) { console.log("gave up waiting after 45 minutes"); process.exit(1); }
}, 1000);
