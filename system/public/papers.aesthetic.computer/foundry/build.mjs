// Foundry page generator. Each typeface is a boxed content card with a
// specimen (inlined SVG for bitmap/vector faces, @font-face for the rest),
// license, and platform usage. Specimens are contextual (AC piece names,
// the kidlisp logotype, paper titles). Run: node build.mjs
// (regenerate the bitmap/vector SVGs first: node gen-specimens.mjs)
import fs from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
const HERE = dirname(fileURLToPath(import.meta.url));
const svg = (n) => { try { return fs.readFileSync(join(HERE, "specimens", n), "utf8"); } catch { return ""; } };
const rows = (...names) => names.map(svg).filter(Boolean).map((s) => `<div class="r">${s}</div>`).join("");
const web = (stack, sample, cls = "") => `<div class="web ${cls}" style="font-family:${stack}">${sample}</div>`;

// the kidlisp.com logotype — Comic Relief, per-letter baby colors
const KIDLISP = (() => {
  const word = [["K","#FF6B6B"],["i","#4ECDC4"],["d","#FFE66D"],["L","#95E1D3"],["i","#F38181"],["s","#AA96DA"],["p","#70D6FF"]];
  const com = [["c","#FF6B6B"],["o","#9370DB"],["m","#90EE90"]];
  const sp = ([c, col]) => `<span style="color:${col};text-shadow:1px 1px 0 rgba(0,0,0,.3);letter-spacing:.05em">${c}</span>`;
  return `<div class="big kidlogo">${word.map(sp).join("")}<span style="opacity:.5">.</span>${com.map(sp).join("")}</div>`;
})();

const GROUPS = [
  { title: "AC Native — cut in-house", color: "pink", faces: [
    { name: "font_1", tag: "bitmap · vector strokes · 6×10", spec: rows("font_1-1.svg","font_1-2.svg","font_1-3.svg"),
      lic: ["native","AC original","OFL candidate — release with a reserved font name."], used: ["chat","melody","tracker","speaker","default text"] },
    { name: "microtype", tag: "bitmap · 3×5 · legacy", lic: ["native","AC original","superseded by MatrixChunky8"], used: ["legacy QR labels"] },
  ]},
  { title: "Bitmap — bundled", color: "cyan", faces: [
    { name: "MatrixChunky8", tag: "BDF · proportional · 8px", spec: rows("mc8-1.svg","mc8-2.svg","mc8-3.svg"),
      lic: ["cc","CC-BY — Trip5 / Conventional Chaos","third-party, attribution required","https://github.com/trip5/Matrix-Fonts"], used: ["HUD","tooltips","gamepad","tapes","autocomplete","typecheck","offline"] },
    { name: "unifont", tag: "BDF · 16×8 · full Unicode", spec: rows("unifont-latin.svg","unifont-multi.svg"),
      lic: ["gpl","GNU GPL v2+ — GNU Unifont","multilingual fallback; copyleft","https://unifoundry.com/unifont/"], used: ["CJK / emoji / accented fallback"] },
  ]},
  { title: "Papers — LaTeX / arXiv", color: "purple", faces: [
    { name: "Latin Modern Roman", tag: "serif · OTF", spec: web("'Latin Modern Roman',serif", `<div class="big">Pieces Not Programs</div>`),
      lic: ["gust","GUST Font License (free, OFL-like)","the TeX default; ships with TeX Live"], used: ["all /papers arXiv PDFs (body)"] },
    { name: "Latin Modern Sans", tag: "sans · OTF", spec: web("'Latin Modern Sans',sans-serif", `<div class="big">Repository Archaeology</div>`),
      lic: ["gust","GUST Font License (free, OFL-like)"], used: ["paper headings & figures"] },
    { name: "Latin Modern Mono", tag: "monospace · OTF", spec: web("'Latin Modern Mono',monospace", `<div class="big">(wipe "blue")</div>`),
      lic: ["gust","GUST Font License (free, OFL-like)"], used: ["paper code listings"] },
  ]},
  { title: "Licensed", color: "gold", faces: [
    { name: "Berkeley Mono", tag: "monospace · variable", spec: web("'Berkeley Mono Variable',monospace", `<div class="big">prompt chat kidlisp</div>`),
      lic: ["commercial","Commercial — Berkeley Graphics, from $75","⚠ not open-source-compatible; commercial use 'UI only', IDE/terminal needs sign-off","https://usgraphics.com/products/berkeley-mono"], used: ["code & UI","papers","kidlisp.com","checkout"] },
    { name: "YWFT Processing", tag: "display · headings", spec: web("'YWFT Processing',sans-serif", `<div class="big">aesthetic.computer</div>`),
      lic: ["proprietary","Proprietary — YouWorkForThem (pageview-licensed)","drawn 2001 for Casey Reas (Processing); no license file in repo — verify","https://www.youworkforthem.com/font-license"], used: ["titles & branding","decks","vscode-extension"] },
  ]},
  { title: "Open & bundled — OFL / CC / public domain", color: "green", faces: [
    { name: "Comic Relief", tag: "display · TTF · the kidlisp.com logotype", spec: web("'Comic Relief',cursive", KIDLISP),
      lic: ["ofl","SIL OFL"], used: ["kidlisp.com logo","kidlisp paper","oven","sosoft"] },
    { name: "Monaspace Argon", tag: "monospace · OTF", spec: web("'Monaspace Argon',monospace", `<div class="big">freehand line box</div>`),
      lic: ["ofl","SIL OFL — GitHub"], used: ["slab/seed"] },
    { name: "Poppins", tag: "geometric · TTF", spec: web("'Poppins',sans-serif", `<div class="big">melody stample tape</div>`),
      lic: ["ofl","SIL OFL"], used: ["reports"] },
    { name: "Proggy Clean", tag: "bitmap mono · TTF", spec: web("'ProggyClean',monospace", `<div class="big">spray smear 1v1</div>`),
      lic: ["pd","Public domain"], used: ["apple HTML fallback"] },
    { name: "Bravura", tag: "music notation · SMuFL", spec: web("'Bravura',serif", `<div class="big">𝄞 ♩ ♪ ♫ ♬ ♭ ♮ ♯</div>`, "tall"),
      lic: ["ofl","SIL OFL — Steinberg"], used: ["slab/menuband"] },
  ]},
  { title: "Candidates — FOSS, bundleable (not yet adopted)", color: "green", faces: [
    { name: "Departure Mono", tag: "pixel mono", spec: web("'Departure Mono',monospace", `<div class="big">freehand tape</div>`),
      lic: ["mit","MIT — Helena Zhang","lo-fi pixel mono","https://departuremono.com"], role: "candidate · bundleable now" },
    { name: "Space Mono", tag: "display mono", spec: web("'Space Mono',monospace", `<div class="big">prompt notepat</div>`),
      lic: ["ofl","SIL OFL — Colophon"], role: "candidate · bundleable now" },
    { name: "Space Grotesk", tag: "geometric display", spec: web("'Space Grotesk',sans-serif", `<div class="big">aesthetic computer</div>`),
      lic: ["ofl","SIL OFL — Florian Karsten"], role: "candidate · bundleable now" },
    { name: "Inter", tag: "UI sans", spec: web("'Inter',sans-serif", `<div class="big">painting melody</div>`),
      lic: ["ofl","SIL OFL — Rasmus Andersson"], role: "candidate · bundleable now" },
    { name: "Silkscreen", tag: "pixel · bitmap", spec: web("'Silkscreen',sans-serif", `<div class="big">kidlisp chat</div>`),
      lic: ["ofl","SIL OFL — Jason Kottke"], role: "candidate · bundleable now" },
    { name: "Press Start 2P", tag: "arcade pixel", spec: web("'Press Start 2P',monospace", `<div class="big">balls</div>`),
      lic: ["ofl","SIL OFL — CodeMan38"], role: "candidate · bundleable now" },
    { name: "Handjet", tag: "variable pixel / element", spec: web("'Handjet',monospace", `<div class="big">wand bleep</div>`),
      lic: ["ofl","SIL OFL — Rosetta"], role: "candidate · bundleable now" },
  ]},
];

const KIND = { native:["AC NATIVE","cyan"], cc:["CC-BY","green"], gpl:["GPL","purple"], gust:["GUST","green"], mit:["MIT","green"], commercial:["COMMERCIAL","pink"], proprietary:["PROPRIETARY","gold"], ofl:["OFL","green"], pd:["PD","green"] };
function licBlock(lic){
  const [kind, label, ...rest] = lic;
  const [klabel, kcolor] = KIND[kind];
  const url = rest.find((n)=>/^https?:/.test(n));
  const notes = rest.filter((n)=>!/^https?:/.test(n));
  return `<div class="lic"><span class="chip ${kcolor}">${klabel}</span> <span class="ll">${label}</span>`
    + notes.map((t)=>`<span class="ln${t.startsWith("⚠")?" warn":""}">${t}</span>`).join("")
    + (url?` <a class="lk" href="${url}" target="_blank" rel="noopener">↗</a>`:"") + `</div>`;
}
const card = (f) => `<div class="face"><div class="fh"><span class="fn">${f.name}</span><span class="ft">${f.tag}</span></div>`
  + (f.spec?`<div class="spec">${f.spec}</div>`:"")
  + licBlock(f.lic)
  + `<div class="used">${f.role || ("used: " + f.used.join(", "))}</div></div>`;
const SECTIONS = GROUPS.map((g)=>`<section class="grp"><div class="gh" data-color="${g.color}"><h2>${g.title}</h2></div><div class="faces">${g.faces.map(card).join("")}</div></section>`).join("");

const HTML = `<!DOCTYPE html><html lang="en"><head>
<meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>type foundry · Aesthetic Computer</title>
<link rel="icon" href="https://aesthetic.computer/icon/128x128/prompt.png" type="image/png" />
<link rel="stylesheet" href="https://aesthetic.computer/type/webfonts/berkeley-mono-variable.css">
<style>
  @font-face{ font-family:'YWFT Processing'; src:url('https://aesthetic.computer/type/webfonts/ywft-processing-regular.woff2') format('woff2'); font-display:swap; }
  @font-face{ font-family:'Comic Relief'; src:url('fonts/ComicRelief-Regular.ttf'); font-weight:700; font-display:swap; }
  @font-face{ font-family:'Monaspace Argon'; src:url('fonts/MonaspaceArgon-Regular.otf'); font-display:swap; }
  @font-face{ font-family:'Poppins'; src:url('fonts/Poppins-Regular.ttf'); font-display:swap; }
  @font-face{ font-family:'ProggyClean'; src:url('fonts/ProggyClean.ttf'); font-display:swap; }
  @font-face{ font-family:'Bravura'; src:url('fonts/Bravura.otf'); font-display:swap; }
  @font-face{ font-family:'Latin Modern Roman'; src:url('fonts/lmroman10-regular.otf'); font-display:swap; }
  @font-face{ font-family:'Latin Modern Sans'; src:url('fonts/lmsans10-regular.otf'); font-display:swap; }
  @font-face{ font-family:'Latin Modern Mono'; src:url('fonts/lmmono10-regular.otf'); font-display:swap; }
  @font-face{ font-family:'Departure Mono'; src:url('fonts/DepartureMono-Regular.woff2') format('woff2'); font-display:swap; }
  @font-face{ font-family:'Space Mono'; src:url('fonts/SpaceMono-Regular.ttf'); font-display:swap; }
  @font-face{ font-family:'Space Grotesk'; src:url('fonts/SpaceGrotesk-Variable.ttf'); font-display:swap; }
  @font-face{ font-family:'Inter'; src:url('fonts/Inter-Variable.ttf'); font-display:swap; }
  @font-face{ font-family:'Silkscreen'; src:url('fonts/Silkscreen-Regular.ttf'); font-display:swap; }
  @font-face{ font-family:'Press Start 2P'; src:url('fonts/PressStart2P-Regular.ttf'); font-display:swap; }
  @font-face{ font-family:'Handjet'; src:url('fonts/Handjet-Variable.ttf'); font-display:swap; }
  :root{ --bg:#1a1a2e; --text:#e8e8e8; --dim:#888; --pink:#cd5c9b; --cyan:#4ecdc4; --purple:#7850b4; --gold:#d4a017; --green:#4ecb71; --box-bg:rgba(255,255,255,0.035); --box-border:rgba(255,255,255,0.11); --spec-bg:rgba(255,255,255,0.04); }
  @media (prefers-color-scheme: light){ :root:not(.dark-mode){ --bg:#f5f5f5; --text:#1a1a2e; --dim:#666; --pink:#b4489a; --cyan:#0891b2; --purple:#7850b4; --gold:#a07800; --green:#0a8a3e; --box-bg:rgba(0,0,0,0.025); --box-border:rgba(0,0,0,0.13); --spec-bg:rgba(0,0,0,0.03); } }
  :root.light-mode{ --bg:#f5f5f5; --text:#1a1a2e; --dim:#666; --pink:#b4489a; --cyan:#0891b2; --purple:#7850b4; --gold:#a07800; --green:#0a8a3e; --box-bg:rgba(0,0,0,0.025); --box-border:rgba(0,0,0,0.13); --spec-bg:rgba(0,0,0,0.03); }
  *{ margin:0; padding:0; box-sizing:border-box; } ::-webkit-scrollbar{ display:none; }
  body{ background:var(--bg); color:var(--text); font-family:'Berkeley Mono Variable','Menlo',monospace; font-size:13px; line-height:1.5; -webkit-text-size-adjust:none; padding:1.4em 1.6em 4em; min-height:100vh; }
  a{ color:var(--purple); text-decoration:none; } a:hover{ text-decoration:underline; }
  .crumb{ color:var(--dim); font-size:0.82em; margin-bottom:0.6em; }
  h1{ font-family:'YWFT Processing',monospace; font-size:2.2em; font-weight:normal; letter-spacing:-0.02em; margin-bottom:0.5em; }
  h1 .dot{ color:var(--pink); }
  .legend{ display:flex; flex-wrap:wrap; gap:0.4em 1em; margin:0 0 2.2em; font-size:0.75em; color:var(--dim); }
  .grp{ margin-bottom:2.4em; }
  .gh{ margin-bottom:1.1em; }
  .gh h2{ font-size:0.9em; font-weight:600; letter-spacing:0.08em; text-transform:uppercase; }
  .gh[data-color="pink"] h2{ color:var(--pink); } .gh[data-color="cyan"] h2{ color:var(--cyan); } .gh[data-color="gold"] h2{ color:var(--gold); } .gh[data-color="green"] h2{ color:var(--green); } .gh[data-color="purple"] h2{ color:var(--purple); }
  .faces{ display:grid; grid-template-columns:1fr; gap:1.2em; }
  @media (min-width:640px){ .faces{ grid-template-columns:1fr 1fr; } }
  @media (min-width:1080px){ .faces{ grid-template-columns:1fr 1fr 1fr; } }
  @media (min-width:1560px){ .faces{ grid-template-columns:1fr 1fr 1fr 1fr; } }
  .face{ min-width:0; border:1px solid var(--box-border); border-radius:9px; background:var(--box-bg); padding:1em 1.1em 0.9em; display:flex; flex-direction:column; }
  .fh{ display:flex; align-items:baseline; gap:0.5em; flex-wrap:wrap; }
  .fn{ font-size:1.05em; font-weight:600; } .ft{ color:var(--dim); font-size:0.72em; }
  .spec{ display:flex; flex-direction:column; gap:8px; margin:0.8em 0; padding:0.9em 1em; border:1px solid var(--box-border); border-radius:7px; background:var(--spec-bg); overflow:hidden; }
  .spec .r{ overflow-x:auto; } .spec svg{ height:42px; width:auto; max-width:100%; display:block; color:var(--text); image-rendering:pixelated; }
  .web{ overflow:hidden; } .web .big{ font-size:2.7em; line-height:1.1; word-break:break-word; }
  .web.tall{ padding:0.35em 0; } .web.tall .big{ font-size:3.2em; line-height:1.55; }
  .kidlogo{ font-weight:700; }
  .lic{ font-size:0.78em; margin-bottom:0.35em; }
  .chip{ display:inline-block; font-size:0.84em; letter-spacing:0.05em; padding:0.02em 0.4em; border-radius:3px; border:1px solid currentColor; }
  .chip.cyan{ color:var(--cyan); } .chip.pink{ color:var(--pink); } .chip.gold{ color:var(--gold); } .chip.green{ color:var(--green); } .chip.purple{ color:var(--purple); }
  .ll{ color:var(--text); } .ln{ color:var(--dim); display:block; margin-top:0.25em; line-height:1.45; } .ln.warn{ color:var(--gold); } .lk{ font-size:1.1em; }
  .used{ color:var(--dim); font-size:0.74em; margin-top:auto; }
  @media (max-width:600px){ body{ padding:1.2em; } h1{ font-size:1.7em; } .web .big{ font-size:2.1em; } .web.tall .big{ font-size:2.6em; } }
</style></head><body>
  <div class="crumb"><a href="/platter.html">← platter</a></div>
  <h1>Type Foundry<span class="dot">.</span></h1>
  <div class="legend">
    <span><span class="chip cyan">AC NATIVE</span> our cut</span>
    <span><span class="chip green">CC / OFL / PD / GUST</span> free to bundle</span>
    <span><span class="chip purple">GPL</span> copyleft</span>
    <span><span class="chip pink">COMMERCIAL</span> paid</span>
    <span><span class="chip gold">PROPRIETARY</span> terms vary</span>
  </div>
  ${SECTIONS}
</body></html>`;

fs.writeFileSync(join(HERE, "index.html"), HTML);
console.log("wrote index.html (" + HTML.length + " bytes), " + GROUPS.reduce((n,g)=>n+g.faces.length,0) + " faces");
