import { createHash } from "node:crypto";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { access, copyFile, lstat, mkdir, mkdtemp, readFile, readdir, realpath, rm, writeFile } from "node:fs/promises";
import { constants } from "node:fs";
import { delimiter, join, resolve } from "node:path";
import { sourceZip } from "../../media/paper/zip.mjs";
import { validateManifest, countEvidenceFigures } from "../../media/paper/aesthetic-eye.mjs";
const execute = promisify(execFile);
const assets = new URL("../../media/paper/", import.meta.url);
const hash = (bytes) => createHash("sha256").update(bytes).digest("hex");
export const kind = "paper";
const object = (properties, required = []) => ({ type: "object", properties, required, additionalProperties: false });
const string = { type: "string" };
export const actions = [
  { name: "write", description: "Write archival manuscript sections, explicit author, bibliography and consulted sources. Section text supports a constrained LaTeX subset.", inputSchema: object({ title: string, author: string, abstract: string, sections: { type: "array", items: object({ title: string, text: string }, ["title", "text"]) }, bibliography: string, sources: string }, ["author", "title", "abstract", "sections"]) },
  { name: "figure", description: "Attach an explicitly selected PNG/JPEG figure and numbered caption; never import arbitrary filesystem paths.", inputSchema: object({ name: string, base64: string, caption: string }, ["name", "base64", "caption"]) },
  { name: "build", description: "Build a real PDF and embedded source ZIP with Tectonic or XeLaTeX. Compilation remains awaiting visual QA.", inputSchema: object({ engine: { enum: ["auto", "tectonic", "xelatex"] } }) },
  { name: "qa", description: "Record completed visual inspection and Figure-Table-QA against the exact current PDF hash; no automatic approval.", inputSchema: object({ aestheticEye: { type: "object" }, figureTable: { type: "object" } }, ["aestheticEye", "figureTable"]) },
  { name: "status", description: "Read manuscript, build, compiler and current-hash QA status.", inputSchema: object({}) },
];
const escape = (text) => String(text ?? "").replace(/[\\{}%&#_$^~]/g, (char) => ({ "\\": "\\textbackslash{}", "^": "\\textasciicircum{}", "~": "\\textasciitilde{}" }[char] || `\\${char}`));
async function safe(root, relative) {
  if (!relative || relative.startsWith("/") || relative.split(/[\\/]/).some((s) => s === ".." || !s)) throw new Error("Paper paths must stay inside the artifact.");
  const base = await realpath(root);
  let path = base;
  for (const part of relative.split("/")) {
    path = join(path, part);
    try { if ((await lstat(path)).isSymbolicLink()) throw new Error("Paper paths cannot use symlinks."); }
    catch (error) { if (error.code !== "ENOENT") throw error; }
  }
  return path;
}
async function get(root, path) { return readFile(await safe(root, path), "utf8"); }
async function put(root, path, value) { await writeFile(await safe(root, path), value); }
async function metadata(root) { return JSON.parse(await get(root, "paper.json")); }
async function save(root, data) { await put(root, "paper.json", `${JSON.stringify(data, null, 2)}\n`); }
async function inventory(root) {
  const names = ["paper.json", "manuscript.tex", "references.bib", "sources.md", "ac-source-bundle.sty"];
  for (const name of await readdir(await safe(root, "figures"))) names.push(`figures/${name}`);
  for (const name of ["manuscript.pdf", "manuscript-source.zip", "manuscript-source-README.txt", "aesthetic-eye.json", "figure-table-qa.json"]) {
    try { await access(await safe(root, name)); names.push(name); } catch (error) { if (error.code !== "ENOENT") throw error; }
  }
  return names;
}
async function dependencies(root) {
  const names = ["manuscript.tex", "references.bib", "ac-source-bundle.sty", ...((await readdir(await safe(root, "figures"))).map((n) => `figures/${n}`))].sort();
  return hash(Buffer.concat(await Promise.all(names.map(async (name) => Buffer.concat([Buffer.from(name + "\0"), await readFile(await safe(root, name))])))));
}
async function result(root, summary) {
  const data = await metadata(root);
  const files = await inventory(root);
  let status = "draft";
  let preview = { path: "manuscript.tex", mime: "text/plain" };
  if (data.build && files.includes("manuscript.pdf")) {
    const current = hash(await readFile(await safe(root, "manuscript.pdf")));
    if (data.build.dependencies === await dependencies(root) && data.build.pdfSha256 === current) {
      status = "built-awaiting-qa";
      if (data.qa?.pdfSha256 === current) {
        try {
          const eye = JSON.parse(await get(root, "aesthetic-eye.json"));
          const figureTable = JSON.parse(await get(root, "figure-table-qa.json"));
          const review = validateManifest(eye, current, countEvidenceFigures(await get(root, "manuscript.tex")));
          if (review.pass && figureTable.pdfSha256 === current && figureTable.status === "pass") status = "ready";
        } catch {}
      }
      preview = { path: "manuscript.pdf", mime: "application/pdf" };
    } else status = "stale-build";
  }
  return { files, preview, summary, status, pdfSha256: data.build?.pdfSha256 || null };
}
export async function create({ root, name = "Untitled paper" }) {
  await mkdir(root, { recursive: true });
  try { await access(await safe(root, "paper.json")); throw new Error("Paper already exists."); } catch (error) { if (error.code !== "ENOENT") throw error; }
  await mkdir(await safe(root, "figures"), { recursive: true });
  await copyFile(new URL("ac-source-bundle.sty", assets), await safe(root, "ac-source-bundle.sty"));
  const body = ["Introduction", "Related Work", "Method", "Implementation", "Evaluation", "Ethics, Privacy and Limitations", "Conclusion"].map((title) => `\\section{${title}}\nDraft pending.\n`).join("\n");
  const template = await readFile(new URL("template.tex", assets), "utf8");
  await put(root, "manuscript.tex", template.replace("@@TITLE@@", escape(name)).replace("@@AUTHOR@@", "Author not set").replace("@@DATE@@", new Date().toISOString().slice(0, 10)).replace("@@ABSTRACT@@", "Draft pending.").replace("@@BODY@@", body));
  await put(root, "references.bib", "% Add verified bibliography entries.\n");
  await put(root, "sources.md", "# Consulted sources\n\nRecord public Platter context and primary evidence consulted before drafting. Keep private evidence outside this artifact.\n");
  await save(root, { version: 1, title: name, author: null, lane: "arxiv", status: "draft" });
  return result(root, "Paper scaffolded. Set an explicit author and consult sources before building.");
}
// A narrow manuscript language prevents TeX file reads, executable packages,
// macro expansion tricks and shell escape from becoming a second tool API.
const commands = new Set("documentclass usepackage title author date today begin end maketitle section subsection subsubsection paragraph textbf textit emph texttt textbackslash textasciicircum textasciitilde label ref pageref cite nocite url href footnote caption includegraphics centering bibliography bibliographystyle item hline toprule midrule bottomrule multicolumn small tiny scriptsize normalsize large Large linewidth textwidth columnwidth newpage clearpage appendix noindent par vspace hspace quad qquad frac sqrt sum prod int lim sin cos log exp alpha beta gamma delta epsilon theta lambda mu pi sigma tau phi omega Delta Sigma Omega mathbb mathcal mathrm mathbf left right cdot times leq geq neq approx pm in infty partial percent".split(" "));
export function validateTex(text) {
  if (text.includes("^^")) throw new Error("Encoded TeX control sequences are not supported.");
  for (const match of text.matchAll(/\\([A-Za-z@]+)/g)) if (!commands.has(match[1])) throw new Error(`Unsupported TeX command: \\${match[1]}`);
  for (const match of text.matchAll(/\\(?:begin|end)\{([^}]+)\}/g)) if (!["document", "abstract", "figure", "figure*", "table", "table*", "tabular", "equation", "equation*", "itemize", "enumerate", "quote", "quotation", "center", "minipage", "displaymath", "math"].includes(match[1])) throw new Error(`Unsupported paper environment: ${match[1]}`);
  for (const match of text.matchAll(/\\(?:documentclass|usepackage)(?:\[[^\]]*\])?\{([^}]+)\}/g)) {
    if (!match[1].split(",").every((name) => ["article", "geometry", "graphicx", "hyperref", "ac-source-bundle"].includes(name))) throw new Error("Only bundled paper packages are allowed.");
  }
  for (const match of text.matchAll(/\\href\{([^}]+)\}/g)) if (!/^(https?:|mailto:)/.test(match[1])) throw new Error("Paper links must use http, https or mailto.");
  for (const match of text.matchAll(/\\(?:bibliography|bibliographystyle)\{([^}]+)\}/g)) if (!["references", "plain"].includes(match[1])) throw new Error("External bibliography files are not allowed.");
}
async function compiler(preference = "auto") {
  if (!["auto", "tectonic", "xelatex"].includes(preference)) throw new Error("Unknown paper compiler.");
  for (const engine of preference === "auto" ? ["tectonic", "xelatex"] : [preference]) {
    for (const directory of (process.env.PATH || "").split(delimiter)) {
      if (!directory) continue;
      const path = resolve(directory, engine);
      try { await access(path, constants.X_OK); return { engine, path }; } catch {}
    }
  }
  throw new Error("Paper build requires Tectonic or XeLaTeX on PATH. Install a TeX toolchain; this PDF is not built.");
}
export async function run({ root, action, input = {} }) {
  const data = await metadata(root);
  if (action === "status") return result(root, "Paper status.");
  if (action === "write") {
    if (!input.author?.trim() || !input.title?.trim() || !Array.isArray(input.sections)) throw new Error("Paper requires explicit author, title and sections.");
    for (const path of ["manuscript.tex", "references.bib", "sources.md", "paper.json"]) await safe(root, path);
    const body = input.sections.map(({ title, text }) => `\\section{${escape(title)}}\n${String(text || "")}\n`).join("\n");
    validateTex(body); validateTex(input.abstract || ""); validateTex(input.bibliography || "");
    const template = await readFile(new URL("template.tex", assets), "utf8");
    await put(root, "manuscript.tex", template.replace("@@TITLE@@", escape(input.title)).replace("@@AUTHOR@@", escape(input.author)).replace("@@DATE@@", new Date().toISOString().slice(0, 10)).replace("@@ABSTRACT@@", input.abstract || "").replace("@@BODY@@", body));
    if (input.bibliography !== undefined) await put(root, "references.bib", input.bibliography);
    if (input.sources !== undefined) await put(root, "sources.md", input.sources);
    await save(root, { ...data, author: input.author, title: input.title, qa: null });
    return result(root, "Manuscript updated; rebuild and visually inspect before marking ready.");
  }
  if (action === "figure") {
    if (!/^[a-zA-Z0-9_-]+\.(png|jpe?g)$/.test(input.name || "")) throw new Error("Figure needs a simple PNG/JPEG filename.");
    const bytes = Buffer.from(input.base64 || "", "base64");
    const png = bytes.subarray(0, 8).equals(Buffer.from([137,80,78,71,13,10,26,10]));
    const jpeg = bytes[0] === 255 && bytes[1] === 216;
    if ((!png && !jpeg) || bytes.length > 20_000_000) throw new Error("Figure must be a PNG/JPEG under 20 MB.");
    const source = await get(root, "manuscript.tex");
    await put(root, `figures/${input.name}`, bytes);
    const figure = `\\begin{figure}[ht]\n\\centering\n\\includegraphics[width=\\linewidth]{figures/${input.name}}\n\\caption{${escape(input.caption)}}\n\\end{figure}\n`;
    await put(root, "manuscript.tex", source.replace("\\bibliographystyle{plain}", figure + "\\bibliographystyle{plain}"));
    await save(root, { ...data, qa: null });
    return result(root, "Figure attached; rebuild to update its numbered caption and PDF.");
  }
  if (action === "build") {
    if (!data.author) throw new Error("Set the paper's explicit author before building.");
    const engine = await compiler(input.engine);
    const source = await get(root, "manuscript.tex"), bib = await get(root, "references.bib");
    validateTex(source); validateTex(bib);
    for (const match of source.matchAll(/\\includegraphics(?:\[[^\]]*\])?\{([^}]+)\}/g)) {
      if (!/^figures\/[a-zA-Z0-9_-]+\.(png|jpe?g)$/.test(match[1])) throw new Error("Figure path must name an attached image.");
      await access(await safe(root, match[1]));
    }
    const keys = new Set([...bib.matchAll(/@\w+\s*\{\s*([^,\s]+)/g)].map((m) => m[1]));
    for (const match of source.matchAll(/\\cite(?:\[[^\]]*\])?\{([^}]+)\}/g)) for (const key of match[1].split(",")) if (!keys.has(key.trim())) throw new Error(`Missing bibliography entry: ${key.trim()}`);
    await copyFile(new URL("ac-source-bundle.sty", assets), await safe(root, "ac-source-bundle.sty"));
    const dependencyHash = await dependencies(root);
    const files = { "manuscript.tex": source, "references.bib": bib, "ac-source-bundle.sty": await get(root, "ac-source-bundle.sty") };
    files["manuscript-source-README.txt"] = "Paper source bundle\n" + Object.keys(files).join("\n") + "\nFor accessibility, audit and reproducible editing. Excludes private evidence, source-consultation notes, credentials, raw datasets, generated PDFs and figures.\n";
    await put(root, "manuscript-source-README.txt", files["manuscript-source-README.txt"]);
    await put(root, "manuscript-source.zip", sourceZip(files));
    const out = await mkdtemp(await safe(root, ".paper-build-"));
    const options = { cwd: await realpath(root), timeout: 120000, maxBuffer: 2_000_000, env: { PATH: process.env.PATH, HOME: process.env.HOME, SOURCE_DATE_EPOCH: "946684800", FORCE_SOURCE_DATE: "1", openin_any: "p", openout_any: "p" } };
    try {
      if (engine.engine === "tectonic") await execute(engine.path, ["--untrusted", "--keep-logs", "--outdir", out, "manuscript.tex"], options);
      else {
        const args = ["-no-shell-escape", "-interaction=nonstopmode", "-halt-on-error", `-output-directory=${out}`, "manuscript.tex"];
        await execute(engine.path, args, options);
        if (keys.size) { await copyFile(await safe(root, "references.bib"), join(out, "references.bib")); await execute("bibtex", ["manuscript"], { ...options, cwd: out }); }
        await execute(engine.path, args, options); await execute(engine.path, args, options);
      }
      if (await dependencies(root) !== dependencyHash) throw new Error("Paper changed during compilation; build again.");
      const pdf = await readFile(join(out, "manuscript.pdf"));
      if (!pdf.subarray(0,5).equals(Buffer.from("%PDF-"))) throw new Error("Compiler did not produce a PDF.");
      const log = await readFile(join(out, "manuscript.log"), "utf8").catch(() => "");
      if (/undefined references|Citation .+ undefined|There were undefined/i.test(log)) throw new Error("Paper has unresolved references or citations.");
      await put(root, "manuscript.pdf", pdf);
      await save(root, { ...data, qa: null, build: { engine: engine.engine, pdfSha256: hash(pdf), dependencies: dependencyHash, builtAt: new Date().toISOString() } });
    } catch (error) { throw new Error(`Paper build failed: ${error.message}\n${String(error.stderr || "").slice(-1800)}`); }
    finally { await rm(out, { recursive: true, force: true }); }
    return result(root, "PDF built with embedded sources. Figure-Table-QA and visual inspection are still required.");
  }
  if (action === "qa") {
    const current = await result(root, "");
    if (current.status !== "built-awaiting-qa" && current.status !== "ready") throw new Error("Build the current manuscript before QA.");
    const review = validateManifest(input.aestheticEye, current.pdfSha256, countEvidenceFigures(await get(root, "manuscript.tex")));
    if (!review.pass) throw new Error(`Aesthetic Eye failed: ${review.errors.join("; ")}`);
    const qa = input.figureTable;
    if (qa?.pdfSha256 !== current.pdfSha256 || qa.status !== "pass" || !qa.reviewedBy || !qa.reviewedAt || !Array.isArray(qa.inspectedPages) || !qa.inspectedPages.length || !qa.inspectedPages.every((page) => Number.isInteger(page) && page > 0)) throw new Error("Figure-Table-QA requires current PDF hash, reviewer, date and inspected page numbers.");
    await put(root, "aesthetic-eye.json", JSON.stringify(input.aestheticEye, null, 2));
    await put(root, "figure-table-qa.json", JSON.stringify(qa, null, 2));
    await save(root, { ...data, qa: { pdfSha256: current.pdfSha256, reviewedAt: qa.reviewedAt } });
    return result(root, "Current PDF passed recorded visual and figure/table review.");
  }
  throw new Error(`Unknown paper action: ${action}`);
}
