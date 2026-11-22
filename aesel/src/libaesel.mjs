// libaesel — Aesel as a library.
//
// A printer is a small program that makes one kind of thing. It is a harness
// folder and a launcher, and the launcher imports this file. Everything a printer
// needs is here: the harness factory, the signed-in session, hosted inference,
// and `main`, the command line every printer shares.
//
//   import { main } from ".../aesel/src/libaesel.mjs";
//   await main(folder, process.argv.slice(2));
//
// Inference is AC hosted and paid in braincells by the @handle running it.
import path from "node:path";
import process from "node:process";
import { parseArgs } from "node:util";
import { ACSession, SITE, USER_AGENT } from "./ac-session.mjs";
import { OUTPUTS, harnessRequest, inputContent, latestRun, readHarness, runHarness, streamText } from "./harness-factory.mjs";
import { planMime, postMime } from "./mime.mjs";

export * from "./harness-factory.mjs";
export { ACSession } from "./ac-session.mjs";
export { printer, printerOutput } from "./printer.mjs";

// A room of laptops on one Wi-Fi drops connections; a request that never got
// an answer is tried twice more after a breath. An HTTP error is not retried.
async function post(url, init, note, tries = 3) {
  try { return await fetch(url, init); } catch (error) {
    if (tries <= 1) throw new Error(`could not reach aesthetic.computer (${error.cause?.code || error.message})`);
    note("  … connection dropped, trying again");
    await new Promise((resolve) => setTimeout(resolve, 1500));
    return post(url, init, note, tries - 1);
  }
}

/** The two calls a harness run makes, bound to one signed-in session. */
export function hosted(session = new ACSession(), { note = () => {}, client = "aesel-harness" } = {}) {
  async function infer(request) {
    const token = await session.token();
    const response = await post(`${SITE}/api/easel-inference`, {
      method: "POST",
      headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}`, "User-Agent": USER_AGENT, "x-ac-client": client },
      body: JSON.stringify(request),
    }, note);
    if (!response.ok) {
      const body = await response.json().catch(() => ({}));
      throw new Error(body?.error?.message || body?.error || `inference failed (HTTP ${response.status})`);
    }
    return streamText(response);
  }
  async function paint(plan) {
    const { hostedPictureImage } = await import("./picture-image.mjs");
    const jobId = `harness-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
    return hostedPictureImage({ provider: plan.provider, model: plan.model, prompt: plan.prompt, size: plan.size, quality: plan.quality }, { reference: plan.reference, jobId, session });
  }
  return { session, infer, paint };
}

/** Post a harness's newest run (or the one named) to mime.ac. */
export async function postRun(harness, { session = new ACSession(), run, caption = "" } = {}) {
  const made = latestRun(harness, run);
  const said = made.inputs.texts.join(" ");
  const plan = planMime(made.file, { caption: caption || `${harness.name}${said ? `: ${said}` : ""}` });
  if (process.env.AESEL_DRY_RUN === "1") return { plan };
  return { plan, posted: await postMime(plan, { session }) };
}

const usage = (harness) => `${harness.name}${harness.description ? ` — ${harness.description}` : ""}
  ${harness.inputs.map((i) => INPUT_HINT[i]).join(" ")} [--dry] [--post] [--model <id>]
  --serve

  --serve  switch it on as a printer: it appears in the Print dialog, and each
           page printed to it comes out as what this makes
  --dry    show what would be sent, spend nothing
  --post   put what it makes on mime.ac under your @handle
  it reads harness.json and prompt.md beside it; edit prompt.md to change it.`;
const INPUT_HINT = { text: '"words"', image: "--image <path>", file: "--file <path>" };

/**
 * The command line of a printer: run the harness in `dir` on what argv names.
 * Returns the run, or null for --help and --dry.
 */
export async function main(dir, argv = [], { out = (text = "") => process.stdout.write(`${text}\n`), note = (text) => process.stderr.write(`${text}\n`), services } = {}) {
  const harness = readHarness(dir);
  const { values, positionals } = parseArgs({ args: argv, allowPositionals: true, options: {
    image: { type: "string", multiple: true }, file: { type: "string", multiple: true }, model: { type: "string" },
    dry: { type: "boolean" }, post: { type: "boolean" }, serve: { type: "boolean" }, help: { type: "boolean", short: "h" } } });
  if (values.help) { out(usage(harness)); return null; }
  if (values.model) harness.model = values.model;
  // Started by the print server with one spooled page: this run is a print job.
  if (process.env.AESEL_PRINT_JOB && positionals.length === 1 && path.isAbsolute(positionals[0]) && !values.serve) {
    const { printJob } = await import("./printer-serve.mjs");
    await printJob(harness, positionals[0], { hosted: (note) => services || hosted(undefined, { note, client: "aesel-printer" }) });
    return null;
  }
  if (values.serve) {
    const { serve } = await import("./printer-serve.mjs");
    return serve(harness, path.join(harness.dir, harness.name));
  }
  const inputs = { texts: positionals, images: values.image || [], files: values.file || [] };
  const goal = `${OUTPUTS[harness.output].glyph} ${harness.output}`;
  if (values.dry) {
    const request = harnessRequest(harness, inputContent(harness, inputs));
    out(`${goal}  ${harness.name} on ${harness.model} — dry run, nothing spent\n`);
    out(request.system);
    out("\n--- user ---");
    for (const block of request.messages[0].content) out(block.type === "image" ? `[image, ${Math.round(block.source.data.length * 0.75 / 1024)} KB png]` : block.text);
    return null;
  }
  const { session, infer, paint } = services || hosted(undefined, { note, client: "aesel-printer" });
  if (!session.signedIn) throw new Error("sign in first: ac login");
  note(`${goal}  ${harness.name}`);
  const run = await runHarness(harness, inputs, { infer, paint, onstep: (step) => note(`  … ${step}`) });
  if (run.problems.length) {
    note("  ✗ still failing after a repair round:");
    for (const problem of run.problems) note(`    - ${problem}`);
    throw new Error(`the model's answer is in ${path.join(run.dir, "answer.md")}`);
  }
  out(path.relative(process.cwd(), run.file) || run.file);
  note(`  ✓ ${OUTPUTS[harness.output].kind} in ${run.rounds} round${run.rounds === 1 ? "" : "s"}`);
  if (values.post) {
    const { plan, posted } = await postRun(harness, { session });
    if (posted) { out(posted.url); note(`  ${goal} on the ${posted.board || plan.type} board`); }
  }
  return run;
}
