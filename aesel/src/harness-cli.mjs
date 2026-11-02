#!/usr/bin/env node
// harness-cli — `ac harness`: list starters, make a harness, run it, post what it made.
//
//   ac harness list                         starters and output goals
//   ac harness new <dir> [--from <starter>] [--in text,image] [--out <mime>]
//   ac harness run <dir> ["words"…] [--image <path>]… [--file <path>]… [--model <id>] [--dry]
//   ac harness post <dir> [--run <id>] ["caption"]
import "./env.mjs";
import path from "node:path";
import process from "node:process";
import { parseArgs } from "node:util";
import { ACSession, SITE, USER_AGENT } from "./ac-session.mjs";
import { INPUTS, OUTPUTS, harnessRequest, inputContent, latestRun, newHarness, outputType, readHarness, runHarness, starters, streamText } from "./harness-factory.mjs";
import { planMime, postMime } from "./mime.mjs";

const out = (text = "") => process.stdout.write(`${text}\n`);
const note = (text) => process.stderr.write(`${text}\n`);
const fail = (message) => { process.stderr.write(`aesthetic harness: ${message}\n`); process.exit(1); };
const goal = (mime) => `${OUTPUTS[mime].glyph} ${mime}`;
const shown = (file) => path.relative(process.cwd(), file) || file;

const [command = "list", ...rest] = process.argv.slice(2);
const session = new ACSession();

// A room of laptops on one Wi-Fi drops connections; a request that never got
// an answer is tried once more after a breath. An HTTP error is not retried.
async function post(url, init, tries = 2) {
  try { return await fetch(url, init); } catch (error) {
    if (tries <= 1) throw new Error(`could not reach aesthetic.computer (${error.cause?.code || error.message})`);
    note("  … connection dropped, trying again");
    await new Promise((resolve) => setTimeout(resolve, 1500));
    return post(url, init, tries - 1);
  }
}

async function infer(request) {
  const token = await session.token();
  const response = await post(`${SITE}/api/easel-inference`, {
    method: "POST",
    headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}`, "User-Agent": USER_AGENT, "x-ac-client": "aesel-harness" },
    body: JSON.stringify(request),
  });
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

try {
  if (command === "list") {
    out("starters — ac harness new <dir> --from <starter>\n");
    for (const s of starters()) out(`  ${s.id.padEnd(14)} ${s.inputs.map((i) => INPUTS[i].glyph + " " + i).join(" + ").padEnd(28)} → ${goal(s.output).padEnd(20)} ${s.description}`);
    out("\noutput goals — --out <mime>\n");
    for (const [mime, o] of Object.entries(OUTPUTS)) out(`  ${goal(mime).padEnd(22)} .${o.ext.padEnd(5)} ${o.kind}${o.as === "image" ? " (model art-directs, image model paints; 5 hosted images a day)" : o.as === "score" ? " (model composes, your machine synthesizes)" : ""}`);
    out("\ninputs — --in <a,b>\n");
    for (const [name, i] of Object.entries(INPUTS)) out(`  ${i.glyph} ${name.padEnd(6)} ${i.about}`);
  } else if (command === "new") {
    const { values, positionals } = parseArgs({ args: rest, allowPositionals: true, options: { from: { type: "string" }, in: { type: "string" }, out: { type: "string" } } });
    if (positionals.length !== 1) fail("usage: ac harness new <dir> [--from <starter>] [--in text,image] [--out <mime>]");
    const harness = newHarness(positionals[0], { from: values.from, inputs: values.in?.split(",").map((s) => s.trim()).filter(Boolean), output: values.out && outputType(values.out) });
    out(`${goal(harness.output)}  ${harness.name}  ← ${harness.inputs.join(" + ")}`);
    out(`edit ${shown(path.join(harness.dir, "prompt.md"))}, then:`);
    out(`  ac harness run ${positionals[0]} ${harness.inputs.map((i) => (i === "text" ? '"…"' : `--${i} <path>`)).join(" ")}`);
  } else if (command === "run") {
    const { values, positionals } = parseArgs({ args: rest, allowPositionals: true, options: {
      image: { type: "string", multiple: true }, file: { type: "string", multiple: true }, model: { type: "string" }, dry: { type: "boolean" } } });
    if (!positionals.length) fail('usage: ac harness run <dir> ["words"…] [--image <path>] [--file <path>] [--dry]');
    const harness = readHarness(positionals[0]);
    if (values.model) harness.model = values.model;
    const inputs = { texts: positionals.slice(1), images: values.image || [], files: values.file || [] };
    if (values.dry) {
      const request = harnessRequest(harness, inputContent(harness, inputs));
      out(`${goal(harness.output)}  ${harness.name} on ${harness.model} — dry run, nothing spent\n`);
      out(request.system);
      out("\n--- user ---");
      for (const block of request.messages[0].content) out(block.type === "image" ? `[image, ${Math.round(block.source.data.length * 0.75 / 1024)} KB png]` : block.text);
    } else {
      if (!session.signedIn) fail("sign in first: ac login");
      note(`${goal(harness.output)}  ${harness.name}`);
      const run = await runHarness(harness, inputs, { infer, paint, onstep: (step) => note(`  … ${step}`) });
      if (run.problems.length) {
        note(`  ✗ still failing after a repair round:`);
        for (const problem of run.problems) note(`    - ${problem}`);
        note(`  the model's answer is in ${shown(path.join(run.dir, "answer.md"))}`);
        process.exit(1);
      }
      out(shown(run.file));
      note(`  ✓ ${OUTPUTS[harness.output].kind} in ${run.rounds} round${run.rounds === 1 ? "" : "s"} · post it: ac harness post ${positionals[0]}`);
    }
  } else if (command === "post") {
    const { values, positionals } = parseArgs({ args: rest, allowPositionals: true, options: { run: { type: "string" } } });
    if (!positionals.length) fail('usage: ac harness post <dir> [--run <id>] ["caption"]');
    const harness = readHarness(positionals[0]);
    const run = latestRun(harness, values.run);
    const said = run.inputs.texts.join(" ");
    const caption = positionals.slice(1).join(" ") || `${harness.name}${said ? `: ${said}` : ""}`;
    const plan = planMime(run.file, { caption });
    if (process.env.AESEL_DRY_RUN === "1") out(JSON.stringify({ ...plan, path: undefined }));
    else {
      const posted = await postMime(plan, { session });
      out(posted.url);
      note(`  ${goal(harness.output)} on the ${posted.board || plan.type} board`);
    }
  } else {
    fail(`unknown command: ${command} — try list, new, run or post`);
  }
} catch (error) {
  fail(error?.message || String(error));
}
