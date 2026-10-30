import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, writeFileSync, existsSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import test from "node:test";
import { encode, decode } from "../media/picture/png.mjs";
import { OUTPUTS, builtInProblems, extract, fit, harnessRequest, inputContent, latestRun, newHarness, outputType, readHarness, runHarness, starters, streamText, validateManifest } from "../src/harness-factory.mjs";
import { frequency, renderScore, scoreProblems } from "../src/wav-score.mjs";

const scratch = () => mkdtempSync(path.join(tmpdir(), "harness-test-"));
const sse = (...events) => new Response(events.map((e) => `data: ${JSON.stringify(e)}\n\n`).join(""));
const answer = (lang, body) => ({ text: "Here it is.\n```" + lang + "\n" + body + "\n```\n", usage: { output_tokens: 10 } });

test("every starter is a valid harness with a prompt", () => {
  const list = starters();
  assert.ok(list.length >= 6);
  for (const s of list) {
    const harness = readHarness(path.join(new URL("../workshop/starters/", import.meta.url).pathname, s.id));
    assert.ok(OUTPUTS[harness.output], s.id);
    assert.ok(harness.prompt.length > 80, s.id);
  }
  const goals = new Set(list.map((s) => s.output));
  for (const mime of ["text/plain", "image/svg+xml", "text/html", "audio/wav", "text/x-lisp", "image/png"]) assert.ok(goals.has(mime), mime);
});

test("manifests name known inputs and a real MIME goal", () => {
  assert.equal(outputType("svg"), "image/svg+xml");
  assert.equal(validateManifest({ name: "x", output: "wav" }).inputs[0], "text");
  assert.throws(() => validateManifest({ name: "x", inputs: ["smell"], output: "svg" }), /unknown input/);
  assert.throws(() => validateManifest({ name: "x", output: "video/mp4" }), /no output goal/);
  assert.throws(() => validateManifest({ name: "Bad Name", output: "svg" }), /lowercase/);
});

test("inputs the harness did not ask for are refused", () => {
  const harness = { name: "poster", inputs: ["text"], output: "image/svg+xml" };
  assert.throws(() => inputContent(harness, { images: ["a.png"] }), /does not take image/);
  assert.throws(() => inputContent(harness, {}), /give poster something/);
  assert.deepEqual(inputContent(harness, { texts: ["jazz", "night"] }), [{ type: "text", text: "jazz night" }]);
});

test("images go out as bounded PNGs", () => {
  const dir = scratch(), file = path.join(dir, "big.png");
  const width = 1600, height = 800, data = Buffer.alloc(width * height * 4, 200);
  writeFileSync(file, encode({ width, height, data }));
  const [label, image] = inputContent({ name: "h", inputs: ["image"] }, { images: [file] });
  assert.match(label.text, /big\.png/);
  const png = decode(Buffer.from(image.source.data, "base64"));
  assert.equal(png.width, 768); assert.equal(png.height, 384);
  assert.deepEqual(fit({ width: 10, height: 10, data: Buffer.alloc(400) }).width, 10);
});

test("the system prompt is the person's prompt plus the goal's contract", () => {
  const request = harnessRequest({ prompt: "Be a poster.", output: "image/svg+xml", model: "m" }, [{ type: "text", text: "hi" }]);
  assert.match(request.system, /^Be a poster\./);
  assert.match(request.system, /```svg block/);
  const repair = harnessRequest({ prompt: "p", output: "text/plain", model: "m" }, [], { previous: "old", problems: ["too long"] });
  assert.equal(repair.messages.length, 3);
  assert.match(repair.messages[2].content[0].text, /too long/);
});

test("answers are extracted and checked by type", () => {
  assert.equal(extract("image/svg+xml", "x\n```svg\n<svg></svg>\n```"), "<svg></svg>");
  assert.equal(extract("text/plain", "no fence"), "no fence");
  assert.deepEqual(builtInProblems("image/svg+xml", '<svg xmlns="http://www.w3.org/2000/svg"></svg>'), []);
  assert.match(builtInProblems("application/json", "{")[0], /does not parse/);
  assert.match(builtInProblems("text/x-lisp", "(wipe \"pink\"")[0], /balance/);
  assert.deepEqual(builtInProblems("text/x-lisp", '(wipe "pink")\n(ink "(")'), []);
  assert.ok(builtInProblems("text/javascript", "const a = 1;").some((p) => /paint/.test(p)));
  assert.deepEqual(builtInProblems("text/javascript", 'function paint({ wipe }) { wipe("pink"); }\nexport { paint };'), []);
});

test("a score becomes a WAV", () => {
  assert.equal(Math.round(frequency("A4")), 440);
  assert.equal(Math.round(frequency(60)), 262);
  assert.ok(scoreProblems({ bpm: 90, notes: [{ at: 0, beats: 1, pitch: "H9" }] }).length);
  const wav = renderScore({ bpm: 120, notes: [{ at: 0, beats: 1, pitch: "C4", wave: "triangle" }, { at: 1, beats: 0.5, pitch: "noise" }] });
  assert.equal(wav.toString("ascii", 0, 4), "RIFF");
  assert.equal(wav.toString("ascii", 8, 12), "WAVE");
  assert.ok(wav.length > 44 + 22050);
});

test("streamed text and usage are gathered from server-sent events", async () => {
  const { text, usage } = await streamText(sse(
    { type: "message_start", message: { usage: { input_tokens: 5 } } },
    { type: "content_block_delta", delta: { type: "text_delta", text: "hel" } },
    { type: "content_block_delta", delta: { type: "text_delta", text: "lo" } },
    { type: "message_delta", usage: { output_tokens: 2 } }));
  assert.equal(text, "hello");
  assert.deepEqual(usage, { input_tokens: 5, output_tokens: 2 });
});

test("a run repairs once, then writes the file its goal names", async () => {
  const dir = scratch();
  const harness = newHarness(path.join(dir, "my-poster"), { inputs: ["text"], output: "image/svg+xml" });
  assert.equal(harness.name, "my-poster");
  const replies = [answer("svg", "<svg>"), answer("svg", '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"></svg>')];
  const seen = [];
  const run = await runHarness(harness, { texts: ["jazz"] }, { infer: async (request) => (seen.push(request), replies.shift()) });
  assert.equal(seen.length, 2);
  assert.equal(seen[1].messages.length, 3);
  assert.equal(run.rounds, 2);
  assert.match(readFileSync(run.file, "utf8"), /^<svg xmlns/);
  assert.equal(latestRun(harness).file, run.file);
  assert.ok(existsSync(path.join(run.dir, "answer.md")));
});

test("a starter's own check.mjs joins the checks", async () => {
  const dir = scratch();
  const harness = newHarness(path.join(dir, "cam"), { from: "haiku-cam" });
  assert.deepEqual(harness.inputs, ["image", "text"]);
  const replies = [answer("text", "one line"), answer("text", "old pond\na frog jumps in\nsplash")];
  const run = await runHarness(harness, { texts: ["rain"] }, { infer: async () => replies.shift() });
  assert.deepEqual(run.problems, []);
  assert.equal(readFileSync(run.file, "utf8").trim().split("\n").length, 3);
});

test("sound and pictures are made from the answer, not as it", async () => {
  const dir = scratch();
  const hum = newHarness(path.join(dir, "hum"), { from: "hum" });
  const sound = await runHarness(hum, { texts: ["dusk"] }, { infer: async () => answer("json", JSON.stringify({ bpm: 80, notes: [{ at: 0, beats: 2, pitch: "E4" }] })) });
  assert.equal(path.extname(sound.file), ".wav");
  assert.equal(readFileSync(sound.file).toString("ascii", 8, 12), "WAVE");

  const sticker = newHarness(path.join(dir, "sticker"), { from: "sticker" });
  const plans = [];
  const png = encode({ width: 2, height: 2, data: Buffer.alloc(16, 255) });
  const picture = await runHarness(sticker, { texts: ["a frog"] }, {
    infer: async () => answer("prompt", "a cheerful frog sticker"),
    paint: async (plan) => (plans.push(plan), { bytes: png }),
  });
  assert.equal(plans[0].prompt, "a cheerful frog sticker");
  assert.equal(plans[0].model, "gpt-image-2");
  assert.equal(path.extname(picture.file), ".png");
});

test("failing runs leave the answer and no file", async () => {
  const dir = scratch();
  const harness = newHarness(path.join(dir, "data"), { output: "json" });
  const run = await runHarness(harness, { texts: ["x"] }, { infer: async () => answer("json", "{nope") });
  assert.equal(run.file, null);
  assert.equal(run.problems.length, 1);
  assert.throws(() => latestRun(harness), /no finished runs/);
});

test("a request that never answers leaves no empty run folder", async () => {
  const dir = scratch();
  const harness = newHarness(path.join(dir, "quiet"), { output: "text/plain" });
  await assert.rejects(runHarness(harness, { texts: ["x"] }, { infer: async () => { throw new Error("could not reach aesthetic.computer"); } }), /could not reach/);
  assert.equal(existsSync(path.join(harness.dir, "runs")) ? (await import("node:fs")).readdirSync(path.join(harness.dir, "runs")).length : 0, 0);
});
