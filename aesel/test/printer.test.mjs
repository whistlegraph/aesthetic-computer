import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { existsSync, mkdtempSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import test from "node:test";
import { AESEL_HOME, printer, printerOutput } from "../src/printer.mjs";
import { main, readHarness } from "../src/libaesel.mjs";

const scratch = () => mkdtempSync(path.join(tmpdir(), "printer-test-"));
const SVG = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><rect width="10" height="10" fill="navy"/></svg>';
const answer = (body) => ({ text: "```svg\n" + body + "\n```\n", usage: { output_tokens: 10 } });

test("a seed file names its own output goal", () => {
  assert.equal(printerOutput("piece.mjs"), "text/javascript");
  assert.equal(printerOutput("a/b/wave.lisp"), "text/x-lisp");
  assert.equal(printerOutput("notes.pdf"), undefined);
  assert.ok(existsSync(path.join(AESEL_HOME, "src", "libaesel.mjs")));
});

test("making a printer from a file writes a harness, its seed and a launcher", () => {
  const cwd = scratch();
  writeFileSync(path.join(cwd, "liked.svg"), SVG);
  const made = printer("Gig Poster", { source: "liked.svg", asks: ["a swiss poster", "  ", "make the number huge"], cwd });
  assert.equal(made.harness.name, "gig-poster");
  assert.equal(made.harness.output, "image/svg+xml");
  assert.equal(readFileSync(made.seed, "utf8"), SVG);
  assert.match(made.harness.prompt, /- a swiss poster\n- make the number huge/);
  assert.match(made.harness.prompt, /```svg\n<svg/);
  assert.ok(statSync(made.launcher).mode & 0o100, "launcher is executable");
  assert.equal(JSON.parse(readFileSync(path.join(made.harness.dir, "harness.json"), "utf8")).printer.seed, "seed.svg");
  assert.throws(() => printer("Gig Poster", { source: "liked.svg", cwd }), /already has a harness/);
});

test("the Printer button names a fresh folder after the seed and preserves earlier work", () => {
  const cwd = scratch();
  writeFileSync(path.join(cwd, "roz.lisp"), "(zoom 1.1)");
  writeFileSync(path.join(cwd, "roz"), "existing file");
  const first = printer(null, { source: "roz.lisp", cwd });
  writeFileSync(path.join(cwd, "roz.lisp"), "(zoom 1.2)");
  const second = printer(null, { source: "roz.lisp", cwd });
  assert.equal(path.basename(first.harness.dir), "roz-2");
  assert.equal(path.basename(second.harness.dir), "roz-3");
  assert.equal(readFileSync(first.seed, "utf8"), "(zoom 1.1)");
  assert.equal(readFileSync(second.seed, "utf8"), "(zoom 1.2)");
  assert.equal(readFileSync(path.join(cwd, "roz"), "utf8"), "existing file");
});

test("a printer needs to know what it makes", () => {
  const cwd = scratch();
  writeFileSync(path.join(cwd, "notes.pdf"), "x");
  assert.throws(() => printer("a", { cwd }), /name what it makes/);
  assert.throws(() => printer("b", { source: "notes.pdf", cwd }), /add --out/);
  assert.throws(() => printer("c", { source: "missing.svg", cwd }), /no such file/);
  const made = printer("d", { output: "wav", cwd });
  assert.equal(made.harness.output, "audio/wav");
  assert.equal(made.seed, "");
});

test("the launcher runs standalone on libaesel", () => {
  const cwd = scratch();
  writeFileSync(path.join(cwd, "liked.svg"), SVG);
  const made = printer("poster", { source: "liked.svg", asks: ["a poster"], cwd });
  const elsewhere = scratch();
  const dry = execFileSync(process.execPath, [made.launcher, "--dry", "noise night"], { cwd: elsewhere, encoding: "utf8", env: { ...process.env, AESEL_HOME: "" } });
  assert.match(dry, /dry run, nothing spent/);
  assert.match(dry, /Output goal: image\/svg\+xml/);
  assert.match(dry, /noise night/);
  assert.match(execFileSync(process.execPath, [made.launcher, "--help"], { encoding: "utf8" }), /--post/);
});

test("main runs the harness with the services it is given", async () => {
  const cwd = scratch();
  writeFileSync(path.join(cwd, "liked.svg"), SVG);
  const made = printer("poster", { source: "liked.svg", cwd });
  const lines = [];
  const services = { session: { signedIn: true }, infer: async () => answer(SVG), paint: async () => { throw new Error("unused"); } };
  const run = await main(made.harness.dir, ["noise night"], { out: (line) => lines.push(line), note: () => {}, services });
  assert.equal(readFileSync(run.file, "utf8").trim(), SVG);
  assert.equal(readHarness(made.harness.dir).name, "poster");
  assert.match(lines[0], /output\.svg$/);
  await assert.rejects(main(made.harness.dir, ["x"], { out: () => {}, note: () => {}, services: { ...services, session: { signedIn: false } } }), /sign in first/);
});
