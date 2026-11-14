import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { inferDirect, DirectJobs, jobText } from "../src/direct-command.mjs";

const project = mkdtempSync(path.join(tmpdir(), "aesel-direct-"));
writeFileSync(path.join(project, "package.json"), JSON.stringify({ scripts: { start: "node server.js", dev: "vite", "build:web": "vite build" } }));
const bare = mkdtempSync(path.join(tmpdir(), "aesel-bare-"));
const run = (text, cwd = project) => inferDirect(text, cwd)?.command ?? null;

test("! runs the line as written", () => {
  assert.equal(run("!npm start"), "npm start");
  assert.equal(run("! ls -la"), "ls -la");
  assert.equal(run("!"), null);
});

test("package-manager lines, and the slips a thumb makes", () => {
  assert.equal(run("npm start"), "npm start");
  assert.equal(run("rpm start"), "npm start");
  assert.match(inferDirect("rpm start", project).note, /rpm → npm/);
  assert.equal(run("npm run dev"), "npm run dev");
  assert.equal(run("npm install three"), "npm install three");
  assert.equal(run("yarn dev"), "yarn dev");
  assert.equal(run("npm frobnicate"), null);
});

test("plain words find the script", () => {
  assert.equal(run("start the server"), "npm start");
  assert.equal(run("run it"), "npm start");
  assert.equal(run("run dev"), "npm run dev");
  assert.equal(run("build:web"), "npm run build:web");
  assert.equal(run("start the server", bare), null);
});

test("programs run, sentences do not", () => {
  assert.equal(run("git status"), "git status");
  assert.equal(run("node server.js"), "node server.js");
  assert.equal(run("ls"), "ls");
  for (const sentence of [
    "make a snake game", "make it bounce", "go faster", "open the door",
    "git gud", "start the server and make the ball red", "how do I npm start?",
    "node", "start a new piece about frogs", "kill the bass",
  ]) assert.equal(run(sentence), null, sentence);
});

test("multi-line text is always a message", () => {
  assert.equal(run("npm start\nthen fix the error"), null);
});

test("a job streams its tail and a failure reaches the next turn once", async () => {
  const updates = [];
  const jobs = new DirectJobs({ cwd: project, shell: "/bin/sh", onUpdate: job => updates.push(job.code) });
  const ok = jobs.run("echo hello; echo world");
  const bad = jobs.run("echo boom >&2; exit 3");
  await new Promise(resolve => { const check = () => (ok.code !== null && bad.code !== null ? resolve() : setTimeout(check, 20)); check(); });
  assert.equal(ok.code, 0);
  assert.deepEqual(ok.lines, ["hello", "world"]);
  assert.match(jobText(ok), /#1 done/);
  assert.match(jobText(bad), /exit 3/);
  const context = jobs.takeContext();
  assert.match(context, /exit 3/);
  assert.match(context, /boom/);
  assert.doesNotMatch(context, /hello/);
  assert.equal(jobs.takeContext(), "");
});

test("kill stops a long job and is not reported as a failure", async () => {
  const jobs = new DirectJobs({ cwd: project, shell: "/bin/sh", onUpdate() {} });
  const job = jobs.run("sleep 30");
  assert.equal(jobs.running().length, 1);
  assert.equal(jobs.kill(String(job.id)).length, 1);
  await new Promise(resolve => { const check = () => (job.code !== null ? resolve() : setTimeout(check, 20)); check(); });
  assert.match(jobText(job), /stopped/);
  assert.equal(jobs.takeContext(), "");
});
