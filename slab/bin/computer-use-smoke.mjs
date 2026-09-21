#!/usr/bin/env node
// A disposable website/profile/daemon. Only --native sends macOS input, and
// only after Frame identifies the fixture window and its observed button.
import assert from "node:assert/strict";
import { createServer } from "node:http";
import { mkdtemp, readFile, writeFile, rm, access } from "node:fs/promises";
import { join, resolve } from "node:path";
import { tmpdir } from "node:os";
import { spawn } from "node:child_process";
import { setTimeout as delay } from "node:timers/promises";
import { chromium } from "playwright-core";
import { createComputerUseClient } from "../lib/computer-use-client.mjs";
import { chooseObservedTarget } from "../lib/jev-computer-use.mjs";

const native = process.argv.includes("--native"), jev = process.argv.includes("--jev");
const root = resolve(import.meta.dirname, "../..");
const dir = await mkdtemp(join(tmpdir(), "computer-use-smoke-"));
const report = { at: new Date().toISOString(), browser: [], native: [], jev: [] };
const children = [];
let context, site;
const text = r => (r.content || []).filter(c => c.type === "text").map(c => c.text).join("\n");
async function listen(server) { await new Promise(resolve => server.listen(0, "127.0.0.1", resolve)); return server.address().port; }
async function freePort() { const s = createServer(); const p = await listen(s); await new Promise(resolve => s.close(resolve)); return p; }
async function readyMCP(port) {
  return (await fetch(`http://127.0.0.1:${port}/mcp`, { method: "POST", headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ jsonrpc: "2.0", id: 0, method: "ping" }), signal: AbortSignal.timeout(1000) })).ok;
}
function start(script, args, env) {
  const child = spawn(process.execPath, [join(root, script), ...args], { env, stdio: ["ignore", "pipe", "pipe"] });
  child.errors = "";
  child.stdout.on("data", () => {});
  child.stderr.on("data", chunk => { child.errors = (child.errors + chunk).slice(-2000); });
  children.push(child); return child;
}
async function waitReady(check) {
  let last;
  for (let i = 0; i < 60; i++) {
    try { if (await check()) return; } catch (error) { last = error; }
    if (children.some(c => c.exitCode !== null)) throw new Error(children.map(c => c.errors).join("\n"));
    await delay(50);
  }
  throw last || new Error("Fixture startup timed out");
}
try {
  const html = await readFile(join(root, "slab/test/fixtures/computer-use.html"));
  site = createServer((req, res) => {
    res.setHeader("Content-Type", "text/html; charset=utf-8");
    res.end(req.url === "/second" ? '<!doctype html><title>Second fixture page</title><h1>Navigation verified</h1>' : html);
  });
  const sitePort = await listen(site);
  context = await chromium.launchPersistentContext(join(dir, "profile"), {
    channel: "chrome", headless: !native, viewport: native ? null : { width: 1000, height: 850 },
    args: ["--remote-debugging-port=0", "--window-size=1000,850", "--force-renderer-accessibility"],
  });
  const browserPort = (await readFile(join(dir, "profile/DevToolsActivePort"), "utf8")).split("\n")[0];
  const page = context.pages()[0];
  await page.goto(`http://127.0.0.1:${sitePort}/`);
  const cdp = await context.newCDPSession(page);
  const { targetInfo } = await cdp.send("Target.getTargetInfo");
  await cdp.detach();
  const target = targetInfo.targetId;
  const config = join(dir, "machines.json"), sock = join(dir, "puppet.sock");
  await writeFile(config, JSON.stringify({ machines: { fixture: { local: true, cdpUrl: `http://127.0.0.1:${browserPort}` } } }));
  const env = { ...process.env, SLAB_PUPPET_CONFIG: config, SLAB_PUPPET_SOCK: sock,
    SLAB_INPUT_LEASE_DIR: process.env.SLAB_INPUT_LEASE_DIR || join(process.env.HOME, ".local/share/slab/input-leases") };
  // The test daemon's status file belongs to the fixture; native input leases
  // retain the real user's root so another controller still excludes us.
  start("slab/bin/puppet.mjs", ["daemon"], { ...env, HOME: dir });
  await waitReady(async () => { await access(sock); return true; });
  const puppetPort = await freePort(), framePort = await freePort();
  start("slab/bin/puppet-mcp.mjs", ["--http", String(puppetPort)], env);
  if (native) start("slab/bin/frame-mcp.mjs", ["--http", String(framePort)], env);
  const servers = { puppet: `http://127.0.0.1:${puppetPort}/mcp`, ...(native ? { frame: `http://127.0.0.1:${framePort}/mcp` } : {}) };
  const tools = ["puppet_list", "puppet_snapshot", "puppet_click", "puppet_fill", "puppet_wait", "frame", "frame_click", "frame_reframe"];
  await waitReady(() => readyMCP(puppetPort));
  if (native) await waitReady(() => readyMCP(framePort));
  const client = createComputerUseClient({ servers, allowedTools: tools });
  await client.discover();
  await waitReady(async () => JSON.parse(text(await client.call("puppet_list", { full: true }))).fixture.connected);
  async function call(name, args, lane = "browser") {
    const start = performance.now();
    const result = await client.call(name, args);
    assert.ok(!result.isError, text(result));
    report[lane].push({ tool: name, ...(args.locator ? { control: args.locator.name || args.locator.label } : {}), ms: Math.round(performance.now() - start) });
    return result;
  }
  const browser = { machine: "fixture", target };
  const snap = JSON.parse(text(await call("puppet_snapshot", browser)));
  assert.match(snap.tree, /Add one/);
  async function click(name, expected) {
    const result = JSON.parse(text(await call("puppet_click", { ...browser, locator: { role: "button", name }, after: { locator: { text: expected } } })));
    assert.equal(result.performed, true); assert.equal(result.verification.ok, true);
  }
  await click("Add one", "Count: 1");
  await call("puppet_fill", { ...browser, locator: { label: "Name" }, value: "Fixture" });
  await click("Save name", "Saved: Fixture");
  await click("Open menu", "Choose blue");
  await click("Choose blue", "Selected: blue");
  await click("Start delayed update", "Ready");

  if (jev) {
    if (!process.env.OPENROUTER_API_KEY) report.jev.push({ skipped: "OPENROUTER_API_KEY unavailable" });
    else for (let i = 0; i < 3; i++) {
      const start = performance.now();
      const candidates = await page.locator("button:visible").evaluateAll(buttons => buttons.map((b, i) => ({ id: `button_${i}`, label: b.textContent, role: "button", visible: true, disabled: b.disabled })));
      const decision = await chooseObservedTarget({ goal: "Increase the counter by one", observation: { id: crypto.randomUUID(), target, capturedAt: new Date().toISOString() }, candidates });
      const sample = { ms: Math.round(performance.now() - start), action: decision.action, label: decision.candidate?.label, probability: decision.probability, reason: decision.reason };
      report.jev.push(sample);
      if (decision.action === "target") assert.equal(decision.candidate.label, "Add one");
    }
  }

  if (native) {
    await page.bringToFront();
    const observed = await call("frame", { machine: "local", fast: true, visual: false }, "native");
    const digest = text(observed);
    assert.match(digest, /capture: ok/);
    assert.match(digest, /frontmost: .*Chrome/);
    assert.match(digest, /Computer-use check/);
    const observation = JSON.parse(digest.match(/^observation: (.+)$/m)[1]);
    const match = digest.match(/AXButton «Add one» @\((\d+),(\d+)\)/) || digest.match(/«Add one» @\((\d+),(\d+)\)/);
    assert.ok(match, "Frame must observe the exact fixture control before native input");
    await call("frame_click", { machine: "local", observationId: observation.id, x: Number(match[1]), y: Number(match[2]), fast: true, visual: false }, "native");
    await page.getByText("Count: 2", { exact: true }).waitFor({ timeout: 3000 });
    report.native.push({ verified: "native click incremented counter exactly once" });
    const reframe = await call("frame_reframe", { machine: "local", fast: true, visual: false }, "native");
    assert.ok(reframe.content.length > 0);
  }
  // Only the generated fixture is retained; the browser profile is removed.
  const artifact = join(tmpdir(), "computer-use-smoke-verified.png");
  await page.screenshot({ path: artifact }); report.screenshot = artifact;
  const navigation = JSON.parse(text(await call("puppet_click", { ...browser, locator: { role: "link", name: "Second page" }, after: { locator: { text: "Navigation verified" } } })));
  assert.equal(navigation.verification.ok, true);
  assert.equal(new URL(page.url()).pathname, "/second");
  report.ok = true;
} catch (error) {
  report.ok = false; report.error = error.message; process.exitCode = 1;
} finally {
  for (const child of children) child.kill();
  await context?.close();
  if (site) await new Promise(resolve => site.close(resolve));
  await rm(dir, { recursive: true, force: true });
  await writeFile(join(tmpdir(), "computer-use-smoke-report.json"), JSON.stringify(report, null, 2));
  console.log(JSON.stringify(report, null, 2));
}
