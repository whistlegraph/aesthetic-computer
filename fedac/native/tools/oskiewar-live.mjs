#!/usr/bin/env node
// Bundle the shared game and hot-upload it to an AC Native LAN server.
import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";
import { watchFile } from "node:fs";

const native = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const root = path.resolve(native, "../..");
const inputs = [path.join(root, "xbox/live/oskiewar.js"),
  path.join(native, "lib/oskiewar-host.mjs"), path.join(native, "pieces/oskiewar.mjs"),
  path.join(root, "xbox/live/lan-test.js")];
export async function bundle() {
  const source = await fs.readFile(inputs[0], "utf8");
  const sourceHash = createHash("sha256").update(source).digest("hex");
  const lanTest = await fs.readFile(inputs[3], "utf8");
  const qr = (await fs.readFile(path.join(root,
    "system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs"), "utf8"))
    .replace(/\r?\nexport\s*\{[\s\S]*?\};\s*$/, "\n");
  const names = "runtime, gamepad, controllers, capabilities, telemetry, gameSignal, drum, synth, wipe, box, line, triangle, triangle3d, disc3d, themeReady, themeAssetReady, themeSprite, themeQuad, accountState, accountAction, accountReport, write, systemWrite, gameView";
  const game = `// Generated from xbox/live/oskiewar.js; do not edit.\nexport const sourceHash = ${JSON.stringify(sourceHash)};\nexport function createOskiewar(host) {\nlet { ${names} } = host;\n${qr}\n${source}\n${lanTest}\nreturn { boot, sim, paint, act, leave, enterGame: () => enterGame(runtime().monotonicUs), state: () => ({ version: buildVersion, debug: debugHitboxes, graphicsTheme: globalThis.__oskiewarGraphicsThemeStatus || "flat", account: globalThis.__oskiewarDeviceAccount || null, menu:globalThis.__oskiewarDeviceMenu || null, error: clientError ? String(clientError.message || clientError) : null, players: players.map(p => ({ name: p.name, x: p.x, y: p.y, alive: p.alive })), map: currentMapName, mode: shellMode, roundResult, matchOver, roundElapsedUs, finalKillReplay: globalThis.__oskiewarFinalKillReplay || null, net: globalThis.__oskiewarNetStats || null, connection: globalThis.__oskiewarLanStatus || null, mismatch: globalThis.__oskiewarLanMismatch || null }) };\n}\n`;
  // Native reloads the entry module, but keeps imported modules cached.
  // Ship one atomic entry so every reload receives the new game AND adapter.
  const host = await fs.readFile(inputs[1], "utf8");
  const entry = (await fs.readFile(inputs[2], "utf8")).replace(/^import .*;\n/gm, "");
  return { sourceHash, files: [["pieces/oskiewar.mjs", `${host}\n${game}\n${entry}`]] };
}
async function request(url, options = {}) {
  const response = await fetch(url, { ...options, signal: AbortSignal.timeout(15000) });
  if (!response.ok) throw new Error(`${options.method || "GET"} ${url}: ${response.status}`);
  return response;
}
async function deploy(base) {
  const status = await (await request(base + "/status")).json();
  if (!status.build || !status.piece) throw new Error("Target is not an AC Native LAN server");
  const build = await bundle();
  for (const [name, contents] of build.files) {
    await request(`${base}/${name}`, { method: "PUT", body: contents });
    const saved = await (await request(`${base}/${name}`)).text();
    if (saved !== contents) throw new Error(`Readback mismatch: ${name}`);
  }
  await request(base + "/jump/oskiewar", { method: "POST" });
  console.log(`Uploaded to ${status.name}: ${build.sourceHash.slice(0, 12)}; reload requested.`);
}
async function main() {
  const args = process.argv.slice(2);
  if (args[0] === "--build") {
    const output = path.resolve(args[1] || path.join(native, "build/oskiewar-live"));
    const build = await bundle();
    for (const [name, contents] of build.files) {
      const target = path.join(output, name);
      await fs.mkdir(path.dirname(target), { recursive: true });
      await fs.writeFile(target, contents);
    }
    console.log(output); return;
  }
  const address = args.find(a => !a.startsWith("--"));
  if (!address) throw new Error("Usage: oskiewar-live.mjs <host-or-url> [--watch] | --build [directory]");
  const base = (address.includes("://") ? address : `http://${address}`).replace(/\/$/, "");
  await deploy(base);
  if (args.includes("--watch")) {
    let queued = false, running = false, timer;
    const changed = () => {
      queued = true; clearTimeout(timer);
      timer = setTimeout(async () => {
        if (running) return;
        running = true;
        try { while (queued) { queued = false; await deploy(base); } }
        catch (error) { console.error(error.message); }
        finally { running = false; }
      }, 300);
    };
    for (const input of inputs) watchFile(input, { interval: 500 }, changed);
    console.log("Watching shared game, native adapter, and piece. Ctrl-C stops.");
  }
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url))
  main().catch(error => { console.error(error.message); process.exitCode = 1; });
