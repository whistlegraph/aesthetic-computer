#!/usr/bin/env node
// Deterministic stand-in for the future agent: plans four measures ahead from
// global time and current weather, then streams fixed plans to the C engine.

import { spawn } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENGINE = resolve(HERE, "c/measure-engine");
const secondsAt = process.argv.indexOf("--seconds");
const seconds = secondsAt >= 0 ? Number(process.argv[secondsAt + 1]) : 180;
const lat = 34.0522, lon = -118.2437;
const context = { cloud: 50, wind: 4, rain: 0, code: 0 };

try {
  const url = `https://api.open-meteo.com/v1/forecast?latitude=${lat}&longitude=${lon}&current=weather_code,wind_speed_10m,precipitation,cloud_cover&timezone=auto`;
  const data = await fetch(url, { signal: AbortSignal.timeout(3000) }).then((response) => response.json());
  context.cloud = data.current?.cloud_cover ?? context.cloud;
  context.wind = data.current?.wind_speed_10m ?? context.wind;
  context.rain = data.current?.precipitation ?? context.rain;
  context.code = data.current?.weather_code ?? context.code;
} catch {}

const child = spawn(ENGINE, ["--seconds", String(seconds)], { stdio: ["pipe", "inherit", "pipe"] });
const progression = [
  [52,55,59,62], [45,52,57,59,64], [48,52,55,59], [47,51,54,57],
];
const melody = [0,3,7,10,7,5,2,0, 3,7,12,10,7,9,5,3];
const scale = [0,2,3,5,7,9,10];
const globalSeed = Math.floor(Date.now() / 60000);
const tempo = 118 * (1 + Math.min(0.04, context.wind / 1000));
const density = context.cloud > 70 ? 0.6 : context.cloud < 25 ? 1 : 0.8;
const pulse = context.rain > 0 ? "aaaa" : "8888";
const planned = new Set();
const degreeMidi = (degree) => 64 + scale[((degree % 7) + 7) % 7] + Math.floor(degree / 7) * 12;
const hash = (bar) => ((globalSeed * 2654435761 + bar * 2246822519 + context.code) >>> 0).toString(16);

function submit(bar) {
  if (planned.has(bar)) return;
  planned.add(bar);
  const chord = progression[bar % progression.length];
  const slots = [0, 3, 6, 10, 13];
  const notes = slots.flatMap((slot, index) => {
    if (((bar * 7 + index * 11 + globalSeed) % 100) / 100 > density) return [];
    const degree = melody[(bar * 4 + index) % melody.length] + (bar % 8 === 7 ? 1 : 0);
    return [`${slot},${degreeMidi(degree)},${index === 4 ? 3 : 2},${88 + index * 5}`];
  }).join(";");
  child.stdin.write(`P|${bar}|${tempo.toFixed(2)}|${chord.join(",")}|${notes}|${pulse}|${hash(bar)}\n`);
}

for (let bar = 0; bar < 8; bar++) submit(bar);
child.stderr.setEncoding("utf8");
child.stderr.on("data", (chunk) => {
  process.stderr.write(chunk);
  for (const match of chunk.matchAll(/BAR (\d+)/g)) submit(Number(match[1]) + 4);
});
child.on("exit", (code) => process.exit(code ?? 0));
process.on("SIGINT", () => child.kill("SIGINT"));
console.error(`context Los Angeles · cloud=${context.cloud}% wind=${context.wind}km/h rain=${context.rain}mm · ${tempo.toFixed(2)} BPM`);
