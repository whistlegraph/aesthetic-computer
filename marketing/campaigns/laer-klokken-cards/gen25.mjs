// gen25.mjs — one-off driver for gpt-image-2.5 while the illy MCP registry is stale.
// usage: node gen25.mjs <model> <prompt.txt> <out.png> [quality]
import { readFileSync, writeFileSync } from "node:fs";
const [model, promptFile, out, quality = "medium"] = process.argv.slice(2);
const env = readFileSync("/Users/jas/aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env", "utf8");
const key = env.split("\n").find((l) => l.startsWith("OPENAI_API_KEY="))?.slice(15).replace(/^["']|["']$/g, "");
const contracts = JSON.parse(readFileSync("/Users/jas/aesthetic-computer/plugins/illy/config/contracts.json", "utf8")).contracts;
const prompt = readFileSync(promptFile, "utf8").trim() + "\n\n" + contracts["physical-accuracy"].prompt.join("\n");
const t0 = Date.now();
const res = await fetch("https://api.openai.com/v1/images/generations", {
  method: "POST", headers: { Authorization: `Bearer ${key}`, "Content-Type": "application/json" },
  body: JSON.stringify({ model, prompt, size: "1024x1024", quality, n: 1 }),
});
const j = await res.json();
if (!res.ok || !j.data?.[0]?.b64_json) { console.error(out, "FAILED", res.status, JSON.stringify(j).slice(0, 300)); process.exit(1); }
writeFileSync(out, Buffer.from(j.data[0].b64_json, "base64"));
console.log(out, model, `${((Date.now() - t0) / 1000).toFixed(1)}s`, j.usage ? JSON.stringify(j.usage.output_tokens) : "");
