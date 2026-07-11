// mesh-pals.mjs — turn a still "pals" logo tile into a low-poly, blinged-out
// 3D model with Meshy 6 (image-to-3D) on fal. Aimed at a rotating menu-bar
// icon: lowpoly + quad topology + PBR (metallic/roughness/normal) = crisp
// facets that catch light as it spins.
//
// Usage: node bin/mesh-pals.mjs [slug]   (default: nat-amethyst)
//   → ~/Desktop/pals-mesh-<slug>.glb  (+ .thumb.png from Meshy's render)
import { falKey, dataUri } from "../../../pop/lib/fal.mjs";
import { writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const slug = process.argv[2] || "nat-amethyst";
const still = resolve(HERE, "..", "out", "pals", `${slug}.png`);
const OUT = `${homedir()}/Desktop/pals-mesh-${slug}.glb`;

const KEY = falKey();
const auth = { Authorization: `Key ${KEY}`, "Content-Type": "application/json" };
const ENDPOINT = "fal-ai/meshy/v6/image-to-3d";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const input = {
  image_url: dataUri(still),
  model_type: "lowpoly",          // clean low-poly for the tray
  topology: "quad",               // smooth faceting
  target_polycount: 6000,         // chunky faceted bling, small icon anyway
  should_texture: true,
  enable_pbr: true,               // metallic/roughness/normal = the sparkle
  texture_prompt:
    "polished faceted amethyst crystal gemstone, deep violet, glossy, " +
    "reflective, blinged out, jewel-like",
  symmetry_mode: "auto",
};

console.log(`\nMeshy 6 lowpoly · ${slug} → ${OUT.split("/").pop()}\n`);
const sub = await fetch(`https://queue.fal.run/${ENDPOINT}`, {
  method: "POST", headers: auth, body: JSON.stringify(input),
});
if (!sub.ok) { console.error("submit failed:", sub.status, (await sub.text()).slice(0, 300)); process.exit(1); }
const q = await sub.json();
const t0 = Date.now();
let status = "";
while (status !== "COMPLETED") {
  await sleep(4000);
  const r = await fetch(q.status_url, { headers: auth });
  const b = await r.json();
  if (b.status !== status) { status = b.status; console.log(`  ${status.toLowerCase()} · ${((Date.now() - t0) / 1000).toFixed(0)}s`); }
  if (status === "FAILED" || b.error) { console.error("generation failed:", JSON.stringify(b).slice(0, 300)); process.exit(1); }
}
const result = await (await fetch(q.response_url, { headers: auth })).json();
const glbUrl = result.model_glb?.url || result.model_urls?.glb?.url;
if (!glbUrl) { console.error("no glb in response:", JSON.stringify(result).slice(0, 400)); process.exit(1); }
const glb = Buffer.from(await (await fetch(glbUrl)).arrayBuffer());
writeFileSync(OUT, glb);
console.log(`\n✓ GLB (${(glb.length / 1024).toFixed(0)} KB) → ${OUT}`);
if (result.thumbnail?.url) {
  const thumb = Buffer.from(await (await fetch(result.thumbnail.url)).arrayBuffer());
  writeFileSync(OUT.replace(/\.glb$/, ".thumb.png"), thumb);
  console.log(`  thumbnail → ${OUT.replace(/\.glb$/, ".thumb.png")}`);
}
