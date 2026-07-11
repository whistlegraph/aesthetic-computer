// spin-pals.mjs — render a rotating turntable of a pals GLB (from mesh-pals.mjs)
// into transparent PNG frames + a preview mp4/gif, using three.js in headless
// Chromium (puppeteer). An IBL environment map gives the PBR metallic/rough
// materials real reflections — the "blinged out" look. The frames double as
// the source for an animated AC Electron menu-bar icon.
//
// Usage: node bin/spin-pals.mjs [slug] [--frames 60] [--size 512]
//   → ~/Desktop/pals-spin-<slug>/frame_###.png  + preview.mp4 + preview.gif
import puppeteer from "puppeteer";
import { mkdirSync, writeFileSync, existsSync } from "node:fs";
import { homedir } from "node:os";
import { resolve, dirname } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");
const argv = process.argv.slice(2);
const slug = argv.find((a) => !a.startsWith("--")) || "nat-amethyst";
const flag = (k, d) => { const i = argv.indexOf(`--${k}`); return i >= 0 ? argv[i + 1] : d; };
const FRAMES = parseInt(flag("frames", "60"), 10);
const SIZE = parseInt(flag("size", "512"), 10);

const glb = `${homedir()}/Desktop/pals-mesh-${slug}.glb`;
if (!existsSync(glb)) { console.error(`no GLB at ${glb} — run mesh-pals.mjs ${slug} first`); process.exit(1); }
const OUT = `${homedir()}/Desktop/pals-spin-${slug}`;
mkdirSync(OUT, { recursive: true });

const threeDir = resolve(REPO, "ac-electron", "node_modules", "three");
const threeURL = pathToFileURL(resolve(threeDir, "build", "three.module.js")).href;
const addonsURL = pathToFileURL(resolve(threeDir, "examples", "jsm")).href + "/";
const glbURL = pathToFileURL(glb).href;

const html = `<!doctype html><html><head><meta charset="utf-8">
<style>html,body{margin:0;background:transparent}#c{width:${SIZE}px;height:${SIZE}px}</style>
<script type="importmap">{"imports":{"three":"${threeURL}","three/addons/":"${addonsURL}"}}</script>
</head><body><canvas id="c" width="${SIZE}" height="${SIZE}"></canvas>
<script type="module">
import * as THREE from 'three';
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';
import { RoomEnvironment } from 'three/addons/environments/RoomEnvironment.js';

const canvas = document.getElementById('c');
const renderer = new THREE.WebGLRenderer({ canvas, alpha:true, antialias:true, preserveDrawingBuffer:true });
renderer.setPixelRatio(1); renderer.setSize(${SIZE}, ${SIZE}, false);
renderer.toneMapping = THREE.ACESFilmicToneMapping; renderer.toneMappingExposure = 1.7;

const scene = new THREE.Scene();
const pmrem = new THREE.PMREMGenerator(renderer);
scene.environment = pmrem.fromScene(new RoomEnvironment(), 0.04).texture; // IBL reflections
const key = new THREE.DirectionalLight(0xffffff, 3.4); key.position.set(2,3,4); scene.add(key);
const rim = new THREE.DirectionalLight(0xb488ff, 2.4); rim.position.set(-3,1,-2); scene.add(rim);
const fill = new THREE.DirectionalLight(0xffffff, 1.4); fill.position.set(0,-2,3); scene.add(fill);
scene.add(new THREE.AmbientLight(0xffffff, 0.55));

const camera = new THREE.PerspectiveCamera(30, 1, 0.01, 100);
const pivot = new THREE.Group(); scene.add(pivot);

window.__ready = false;
new GLTFLoader().load('${glbURL}', (gltf) => {
  const model = gltf.scene;
  // "blinged out": brighten the violet, add metallic sheen + strong env
  // reflections + a faint inner glow so the crystal pops on a small icon.
  model.traverse((o) => {
    if (!o.isMesh || !o.material) return;
    const mats = Array.isArray(o.material) ? o.material : [o.material];
    for (const m of mats) {
      if ('metalness' in m) m.metalness = 0.45;
      if ('roughness' in m) m.roughness = 0.12;
      if ('envMapIntensity' in m) m.envMapIntensity = 2.2;
      if (m.color) m.color.multiplyScalar(1.9);            // lift the dark purple
      if (m.emissive) { m.emissive.setHex(0x3a1170); m.emissiveIntensity = 0.35; }
    }
  });
  // center + scale to a unit sphere, then frame the camera with margin
  const box = new THREE.Box3().setFromObject(model);
  const size = box.getSize(new THREE.Vector3());
  const center = box.getCenter(new THREE.Vector3());
  model.position.sub(center);
  const maxDim = Math.max(size.x, size.y, size.z) || 1;
  const s = 2 / maxDim; model.scale.setScalar(s);
  pivot.add(model);
  // fit distance = radius/sin(fov/2); ×1.35 leaves generous margin so the
  // widest rotation angle never clips the arms.
  const dist = (1.0 / Math.sin((30 * Math.PI/180)/2)) * 1.35;
  camera.position.set(0, 0.15, dist); camera.lookAt(0,0,0);
  window.__ready = true;
}, undefined, (e) => { window.__err = String(e); window.__ready = 'err'; });

window.__frame = (angle) => {
  pivot.rotation.y = angle;
  renderer.render(scene, camera);
  return canvas.toDataURL('image/png');
};
</script></body></html>`;

const CHROME = [
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Chromium.app/Contents/MacOS/Chromium",
].find(existsSync);
const htmlPath = resolve(OUT, "_render.html");
writeFileSync(htmlPath, html);
const browser = await puppeteer.launch({ headless: "new", executablePath: CHROME, args: ["--no-sandbox", "--allow-file-access-from-files", "--use-gl=angle", "--use-angle=metal", "--enable-webgl", "--ignore-gpu-blocklist"] });
const page = await browser.newPage();
await page.setViewport({ width: SIZE, height: SIZE, deviceScaleFactor: 1 });
page.on("console", (m) => { if (m.type() === "error") console.log("  [page]", m.text()); });
await page.goto(pathToFileURL(htmlPath).href, { waitUntil: "load" });
// wait for model load
const ok = await page.waitForFunction("window.__ready === true || window.__ready === 'err'", { timeout: 60000 }).then(() => page.evaluate("window.__ready"));
if (ok !== true) { console.error("model load failed:", await page.evaluate("window.__err || 'unknown'")); await browser.close(); process.exit(1); }

console.log(`\nRendering ${FRAMES} frames @ ${SIZE}px from ${glb.split("/").pop()}:\n`);
for (let i = 0; i < FRAMES; i++) {
  const angle = (i / FRAMES) * Math.PI * 2;
  const dataUrl = await page.evaluate((a) => window.__frame(a), angle);
  const png = Buffer.from(dataUrl.split(",")[1], "base64");
  writeFileSync(resolve(OUT, `frame_${String(i).padStart(3, "0")}.png`), png);
  process.stdout.write(`\r  ${i + 1}/${FRAMES}`);
}
process.stdout.write("\n");
await browser.close();

// preview mp4 (transparent → over checkerboard-ish dark) + looping gif
const fps = Math.max(12, Math.round(FRAMES / 3));
try {
  execFileSync("ffmpeg", ["-y", "-framerate", String(fps), "-i", resolve(OUT, "frame_%03d.png"),
    "-c:v", "libx264", "-pix_fmt", "yuv420p", "-vf", "pad=ceil(iw/2)*2:ceil(ih/2)*2",
    resolve(OUT, "preview.mp4")], { stdio: "ignore" });
  execFileSync("ffmpeg", ["-y", "-framerate", String(fps), "-i", resolve(OUT, "frame_%03d.png"),
    "-vf", "scale=256:-1:flags=lanczos", resolve(OUT, "preview.gif")], { stdio: "ignore" });
  console.log(`✓ ${FRAMES} frames + preview.mp4 + preview.gif → ${OUT}`);
} catch (e) { console.log(`✓ ${FRAMES} frames → ${OUT} (ffmpeg preview skipped: ${e.message})`); }
