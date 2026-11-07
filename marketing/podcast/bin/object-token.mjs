#!/usr/bin/env node
// object-token.mjs — one of @jeffrey's dool-built KidLisp objects, minted as
// a 1/1 on Teia (the hic et nunc objkts contract), signed by aesthetic.tez.
//
//   source  — the object's saved $code, checked to be @jeffrey's and 3D
//   ground  — a background colour drawn from the object's dominant ink,
//             kept distinct from the grounds minted before it; the object
//             is stored again under @jeffrey with `; #bg` and `; #door`
//             pragmas, so the token's $code renders the same everywhere
//   render  — a turntable GIF and a still, drawn here with the runtime's own
//             object viewer (no browser, no oven)
//   pack    — the $code packed by the oven's bundler (PACK mode) with the
//             doorway: a tap opens aesthetic.computer with the room up
//   pin     — index.html + covers + source as an IPFS directory, plus GIF,
//             thumbnail and TZIP-21 metadata (/api/ipfs-add)
//   mint    — mint_OBJKT on the hic et nunc minter
//   list    — a Teia Community Marketplace swap for the edition
//
// Every stage writes into out/objects/<code>.token.json as it lands, so a
// failed run resumes where it stopped and a finished one never double-mints.
//
// Usage:
//   node bin/object-token.mjs --code 62i                 # mint the pistol
//   node bin/object-token.mjs --code 62i --dry           # render, pack, metadata; nothing stored, pinned or minted
//   node bin/object-token.mjs --code 62i --door https://aesthetic.computer/?dool
//
// Secrets come from the environment or --env <file> (repeatable):
//   AESTHETIC_KEY, AESTHETIC_ADDRESS   the signer (must be aesthetic.tez)
//   AC_TOKEN or ~/.ac-token            an @jeffrey AC session, for store-kidlisp and /api/ipfs-add
// Tuning: OBJECT_PRICE_XTZ (5), OBJECT_ROYALTIES_PERMILLE (150), OBJECT_DOOR.

import { writeFileSync, mkdirSync, existsSync, readFileSync, readdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createRequire } from "node:module";
import { freshSession } from "../../../shared/ac-token.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..", "..");

const argv = process.argv.slice(2);
const flags = { env: [] };
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (!a.startsWith("--")) continue;
  const k = a.slice(2), nx = argv[i + 1];
  const v = nx !== undefined && !nx.startsWith("--") ? (i++, nx) : true;
  if (k === "env") flags.env.push(v); else flags[k] = v;
}
for (const file of flags.env) {
  for (const line of readFileSync(file, "utf8").split("\n")) {
    const m = line.match(/^\s*(?:export\s+)?([A-Z0-9_]+)\s*=\s*"?([^"\n]*)"?\s*$/);
    if (m && process.env[m[1]] === undefined) process.env[m[1]] = m[2];
  }
}

const code = typeof flags.code === "string" ? flags.code.replace(/^\$/, "") : null;
if (!code || !/^[A-Za-z0-9]{3,64}$/.test(code)) { console.error("✗ --code <$code> of a saved object is required"); process.exit(1); }

const SIGNER = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"; // aesthetic.tez
const HEN_MINTER = "KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9";
const HEN_OBJKTS = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
const TEIA_MARKET = "KT1PHubm9HtyQEJ4BBpMTVomq6mhbfNZ9z5w"; // Teia Community Marketplace
const RPC = process.env.TEZOS_RPC || "https://rpc.tzkt.io/mainnet";
const TZKT = "https://api.tzkt.io/v1";
const AC = process.env.AC_URL || "https://aesthetic.computer";
const HANDLE = "jeffrey";

const EDITIONS = 1;
const PRICE_XTZ = Number(flags.price || process.env.OBJECT_PRICE_XTZ || 5);
const ROYALTIES = Number(process.env.OBJECT_ROYALTIES_PERMILLE || 150); // HEN and Teia are per-mille
const DOOR = typeof flags.door === "string" ? flags.door : (process.env.OBJECT_DOOR || "https://aesthetic.computer/?dool");
const MIN_BALANCE_XTZ = 0.15;
const SIZE = 640, FPS = 8, SECONDS = 8;
const OBJECT_STYLE = "#aesthetic-computer { position: relative; overflow: hidden; }";

if (!/^https:\/\/\S+$/.test(DOOR)) { console.error(`✗ --door must be an https URL, not ${DOOR}`); process.exit(1); }

// The AC session: AC_TOKEN if given, else this machine's own ~/.ac-token.
async function acToken() {
  if (process.env.AC_TOKEN) return process.env.AC_TOKEN;
  const file = resolve(process.env.HOME, ".ac-token");
  if (!existsSync(file)) return null;
  const record = JSON.parse(readFileSync(file, "utf8"));
  if (!record.expires_at || Date.now() < record.expires_at - 60_000) return record.access_token;
  if (!record.refresh_token) return null;
  return (await freshSession({ file })).access_token;
}
const AC_TOKEN = flags.dry ? null : await acToken();

const outDir = resolve(typeof flags["out-dir"] === "string" ? flags["out-dir"] : resolve(ROOT, "out"), "objects");
mkdirSync(outDir, { recursive: true });
const receiptPath = resolve(outDir, `${code}.token.json`);
const receipt = existsSync(receiptPath) ? JSON.parse(readFileSync(receiptPath, "utf8")) : { originalCode: code };
if (receipt.dryRun && !flags.dry) {
  if (receipt.metadataUri || receipt.mintOp || receipt.tokenId !== undefined) throw new Error("Dry receipt unexpectedly contains a published operation");
  for (const key of ["code", "source", "dryRun"]) delete receipt[key];
}
const save = () => writeFileSync(receiptPath, JSON.stringify(receipt, null, 2) + "\n");
if (receipt.listed) { console.log(`✓ $${code} already minted and listed: ${receipt.teiaUrl}`); process.exit(0); }

// ── 0. the object ────────────────────────────────────────────────────────
// The runtime's own viewer decides what counts as an object and compiles it;
// the same code draws it on aesthetic.computer, in the pack, and here.
const viewerPath = resolve(REPO, "system/public/aesthetic.computer/lib/object/viewer.mjs");
const { objectPiece, isObjectLisp, sourceBackground, sourceDoor } = await import(viewerPath);
const { parseSource, sourceFrame } = await import(resolve(REPO, "system/public/aesthetic.computer/lib/object/object-language.mjs"));

async function fetchObject(code) {
  // The handle feed is the authority on authorship; the single-code read fills in older pieces.
  const feed = await (await fetch(`${AC}/api/store-kidlisp?recent=1&handle=${HANDLE}&limit=300&sort=recent`)).json().catch(() => ({}));
  let row = (feed.recent || []).find((p) => p.code === code);
  if (!row) {
    const r = await fetch(`${AC}/api/store-kidlisp?code=${code}`);
    const j = await r.json().catch(() => ({}));
    if (!r.ok || !j.source) throw new Error(`store-kidlisp ${r.status}: ${JSON.stringify(j).slice(0, 200)}`);
    if ((j.handle || "").replace(/^@/, "") !== HANDLE) throw new Error(`$${code} is not @${HANDLE}'s (handle ${j.handle || "unknown"})`);
    row = j;
  }
  return row;
}

if (!receipt.source) {
  const row = await fetchObject(code);
  if (!isObjectLisp(row.source)) { console.error(`✗ $${code} is not a 3D object`); process.exit(1); }
  if (sourceBackground(row.source) || sourceDoor(row.source)) { console.error(`✗ $${code} already carries #bg/#door pragmas; mint from the original build version`); process.exit(1); }
  const name = (row.source.match(/^\s*;\s*(?![ \t]*#)([^\n]+)/) || [])[1]?.trim();
  if (!name) { console.error(`✗ $${code} has no name line (first comment)`); process.exit(1); }
  Object.assign(receipt, { originalSource: row.source, name, built: (row.when || new Date().toISOString()).slice(0, 10) });
  save();
}
const title = receipt.name.replace(/\b\w/g, (c) => c.toUpperCase());
const shapeCount = (receipt.originalSource.match(/\((?:slab|orb|capsule)\s/g) || []).length;
console.log(`▸ $${code}: "${title}" — ${shapeCount} shapes, built ${receipt.built}`);

// ── 1. ground ────────────────────────────────────────────────────────────
// The dominant ink by drawn area becomes the ground: same hue, nudged
// warmer, muted, deep. Grey inks sit on slate. A ground already used by an
// earlier object in this series turns a few degrees so the row stays varied.
function dominantInk(source) {
  const v = sourceFrame(parseSource(source), 0).vertices;
  const area = new Map();
  for (let i = 0; i < v.length; i += 18) {
    const ax = v[i + 6] - v[i], ay = v[i + 7] - v[i + 1], az = v[i + 8] - v[i + 2];
    const bx = v[i + 12] - v[i], by = v[i + 13] - v[i + 1], bz = v[i + 14] - v[i + 2];
    const a = Math.hypot(ay * bz - az * by, az * bx - ax * bz, ax * by - ay * bx) / 2;
    const key = [v[i + 3], v[i + 4], v[i + 5]].map((c) => Math.round(c * 255)).join(",");
    area.set(key, (area.get(key) || 0) + a);
  }
  const [top] = [...area.entries()].sort((a, b) => b[1] - a[1]);
  return top ? top[0].split(",").map(Number) : [128, 128, 128];
}
const rgb2hsl = (r, g, b) => {
  r /= 255; g /= 255; b /= 255;
  const M = Math.max(r, g, b), m = Math.min(r, g, b), l = (M + m) / 2;
  if (M === m) return [0, 0, l];
  const d = M - m, s = l > 0.5 ? d / (2 - M - m) : d / (M + m);
  const h = M === r ? (g - b) / d + (g < b ? 6 : 0) : M === g ? (b - r) / d + 2 : (r - g) / d + 4;
  return [h / 6, s, l];
};
const hsl2rgb = (h, s, l) => {
  const f = (p, q, t) => { t = (t + 1) % 1; return t < 1 / 6 ? p + (q - p) * 6 * t : t < 0.5 ? q : t < 2 / 3 ? p + (q - p) * (2 / 3 - t) * 6 : p; };
  if (!s) return [l, l, l].map((v) => Math.round(v * 255));
  const q = l < 0.5 ? l * (1 + s) : l + s - l * s, p = 2 * l - q;
  return [f(p, q, h + 1 / 3), f(p, q, h), f(p, q, h - 1 / 3)].map((v) => Math.round(v * 255));
};
function ground(ink, taken) {
  let [h, s] = rgb2hsl(...ink);
  if (s < 0.12) { h = 0.62; s = 0.22; } else { h = (h + 0.055) % 1; s = Math.min(0.38, s * 0.6); }
  for (let turn = 0; turn < 8; turn++) {
    const bg = hsl2rgb((h + turn * 0.09) % 1, s, 0.13);
    if (!taken.some((t) => Math.hypot(t[0] - bg[0], t[1] - bg[1], t[2] - bg[2]) < 12)) return bg;
  }
  return hsl2rgb(h, s, 0.13);
}
const earlier = readdirSync(outDir).filter((f) => f.endsWith(".token.json") && f !== `${code}.token.json`)
  .map((f) => JSON.parse(readFileSync(resolve(outDir, f), "utf8")).ground).filter(Array.isArray);

async function store(source) {
  const r = await fetch(`${AC}/api/store-kidlisp`, { method: "POST", headers: { "content-type": "application/json", authorization: `Bearer ${AC_TOKEN}` }, body: JSON.stringify({ source }) });
  const j = await r.json().catch(() => ({}));
  if (!r.ok || !j.code) throw new Error(`store-kidlisp ${r.status}: ${JSON.stringify(j).slice(0, 200)}`);
  return j.code;
}

if (!receipt.code) {
  const ink = dominantInk(receipt.originalSource);
  const bg = ground(ink, earlier);
  // The name line stays first (it is the title); the pragmas follow it.
  const lines = receipt.originalSource.split("\n");
  const at = lines.findIndex((l) => /^\s*;/.test(l)) + 1;
  lines.splice(at, 0, `; #bg ${bg.join(" ")}`, `; #door ${DOOR}`);
  const source = lines.join("\n");
  if (source.length > 50000) { console.error(`✗ the object is ${source.length} chars (store-kidlisp takes 50000)`); process.exit(1); }
  const minted = flags.dry ? `preview-${code}` : await store(source);
  Object.assign(receipt, { code: minted, source, ink, ground: bg, door: DOOR, dryRun: Boolean(flags.dry), priceXtz: PRICE_XTZ });
  save();
  console.log(`  ✓ ink rgb(${ink}) → ground rgb(${bg}); stored as $${minted} with the doorway ${DOOR}`);
}

// ── 1b. render ───────────────────────────────────────────────────────────
// The viewer's preview pose, one full turn, drawn into raw RGBA frames that
// ffmpeg turns into the GIF and the still.
const gifPath = resolve(outDir, `${code}.gif`), thumbPath = resolve(outDir, `${code}-thumb.png`);
if (!existsSync(gifPath) || !existsSync(thumbPath)) {
  const view = { preview: true, time: 0, yaw: 0.6 };
  const piece = objectPiece(receipt.source, "", view);
  const screen = { width: SIZE, height: SIZE, pixels: new Uint8ClampedArray(SIZE * SIZE * 4) };
  const api = { screen, ink() { return api; }, write() { return api; } };
  piece.boot(api);
  const count = FPS * SECONDS;
  const raw = Buffer.alloc(SIZE * SIZE * 4 * count);
  for (let i = 0; i < count; i++) {
    view.yaw = 0.6 + (i / count) * Math.PI * 2;
    piece.paint(api);
    raw.set(screen.pixels, i * SIZE * SIZE * 4);
  }
  const rawPath = resolve(outDir, `${code}.rgba`);
  writeFileSync(rawPath, raw);
  const ff = (args) => { const r = spawnSync("ffmpeg", ["-loglevel", "error", "-y", ...args]); if (r.status !== 0) { console.error(`✗ ffmpeg: ${r.stderr}`); process.exit(1); } };
  ff(["-f", "rawvideo", "-pix_fmt", "rgba", "-s", `${SIZE}x${SIZE}`, "-r", String(FPS), "-i", rawPath,
    "-vf", "split[a][b];[a]palettegen=max_colors=128:stats_mode=diff[p];[b][p]paletteuse=dither=none", "-loop", "0", gifPath]);
  ff(["-f", "rawvideo", "-pix_fmt", "rgba", "-s", `${SIZE}x${SIZE}`, "-i", rawPath, "-frames:v", "1", "-vf", "scale=320:320:flags=area", thumbPath]);
  Object.assign(receipt, { frames: count, gifBytes: readFileSync(gifPath).length });
  save();
  console.log(`  ✓ turntable ${count} frames (${(receipt.gifBytes / 1024).toFixed(0)} KB) → out/objects/${code}.gif`);
}

// ── 1c. pack ─────────────────────────────────────────────────────────────
// The oven's bundler packs the stored source byte for byte, with the doorway
// in the page shell; index.html + covers + source make the Teia directory.
process.env.AC_SOURCE_DIR ||= resolve(REPO, "system", "public", "aesthetic.computer");
const { createBundleFromSource } = await import(resolve(REPO, "oven", "bundler.mjs"));
const { teiaPackage, TEIA_FORMAT } = await import(resolve(REPO, "system", "backend", "whistlegraph-teia.mjs"));
const htmlPath = resolve(outDir, `${code}.html`);
if (!existsSync(htmlPath)) {
  const { html } = await createBundleFromSource(receipt.code, receipt.source, { authorHandle: HANDLE, style: OBJECT_STYLE, doorway: DOOR });
  if (!html.includes("window.acDOORWAY")) { console.error("✗ the pack has no doorway; the bundler on this checkout predates it"); process.exit(1); }
  writeFileSync(htmlPath, html);
  console.log(`  ✓ packed ${(html.length / 1024).toFixed(0)} KB → out/objects/${code}.html`);
}
const base = teiaPackage(readFileSync(htmlPath, "utf8"), { gif: readFileSync(gifPath), thumbnail: readFileSync(thumbPath), frames: receipt.frames || FPS * SECONDS });
const about = { title, name: receipt.name, code: receipt.code, originalCode: code, built: receipt.built, shapes: shapeCount, ground: receipt.ground, door: DOOR, live: `${AC}/$${receipt.code}` };
const files = [...base.files,
  { name: "source.lisp", mime: "text/plain", content: Buffer.from(receipt.source) },
  { name: "object.json", mime: "application/json", content: Buffer.from(JSON.stringify(about, null, 2) + "\n") },
];

const builtOn = new Date(`${receipt.built}T12:00:00Z`).toLocaleDateString("en-US", { year: "numeric", month: "long", day: "numeric", timeZone: "UTC" });
const metadataFor = (uris) => ({
  name: title,
  description: `Built in dool, the room under aesthetic.computer, on ${builtOn}. A KidLisp object of ${shapeCount} slabs, orbs and capsules. Tap it to open aesthetic.computer with the room already up, where you can orbit it, read its source, and build your own.\n\nLive: ${AC}/$${receipt.code}\nDool: ${DOOR}`,
  tags: ["aesthetic.computer", "kidlisp", "3d", "object", "dool", receipt.name.toLowerCase()],
  symbol: "OBJKT",
  artifactUri: uris.directory,
  displayUri: uris.gif,
  thumbnailUri: uris.thumb,
  creators: [SIGNER],
  formats: [
    { uri: uris.directory, mimeType: TEIA_FORMAT, dimensions: { value: "responsive", unit: "viewport" } },
    { uri: uris.gif, mimeType: "image/gif", dimensions: { value: `${SIZE}x${SIZE}`, unit: "px" } },
  ],
  decimals: 0,
  isBooleanAmount: false,
  shouldPreferSymbol: false,
  date: new Date().toISOString(),
});

if (flags.dry) {
  const dry = (f) => `ipfs://<${f}>`;
  writeFileSync(resolve(outDir, `${code}.metadata.json`), JSON.stringify(metadataFor({ directory: dry(`${code}-directory`), gif: dry(`${code}.gif`), thumb: dry(`${code}-thumb.png`) }), null, 2) + "\n");
  console.log(`✓ dry run: "${title}" rendered and packed; metadata → out/objects/${code}.metadata.json; nothing stored, pinned or minted.`);
  process.exit(0);
}

// ── 2. pin ───────────────────────────────────────────────────────────────
if (!AC_TOKEN) { console.error("✗ no AC session: sign in with node tezos/ac-login.mjs as @jeffrey"); process.exit(1); }
async function pin(payload) {
  const r = await fetch(`${AC}/api/ipfs-add`, { method: "POST", headers: { authorization: `Bearer ${AC_TOKEN}`, "content-type": "application/json" }, body: JSON.stringify(payload), signal: AbortSignal.timeout(180000) });
  const j = await r.json().catch(() => ({}));
  if (!r.ok || !j.uri) throw new Error(`ipfs-add ${r.status}: ${JSON.stringify(j).slice(0, 200)}`);
  return j.uri;
}
const pinFile = (path, name, mimeType) => pin({ name, mimeType, base64: readFileSync(path).toString("base64") });
if (!receipt.metadataUri) {
  const uris = {
    directory: await pin({ files: files.map(({ name, mime, content }) => ({ name, mimeType: mime, base64: content.toString("base64") })) }),
    gif: await pinFile(gifPath, `${code}.gif`, "image/gif"),
    thumb: await pinFile(thumbPath, `${code}-thumb.png`, "image/png"),
  };
  const metadata = metadataFor(uris);
  const metadataUri = await pin({ name: `${code}.json`, json: metadata });
  Object.assign(receipt, { artifactMimeType: TEIA_FORMAT, artifactUri: metadata.artifactUri, displayUri: metadata.displayUri, thumbnailUri: metadata.thumbnailUri, metadataUri });
  save();
  console.log(`  ✓ pinned ${receipt.metadataUri}`);
}

// ── 3. mint ──────────────────────────────────────────────────────────────
const requireTezos = createRequire(resolve(REPO, "tezos", "package.json"));
const { TezosToolkit } = requireTezos("@taquito/taquito");
const { InMemorySigner } = requireTezos("@taquito/signer");
if (!process.env.AESTHETIC_KEY) { console.error("✗ AESTHETIC_KEY not set"); process.exit(1); }
const tezos = new TezosToolkit(RPC);
tezos.setProvider({ signer: await InMemorySigner.fromSecretKey(process.env.AESTHETIC_KEY) });
const signer = await tezos.signer.publicKeyHash();
if (signer !== SIGNER) { console.error(`✗ key belongs to ${signer}, not aesthetic.tez; refusing`); process.exit(1); }
const balance = (await tezos.tz.getBalance(signer)).toNumber() / 1e6;
if (balance < MIN_BALANCE_XTZ) { console.error(`✗ aesthetic.tez holds ${balance} XTZ (< ${MIN_BALANCE_XTZ})`); process.exit(1); }
const tzkt = async (path) => (await fetch(`${TZKT}${path}`)).json();

if (receipt.tokenId === undefined) {
  if (!receipt.mintOp) {
    const minter = await tezos.contract.at(HEN_MINTER);
    const op = await minter.methodsObject.mint_OBJKT({
      address: SIGNER, amount: String(EDITIONS),
      metadata: Buffer.from(receipt.metadataUri).toString("hex"),
      royalties: String(ROYALTIES),
    }).send();
    receipt.mintOp = op.hash; save();
    console.log(`  minting… ${op.hash}`);
    await op.confirmation(1);
  }
  for (let i = 0; i < 30 && receipt.tokenId === undefined; i++) {
    const txs = (await tzkt(`/operations/transactions/${receipt.mintOp}`)) || [];
    const failed = txs.find((tx) => tx.status !== "applied");
    if (failed) { console.error(`✗ mint ${failed.status}`); process.exit(1); }
    const ids = txs.map((tx) => tx.id).join(",");
    const [t] = ids ? await tzkt(`/tokens/transfers?transactionId.in=${ids}&token.contract=${HEN_OBJKTS}`) : [];
    if (t) { receipt.tokenId = t.token.tokenId; save(); break; }
    await new Promise((r) => setTimeout(r, 10000));
  }
  if (receipt.tokenId === undefined) { console.error("✗ mint confirmed but the indexer hasn't shown the token yet; rerun to resume"); process.exit(1); }
  console.log(`  ✓ minted OBJKT #${receipt.tokenId} ×${EDITIONS}`);
}

// ── 4. list ──────────────────────────────────────────────────────────────
// Teia takes the edition into its own custody, so it is made an operator first.
const token = await tezos.contract.at(HEN_OBJKTS);
const market = await tezos.contract.at(TEIA_MARKET);
const op = await tezos.contract.batch()
  .withContractCall(token.methodsObject.update_operators([
    { add_operator: { owner: SIGNER, operator: TEIA_MARKET, token_id: String(receipt.tokenId) } },
  ]))
  .withContractCall(market.methodsObject.swap({
    fa2: HEN_OBJKTS, objkt_id: String(receipt.tokenId), objkt_amount: String(EDITIONS),
    xtz_per_objkt: String(Math.round(PRICE_XTZ * 1e6)), royalties: String(ROYALTIES), creator: SIGNER,
  }))
  .send();
await op.confirmation(1);
Object.assign(receipt, {
  listOp: op.hash, teiaSwapOp: op.hash, listed: true, listedOn: "teia", editions: EDITIONS, priceXtz: PRICE_XTZ,
  objktUrl: `https://objkt.com/tokens/hicetnunc/${receipt.tokenId}`,
  teiaUrl: `https://teia.art/objkt/${receipt.tokenId}`,
});
save();
console.log(`✓ "${title}": OBJKT #${receipt.tokenId} — ${EDITIONS} × ${PRICE_XTZ} XTZ on Teia — ${receipt.teiaUrl}`);
