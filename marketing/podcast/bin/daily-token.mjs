#!/usr/bin/env node
// daily-token.mjs — each day's podcast update, minted as a hic et nunc 1/1.
//
//   page    — the episode as a Star Wars crawl in AC's pixel font: a GIF drawn
//             here (lib/crawl.mjs), and the same crawl as a live KidLisp
//             $code on AC; the thumbnail is one of the GIF's frames
//   bundle  — the $code packed as a Keep is (oven/bundler.mjs, PACK mode):
//             one self-extracting HTML that runs offline in objkt's sandbox
//   pin     — bundle, GIF, thumb and TZIP-21 metadata to AC's IPFS node
//             (/api/ipfs-add); the bundle is the artifact, the GIF the display
//   mint    — mint_OBJKT on the hic et nunc minter, signed by aesthetic.tez
//   list    — an objkt ask for the whole edition
//
// Every stage writes into out/daily/<slug>.token.json as it lands, so a
// failed run resumes where it stopped and a finished one never double-mints.
//
// Usage:
//   node bin/daily-token.mjs                      # today's episode
//   node bin/daily-token.mjs --date 2026-09-26
//   node bin/daily-token.mjs --dry                # page, bundle, metadata; no pin/mint/list
//
// Secrets come from the environment or --env <file> (repeatable):
//   AESTHETIC_KEY, AESTHETIC_ADDRESS   the signer (must be aesthetic.tez)
//   AC_TOKEN or ~/.ac-token            an @jeffrey AC session, for /api/ipfs-add
// Tuning: DAILY_EDITIONS (1), DAILY_PRICE_XTZ (3), DAILY_ROYALTIES_PERMILLE (150),
//         DAILY_ARTIFACT (html | gif: what the artifactUri is; see lib/artifact.mjs).

import { writeFileSync, mkdirSync, existsSync, readFileSync } from "node:fs";
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

const iso = (d) => `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
const date = typeof flags.date === "string" ? flags.date : iso(new Date());
if (!/^\d{4}-\d{2}-\d{2}$/.test(date)) { console.error(`✗ bad --date ${date}`); process.exit(1); }
const slug = `daily-${date}`;

const SIGNER = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"; // aesthetic.tez
const HEN_MINTER = "KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9";
const HEN_OBJKTS = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
const OBJKT_MARKET = "KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X"; // objkt marketplace v6.2
const RPC = process.env.TEZOS_RPC || "https://rpc.tzkt.io/mainnet";
const TZKT = "https://api.tzkt.io/v1";
const OVEN = process.env.OVEN_URL || "https://oven.aesthetic.computer";
const AC = process.env.AC_URL || "https://aesthetic.computer";
const SHOW = "https://www.buzzsprout.com/2628235";

const EDITIONS = Number(process.env.DAILY_EDITIONS || 1);
const PRICE_XTZ = Number(process.env.DAILY_PRICE_XTZ || 3);
const ROYALTIES = Number(process.env.DAILY_ROYALTIES_PERMILLE || 150); // HEN is per-mille
const MIN_BALANCE_XTZ = 0.15; // a mint + a listing burn ~0.06

const { artifactMode, crawlBundle, tokenMetadata } = await import(resolve(ROOT, "lib", "artifact.mjs"));
let ARTIFACT;
try { ARTIFACT = artifactMode(); } catch (err) { console.error(`✗ ${err.message}`); process.exit(1); }

// The AC session: AC_TOKEN if given, else this machine's own ~/.ac-token
// (from its own `ac-login`; never a copy from another machine, since Auth0
// rotates refresh tokens), renewed a minute early under the shared lock.
async function acToken() {
  if (process.env.AC_TOKEN) return process.env.AC_TOKEN;
  const file = resolve(process.env.HOME, ".ac-token");
  if (!existsSync(file)) return null;
  const record = JSON.parse(readFileSync(file, "utf8"));
  if (!record.expires_at || Date.now() < record.expires_at - 60_000) return record.access_token;
  if (!record.refresh_token) return null;
  return (await freshSession({ file })).access_token;
}
const AC_TOKEN = await acToken();

const dailyDir = resolve(ROOT, "out", "daily");
mkdirSync(dailyDir, { recursive: true });
const receiptPath = resolve(dailyDir, `${slug}.token.json`);
const receipt = existsSync(receiptPath) ? JSON.parse(readFileSync(receiptPath, "utf8")) : { slug, date };
const save = () => writeFileSync(receiptPath, JSON.stringify(receipt, null, 2) + "\n");

if (receipt.listed) {
  console.log(`✓ ${slug} already minted and listed: ${receipt.objktUrl}`);
  process.exit(0);
}

// ── 0. the episode ───────────────────────────────────────────────────────
// The script daily.mjs wrote is the material; its redaction guard already
// ran, so nothing here reaches the model or the chain that the show didn't say.
const mdPath = resolve(dailyDir, `${slug}.md`);
if (!existsSync(mdPath)) { console.error(`✗ no episode script at ${mdPath}; run daily.mjs first`); process.exit(1); }
const md = readFileSync(mdPath, "utf8");
const fm = md.match(/^---\n[\s\S]*?\btitle:\s*(.+?)\n[\s\S]*?\n---\n([\s\S]+)$/);
if (!fm) { console.error("✗ episode script has no frontmatter"); process.exit(1); }
const title = fm[1].trim();
const body = fm[2].trim();
const buzz = resolve(ROOT, "out", `${slug}.buzzsprout.json`);
const episodeUrl = existsSync(buzz) ? `${SHOW}/${JSON.parse(readFileSync(buzz, "utf8")).id}` : SHOW;
console.log(`▸ ${slug}: "${title}"`);

// ── 1. page ──────────────────────────────────────────────────────────────
// The token's image is the update itself, as a Star Wars crawl in AC's pixel
// font over a starfield, its colours turning with the date (lib/crawl.mjs).
// The GIF is drawn here, not on the oven: the oven repaints a page this heavy
// only every few seconds while it captures. The same crawl, moving with the
// clock, is stored as a live KidLisp $code and linked from the token.
const { crawlLayout, crawlPiece, renderCrawlGif } = await import(resolve(ROOT, "lib", "crawl.mjs"));
const CRAWL_SECONDS = 30, CRAWL_FPS = 10;

async function store(source) {
  const headers = { "content-type": "application/json" };
  if (AC_TOKEN) headers.authorization = `Bearer ${AC_TOKEN}`;
  const r = await fetch(`${AC}/api/store-kidlisp`, { method: "POST", headers, body: JSON.stringify({ source }) });
  const j = await r.json().catch(() => ({}));
  if (!r.ok || !j.code) throw new Error(`store-kidlisp ${r.status}: ${JSON.stringify(j).slice(0, 200)}`);
  return j.code;
}

if (!receipt.code) {
  const layout = crawlLayout({ title, body, date });
  const source = crawlPiece(layout, { periodMs: CRAWL_SECONDS * 1000 });
  if (source.length > 50000) { console.error(`✗ the crawl piece is ${source.length} chars (store-kidlisp takes 50000)`); process.exit(1); }
  const code = await store(source);
  console.log(`  page stored as $${code} — rendering the crawl…`);
  const gifPath = resolve(dailyDir, `${slug}.gif`);
  const frames = await renderCrawlGif(layout, gifPath, { seconds: CRAWL_SECONDS, fps: CRAWL_FPS });
  const gifBytes = readFileSync(gifPath).length;
  // The loop opens on empty sky while the title rises, so the thumbnail is
  // taken a third of the way in, with the crawl in full view.
  const t = spawnSync("ffmpeg", ["-loglevel", "error", "-y", "-i", gifPath, "-vf", `select=eq(n\\,${Math.round(frames / 3)}),scale=256:256:flags=area`, "-frames:v", "1", resolve(dailyDir, `${slug}-thumb.png`)]);
  if (t.status !== 0) { console.error(`✗ thumbnail: ${t.stderr}`); process.exit(1); }
  Object.assign(receipt, { code, source, gifBytes, frames, palette: layout.palette });
  save();
  console.log(`  ✓ $${code} · crawl ${frames} frames (${(gifBytes / 1024).toFixed(0)} KB) → out/daily/${slug}.gif`);
}

// ── 1b. bundle ───────────────────────────────────────────────────────────
// Packed from the source the receipt holds, so it is the stored $code byte
// for byte; a receipt from a gif-only night gets its bundle on resume.
const htmlPath = resolve(dailyDir, `${slug}.html`);
if (ARTIFACT === "html" && !existsSync(htmlPath)) {
  const html = await crawlBundle(receipt.code, receipt.source);
  writeFileSync(htmlPath, html);
  receipt.htmlBytes = Buffer.byteLength(html);
  save();
  console.log(`  ✓ bundled $${receipt.code} → out/daily/${slug}.html (${(receipt.htmlBytes / 1024).toFixed(0)} KB)`);
}

const metadataFor = (uris) => tokenMetadata({ title, body, date, episodeUrl, code: receipt.code, creator: SIGNER, artifact: ARTIFACT, uris, ac: AC });

if (flags.dry) {
  const dry = (f) => `ipfs://<${f}>`;
  const metadata = metadataFor({ html: dry(`${slug}.html`), gif: dry(`${slug}.gif`), thumb: dry(`${slug}-thumb.png`) });
  writeFileSync(resolve(dailyDir, `${slug}.metadata.json`), JSON.stringify(metadata, null, 2) + "\n");
  console.log(`✓ dry run (${ARTIFACT}): $${receipt.code} rendered; metadata → out/daily/${slug}.metadata.json; nothing pinned or minted.`);
  process.exit(0);
}

// ── 2. pin ───────────────────────────────────────────────────────────────
// Through AC's own IPFS node (/api/ipfs-add, admin-only) — the Kubo node Keeps
// pins to — so the daily doesn't ride on a third-party pinning plan.
if (!AC_TOKEN) { console.error("✗ no AC session: sign in with node tezos/ac-login.mjs as @jeffrey and copy ~/.ac-token here"); process.exit(1); }

async function pin(payload) {
  const r = await fetch(`${AC}/api/ipfs-add`, {
    method: "POST",
    headers: { authorization: `Bearer ${AC_TOKEN}`, "content-type": "application/json" },
    body: JSON.stringify(payload),
    signal: AbortSignal.timeout(180000),
  });
  const j = await r.json().catch(() => ({}));
  if (!r.ok || !j.uri) throw new Error(`ipfs-add ${r.status}: ${JSON.stringify(j).slice(0, 200)}`);
  return j.uri;
}
const pinFile = (path, name, mimeType) => pin({ name, mimeType, base64: readFileSync(path).toString("base64") });

if (!receipt.metadataUri) {
  const uris = {
    html: ARTIFACT === "html" ? await pinFile(htmlPath, `${slug}.html`, "text/html") : undefined,
    gif: await pinFile(resolve(dailyDir, `${slug}.gif`), `${slug}.gif`, "image/gif"),
    thumb: await pinFile(resolve(dailyDir, `${slug}-thumb.png`), `${slug}-thumb.png`, "image/png"),
  };
  const metadata = metadataFor(uris);
  const metadataUri = await pin({ name: `${slug}.json`, json: metadata });
  Object.assign(receipt, { artifact: ARTIFACT, artifactUri: metadata.artifactUri, displayUri: metadata.displayUri, thumbnailUri: metadata.thumbnailUri, metadataUri });
  save();
  console.log(`  ✓ pinned ${receipt.metadataUri}`);
}

// ── 3. mint ──────────────────────────────────────────────────────────────
// Taquito lives in tezos/'s node_modules (npm install --prefix tezos).
const requireTezos = createRequire(resolve(REPO, "tezos", "package.json"));
const { TezosToolkit } = requireTezos("@taquito/taquito");
const { InMemorySigner } = requireTezos("@taquito/signer");

if (!process.env.AESTHETIC_KEY) { console.error("✗ AESTHETIC_KEY not set"); process.exit(1); }
const tezos = new TezosToolkit(RPC);
tezos.setProvider({ signer: await InMemorySigner.fromSecretKey(process.env.AESTHETIC_KEY) });
const signer = await tezos.signer.publicKeyHash();
if (signer !== SIGNER) { console.error(`✗ key belongs to ${signer}, not aesthetic.tez; refusing`); process.exit(1); }

const balance = (await tezos.tz.getBalance(signer)).toNumber() / 1e6;
if (balance < MIN_BALANCE_XTZ) { console.error(`✗ aesthetic.tez holds ${balance} XTZ (< ${MIN_BALANCE_XTZ}); fund it to keep minting`); process.exit(1); }

const tzkt = async (path) => (await fetch(`${TZKT}${path}`)).json();

if (receipt.tokenId === undefined) {
  if (!receipt.mintOp) {
    const minter = await tezos.contract.at(HEN_MINTER);
    const op = await minter.methodsObject.mint_OBJKT({
      address: SIGNER,
      amount: String(EDITIONS),
      metadata: Buffer.from(receipt.metadataUri).toString("hex"),
      royalties: String(ROYALTIES),
    }).send();
    receipt.mintOp = op.hash;
    save();
    console.log(`  minting… ${op.hash}`);
    await op.confirmation(1);
  }
  // The new OBJKT id is read back from the indexer: the op's token transfer
  // (a mint is a transfer from nobody) carries it.
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
const token = await tezos.contract.at(HEN_OBJKTS);
const market = await tezos.contract.at(OBJKT_MARKET);
const op = await tezos.contract.batch()
  .withContractCall(token.methodsObject.update_operators([
    { add_operator: { owner: SIGNER, operator: OBJKT_MARKET, token_id: String(receipt.tokenId) } },
  ]))
  .withContractCall(market.methodsObject.ask({
    token: { address: HEN_OBJKTS, token_id: String(receipt.tokenId) },
    currency: { tez: {} },
    amount: String(Math.round(PRICE_XTZ * 1e6)),
    editions: String(EDITIONS),
    shares: { [SIGNER]: String(ROYALTIES * 10) }, // objkt shares are per ten-thousand
    start_time: null,
    expiry_time: null,
    referral_bonus: "500",
    condition: null,
  }))
  .send();
await op.confirmation(1);
Object.assign(receipt, {
  listOp: op.hash, listed: true, editions: EDITIONS, priceXtz: PRICE_XTZ,
  objktUrl: `https://objkt.com/tokens/hicetnunc/${receipt.tokenId}`,
});
save();
console.log(`✓ ${slug}: OBJKT #${receipt.tokenId} — ${EDITIONS} × ${PRICE_XTZ} XTZ — ${receipt.objktUrl}`);
