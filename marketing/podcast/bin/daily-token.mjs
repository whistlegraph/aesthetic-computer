#!/usr/bin/env node
// daily-token.mjs — the daily episode, minted as a hic et nunc token.
//
//   sketch  — one of @jeffrey's most-played KidLisp $codes, recolored for the
//             day: `claude -p` reads the episode and picks the piece, palette
//             and a word; the result is stored as a new $code on AC
//   render  — the oven grabs the $code as an animated GIF (+ a PNG thumb);
//             a blank or failed grab gets a second pick, then a date-seeded one
//   pin     — GIF, thumb and TZIP-21 metadata to AC's IPFS node (/api/ipfs-add)
//   mint    — mint_OBJKT on the hic et nunc minter, signed by aesthetic.tez
//   list    — an objkt ask for the whole edition
//
// Every stage writes into out/daily/<slug>.token.json as it lands, so a
// failed run resumes where it stopped and a finished one never double-mints.
//
// Usage:
//   node bin/daily-token.mjs                      # today's episode
//   node bin/daily-token.mjs --date 2026-09-26
//   node bin/daily-token.mjs --dry                # sketch + render only; no pin/mint/list
//
// Secrets come from the environment or --env <file> (repeatable):
//   AESTHETIC_KEY, AESTHETIC_ADDRESS   the signer (must be aesthetic.tez)
//   AC_TOKEN or ~/.ac-token            an @jeffrey AC session, for /api/ipfs-add
// Tuning: DAILY_EDITIONS (1), DAILY_PRICE_XTZ (10), DAILY_ROYALTIES_PERMILLE (150).

import { writeFileSync, mkdirSync, existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createRequire } from "node:module";

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
const PRICE_XTZ = Number(process.env.DAILY_PRICE_XTZ || 10);
const ROYALTIES = Number(process.env.DAILY_ROYALTIES_PERMILLE || 150); // HEN is per-mille
const MIN_BALANCE_XTZ = 0.15; // a mint + a listing burn ~0.06

// The AC session: AC_TOKEN if given, else ~/.ac-token (written once by
// tezos/ac-login.mjs and copied over), refreshed through its refresh token a
// minute early — the same grant easel uses — so the nightly run never lapses.
const AUTH0 = "hi.aesthetic.computer";
const AUTH0_CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
async function acToken() {
  if (process.env.AC_TOKEN) return process.env.AC_TOKEN;
  const file = resolve(process.env.HOME, ".ac-token");
  if (!existsSync(file)) return null;
  const record = JSON.parse(readFileSync(file, "utf8"));
  if (!record.expires_at || Date.now() < record.expires_at - 60_000) return record.access_token;
  if (!record.refresh_token) return null;
  const r = await fetch(`https://${AUTH0}/oauth/token`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ grant_type: "refresh_token", client_id: AUTH0_CLIENT_ID, refresh_token: record.refresh_token }),
  });
  if (!r.ok) throw new Error(`AC session refresh failed (${r.status}); rerun tezos/ac-login.mjs and copy ~/.ac-token over`);
  const next = await r.json();
  record.access_token = next.access_token;
  if (next.refresh_token) record.refresh_token = next.refresh_token;
  record.expires_at = Date.now() + (next.expires_in || 3600) * 1000;
  writeFileSync(file, JSON.stringify(record, null, 2), { mode: 0o600 });
  return record.access_token;
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

// ── 1. sketch ────────────────────────────────────────────────────────────
// Freehand model KidLisp renders unevenly, so the day's sketch is a remix of
// one of @jeffrey's own most-played $codes: the model reads the episode and
// picks the piece, a three-color palette and a word; the forms stay proven.
const REMIXES = {
  roz: ({ a, b, c }) => `fade:${a}-${b}-black-${b}-${a} ink (? ${c} white 0) (1s... 24 64) line w/2 0 w/2 h (spin (2s... -1.125 1.125)) (zoom 1.1) (0.5s (contrast 1.05)) (scroll (? -0.1 0 0.1) (? -0.1 0 0.1)) ink (? ${a} ${b} ${c}) 8 circle w/2 h/2 (? 2 4 8)`,
  ceo: ({ a, b }) => `(1s (coat fade:black-${a}-${b}-${a}-black:frame 64)) (0.3s (zoom 0.5)) (scroll 1)`,
  air: ({ a, b, c }) => `fade:${a}-${b}-${a} ink (? ${c} ${b}) 32 line (0.15s (zoom 0.2)) scroll 1 0.25 (0.1s (contrast 1.01)) (0.5s (ink rainbow 96) (repeat 10 point))`,
  inz: ({ a, word }) => `(${a}) (ink (0.25s... 127 0 rainbow)) (write "${word}" 3 3) (scroll 18 3) (blur 0.1) (1.25s (zoom (? 0.25 1.5)))`,
};
const COLORS = ["red", "orange", "yellow", "lime", "green", "cyan", "teal", "blue", "navy", "purple", "magenta", "pink", "salmon", "coral", "gold", "beige", "brown", "maroon", "olive", "white", "gray", "silver", "palegreen", "skyblue", "violet", "indigo", "turquoise", "crimson", "tomato", "orchid"];

// A pick that doesn't need the model: stable per date, so reruns agree.
function defaultPick() {
  let h = 0;
  for (const ch of date) h = (h * 31 + ch.charCodeAt(0)) >>> 0;
  const names = Object.keys(REMIXES);
  return { piece: names[h % names.length], colors: [COLORS[h % 30], COLORS[(h >> 5) % 30], COLORS[(h >> 10) % 30]], word: title.split(/\s+/)[0] };
}

function choose(feedback = "") {
  const prompt = `Today's episode of "the daily" becomes a token: a remix of one of these
KidLisp pieces by @jeffrey, recolored for the day. Pick the piece whose motion
fits the episode's mood, three colors from the list, and one short word
(max 12 letters, lowercase) from the episode.

Pieces: roz (spinning radial lines, circles), ceo (slow scrolling color
bands), air (drifting lines, sparkles), inz (a written word smeared into rainbow)

Colors: ${COLORS.join(" ")}
${feedback ? `\nThe previous pick failed to render (${feedback}); pick a different piece.\n` : ""}
Print ONLY one line of JSON: {"piece":"...","colors":["...","...","..."],"word":"..."}

Episode title: ${title}
Episode script:
${body}`;
  const w = spawnSync("claude", ["-p", "--model", "sonnet", "--output-format", "text", "--tools", ""], {
    input: prompt, encoding: "utf8", timeout: 240000, maxBuffer: 1 << 20,
  });
  try {
    const pick = JSON.parse(w.stdout.match(/\{[\s\S]*\}/)[0]);
    const ok = REMIXES[pick.piece] && pick.colors?.length === 3 && pick.colors.every((c) => COLORS.includes(c)) && /^[a-z]{1,12}$/.test(pick.word);
    return ok ? pick : null;
  } catch { return null; }
}

const remix = ({ piece, colors: [a, b, c], word }) => REMIXES[piece]({ a, b, c, word });

async function store(source) {
  const headers = { "content-type": "application/json" };
  if (AC_TOKEN) headers.authorization = `Bearer ${AC_TOKEN}`;
  const r = await fetch(`${AC}/api/store-kidlisp`, { method: "POST", headers, body: JSON.stringify({ source }) });
  const j = await r.json().catch(() => ({}));
  if (!r.ok || !j.code) throw new Error(`store-kidlisp ${r.status}: ${JSON.stringify(j).slice(0, 200)}`);
  return j.code;
}

// A real 512² animated grab of a moving sketch runs to hundreds of KB or
// more; flat renders (a transparent checkerboard, a still wash with a box —
// why $r2f and $4bb, which need longer than a grab to develop, are out of
// the pool) come in under ~150 KB.
const MIN_GIF_BYTES = 300_000;
async function grab(code, format, size, query) {
  const url = `${OVEN}/grab/${format}/${size}/${size}/$${code}?${new URLSearchParams({ skipCache: "true", ...query })}`;
  const r = await fetch(url, { signal: AbortSignal.timeout(300000) });
  if (!r.ok) throw new Error(`oven ${r.status} for $${code}`);
  if (r.headers.get("x-oven-status") === "baking") throw new Error("oven returned a baking placeholder");
  return Buffer.from(await r.arrayBuffer());
}

if (!receipt.code) {
  let pick = null, source = null, code = null, gif = null, feedback = "";
  for (let attempt = 0; attempt < 3 && !gif; attempt++) {
    pick = (attempt < 2 && choose(feedback)) || defaultPick();
    source = remix(pick);
    console.log(`  sketch ${attempt + 1}: $${pick.piece} in ${pick.colors.join("/")} "${pick.word}"`);
    try {
      code = await store(source);
      console.log(`  stored $${code} — rendering…`);
      const bytes = await grab(code, "gif", 512, { duration: "6000", fps: "10" });
      if (bytes.length < MIN_GIF_BYTES) { feedback = `it rendered blank or still (${bytes.length} bytes)`; console.log(`  ✗ ${feedback}`); continue; }
      gif = bytes;
    } catch (e) {
      feedback = e.message;
      console.log(`  ✗ ${feedback}`);
    }
  }
  if (!gif) { console.error("✗ no sketch rendered; refusing to mint"); process.exit(1); }
  writeFileSync(resolve(dailyDir, `${slug}.gif`), gif);
  writeFileSync(resolve(dailyDir, `${slug}-thumb.png`), await grab(code, "png", 256, {}));
  Object.assign(receipt, { code, source, remixOf: pick.piece, gifBytes: gif.length });
  save();
  console.log(`  ✓ $${code} rendered (${(gif.length / 1024).toFixed(0)} KB) → out/daily/${slug}.gif`);
}

if (flags.dry) { console.log(`✓ dry run: $${receipt.code} rendered; nothing pinned or minted.`); process.exit(0); }

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
  const artifactUri = await pinFile(resolve(dailyDir, `${slug}.gif`), `${slug}.gif`, "image/gif");
  const thumbnailUri = await pinFile(resolve(dailyDir, `${slug}-thumb.png`), `${slug}-thumb.png`, "image/png");
  const metadata = {
    name: `${title} — the daily, ${date}`,
    description: `${body}\n\n— the daily from Aesthetic Dot Computer, ${date}. Listen: ${episodeUrl}\nPlay it live: ${AC}/$${receipt.code} (a remix of $${receipt.remixOf})`,
    tags: ["aesthetic.computer", "kidlisp", "thedaily", "podcast", "generative"],
    symbol: "OBJKT",
    artifactUri,
    displayUri: artifactUri,
    thumbnailUri,
    creators: [SIGNER],
    formats: [{ uri: artifactUri, mimeType: "image/gif" }],
    decimals: 0,
    isBooleanAmount: false,
    shouldPreferSymbol: false,
    date: new Date(`${date}T20:30:00-04:00`).toISOString(),
  };
  const metadataUri = await pin({ name: `${slug}.json`, json: metadata });
  Object.assign(receipt, { artifactUri, thumbnailUri, metadataUri });
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
