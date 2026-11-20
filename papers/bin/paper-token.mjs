#!/usr/bin/env node
// paper-token.mjs — one AC paper, minted as an edition in the papers
// collection on objkt, signed by aesthetic.tez.
//
//   collection — the papers collection, made once through objkt's Minting
//                Factory (create_artist_collection). Its id and contract
//                address are read back from the operation and kept in
//                <dir>/collection.json for every later paper.
//   pin        — the PDF, a display image and a thumbnail, then TZIP-21
//                metadata, through AC's own IPFS node (/api/ipfs-add)
//   mint       — mint_artist on the factory, editions to aesthetic.tez
//   list       — an ask on the objkt marketplace for the whole edition
//
// Every stage writes into <dir>/<slug>.token.json as it lands, so a failed run
// resumes where it stopped and a finished one never double-mints.
//
// Usage:
//   node paper-token.mjs --stage collection --dir <work-dir>
//   node paper-token.mjs --stage mint --dir <work-dir> --slug plorking-the-planet-26-arxiv \
//        --pdf paper.pdf --display cover.png --thumb thumb.jpg --meta token.json
//   add --dry to print what would be sent and sign nothing
//
// <work-dir>/collection-meta.json holds {name, description, homepage}.
// --meta holds {name, description, tags, editions, priceXtz, royaltiesBps}.
//
// Secrets come from the environment or --env <file> (repeatable):
//   AESTHETIC_KEY                the signer (must be aesthetic.tez)
//   AC_TOKEN or ~/.ac-token      an @jeffrey AC session, for /api/ipfs-add
// --repo <path> names the checkout whose tezos/node_modules holds Taquito.

import { existsSync, readFileSync, writeFileSync, statSync } from "node:fs";
import { resolve, basename, dirname } from "node:path";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";

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

const fail = (message) => { console.error(`✗ ${message}`); process.exit(1); };
const STAGE = flags.stage;
if (!["collection", "mint"].includes(STAGE)) fail("--stage collection | mint");
if (typeof flags.dir !== "string" || !existsSync(flags.dir)) fail("--dir <existing work dir>");
const DIR = resolve(flags.dir);
const REPO = resolve(typeof flags.repo === "string" ? flags.repo : resolve(dirname(fileURLToPath(import.meta.url)), "..", ".."));
const DRY = Boolean(flags.dry);

const SIGNER = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"; // aesthetic.tez
const FACTORY = "KT1Aq4wWmVanpQhq4TTfjZXB5AjFpx15iQMM"; // objkt.com Minting Factory
const OBJKT_MARKET = "KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X"; // objkt marketplace v6.2
const RPC = process.env.TEZOS_RPC || "https://rpc.tzkt.io/mainnet";
const TZKT = "https://api.tzkt.io/v1";
const AC = process.env.AC_URL || "https://aesthetic.computer";
const GATEWAY = process.env.IPFS_GATEWAY || "https://ipfs.aesthetic.computer";

const readJson = (path, fallback) => (existsSync(path) ? JSON.parse(readFileSync(path, "utf8")) : fallback);
const hex = (text) => Buffer.from(text).toString("hex");
const tzkt = async (path) => (await fetch(`${TZKT}${path}`)).json();
const sleep = (ms) => new Promise((done) => setTimeout(done, ms));

const collectionPath = resolve(DIR, "collection.json");
const collection = readJson(collectionPath, {});
const saveCollection = () => writeFileSync(collectionPath, JSON.stringify(collection, null, 2) + "\n");

// ── the AC session, for pinning ─────────────────────────────────────────────
async function acToken() {
  if (process.env.AC_TOKEN) return process.env.AC_TOKEN;
  const file = resolve(process.env.HOME, ".ac-token");
  if (!existsSync(file)) return null;
  const record = JSON.parse(readFileSync(file, "utf8"));
  if (!record.expires_at || Date.now() < record.expires_at - 60_000) return record.access_token;
  if (!record.refresh_token) return null;
  const { freshSession } = await import(resolve(REPO, "shared", "ac-token.mjs"));
  return (await freshSession({ file })).access_token;
}
let AC_TOKEN = null;
async function pin(payload) {
  if (DRY) return `ipfs://<dry:${payload.name || "directory"}>`;
  AC_TOKEN ||= await acToken();
  if (!AC_TOKEN) fail("no AC session: sign in with node tezos/ac-login.mjs as @jeffrey");
  const r = await fetch(`${AC}/api/ipfs-add`, {
    method: "POST", headers: { authorization: `Bearer ${AC_TOKEN}`, "content-type": "application/json" },
    body: JSON.stringify(payload), signal: AbortSignal.timeout(300000),
  });
  const j = await r.json().catch(() => ({}));
  if (!r.ok || !j.uri) throw new Error(`ipfs-add ${r.status}: ${JSON.stringify(j).slice(0, 200)}`);
  return j.uri;
}
const pinFile = (path, mimeType) => pin({ name: basename(path), mimeType, base64: readFileSync(path).toString("base64") });
// A pin is trusted only after the gateway hands the same bytes back.
async function readBack(uri, expected) {
  if (DRY) return;
  const r = await fetch(`${GATEWAY}/ipfs/${uri.replace("ipfs://", "")}`, { signal: AbortSignal.timeout(120000) });
  if (!r.ok) throw new Error(`gateway ${r.status} for ${uri}`);
  const got = Buffer.from(await r.arrayBuffer());
  if (!got.equals(expected)) throw new Error(`pinned bytes differ for ${uri}`);
}

// ── the signer ─────────────────────────────────────────────────────────────
let tezos = null;
async function signer() {
  if (tezos) return tezos;
  if (!process.env.AESTHETIC_KEY) fail("AESTHETIC_KEY not set");
  const requireTezos = createRequire(resolve(REPO, "tezos", "package.json"));
  const { TezosToolkit } = requireTezos("@taquito/taquito");
  const { InMemorySigner } = requireTezos("@taquito/signer");
  tezos = new TezosToolkit(RPC);
  tezos.setProvider({ signer: await InMemorySigner.fromSecretKey(process.env.AESTHETIC_KEY) });
  const address = await tezos.signer.publicKeyHash();
  if (address !== SIGNER) fail(`signer is ${address}, not aesthetic.tez`);
  return tezos;
}
async function balanceXtz() { return (await tzkt(`/accounts/${SIGNER}`)).balance / 1e6; }
// One operation's applied transactions, or a failure if any part of it failed.
async function applied(hash) {
  for (let i = 0; i < 30; i++) {
    const ops = (await tzkt(`/operations/${hash}`)) || [];
    const bad = ops.find((op) => op.status && op.status !== "applied");
    if (bad) fail(`operation ${hash} ${bad.status}: ${JSON.stringify(bad.errors || "").slice(0, 300)}`);
    if (ops.length) return ops;
    await sleep(10000);
  }
  fail(`operation ${hash} not indexed yet; rerun to resume`);
}

// ── stage: collection ──────────────────────────────────────────────────────
if (STAGE === "collection") {
  if (collection.contract) { console.log(`✓ collection ${collection.id} already exists: ${collection.contract}`); process.exit(0); }
  const meta = readJson(resolve(DIR, "collection-meta.json"), null) || fail("collection-meta.json missing");
  const image = typeof flags.image === "string" ? resolve(flags.image) : null;
  if (!image || !existsSync(image)) fail("--image <collection image> is required");
  if (!collection.metadataUri) {
    collection.imageUri ||= await pinFile(image, "image/png");
    const body = { name: meta.name, description: meta.description, authors: [SIGNER], homepage: meta.homepage, imageUri: collection.imageUri, interfaces: ["TZIP-12"], type: "art" };
    collection.metadata = body;
    collection.metadataUri = await pin({ name: "papers-collection.json", json: body });
    if (!DRY) { saveCollection(); await readBack(collection.imageUri, readFileSync(image)); }
    console.log(`  ✓ collection metadata ${collection.metadataUri}`);
  }
  const need = 1.05; // origination storage 0.885 + allocation 0.064 + factory entry 0.031, plus fees
  const have = await balanceXtz();
  console.log(`  aesthetic.tez holds ${have} XTZ; a new collection needs about ${need}`);
  if (DRY) { console.log(`✓ dry: would call create_artist_collection(${collection.metadataUri})`); console.log(JSON.stringify(collection.metadata, null, 2)); process.exit(0); }
  if (have < need) fail(`not enough XTZ to create the collection (${have} < ${need})`);
  if (!collection.createOp) {
    const t = await signer();
    const factory = await t.contract.at(FACTORY);
    const op = await factory.methods.create_artist_collection(hex(collection.metadataUri)).send();
    collection.createOp = op.hash; saveCollection();
    console.log(`  creating… ${op.hash}`);
    await op.confirmation(1);
  }
  const ops = await applied(collection.createOp);
  const entry = ops.flatMap((op) => op.diffs || []).find((diff) => diff.path === "artist_collections" && diff.action === "add_key");
  if (!entry) fail("collection created but its id was not found in the operation; inspect the op on tzkt");
  if (entry.content.value.creator !== SIGNER) fail("collection entry has another creator");
  Object.assign(collection, { id: entry.content.key, contract: entry.content.value.contract, objktUrl: `https://objkt.com/collections/${entry.content.value.contract}` });
  saveCollection();
  console.log(`✓ collection ${collection.id}: ${collection.contract} — ${collection.objktUrl}`);
  process.exit(0);
}

// ── stage: mint ────────────────────────────────────────────────────────────
if (!collection.contract && !DRY) fail("no collection yet: run --stage collection first");
const slug = typeof flags.slug === "string" ? flags.slug : fail("--slug <paper slug>");
const receiptPath = resolve(DIR, `${slug}.token.json`);
const receipt = readJson(receiptPath, { slug });
const save = () => { if (!DRY) writeFileSync(receiptPath, JSON.stringify(receipt, null, 2) + "\n"); };
if (receipt.listed) { console.log(`✓ ${slug} already minted and listed: ${receipt.objktUrl}`); process.exit(0); }

const need = (flag) => { const p = typeof flags[flag] === "string" ? resolve(flags[flag]) : null; if (!p || !existsSync(p)) fail(`--${flag} <file> is required`); return p; };
const pdf = need("pdf"), display = need("display"), thumb = need("thumb");
const meta = readJson(need("meta"), null);
for (const key of ["name", "description", "tags", "editions", "priceXtz", "royaltiesBps"]) if (meta[key] === undefined) fail(`--meta lacks ${key}`);
const EDITIONS = Number(meta.editions), PRICE = Number(meta.priceXtz), ROYALTIES = Number(meta.royaltiesBps);
const mimeOf = (path) => (/\.png$/i.test(path) ? "image/png" : /\.jpe?g$/i.test(path) ? "image/jpeg" : "application/pdf");

if (!receipt.metadataUri) {
  receipt.artifactUri ||= await pinFile(pdf, "application/pdf");
  receipt.displayUri ||= await pinFile(display, mimeOf(display));
  receipt.thumbnailUri ||= await pinFile(thumb, mimeOf(thumb));
  save();
  if (!DRY) { await readBack(receipt.artifactUri, readFileSync(pdf)); console.log("  ✓ pinned PDF read back byte for byte"); }
  const format = (uri, path, name) => ({ uri, mimeType: mimeOf(path), fileName: name, fileSize: statSync(path).size });
  receipt.metadata = {
    name: meta.name, description: meta.description, tags: meta.tags, symbol: "OBJKTCOM", decimals: 0, isBooleanAmount: false,
    artifactUri: receipt.artifactUri, displayUri: receipt.displayUri, thumbnailUri: receipt.thumbnailUri, image: receipt.displayUri,
    formats: [format(receipt.artifactUri, pdf, "artifact.pdf"), format(receipt.displayUri, display, `display.${display.split(".").pop()}`), format(receipt.thumbnailUri, thumb, `thumbnail.${thumb.split(".").pop()}`)],
    creators: [SIGNER], minter: FACTORY, mintingTool: "https://aesthetic.computer", date: new Date().toISOString(), language: "en",
    rights: meta.rights || "© @jeffrey, Aesthetic.Computer. Free to read and share with attribution.",
    royalties: { decimals: 4, shares: { [SIGNER]: String(ROYALTIES) } }, attributes: meta.attributes || [],
  };
  receipt.metadataUri = await pin({ name: `${slug}.json`, json: receipt.metadata });
  Object.assign(receipt, { editions: EDITIONS, priceXtz: PRICE, pdfBytes: statSync(pdf).size });
  save();
  console.log(`  ✓ token metadata ${receipt.metadataUri}`);
}
if (DRY) { console.log(JSON.stringify(receipt.metadata, null, 2)); console.log(`✓ dry: would mint ${EDITIONS} editions into collection ${collection.id || "<none yet>"} and ask ${PRICE} XTZ each`); process.exit(0); }

const t = await signer();
if ((await balanceXtz()) < 0.2) fail("aesthetic.tez holds under 0.2 XTZ; fund it before minting");
if (receipt.tokenId === undefined) {
  if (!receipt.mintOp) {
    const factory = await t.contract.at(FACTORY);
    const op = await factory.methodsObject.mint_artist({ collection_id: String(collection.id), editions: String(EDITIONS), metadata_cid: hex(receipt.metadataUri), target: SIGNER }).send();
    receipt.mintOp = op.hash; save();
    console.log(`  minting… ${op.hash}`);
    await op.confirmation(1);
  }
  await applied(receipt.mintOp);
  for (let i = 0; i < 30 && receipt.tokenId === undefined; i++) {
    const ids = ((await tzkt(`/operations/transactions/${receipt.mintOp}`)) || []).map((tx) => tx.id).join(",");
    const [moved] = ids ? await tzkt(`/tokens/transfers?transactionId.in=${ids}&token.contract=${collection.contract}`) : [];
    if (moved) { receipt.tokenId = moved.token.tokenId; save(); break; }
    await sleep(10000);
  }
  if (receipt.tokenId === undefined) fail("mint applied but the indexer has not shown the token; rerun to resume");
  console.log(`  ✓ minted token ${receipt.tokenId} ×${EDITIONS} in ${collection.contract}`);
}

if (!receipt.listed) {
  const token = await t.contract.at(collection.contract);
  const market = await t.contract.at(OBJKT_MARKET);
  const op = await t.contract.batch()
    .withContractCall(token.methodsObject.update_operators([{ add_operator: { owner: SIGNER, operator: OBJKT_MARKET, token_id: String(receipt.tokenId) } }]))
    .withContractCall(market.methodsObject.ask({
      token: { address: collection.contract, token_id: String(receipt.tokenId) }, currency: { tez: {} },
      amount: String(Math.round(PRICE * 1e6)), editions: String(EDITIONS), shares: { [SIGNER]: String(ROYALTIES) },
      start_time: null, expiry_time: null, referral_bonus: "500", condition: null,
    }))
    .send();
  await op.confirmation(1);
  await applied(op.hash);
  Object.assign(receipt, { listOp: op.hash, listed: true, listedOn: "objkt", objktUrl: `https://objkt.com/tokens/${collection.contract}/${receipt.tokenId}` });
  save();
}
console.log(`✓ ${slug}: token ${receipt.tokenId} — ${EDITIONS} × ${PRICE} XTZ on objkt — ${receipt.objktUrl}`);
