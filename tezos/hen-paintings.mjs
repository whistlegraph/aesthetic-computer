#!/usr/bin/env node
// hen-paintings.mjs — @jeffrey's finished paintings, minted as hic et nunc 1/1s.
//
// The queue: every painting the handle finishes (nopaint's "Done", or any
// other path that seals a painting) shows up in /media-collection. Anything
// newer than the watermark that aesthetic.tez hasn't minted yet gets:
//
//   fetch   — the PNG itself (the artifact: the true pixels), plus a crisp
//             nearest-neighbour upscale for objkt's display and thumbnail
//   pin     — the three images and TZIP-21 metadata via /api/ipfs-add
//   mint    — mint_OBJKT on the hic et nunc minter, signed by aesthetic.tez
//   list    — an objkt ask for the edition (skip with --no-list)
//
// Each painting writes out/hen-paintings/<code>.token.json as stages land,
// so a failed run resumes and a finished one never double-mints. The chain
// is the second guard: every token carries the tag `ac-painting-<code>`,
// and a code already tagged on aesthetic.tez's OBJKTs is skipped.
//
// Usage:
//   node tezos/hen-paintings.mjs status             # newest paintings and whether each is minted
//   node tezos/hen-paintings.mjs mint --last 3      # mint + list the newest 3 unminted paintings
//   node tezos/hen-paintings.mjs mint --code hf2    # one painting by code
//   node tezos/hen-paintings.mjs poll               # queue tick: mint everything finished after the watermark
//   node tezos/hen-paintings.mjs watermark          # set the watermark to the newest painting (nothing older is auto-minted)
//
// Flags: --dry (metadata only; nothing pinned, minted or listed), --no-list,
//        --price <xtz>, --editions <n>, --handle <name>, --env <file> (repeatable)
//
// Secrets: AESTHETIC_KEY (the signer; must be aesthetic.tez) from the env, an
// --env file, or the vault's tezos/kidlisp/.env when it is decrypted next to
// the repo; AC_TOKEN or ~/.ac-token for an @jeffrey session (/api/ipfs-add is
// admin-only). Tuning: HEN_PAINTING_PRICE_XTZ (5), HEN_PAINTING_EDITIONS (1),
// HEN_PAINTING_ROYALTIES_PERMILLE (150).

import { writeFileSync, mkdirSync, existsSync, readFileSync, readdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..");
const requireTezos = createRequire(resolve(HERE, "package.json"));

export const SIGNER = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"; // aesthetic.tez
export const HEN_MINTER = "KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9";
export const HEN_OBJKTS = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
export const OBJKT_MARKET = "KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X"; // objkt marketplace v6.2
const TZKT = "https://api.tzkt.io/v1";
const MIN_BALANCE_XTZ = 0.15; // a mint + a listing burn ~0.06

// ── pure pieces (tested in test/hen-paintings.test.mjs) ─────────────────

// A painting slug is a timestamp: 2026.2.24.15.46.16.302 — months and days
// are not zero-padded in the old ones, so compare field by field.
export function slugKey(slug) {
  return slug.split("/").pop().split(".").map((n) => n.padStart(4, "0")).join(".");
}
export const bySlugDesc = (a, b) => slugKey(b.slug).localeCompare(slugKey(a.slug));

// /media-collection?for=@handle/painting answers with PNG URLs; the slug is
// everything after "/painting/" minus the extension.
export function paintingsFromCollection(files) {
  return files
    .filter((url) => url.includes("/painting/"))
    .map((url) => ({ url, slug: decodeURIComponent(url.split("/painting/")[1].replace(/\.png$/, "")) }))
    .sort(bySlugDesc);
}

export const paintingTag = (code) => `ac-painting-${code}`;

// Integer upscale so pixel art stays pixel art; the longer side lands at or
// just past `target`. A painting already that big is left at 1×.
export function upscaleFactor(width, height, target) {
  return Math.max(1, Math.ceil(target / Math.max(width, height)));
}

export function nearestNeighbour(src, width, height, factor) {
  const out = Buffer.alloc(width * factor * height * factor * 4);
  const W = width * factor;
  for (let y = 0; y < height * factor; y++) {
    const sy = Math.floor(y / factor);
    for (let x = 0; x < W; x++) {
      const si = (sy * width + Math.floor(x / factor)) * 4, di = (y * W + x) * 4;
      out[di] = src[si]; out[di + 1] = src[si + 1]; out[di + 2] = src[si + 2]; out[di + 3] = src[si + 3];
    }
  }
  return out;
}

export function tokenMetadata({ code, handle, when, width, height, uris, display, thumb, creator = SIGNER, ac = "https://aesthetic.computer" }) {
  const day = when.slice(0, 10);
  const px = (w, h) => ({ value: `${w}x${h}`, unit: "px" });
  return {
    name: `#${code}`,
    description: `A painting by @${handle} on Aesthetic Computer, finished ${day}. ${width}×${height} pixels.\n\nThe painting lives at ${ac}/painting~${code} — open it there to keep painting from it.\n\nCome say hi at https://laklok.com.`,
    tags: ["aesthetic.computer", "painting", "pixelart", "nopaint", paintingTag(code)],
    symbol: "OBJKT",
    artifactUri: uris.painting,
    displayUri: uris.display,
    thumbnailUri: uris.thumb,
    creators: [creator],
    formats: [
      { uri: uris.painting, mimeType: "image/png", fileName: `${code}.png`, dimensions: px(width, height) },
      { uri: uris.display, mimeType: "image/png", fileName: `${code}-display.png`, dimensions: px(...display) },
      { uri: uris.thumb, mimeType: "image/png", fileName: `${code}-thumb.png`, dimensions: px(...thumb) },
    ],
    attributes: [
      { name: "code", value: code },
      { name: "handle", value: `@${handle}` },
      { name: "size", value: `${width}×${height}` },
    ],
    decimals: 0,
    isBooleanAmount: false,
    shouldPreferSymbol: false,
    date: new Date(when).toISOString(),
  };
}

// ── the CLI ──────────────────────────────────────────────────────────────

async function main(argv) {
  const flags = { env: [] };
  const words = [];
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (!a.startsWith("--")) { words.push(a); continue; }
    const k = a.slice(2), nx = argv[i + 1];
    const v = nx !== undefined && !nx.startsWith("--") ? (i++, nx) : true;
    if (k === "env") flags.env.push(v); else flags[k] = v;
  }
  const command = words[0] || "status";

  const vaultEnv = resolve(REPO, "aesthetic-computer-vault", "tezos", "kidlisp", ".env");
  for (const file of [...flags.env, ...(flags.env.length ? [] : [vaultEnv])]) {
    if (!existsSync(file)) continue;
    for (const line of readFileSync(file, "utf8").split("\n")) {
      const m = line.match(/^\s*(?:export\s+)?([A-Z0-9_]+)\s*=\s*"?([^"\n]*)"?\s*$/);
      if (m && process.env[m[1]] === undefined) process.env[m[1]] = m[2];
    }
  }

  const HANDLE = String(flags.handle || process.env.HEN_PAINTING_HANDLE || "jeffrey").replace(/^@/, "");
  const AC = process.env.AC_URL || "https://aesthetic.computer";
  const RPC = process.env.TEZOS_RPC || "https://rpc.tzkt.io/mainnet";
  const EDITIONS = Number(flags.editions || process.env.HEN_PAINTING_EDITIONS || 1);
  const PRICE_XTZ = Number(flags.price || process.env.HEN_PAINTING_PRICE_XTZ || 5);
  const ROYALTIES = Number(process.env.HEN_PAINTING_ROYALTIES_PERMILLE || 150);
  const LIST = !flags["no-list"] && process.env.HEN_PAINTING_LIST !== "0";
  const DRY = Boolean(flags.dry);

  const outDir = resolve(HERE, "out", "hen-paintings");
  mkdirSync(outDir, { recursive: true });
  const watermarkPath = resolve(outDir, ".watermark");
  const receiptPath = (code) => resolve(outDir, `${code}.token.json`);
  const readJson = (p) => JSON.parse(readFileSync(p, "utf8"));
  const log = (...a) => console.log(new Date().toISOString().slice(11, 19), ...a);

  const getJson = async (url, init) => {
    const r = await fetch(url, { signal: AbortSignal.timeout(60000), ...init });
    if (!r.ok) throw new Error(`${r.status} ${url}`);
    return r.json();
  };

  // ── 1. what the handle has finished ──────────────────────────────────
  const collection = await getJson(`${AC}/media-collection?for=${encodeURIComponent(`@${HANDLE}/painting`)}`);
  const paintings = paintingsFromCollection(collection.files || []);
  if (!paintings.length) { console.error(`✗ no finished paintings for @${HANDLE}`); return 1; }

  // Local receipts and the chain both know what is already minted.
  const minted = new Map(); // code → { tokenId, source }
  for (const f of readdirSync(outDir)) {
    if (!f.endsWith(".token.json")) continue;
    const r = readJson(resolve(outDir, f));
    if (r.tokenId !== undefined) minted.set(r.code, { tokenId: r.tokenId, source: "receipt", listed: r.listed });
  }
  const onChain = await getJson(`${TZKT}/tokens?contract=${HEN_OBJKTS}&firstMinter=${SIGNER}&select=tokenId,metadata.tags&limit=10000`);
  for (const t of onChain) {
    for (const tag of t["metadata.tags"] || []) {
      const m = /^ac-painting-([A-Za-z0-9]+)$/.exec(tag);
      if (m && !minted.has(m[1])) minted.set(m[1], { tokenId: t.tokenId, source: "chain" });
    }
  }

  // Codes come from /api/painting-metadata (slug → code); dates from /api/get-painting.
  async function describe(p) {
    const meta = await getJson(`${AC}/api/painting-metadata?slug=${encodeURIComponent(p.slug)}&handle=${HANDLE}`);
    if (!meta.code) throw new Error(`no code for ${p.slug}`);
    const info = await getJson(`${AC}/api/get-painting?code=${meta.code}`);
    if (info.nuked) return null;
    return { ...p, code: meta.code, fullSlug: meta.slug, when: info.when, user: info.user };
  }

  const watermark = existsSync(watermarkPath) ? readFileSync(watermarkPath, "utf8").trim() : null;
  const newerThanWatermark = (p) => !watermark || slugKey(p.slug).localeCompare(slugKey(watermark)) > 0;

  if (command === "watermark") {
    writeFileSync(watermarkPath, paintings[0].slug + "\n");
    log(`✓ watermark set to ${paintings[0].slug}; only paintings finished after it are auto-minted by poll`);
    return 0;
  }

  if (command === "status") {
    const n = Number(flags.last || 8);
    console.log(`@${HANDLE}: ${paintings.length} finished paintings; ${minted.size} minted on hic et nunc as aesthetic.tez; watermark ${watermark || "(none — run watermark before installing poll)"}`);
    for (const p of paintings.slice(0, n)) {
      let d = null;
      try { d = await describe(p); } catch (err) { console.log(`  ${p.slug}  ✗ ${err.message}`); continue; }
      if (!d) { console.log(`  ${p.slug}  (nuked)`); continue; }
      const m = minted.get(d.code);
      console.log(`  ${d.slug}  #${d.code}  ${m ? `✓ OBJKT #${m.tokenId}` : newerThanWatermark(p) ? "· unminted (in queue)" : "· unminted (below watermark)"}`);
    }
    return 0;
  }

  if (command !== "mint" && command !== "poll") { console.error(`✗ unknown command ${command}`); return 1; }

  // ── 2. which paintings this run takes ────────────────────────────────
  let queue = [];
  if (command === "poll") {
    if (!watermark) { console.error("✗ no watermark: run `hen-paintings.mjs watermark` first so poll never mints the back catalogue"); return 1; }
    queue = paintings.filter(newerThanWatermark);
  } else if (flags.code) {
    const want = String(flags.code).replace(/^#/, "");
    for (const p of paintings) {
      const d = await describe(p).catch(() => null);
      if (d?.code === want) { queue = [p]; break; }
    }
    if (!queue.length) { console.error(`✗ no finished painting #${want} for @${HANDLE}`); return 1; }
  } else {
    const n = Number(flags.last || 3);
    for (const p of paintings) {
      if (queue.length >= n) break;
      const d = await describe(p).catch(() => null);
      if (d && !minted.has(d.code)) queue.push(p);
    }
  }
  const described = (await Promise.all(queue.map((p) => describe(p)))).filter(Boolean).filter((d) => !minted.has(d.code));
  if (!described.length) { log(command === "poll" ? "· nothing new since the watermark" : "· nothing to mint"); return 0; }
  log(`${DRY ? "dry run: " : ""}${described.length} painting(s): ${described.map((d) => `#${d.code}`).join(" ")}`);

  // ── 3. the chain and the session, once ───────────────────────────────
  let tezos, acToken;
  if (!DRY) {
    const { TezosToolkit } = requireTezos("@taquito/taquito");
    const { InMemorySigner } = requireTezos("@taquito/signer");
    if (!process.env.AESTHETIC_KEY) { console.error("✗ AESTHETIC_KEY not set (decrypt the vault or pass --env)"); return 1; }
    tezos = new TezosToolkit(RPC);
    tezos.setProvider({ signer: await InMemorySigner.fromSecretKey(process.env.AESTHETIC_KEY) });
    const who = await tezos.signer.publicKeyHash();
    if (who !== SIGNER) { console.error(`✗ key belongs to ${who}, not aesthetic.tez; refusing`); return 1; }
    const balance = (await tezos.tz.getBalance(SIGNER)).toNumber() / 1e6;
    if (balance < MIN_BALANCE_XTZ * described.length) { console.error(`✗ aesthetic.tez holds ${balance} XTZ; fund it before minting ${described.length}`); return 1; }

    acToken = process.env.AC_TOKEN;
    if (!acToken) {
      const file = resolve(homedir(), ".ac-token");
      if (!existsSync(file)) { console.error("✗ no AC session: sign in with node tezos/ac-login.mjs as @jeffrey"); return 1; }
      const record = readJson(file);
      if (record.expires_at && Date.now() >= record.expires_at - 60_000) {
        const { freshSession } = await import(resolve(REPO, "shared", "ac-token.mjs"));
        acToken = (await freshSession({ file })).access_token;
      } else acToken = record.access_token;
    }
  }
  const pin = async (payload) => {
    const r = await fetch(`${AC}/api/ipfs-add`, {
      method: "POST", headers: { authorization: `Bearer ${acToken}`, "content-type": "application/json" },
      body: JSON.stringify(payload), signal: AbortSignal.timeout(180000),
    });
    const j = await r.json().catch(() => ({}));
    if (!r.ok || !j.uri) throw new Error(`ipfs-add ${r.status}: ${JSON.stringify(j).slice(0, 200)}`);
    return j.uri;
  };
  const tzkt = (path) => getJson(`${TZKT}${path}`);
  const { PNG } = requireTezos("pngjs");

  // ── 4. each painting: fetch → pin → mint → list ──────────────────────
  let failures = 0;
  for (const d of described) {
    const path = receiptPath(d.code);
    const receipt = existsSync(path) ? readJson(path) : { code: d.code, slug: d.fullSlug, handle: HANDLE, when: d.when, url: d.url };
    const save = () => { if (!DRY) writeFileSync(path, JSON.stringify(receipt, null, 2) + "\n"); };
    try {
      if (receipt.listed || (receipt.tokenId !== undefined && !LIST)) { log(`✓ #${d.code} already done: ${receipt.objktUrl}`); continue; }

      // fetch + upscale
      const png = PNG.sync.read(Buffer.from(await (await fetch(d.url, { redirect: "follow", signal: AbortSignal.timeout(60000) })).arrayBuffer()));
      const scaled = (target) => {
        const f = upscaleFactor(png.width, png.height, target);
        const out = new PNG({ width: png.width * f, height: png.height * f });
        out.data = nearestNeighbour(png.data, png.width, png.height, f);
        return { buffer: PNG.sync.write(out), size: [out.width, out.height] };
      };
      const display = scaled(1024), thumb = scaled(350);
      const original = PNG.sync.write(png);
      Object.assign(receipt, { width: png.width, height: png.height, display: display.size, thumb: thumb.size });

      if (DRY) {
        const uris = { painting: "ipfs://<painting>", display: "ipfs://<display>", thumb: "ipfs://<thumb>" };
        const metadata = tokenMetadata({ code: d.code, handle: HANDLE, when: d.when, width: png.width, height: png.height, uris, display: display.size, thumb: thumb.size, ac: AC });
        writeFileSync(resolve(outDir, `${d.code}.dry.json`), JSON.stringify({ receipt, metadata }, null, 2) + "\n");
        log(`✓ #${d.code} ${png.width}×${png.height} → display ${display.size.join("×")}, thumb ${thumb.size.join("×")}; metadata in out/hen-paintings/${d.code}.dry.json`);
        continue;
      }

      // pin
      if (!receipt.metadataUri) {
        const uris = {
          painting: await pin({ name: `${d.code}.png`, mimeType: "image/png", base64: original.toString("base64") }),
          display: await pin({ name: `${d.code}-display.png`, mimeType: "image/png", base64: display.buffer.toString("base64") }),
          thumb: await pin({ name: `${d.code}-thumb.png`, mimeType: "image/png", base64: thumb.buffer.toString("base64") }),
        };
        const metadata = tokenMetadata({ code: d.code, handle: HANDLE, when: d.when, width: png.width, height: png.height, uris, display: display.size, thumb: thumb.size, ac: AC });
        receipt.metadataUri = await pin({ name: `${d.code}.json`, json: metadata });
        Object.assign(receipt, { artifactUri: uris.painting, displayUri: uris.display, thumbnailUri: uris.thumb });
        save();
        log(`  ✓ #${d.code} pinned ${receipt.metadataUri}`);
      }

      // mint
      if (receipt.tokenId === undefined) {
        if (!receipt.mintOp) {
          const minter = await tezos.contract.at(HEN_MINTER);
          const op = await minter.methodsObject.mint_OBJKT({
            address: SIGNER, amount: String(EDITIONS),
            metadata: Buffer.from(receipt.metadataUri).toString("hex"), royalties: String(ROYALTIES),
          }).send();
          receipt.mintOp = op.hash; receipt.editions = EDITIONS; save();
          log(`  minting #${d.code}… ${op.hash}`);
          await op.confirmation(1);
        }
        for (let i = 0; i < 30 && receipt.tokenId === undefined; i++) {
          const txs = (await tzkt(`/operations/transactions/${receipt.mintOp}`)) || [];
          const failed = txs.find((tx) => tx.status !== "applied");
          if (failed) throw new Error(`mint ${failed.status}`);
          const ids = txs.map((tx) => tx.id).join(",");
          const [t] = ids ? await tzkt(`/tokens/transfers?transactionId.in=${ids}&token.contract=${HEN_OBJKTS}`) : [];
          if (t) { receipt.tokenId = t.token.tokenId; receipt.objktUrl = `https://objkt.com/tokens/hicetnunc/${receipt.tokenId}`; save(); break; }
          await new Promise((r) => setTimeout(r, 10000));
        }
        if (receipt.tokenId === undefined) throw new Error("mint confirmed but the indexer hasn't shown the token yet; rerun to resume");
        log(`  ✓ #${d.code} minted OBJKT #${receipt.tokenId} ×${EDITIONS}`);
      }

      // list
      if (LIST && !receipt.listed) {
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
            start_time: null, expiry_time: null, referral_bonus: "500", condition: null,
          }))
          .send();
        await op.confirmation(1);
        Object.assign(receipt, { listOp: op.hash, listed: true, priceXtz: PRICE_XTZ });
        save();
      }
      log(`✓ #${d.code}: OBJKT #${receipt.tokenId}${LIST ? ` — ${EDITIONS} × ${PRICE_XTZ} XTZ` : " (unlisted)"} — ${receipt.objktUrl}`);
    } catch (err) {
      failures++;
      receipt.error = { at: new Date().toISOString(), message: err.message }; save();
      console.error(`✗ #${d.code}: ${err.message}`);
    }
  }

  // The watermark moves past everything this run handled, so a painting that
  // failed mid-way is retried by its receipt, not re-queued forever.
  if (!DRY && !failures) {
    const newest = described.sort(bySlugDesc)[0].slug;
    if (!watermark || slugKey(newest).localeCompare(slugKey(watermark)) > 0) writeFileSync(watermarkPath, newest + "\n");
  }
  return failures ? 1 : 0;
}

const invoked = process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (invoked) main(process.argv.slice(2)).then((code) => process.exit(code), (err) => { console.error(`✗ ${err.message}`); process.exit(1); });
