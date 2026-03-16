#!/usr/bin/env node
// regenerate-media.mjs — Regenerate media (bundle + thumbnail) for minted KidLisp Keeps
//
// Single token:  node kidlisp-cli/regenerate-media.mjs $cow
// Batch (all):   node kidlisp-cli/regenerate-media.mjs --all
// Dry run:       node kidlisp-cli/regenerate-media.mjs --all --dry-run
// Custom API:    node kidlisp-cli/regenerate-media.mjs $cow --api https://localhost:8888
// Custom contract: node kidlisp-cli/regenerate-media.mjs --all --contract KT1Q1ir...
//
// Flow per token:
//   1. Verify token exists on-chain (TzKT lookup)
//   2. Rebake  → POST /api/keep-mint { piece, regenerate: true, walletAddress }  (SSE)
//   3. Update  → POST /api/keep-update { piece, tokenId, artifactUri, thumbnailUri } (SSE)
//   4. Confirm → POST /api/keep-update-confirm { piece, tokenId, txHash, ... }

import fs from "fs";
import path from "path";
import os from "os";
import { execFileSync } from "child_process";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const AC_LOGIN = path.join(__dirname, "..", "tezos", "ac-login.mjs");

// ── CLI args ──
const args = process.argv.slice(2);

function flag(name) {
  return args.includes(`--${name}`);
}

function opt(name, fallback) {
  const i = args.indexOf(`--${name}`);
  return i >= 0 && args[i + 1] ? args[i + 1] : fallback;
}

const DRY_RUN = flag("dry-run");
const ALL = flag("all");
const SKIP_UPDATE = flag("skip-update");
const API_BASE = opt("api", "https://aesthetic.computer").replace(/\/$/, "");
const CONTRACT_OVERRIDE = opt("contract", null);
const CREATOR_FILTER = opt("creator", null);
const DELAY_MS = parseInt(opt("delay", "2000"), 10);

// Piece names from positional args (skip flags and flag values)
const flagsWithValues = new Set(["api", "contract", "creator", "delay"]);
const pieces = [];
for (let i = 0; i < args.length; i++) {
  const a = args[i];
  if (a.startsWith("--")) {
    const name = a.slice(2);
    if (flagsWithValues.has(name)) i++; // skip next arg (the value)
    continue;
  }
  pieces.push(a);
}

if (!ALL && pieces.length === 0) {
  console.log(`
  regenerate-media — Regenerate media for minted KidLisp Keeps

  Usage:
    node kidlisp-cli/regenerate-media.mjs \\$cow              # Single piece
    node kidlisp-cli/regenerate-media.mjs \\$cow \\$sun         # Multiple pieces
    node kidlisp-cli/regenerate-media.mjs --all               # All tokens on contract
    node kidlisp-cli/regenerate-media.mjs --all --creator tz1... # Filter by minter

  Flags:
    --dry-run       List what would be regenerated, don't execute
    --skip-update   Rebake media only, don't update on-chain metadata
    --api URL       API base URL (default: https://aesthetic.computer)
    --contract KT1  Override contract address
    --creator tz1   Filter --all by original minter address
    --delay MS      Delay between tokens in ms (default: 2000)
    --tezos-key     Tezos private key for signing (or TEZOS_PRIVATE_KEY env / ~/.ac-tezos-key)
  `);
  process.exit(0);
}

// ── Logging ──
function log(prefix, msg) {
  const ts = new Date().toISOString().slice(11, 23);
  console.log(`  ${ts} [${prefix}] ${msg}`);
}

function logInline(prefix, msg) {
  const ts = new Date().toISOString().slice(11, 23);
  process.stdout.write(`\r  ${ts} [${prefix}] ${msg.padEnd(65)}`);
}

// ── Auth (auto-login if needed) ──
const authPath = path.join(os.homedir(), ".ac-token");

function readToken() {
  if (!fs.existsSync(authPath)) return null;
  try {
    const data = JSON.parse(fs.readFileSync(authPath, "utf8"));
    if (data.expires_at && Date.now() > data.expires_at) {
      log("auth", "Token expired — need to re-login");
      return null;
    }
    return data;
  } catch {
    return null;
  }
}

let auth = readToken();

if (!auth) {
  console.log("\n🔐 Not logged in — launching ac-login...\n");
  try {
    execFileSync("node", [AC_LOGIN], { stdio: "inherit" });
  } catch {
    console.error("❌ Login failed or was cancelled.");
    process.exit(1);
  }
  auth = readToken();
  if (!auth) {
    console.error("❌ Still no valid token after login.");
    process.exit(1);
  }
  console.log();
}

const TOKEN = auth.access_token;
const USER_DISPLAY =
  auth.user?.handle ? `@${auth.user.handle}` : auth.user?.email || "unknown";
log("auth", `Logged in as ${USER_DISPLAY}`);

// Allow self-signed certs for localhost
if (API_BASE.includes("localhost")) {
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
}

// ── SSE stream parser ──
async function consumeSSE(response, handlers = {}) {
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let buffer = "";
  let currentEvent = "message";

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value, { stream: true });
    const lines = buffer.split("\n");
    buffer = lines.pop() || "";

    for (const line of lines) {
      if (line.startsWith("event: ")) {
        currentEvent = line.slice(7).trim();
        continue;
      }
      if (line.startsWith("data: ")) {
        try {
          const data = JSON.parse(line.slice(6));
          const handler = handlers[currentEvent];
          if (handler) handler(data);
          else if (handlers._default) handlers._default(currentEvent, data);
        } catch {
          // Partial JSON, ignore
        }
      }
    }
  }
}

// ── Fetch contract config ──
async function getContractConfig() {
  if (CONTRACT_OVERRIDE) {
    log("config", `Using override contract: ${CONTRACT_OVERRIDE}`);
    return { contractAddress: CONTRACT_OVERRIDE, profile: "override" };
  }

  log("config", `Fetching keeps-config from ${API_BASE}...`);
  try {
    const res = await fetch(`${API_BASE}/api/keeps-config`);
    const config = await res.json();
    log(
      "config",
      `Contract: ${config.contractAddress} (${config.profile} ${config.version})`,
    );
    log("config", `Network: ${config.network} | RPC: ${config.rpcUrl}`);
    return config;
  } catch (e) {
    log("config", `⚠️  Could not fetch keeps-config: ${e.message}`);
    log("config", `Falling back to hardcoded v11 mainnet contract`);
    return {
      contractAddress: "KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB",
      profile: "v11-fallback",
    };
  }
}

// ── Look up a single token by piece name on TzKT ──
async function lookupToken(contractAddress, pieceName) {
  const piece = pieceName.replace(/^\$/, "");
  const tokenName = `$${piece}`;

  log("tzkt", `Looking up "${tokenName}" on ${contractAddress.slice(0, 10)}...`);

  const url = `https://api.tzkt.io/v1/tokens?contract=${contractAddress}&metadata.name=${encodeURIComponent(tokenName)}&select=tokenId,metadata.name,firstMinter.address`;
  const res = await fetch(url);
  if (!res.ok) throw new Error(`TzKT lookup failed: ${res.status}`);
  const tokens = await res.json();

  if (tokens.length === 0) {
    throw new Error(
      `Token "${tokenName}" not found on contract ${contractAddress}`,
    );
  }

  const t = tokens[0];
  const minterAddr = t["firstMinter.address"];
  log(
    "tzkt",
    `Found token #${t.tokenId} "${t["metadata.name"]}" minted by ${minterAddr}`,
  );

  return {
    tokenId: t.tokenId,
    name: t["metadata.name"],
    minter: minterAddr,
  };
}

// ── Enumerate all tokens on contract ──
async function enumerateTokens(contractAddress) {
  log("tzkt", `Enumerating all tokens on ${contractAddress}...`);

  const allTokens = [];
  let offset = 0;
  const limit = 100;

  while (true) {
    const url = `https://api.tzkt.io/v1/tokens?contract=${contractAddress}&limit=${limit}&offset=${offset}&select=tokenId,metadata.name,firstMinter.address`;
    const res = await fetch(url);
    if (!res.ok) throw new Error(`TzKT error: ${res.status}`);
    const batch = await res.json();
    if (batch.length === 0) break;
    allTokens.push(...batch);
    offset += limit;
    log("tzkt", `  Fetched ${allTokens.length} tokens so far...`);
    if (batch.length < limit) break;
  }

  log("tzkt", `Total: ${allTokens.length} tokens on contract`);

  let tokens = allTokens;
  if (CREATOR_FILTER) {
    tokens = allTokens.filter(
      (t) => t["firstMinter.address"] === CREATOR_FILTER,
    );
    log("tzkt", `Filtered to ${tokens.length} by creator ${CREATOR_FILTER}`);
  }

  return tokens.map((t) => ({
    tokenId: t.tokenId,
    name: t["metadata.name"] || `#${t.tokenId}`,
    piece: (t["metadata.name"] || "").replace(/^\$/, ""),
    minter: t["firstMinter.address"],
  }));
}

// ── STEP 1: Rebake media (job-based — same as keeps.kidlisp.com) ──
//
// Uses keep-prepare + keep-status polling instead of keep-mint SSE streaming.
// This is more reliable because the heavy work runs in a Netlify background
// function (15-minute timeout) and we just poll for status.
//
const POLL_INTERVAL_MS = 3000;
const POLL_TIMEOUT_MS = 180000; // 3 minutes max

async function rebakeMedia(piece, walletAddress) {
  log("rebake", `POST /api/keep-prepare — piece=$${piece} regenerate=true`);
  log("rebake", `  walletAddress=${walletAddress || "(none)"}`);

  if (!walletAddress) {
    throw new Error("walletAddress required for rebake (minter's tz1 address)");
  }

  // Step 1a: Create the job
  const startTime = Date.now();
  const prepRes = await fetch(`${API_BASE}/api/keep-prepare`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${TOKEN}`,
    },
    body: JSON.stringify({
      piece: `$${piece}`,
      walletAddress,
      network: "mainnet",
      regenerate: true,
      force: true,
    }),
  });

  if (!prepRes.ok) {
    const text = await prepRes.text();
    let errMsg;
    try { errMsg = JSON.parse(text).error; } catch { errMsg = text.slice(0, 300); }
    throw new Error(`keep-prepare HTTP ${prepRes.status}: ${errMsg}`);
  }

  const prepData = await prepRes.json();

  // Already minted but not regenerating?
  if (prepData.alreadyMinted) {
    throw new Error(`Already minted as token #${prepData.tokenId} — pass regenerate:true`);
  }

  const jobId = prepData.jobId;
  if (!jobId) {
    log("rebake", `Response: ${JSON.stringify(prepData).slice(0, 200)}`);
    throw new Error("No jobId returned from keep-prepare");
  }

  log("rebake", `Job created: ${jobId}`);
  log("rebake", `  status=${prepData.status} stage=${prepData.stage || "pending"}`);

  // Step 1b: Poll keep-status until ready or failed
  const pollStart = Date.now();
  let lastStage = "";

  while (true) {
    const elapsed = ((Date.now() - startTime) / 1000).toFixed(0);

    if (Date.now() - pollStart > POLL_TIMEOUT_MS) {
      throw new Error(`Rebake timed out after ${POLL_TIMEOUT_MS / 1000}s — job may still be running`);
    }

    await new Promise((r) => setTimeout(r, POLL_INTERVAL_MS));

    const statusRes = await fetch(
      `${API_BASE}/api/keep-status?jobId=${encodeURIComponent(jobId)}`,
    );
    if (!statusRes.ok) {
      log("rebake", `⚠️  Poll returned ${statusRes.status}, retrying...`);
      continue;
    }

    const job = await statusRes.json();
    const stage = job.stage || "unknown";
    const status = job.status;

    // Log stage changes
    if (stage !== lastStage) {
      lastStage = stage;
      logInline("rebake", `[${elapsed}s] ${status}: ${stage} — ${job.message || ""}...`);
    }

    if (status === "ready") {
      process.stdout.write("\n");
      const totalElapsed = ((Date.now() - startTime) / 1000).toFixed(1);
      log("rebake", `✓ Ready in ${totalElapsed}s`);
      if (job.artifactUri) log("rebake", `  artifact: ${job.artifactUri.slice(0, 50)}...`);
      if (job.thumbnailUri) log("rebake", `  thumbnail: ${job.thumbnailUri.slice(0, 50)}...`);
      if (job.metadataUri) log("rebake", `  metadata: ${job.metadataUri.slice(0, 50)}...`);
      return {
        artifactUri: job.artifactUri,
        thumbnailUri: job.thumbnailUri,
        metadataUri: job.metadataUri,
        tokenId: job.tokenId,
        piece,
      };
    }

    if (status === "failed") {
      process.stdout.write("\n");
      log("rebake", `✗ FAILED at stage "${stage}": ${job.error || "unknown"}`);
      throw new Error(job.error || `Rebake failed at stage ${stage}`);
    }

    // Still preparing — keep polling
  }
}

// ── STEP 2: Update on-chain metadata ──
//
// Uses keep-update with mode="prepare" to get Michelson params,
// then signs locally with the user's Tezos private key (same flow as
// keeps.kidlisp.com's Beacon wallet signing, but headless).
//
// Private key sources (checked in order):
//   1. --tezos-key CLI flag
//   2. TEZOS_PRIVATE_KEY env var
//   3. ~/.ac-tezos-key file

let _taquitoModules = null;
async function getTaquito() {
  if (!_taquitoModules) {
    const [taquito, signer] = await Promise.all([
      import("@taquito/taquito"),
      import("@taquito/signer"),
    ]);
    _taquitoModules = { TezosToolkit: taquito.TezosToolkit, InMemorySigner: signer.InMemorySigner };
  }
  return _taquitoModules;
}

function getTezosPrivateKey() {
  // CLI flag
  const keyFromFlag = opt("tezos-key", null);
  if (keyFromFlag) return keyFromFlag;

  // Env var
  if (process.env.TEZOS_PRIVATE_KEY) return process.env.TEZOS_PRIVATE_KEY;

  // File
  const keyFile = path.join(os.homedir(), ".ac-tezos-key");
  if (fs.existsSync(keyFile)) {
    return fs.readFileSync(keyFile, "utf8").trim();
  }

  return null;
}

async function updateChain(piece, tokenId, artifactUri, thumbnailUri, walletAddress, contractAddress) {
  // Step 2a: Get Michelson params from keep-update (prepare mode)
  log("update", `POST /api/keep-update — piece=$${piece} tokenId=${tokenId} mode=prepare`);
  log("update", `  artifactUri=${artifactUri?.slice(0, 40)}...`);
  log("update", `  thumbnailUri=${thumbnailUri?.slice(0, 40)}...`);
  log("update", `  walletAddress=${walletAddress}`);

  const startTime = Date.now();
  const response = await fetch(`${API_BASE}/api/keep-update`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${TOKEN}`,
      Accept: "text/event-stream",
    },
    body: JSON.stringify({
      piece: `$${piece}`,
      tokenId,
      artifactUri,
      thumbnailUri,
      walletAddress,
      mode: "prepare", // Returns Michelson params for local signing
    }),
  });

  if (!response.ok) {
    const text = await response.text();
    throw new Error(
      `Update chain HTTP ${response.status}: ${text.slice(0, 300)}`,
    );
  }

  log("update", `SSE stream opened (${response.status})...`);

  let prepared = null;
  let lastError = null;

  await consumeSSE(response, {
    progress(data) {
      const msg = data.message || data.stage || "";
      logInline("update", msg);
    },
    prepared(data) {
      prepared = data;
      process.stdout.write("\n");
      log("update", `✓ Params prepared — ready for signing`);
      if (data.metadataUri)
        log("update", `  new metadataUri: ${data.metadataUri.slice(0, 50)}...`);
    },
    complete(data) {
      // Server-side signing path (shouldn't hit this with mode=prepare)
      prepared = data;
      process.stdout.write("\n");
      log("update", `✓ Complete (server-signed)`);
    },
    error(data) {
      lastError = data.error || "Unknown error";
      process.stdout.write("\n");
      log("update", `✗ ERROR: ${lastError}`);
    },
    _default(event, data) {
      log("update", `  [${event}] ${JSON.stringify(data).slice(0, 120)}`);
    },
  });

  if (lastError) throw new Error(lastError);
  if (!prepared) throw new Error("No prepared params from update stream");

  // Step 2b: Sign and broadcast locally with Taquito
  const privateKey = getTezosPrivateKey();
  if (!privateKey) {
    throw new Error(
      "Tezos private key required for signing. Set TEZOS_PRIVATE_KEY env var, " +
      "pass --tezos-key, or create ~/.ac-tezos-key",
    );
  }

  log("sign", `Signing edit_metadata with local key...`);
  const { TezosToolkit, InMemorySigner } = await getTaquito();

  const rpcUrl = prepared.rpcUrl || "https://mainnet.ecadinfra.com";
  const tezos = new TezosToolkit(rpcUrl);
  const signer = new InMemorySigner(privateKey);
  tezos.setProvider({ signer });

  const signerAddress = await signer.publicKeyHash();
  log("sign", `  Signer: ${signerAddress}`);

  const destination = prepared.contractAddress || contractAddress;
  log("sign", `  Contract: ${destination}`);
  log("sign", `  Entrypoint: ${prepared.entrypoint || "edit_metadata"}`);

  const op = await tezos.contract.transfer({
    to: destination,
    amount: 0,
    parameter: prepared.michelsonParams,
  });

  log("sign", `  Submitted: ${op.hash}`);
  log("sign", `  Waiting for confirmation...`);

  await op.confirmation(1);

  const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
  log("sign", `✓ Confirmed on-chain in ${elapsed}s`);
  log("sign", `  https://tzkt.io/${op.hash}`);

  return {
    opHash: op.hash,
    artifactUri: prepared.artifactUri || artifactUri,
    thumbnailUri: prepared.thumbnailUri || thumbnailUri,
    metadataUri: prepared.metadataUri,
    explorerUrl: `https://tzkt.io/${op.hash}`,
  };
}

// ── STEP 3: Confirm update in MongoDB ──
async function confirmUpdate(
  piece,
  tokenId,
  txHash,
  artifactUri,
  thumbnailUri,
  metadataUri,
  contractAddress,
) {
  log("confirm", `POST /api/keep-update-confirm — $${piece} token #${tokenId}`);
  try {
    const response = await fetch(`${API_BASE}/api/keep-update-confirm`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${TOKEN}`,
      },
      body: JSON.stringify({
        piece: `$${piece}`,
        tokenId,
        txHash,
        artifactUri,
        thumbnailUri,
        metadataUri,
        contractAddress,
      }),
    });

    if (response.ok) {
      log("confirm", `✓ MongoDB updated`);
    } else {
      log("confirm", `⚠️  HTTP ${response.status} (non-fatal)`);
    }
  } catch (e) {
    log("confirm", `⚠️  ${e.message} (non-fatal)`);
  }
}

// ── Process a single token ──
async function processToken(
  pieceName,
  index,
  total,
  contractAddress,
  knownTokenId,
  knownMinter,
) {
  const piece = pieceName.replace(/^\$/, "");
  console.log(
    `\n${"─".repeat(60)}\n[${index + 1}/${total}] 🎨 $${piece}`,
  );

  // Look up on-chain token info if we don't already have it
  let tokenId = knownTokenId;
  let minter = knownMinter;

  if (tokenId == null) {
    try {
      const info = await lookupToken(contractAddress, piece);
      tokenId = info.tokenId;
      minter = info.minter;
    } catch (e) {
      log("lookup", `✗ ${e.message}`);
      return { piece, success: false, error: e.message };
    }
  } else {
    log("info", `Token #${tokenId} | minter: ${minter || "unknown"}`);
  }

  if (DRY_RUN) {
    log("dry-run", `Would rebake + update chain for token #${tokenId}`);
    return { piece, success: true, dryRun: true };
  }

  try {
    // Step 1: Rebake media (pass minter as walletAddress for ownership verification)
    const rebakeResult = await rebakeMedia(piece, minter);

    // Use TzKT tokenId if rebake didn't return one
    const effectiveTokenId = rebakeResult.tokenId || tokenId;

    if (SKIP_UPDATE) {
      log("skip", `--skip-update: not updating on-chain metadata`);
      return {
        piece,
        success: true,
        artifactUri: rebakeResult.artifactUri,
        thumbnailUri: rebakeResult.thumbnailUri,
        skippedUpdate: true,
      };
    }

    // Step 2: Update on-chain metadata (prepare + local signing)
    const updateResult = await updateChain(
      piece,
      effectiveTokenId,
      rebakeResult.artifactUri,
      rebakeResult.thumbnailUri,
      minter, // walletAddress for authorization
      contractAddress,
    );

    // Step 3: Confirm in MongoDB
    if (updateResult.opHash) {
      await confirmUpdate(
        piece,
        effectiveTokenId,
        updateResult.opHash,
        updateResult.artifactUri || rebakeResult.artifactUri,
        updateResult.thumbnailUri || rebakeResult.thumbnailUri,
        updateResult.metadataUri,
        contractAddress,
      );
    }

    log("done", `✅ $${piece} fully regenerated`);
    return {
      piece,
      success: true,
      tokenId: effectiveTokenId,
      opHash: updateResult.opHash,
      artifactUri: rebakeResult.artifactUri,
      thumbnailUri: rebakeResult.thumbnailUri,
    };
  } catch (err) {
    log("error", `❌ $${piece} failed: ${err.message}`);
    return { piece, success: false, error: err.message };
  }
}

// ── Main ──
async function main() {
  console.log(`\n🔄 regenerate-media\n`);
  const config = await getContractConfig();
  const contractAddress = config.contractAddress;

  let tokenList;

  if (ALL) {
    const tokens = await enumerateTokens(contractAddress);
    tokenList = tokens
      .filter((t) => t.piece)
      .map((t) => ({
        piece: t.piece,
        tokenId: t.tokenId,
        minter: t.minter,
      }));
  } else {
    // For named pieces, we'll look them up individually
    tokenList = pieces.map((p) => ({
      piece: p.replace(/^\$/, ""),
      tokenId: null,
      minter: null,
    }));
  }

  if (tokenList.length === 0) {
    console.log("No tokens to process.");
    process.exit(0);
  }

  console.log(`\n${"═".repeat(60)}`);
  console.log(
    `  ${tokenList.length} token${tokenList.length === 1 ? "" : "s"} to regenerate`,
  );
  if (DRY_RUN) console.log("  🏜️  DRY RUN — no changes will be made");
  if (SKIP_UPDATE) console.log("  ⏭️  SKIP UPDATE — rebake only, no chain tx");
  console.log(`  API: ${API_BASE}`);
  console.log(`  Contract: ${contractAddress}`);
  console.log(`${"═".repeat(60)}`);

  const results = { success: 0, failed: 0 };
  const failures = [];
  const successes = [];
  const startTime = Date.now();

  for (let i = 0; i < tokenList.length; i++) {
    const t = tokenList[i];
    const result = await processToken(
      t.piece,
      i,
      tokenList.length,
      contractAddress,
      t.tokenId,
      t.minter,
    );

    if (result.success) {
      results.success++;
      if (!result.dryRun) successes.push(result);
    } else {
      results.failed++;
      failures.push({ piece: result.piece, error: result.error });
    }

    // Delay between tokens
    if (i < tokenList.length - 1 && !DRY_RUN) {
      log("wait", `${DELAY_MS}ms delay before next token...`);
      await new Promise((r) => setTimeout(r, DELAY_MS));
    }
  }

  const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
  console.log(`\n${"═".repeat(60)}`);
  console.log(`  Finished in ${elapsed}s`);
  console.log(`  ✅ Success: ${results.success}`);
  if (successes.length > 0) {
    for (const s of successes) {
      console.log(
        `     $${s.piece} → token #${s.tokenId || "?"}${s.opHash ? " tx:" + s.opHash.slice(0, 12) + "..." : ""}`,
      );
    }
  }
  if (results.failed > 0) {
    console.log(`  ❌ Failed:  ${results.failed}`);
    for (const f of failures) {
      console.log(`     $${f.piece}: ${f.error}`);
    }
  }
  console.log(`${"═".repeat(60)}\n`);

  process.exit(results.failed > 0 ? 1 : 0);
}

main().catch((err) => {
  console.error("\nFatal:", err);
  process.exit(1);
});
