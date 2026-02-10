#!/usr/bin/env node
/**
 * keep-cli.mjs - Interactive CLI for keeping KidLisp pieces on Tezos
 * 
 * Features:
 * - Shows AC user info and wallet status
 * - Previews the piece and artifact
 * - Uploads to IPFS with progress
 * - Connects to Temple via Beacon P2P
 * - Resumes from any stage if interrupted
 * 
 * Usage:
 *   node keep-cli.mjs <piece-code>
 *   node keep-cli.mjs puf
 *   node keep-cli.mjs puf --resume ipfs    # Resume from IPFS upload
 *   node keep-cli.mjs puf --preview        # Preview artifact only
 */

import { pairWallet, sendContractCall } from "./beacon-node.mjs";
import { connect } from "../system/backend/database.mjs";
import { analyzeKidLisp, ANALYZER_VERSION } from "../system/backend/kidlisp-analyzer.mjs";
import { TezosToolkit } from "@taquito/taquito";
import * as fs from "fs";
import * as path from "path";
import * as readline from "readline";

// ═══════════════════════════════════════════════════════════════════
// Configuration
// ═══════════════════════════════════════════════════════════════════

const CONTRACT_ADDRESS = "KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K"; // Ghostnet v3
const NETWORK = "ghostnet";
const RPC_URL = "https://ghostnet.ecadinfra.com";
const OVEN_URL = process.env.OVEN_URL || "https://oven.aesthetic.computer";
const MINT_FEE = "5000000"; // 5 XTZ in mutez

// State file for resume functionality
const STATE_DIR = "/tmp/keep-cli";
const getStateFile = (code) => `${STATE_DIR}/${code}.json`;

// ═══════════════════════════════════════════════════════════════════
// Colors & Formatting
// ═══════════════════════════════════════════════════════════════════

const RESET = "\x1b[0m";
const BOLD = "\x1b[1m";
const DIM = "\x1b[2m";
const ITALIC = "\x1b[3m";
const UNDERLINE = "\x1b[4m";

const BLACK = "\x1b[30m";
const RED = "\x1b[31m";
const GREEN = "\x1b[32m";
const YELLOW = "\x1b[33m";
const BLUE = "\x1b[34m";
const MAGENTA = "\x1b[35m";
const CYAN = "\x1b[36m";
const WHITE = "\x1b[37m";

const BG_BLACK = "\x1b[40m";
const BG_RED = "\x1b[41m";
const BG_GREEN = "\x1b[42m";
const BG_YELLOW = "\x1b[43m";
const BG_BLUE = "\x1b[44m";
const BG_MAGENTA = "\x1b[45m";
const BG_CYAN = "\x1b[46m";
const BG_WHITE = "\x1b[47m";

function box(title, content, color = CYAN) {
  const width = 66;
  const top = `${color}╔${"═".repeat(width)}╗${RESET}`;
  const bottom = `${color}╚${"═".repeat(width)}╝${RESET}`;
  const titleLine = `${color}║${RESET}  ${BOLD}${title}${RESET}${" ".repeat(width - title.length - 2)}${color}║${RESET}`;
  console.log(top);
  console.log(titleLine);
  if (content) {
    for (const line of content.split("\n")) {
      const paddedLine = line.padEnd(width - 2);
      console.log(`${color}║${RESET}  ${paddedLine}${color}║${RESET}`);
    }
  }
  console.log(bottom);
}

function section(title) {
  console.log(`\n${BOLD}${CYAN}▸ ${title}${RESET}\n`);
}

function status(icon, message, detail = "") {
  const detailStr = detail ? `${DIM} ${detail}${RESET}` : "";
  console.log(`  ${icon} ${message}${detailStr}`);
}

function progress(current, total, label) {
  const width = 40;
  const filled = Math.round((current / total) * width);
  const bar = "█".repeat(filled) + "░".repeat(width - filled);
  const pct = Math.round((current / total) * 100);
  process.stdout.write(`\r  ${CYAN}${bar}${RESET} ${pct}% ${DIM}${label}${RESET}`);
  if (current === total) console.log();
}

function clearLine() {
  process.stdout.write("\r" + " ".repeat(80) + "\r");
}

// ═══════════════════════════════════════════════════════════════════
// State Management (for resume)
// ═══════════════════════════════════════════════════════════════════

function loadState(code) {
  try {
    const stateFile = getStateFile(code);
    if (fs.existsSync(stateFile)) {
      return JSON.parse(fs.readFileSync(stateFile, "utf8"));
    }
  } catch (e) {
    // Ignore errors
  }
  return null;
}

function saveState(code, state) {
  try {
    if (!fs.existsSync(STATE_DIR)) {
      fs.mkdirSync(STATE_DIR, { recursive: true });
    }
    fs.writeFileSync(getStateFile(code), JSON.stringify(state, null, 2));
  } catch (e) {
    console.error(`${RED}Warning: Could not save state: ${e.message}${RESET}`);
  }
}

function clearState(code) {
  try {
    const stateFile = getStateFile(code);
    if (fs.existsSync(stateFile)) {
      fs.unlinkSync(stateFile);
    }
  } catch (e) {
    // Ignore
  }
}

// ═══════════════════════════════════════════════════════════════════
// Database & User Info
// ═══════════════════════════════════════════════════════════════════

let db = null;

async function initDB() {
  if (!db) {
    const conn = await connect();
    db = conn.db;
  }
  return db;
}

async function getUserByHandle(handle) {
  const db = await initDB();
  return db.collection("@handles").findOne({ _id: handle.replace(/^@/, "") });
}

async function getUserById(userId) {
  const db = await initDB();
  return db.collection("users").findOne({ _id: userId });
}

async function getPiece(code) {
  const db = await initDB();
  // KidLisp pieces are in the 'kidlisp' collection, stored by 'code'
  const piece = await db.collection("kidlisp").findOne({ code: code });
  return piece;
}

async function getPinataCredentials() {
  const db = await initDB();
  const secrets = await db.collection("secrets").findOne({ _id: "pinata" });
  if (!secrets) throw new Error("Pinata credentials not found");
  return { jwt: secrets.jwt };
}

// ═══════════════════════════════════════════════════════════════════
// Tezos Helpers
// ═══════════════════════════════════════════════════════════════════

function stringToBytes(str) {
  return Buffer.from(str, "utf8").toString("hex");
}

async function checkMintStatus(pieceName) {
  const keyBytes = stringToBytes(pieceName);
  const url = `https://api.${NETWORK}.tzkt.io/v1/contracts/${CONTRACT_ADDRESS}/bigmaps/content_hashes/keys/${keyBytes}`;
  
  try {
    const response = await fetch(url);
    if (response.status === 200) {
      const data = await response.json();
      if (data.active) {
        return { 
          minted: true, 
          tokenId: data.value,
          objktUrl: `https://ghostnet.objkt.com/asset/${CONTRACT_ADDRESS}/${data.value}`,
        };
      }
    }
    return { minted: false };
  } catch (e) {
    return { minted: false };
  }
}

async function getWalletBalance(address) {
  try {
    const tezos = new TezosToolkit(RPC_URL);
    const balance = await tezos.tz.getBalance(address);
    return balance.toNumber() / 1000000;
  } catch (e) {
    return null;
  }
}

// ═══════════════════════════════════════════════════════════════════
// IPFS Upload
// ═══════════════════════════════════════════════════════════════════

async function uploadToIPFS(content, name, pinata) {
  const formData = new FormData();
  
  // Create blob from content
  const blob = new Blob([content], { type: "text/html" });
  formData.append("file", blob, name);
  
  // Add metadata
  formData.append("pinataMetadata", JSON.stringify({
    name: `keep-${name}`,
    keyvalues: { type: "keep-artifact" }
  }));
  
  const response = await fetch("https://api.pinata.cloud/pinning/pinFileToIPFS", {
    method: "POST",
    headers: {
      "Authorization": `Bearer ${pinata.jwt}`,
    },
    body: formData,
  });
  
  if (!response.ok) {
    throw new Error(`Pinata upload failed: ${response.status}`);
  }
  
  const result = await response.json();
  return {
    cid: result.IpfsHash,
    url: `ipfs://${result.IpfsHash}`,
    gateway: `https://gateway.pinata.cloud/ipfs/${result.IpfsHash}`,
  };
}

async function uploadJSONToIPFS(data, name, pinata) {
  const response = await fetch("https://api.pinata.cloud/pinning/pinJSONToIPFS", {
    method: "POST",
    headers: {
      "Authorization": `Bearer ${pinata.jwt}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      pinataContent: data,
      pinataMetadata: { name: `keep-${name}-metadata` },
    }),
  });
  
  if (!response.ok) {
    throw new Error(`Pinata JSON upload failed: ${response.status}`);
  }
  
  const result = await response.json();
  return {
    cid: result.IpfsHash,
    url: `ipfs://${result.IpfsHash}`,
    gateway: `https://gateway.pinata.cloud/ipfs/${result.IpfsHash}`,
  };
}

// ═══════════════════════════════════════════════════════════════════
// Artifact Generation
// ═══════════════════════════════════════════════════════════════════

const dev = process.env.NODE_ENV !== "production";
const BUNDLE_BASE = dev ? "https://localhost:8888/api" : "https://aesthetic.computer/api";

async function generateArtifact(code, piece) {
  // Use the local dev server or production bundle endpoint
  const bundleUrl = `${BUNDLE_BASE}/bundle-html?code=${encodeURIComponent(code)}&format=json`;
  
  status("📦", "Generating artifact bundle...", bundleUrl);
  
  // Allow self-signed certs in dev
  if (dev) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
  
  const response = await fetch(bundleUrl);
  if (!response.ok) {
    throw new Error(`Bundle generation failed: ${response.status}`);
  }
  
  const data = await response.json();
  if (data.error) {
    throw new Error(`Bundle error: ${data.error}`);
  }
  
  const html = Buffer.from(data.content, "base64").toString("utf8");
  return html;
}

async function generateThumbnail(code) {
  const thumbUrl = `${OVEN_URL}/thumbnail?code=${encodeURIComponent(code)}&width=512&height=512`;
  
  status("🖼️ ", "Generating thumbnail...", thumbUrl);
  
  const response = await fetch(thumbUrl, { timeout: 60000 });
  if (!response.ok) {
    console.log(`${YELLOW}  ⚠ Thumbnail generation failed, using placeholder${RESET}`);
    return null;
  }
  
  return await response.arrayBuffer();
}

// ═══════════════════════════════════════════════════════════════════
// Build Michelson Parameters
// ═══════════════════════════════════════════════════════════════════

function buildKeepParams(metadata) {
  // Build the deeply nested Michelson params for the keep entrypoint
  return {
    prim: "Pair",
    args: [
      { bytes: metadata.artifactUri },
      { prim: "Pair", args: [
        { bytes: metadata.attributes },
        { prim: "Pair", args: [
          { bytes: metadata.content_hash },
          { prim: "Pair", args: [
            { bytes: metadata.content_type },
            { prim: "Pair", args: [
              { bytes: metadata.creators },
              { prim: "Pair", args: [
                { bytes: metadata.decimals },
                { prim: "Pair", args: [
                  { bytes: metadata.description },
                  { prim: "Pair", args: [
                    { bytes: metadata.displayUri },
                    { prim: "Pair", args: [
                      { bytes: metadata.formats },
                      { prim: "Pair", args: [
                        { bytes: metadata.isBooleanAmount },
                        { prim: "Pair", args: [
                          { bytes: metadata.metadata_uri },
                          { prim: "Pair", args: [
                            { bytes: metadata.name },
                            { prim: "Pair", args: [
                              { string: metadata.owner },
                              { prim: "Pair", args: [
                                { bytes: metadata.rights },
                                { prim: "Pair", args: [
                                  { bytes: metadata.shouldPreferSymbol },
                                  { prim: "Pair", args: [
                                    { bytes: metadata.symbol },
                                    { prim: "Pair", args: [
                                      { bytes: metadata.tags },
                                      { bytes: metadata.thumbnailUri }
                                    ]}
                                  ]}
                                ]}
                              ]}
                            ]}
                          ]}
                        ]}
                      ]}
                    ]}
                  ]}
                ]}
              ]}
            ]}
          ]}
        ]}
      ]}
    ]
  };
}

// ═══════════════════════════════════════════════════════════════════
// Interactive Prompts
// ═══════════════════════════════════════════════════════════════════

function prompt(question) {
  return new Promise((resolve) => {
    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });
    rl.question(question, (answer) => {
      rl.close();
      resolve(answer.trim());
    });
  });
}

async function confirm(question) {
  const answer = await prompt(`${question} ${DIM}(y/n)${RESET} `);
  return answer.toLowerCase() === "y" || answer.toLowerCase() === "yes";
}

// ═══════════════════════════════════════════════════════════════════
// Main CLI Flow
// ═══════════════════════════════════════════════════════════════════

async function main() {
  const args = process.argv.slice(2);
  let pieceCode = args.find(a => !a.startsWith("--"));
  const resumeStage = args.includes("--resume") ? args[args.indexOf("--resume") + 1] : null;
  const previewOnly = args.includes("--preview");
  const skipConfirm = args.includes("--yes") || args.includes("-y");
  
  console.clear();
  
  // ─────────────────────────────────────────────────────────────────
  // Header
  // ─────────────────────────────────────────────────────────────────
  
  console.log(`
${MAGENTA}${BOLD}
    ██╗  ██╗███████╗███████╗██████╗ 
    ██║ ██╔╝██╔════╝██╔════╝██╔══██╗
    █████╔╝ █████╗  █████╗  ██████╔╝
    ██╔═██╗ ██╔══╝  ██╔══╝  ██╔═══╝ 
    ██║  ██╗███████╗███████╗██║     
    ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝     
${RESET}
    ${DIM}KidLisp NFT Minting CLI for Aesthetic Computer${RESET}
    ${DIM}Network: ${CYAN}${NETWORK}${RESET} ${DIM}| Contract: ${CYAN}${CONTRACT_ADDRESS.slice(0, 12)}...${RESET}
`);

  // ─────────────────────────────────────────────────────────────────
  // Get piece code
  // ─────────────────────────────────────────────────────────────────
  
  if (!pieceCode) {
    pieceCode = await prompt(`${CYAN}?${RESET} Enter piece code (e.g., ${GREEN}puf${RESET}): `);
    if (!pieceCode) {
      console.log(`${RED}✗ No piece code provided${RESET}\n`);
      process.exit(1);
    }
  }
  
  const cleanCode = pieceCode.replace(/^\$/, "");
  
  // ─────────────────────────────────────────────────────────────────
  // Load saved state (for resume)
  // ─────────────────────────────────────────────────────────────────
  
  let state = loadState(cleanCode) || {
    stage: "init",
    code: cleanCode,
    startedAt: new Date().toISOString(),
  };
  
  if (resumeStage) {
    status("🔄", `Resuming from stage: ${resumeStage}`);
    state.stage = resumeStage;
  }
  
  // ─────────────────────────────────────────────────────────────────
  // Step 1: Load piece from database
  // ─────────────────────────────────────────────────────────────────
  
  section("Piece Information");
  
  status("🔍", "Loading piece from database...");
  
  let piece;
  try {
    piece = await getPiece(cleanCode);
  } catch (e) {
    console.log(`${RED}  ✗ Database error: ${e.message}${RESET}`);
    process.exit(1);
  }
  
  if (!piece) {
    console.log(`${RED}  ✗ Piece not found: $${cleanCode}${RESET}`);
    console.log(`${DIM}    Make sure the piece exists in the database${RESET}\n`);
    process.exit(1);
  }
  
  // Get owner info (KidLisp pieces use 'user' field for owner ID)
  let owner = null;
  let ownerHandle = null;
  if (piece.user) {
    owner = await getUserById(piece.user);
    if (owner?.atproto?.handle) {
      // Handle is stored in atproto.handle (e.g. "jeffrey.at.aesthetic.computer" -> "@jeffrey")
      const fullHandle = owner.atproto.handle;
      ownerHandle = fullHandle.replace('.at.aesthetic.computer', '');
    } else if (owner?.handle) {
      ownerHandle = owner.handle;
    }
  }
  
  console.log();
  box(`📜 $${cleanCode}`, [
    `${BOLD}Name:${RESET}        ${piece.name || cleanCode}`,
    `${BOLD}Owner:${RESET}       ${ownerHandle ? `@${ownerHandle}` : piece.user || "unknown"}`,
    `${BOLD}Created:${RESET}     ${piece.when ? new Date(piece.when).toLocaleDateString() : "unknown"}`,
    `${BOLD}Hits:${RESET}        ${piece.hits || 0}`,
    `${BOLD}Lines:${RESET}       ${piece.source?.split("\\n").length || "?"} lines of KidLisp`,
  ].join("\\n"), GREEN);
  
  // Show code preview (KidLisp uses 'source' field)
  if (piece.source) {
    console.log(`\n${DIM}  Code preview:${RESET}`);
    const lines = piece.source.split("\n").slice(0, 5);
    for (const line of lines) {
      console.log(`${DIM}    ${line.slice(0, 60)}${line.length > 60 ? "..." : ""}${RESET}`);
    }
    if (piece.source.split("\n").length > 5) {
      console.log(`${DIM}    ... (${piece.source.split("\n").length - 5} more lines)${RESET}`);
    }
  }
  
  // ─────────────────────────────────────────────────────────────────
  // Step 2: Check mint status
  // ─────────────────────────────────────────────────────────────────
  
  section("Mint Status");
  
  status("🔎", "Checking if already minted...");
  
  const mintStatus = await checkMintStatus(cleanCode);
  
  if (mintStatus.minted) {
    console.log(`\n${YELLOW}  ⚠ This piece is already minted!${RESET}`);
    console.log(`${DIM}    Token ID: ${mintStatus.tokenId}${RESET}`);
    console.log(`${DIM}    ${mintStatus.objktUrl}${RESET}\n`);
    
    if (!await confirm(`${YELLOW}Continue anyway?${RESET}`)) {
      process.exit(0);
    }
  } else {
    status("✓", "Not yet minted", GREEN);
  }
  
  // ─────────────────────────────────────────────────────────────────
  // Step 3: Connect wallet
  // ─────────────────────────────────────────────────────────────────
  
  section("Wallet Connection");
  
  let walletAddress = state.walletAddress;
  let client = null;
  
  if (!walletAddress || state.stage === "init") {
    status("📱", "Connecting to Temple wallet via Beacon P2P...");
    console.log();
    
    try {
      const pairResult = await pairWallet("Aesthetic Computer Keep");
      
      if (!pairResult?.permissionResponse?.address) {
        throw new Error("Failed to get wallet address");
      }
      
      client = pairResult.client;
      walletAddress = pairResult.permissionResponse.address;
      
      state.walletAddress = walletAddress;
      state.stage = "wallet";
      saveState(cleanCode, state);
      
    } catch (e) {
      console.log(`${RED}  ✗ Wallet connection failed: ${e.message}${RESET}\n`);
      process.exit(1);
    }
  } else {
    status("✓", `Using saved wallet: ${walletAddress.slice(0, 12)}...`, GREEN);
    // Need to reconnect for the transaction
    status("📱", "Reconnecting wallet for transaction...");
    console.log();
    
    try {
      const pairResult = await pairWallet("Aesthetic Computer Keep");
      client = pairResult.client;
      walletAddress = pairResult.permissionResponse.address;
    } catch (e) {
      console.log(`${RED}  ✗ Wallet reconnection failed: ${e.message}${RESET}\n`);
      process.exit(1);
    }
  }
  
  // Get wallet balance
  const balance = await getWalletBalance(walletAddress);
  
  console.log();
  box("💳 Wallet", [
    `${BOLD}Address:${RESET}  ${walletAddress}`,
    `${BOLD}Balance:${RESET}  ${balance !== null ? `${balance.toFixed(2)} ꜩ` : "unknown"}`,
    `${BOLD}Network:${RESET}  ${NETWORK}`,
  ].join("\n"), BLUE);
  
  if (balance !== null && balance < 6) {
    console.log(`\n${YELLOW}  ⚠ Low balance! You need at least 6 ꜩ (5 ꜩ mint fee + gas)${RESET}`);
    console.log(`${DIM}    Get testnet XTZ: https://faucet.ghostnet.teztnets.com${RESET}\n`);
  }
  
  // ─────────────────────────────────────────────────────────────────
  // Step 4: Generate artifact
  // ─────────────────────────────────────────────────────────────────
  
  section("Artifact Generation");
  
  let artifactHtml = state.artifactHtml;
  let thumbnailData = state.thumbnailCid ? { cid: state.thumbnailCid } : null;
  
  if (!artifactHtml || state.stage === "wallet") {
    try {
      artifactHtml = await generateArtifact(cleanCode, piece);
      status("✓", `Bundle generated (${(artifactHtml.length / 1024).toFixed(1)} KB)`, GREEN);
      
      state.artifactHtml = artifactHtml;
      state.stage = "artifact";
      saveState(cleanCode, state);
      
    } catch (e) {
      console.log(`${RED}  ✗ Artifact generation failed: ${e.message}${RESET}\n`);
      process.exit(1);
    }
  } else {
    status("✓", `Using cached artifact (${(artifactHtml.length / 1024).toFixed(1)} KB)`, GREEN);
  }
  
  // Preview option
  if (previewOnly) {
    const previewPath = `/tmp/keep-preview-${cleanCode}.html`;
    fs.writeFileSync(previewPath, artifactHtml);
    console.log(`\n${CYAN}  Preview saved to: ${previewPath}${RESET}`);
    console.log(`${DIM}  Open in browser to test the artifact${RESET}\n`);
    
    const openPreview = await confirm(`${CYAN}Open in browser?${RESET}`);
    if (openPreview) {
      const { exec } = await import("child_process");
      exec(`$BROWSER "file://${previewPath}"`);
    }
    
    if (!await confirm(`${CYAN}Continue with minting?${RESET}`)) {
      process.exit(0);
    }
  }
  
  // ─────────────────────────────────────────────────────────────────
  // Step 5: Upload to IPFS
  // ─────────────────────────────────────────────────────────────────
  
  section("IPFS Upload");
  
  let pinata;
  try {
    pinata = await getPinataCredentials();
    status("✓", "Pinata credentials loaded", GREEN);
  } catch (e) {
    console.log(`${RED}  ✗ Could not load Pinata credentials: ${e.message}${RESET}\n`);
    process.exit(1);
  }
  
  let artifactCid = state.artifactCid;
  let metadataCid = state.metadataCid;
  
  // Upload artifact HTML
  if (!artifactCid || state.stage === "artifact") {
    status("📤", "Uploading artifact to IPFS...");
    
    try {
      const result = await uploadToIPFS(artifactHtml, `${cleanCode}.html`, pinata);
      artifactCid = result.cid;
      
      status("✓", `Artifact uploaded: ${CYAN}${artifactCid.slice(0, 20)}...${RESET}`, GREEN);
      console.log(`${DIM}    Gateway: ${result.gateway}${RESET}`);
      
      state.artifactCid = artifactCid;
      state.stage = "ipfs-artifact";
      saveState(cleanCode, state);
      
    } catch (e) {
      console.log(`${RED}  ✗ Artifact upload failed: ${e.message}${RESET}\n`);
      console.log(`${DIM}    Run with --resume ipfs-artifact to retry${RESET}\n`);
      process.exit(1);
    }
  } else {
    status("✓", `Using cached artifact CID: ${artifactCid.slice(0, 20)}...`, GREEN);
  }
  
  // Build metadata
  const now = new Date().toISOString();
  const metadata = {
    name: `$${cleanCode}`,
    symbol: `$${cleanCode}`,
    description: piece.description || `KidLisp piece: $${cleanCode}`,
    artifactUri: `ipfs://${artifactCid}`,
    displayUri: `https://aesthetic.computer/$${cleanCode}`,
    thumbnailUri: thumbnailData?.cid ? `ipfs://${thumbnailData.cid}` : `https://aesthetic.computer/$${cleanCode}/thumbnail`,
    creators: [walletAddress],
    decimals: 0,
    isBooleanAmount: true,
    shouldPreferSymbol: false,
    date: now,
    tags: ["kidlisp", "aesthetic-computer"],
    attributes: [],
    formats: [
      {
        uri: `ipfs://${artifactCid}`,
        mimeType: "text/html",
        fileName: `${cleanCode}.html`,
      }
    ],
    rights: "",
  };
  
  // Upload metadata JSON
  if (!metadataCid || state.stage === "ipfs-artifact") {
    status("📤", "Uploading metadata to IPFS...");
    
    try {
      const result = await uploadJSONToIPFS(metadata, cleanCode, pinata);
      metadataCid = result.cid;
      
      status("✓", `Metadata uploaded: ${CYAN}${metadataCid.slice(0, 20)}...${RESET}`, GREEN);
      console.log(`${DIM}    Gateway: ${result.gateway}${RESET}`);
      
      state.metadataCid = metadataCid;
      state.stage = "ipfs-metadata";
      saveState(cleanCode, state);
      
    } catch (e) {
      console.log(`${RED}  ✗ Metadata upload failed: ${e.message}${RESET}\n`);
      console.log(`${DIM}    Run with --resume ipfs-metadata to retry${RESET}\n`);
      process.exit(1);
    }
  } else {
    status("✓", `Using cached metadata CID: ${metadataCid.slice(0, 20)}...`, GREEN);
  }
  
  // ─────────────────────────────────────────────────────────────────
  // Step 6: Mint confirmation
  // ─────────────────────────────────────────────────────────────────
  
  section("Ready to Mint");
  
  console.log();
  box("🏺 Keep Summary", [
    `${BOLD}Piece:${RESET}         $${cleanCode}`,
    `${BOLD}Owner:${RESET}         ${walletAddress.slice(0, 20)}...`,
    `${BOLD}Artifact:${RESET}      ipfs://${artifactCid.slice(0, 20)}...`,
    `${BOLD}Metadata:${RESET}      ipfs://${metadataCid.slice(0, 20)}...`,
    `${BOLD}Mint Fee:${RESET}      5 ꜩ`,
    `${BOLD}Network:${RESET}       ${NETWORK}`,
    `${BOLD}Contract:${RESET}      ${CONTRACT_ADDRESS}`,
  ].join("\n"), MAGENTA);
  
  console.log();
  
  if (!skipConfirm) {
    const proceed = await confirm(`${BOLD}${GREEN}Proceed with minting?${RESET}`);
    if (!proceed) {
      console.log(`\n${YELLOW}Minting cancelled. State saved for resume.${RESET}\n`);
      process.exit(0);
    }
  }
  
  // ─────────────────────────────────────────────────────────────────
  // Step 7: Execute mint transaction
  // ─────────────────────────────────────────────────────────────────
  
  section("Minting");
  
  // Build Michelson parameters
  const keepMetadata = {
    artifactUri: stringToBytes(`ipfs://${artifactCid}`),
    attributes: stringToBytes("[]"),
    content_hash: stringToBytes(cleanCode),
    content_type: stringToBytes("text/html"),
    creators: stringToBytes(JSON.stringify([walletAddress])),
    decimals: stringToBytes("0"),
    description: stringToBytes(metadata.description),
    displayUri: stringToBytes(metadata.displayUri),
    formats: stringToBytes(JSON.stringify(metadata.formats)),
    isBooleanAmount: stringToBytes("true"),
    metadata_uri: stringToBytes(`ipfs://${metadataCid}`),
    name: stringToBytes(metadata.name),
    owner: walletAddress,
    rights: stringToBytes(""),
    shouldPreferSymbol: stringToBytes("false"),
    symbol: stringToBytes(metadata.symbol),
    tags: stringToBytes(JSON.stringify(metadata.tags)),
    thumbnailUri: stringToBytes(metadata.thumbnailUri),
  };
  
  const keepParams = buildKeepParams(keepMetadata);
  
  status("📤", "Sending transaction to wallet...");
  console.log(`\n${YELLOW}  📱 Check Temple wallet to approve the transaction${RESET}\n`);
  
  try {
    const response = await sendContractCall(
      client,
      CONTRACT_ADDRESS,
      "keep",
      keepParams,
      MINT_FEE
    );
    
    if (response.transactionHash) {
      state.txHash = response.transactionHash;
      state.stage = "complete";
      saveState(cleanCode, state);
      
      console.log();
      box("🎉 Keep Minted Successfully!", [
        `${BOLD}Transaction:${RESET}  ${response.transactionHash}`,
        ``,
        `${BOLD}View on TzKT:${RESET}`,
        `  https://ghostnet.tzkt.io/${response.transactionHash}`,
        ``,
        `${BOLD}View on Objkt:${RESET}`,
        `  https://ghostnet.objkt.com/asset/${CONTRACT_ADDRESS}`,
      ].join("\n"), GREEN);
      
      // Clear state on success
      clearState(cleanCode);
      
    } else {
      throw new Error("No transaction hash returned");
    }
    
  } catch (e) {
    console.log(`\n${RED}  ✗ Minting failed: ${e.message}${RESET}\n`);
    console.log(`${DIM}    State saved. Run again to retry from mint step.${RESET}\n`);
    process.exit(1);
  }
  
  console.log();
}

// Run
main().catch(err => {
  console.error(`${RED}Error: ${err.message}${RESET}`);
  if (err.stack && process.env.DEBUG) {
    console.error(err.stack);
  }
  process.exit(1);
});
