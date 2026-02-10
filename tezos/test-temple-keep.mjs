#!/usr/bin/env node
/**
 * test-temple-keep.mjs - Test keeping a piece via Temple mobile wallet
 * 
 * This script:
 * 1. Connects to Temple via QR code (Beacon P2P)
 * 2. Prepares the keep parameters
 * 3. Sends the operation to Temple for signing
 * 
 * Usage:
 *   node test-temple-keep.mjs <piece-code>
 *   node test-temple-keep.mjs puf
 */

import { pairWallet, sendContractCall } from "./beacon-node.mjs";
import { connect } from "../system/backend/database.mjs";
import { TezosToolkit } from "@taquito/taquito";

// Contract and network config - Ghostnet v3
const CONTRACT_ADDRESS = "KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K";
const NETWORK = "ghostnet";
const RPC = "https://ghostnet.ecadinfra.com";

// Colors
const RESET = "\x1b[0m";
const BOLD = "\x1b[1m";
const DIM = "\x1b[2m";
const CYAN = "\x1b[36m";
const GREEN = "\x1b[32m";
const YELLOW = "\x1b[33m";
const RED = "\x1b[31m";

// String to hex bytes (for Michelson)
function stringToBytes(str) {
  return Buffer.from(str, "utf8").toString("hex");
}

async function main() {
  const pieceCode = process.argv[2];
  
  if (!pieceCode) {
    console.log(`${RED}Usage: node test-temple-keep.mjs <piece-code>${RESET}`);
    console.log(`${DIM}Example: node test-temple-keep.mjs puf${RESET}\n`);
    process.exit(1);
  }
  
  const cleanCode = pieceCode.replace(/^\$/, '');
  
  console.log(`\n${BOLD}${CYAN}╔════════════════════════════════════════════════════════════════╗${RESET}`);
  console.log(`${BOLD}${CYAN}║  🏺 Keep via Temple Wallet: $${cleanCode}${RESET.padEnd(30)}${BOLD}${CYAN}║${RESET}`);
  console.log(`${BOLD}${CYAN}╚════════════════════════════════════════════════════════════════╝${RESET}\n`);
  
  // Step 1: Pair with Temple
  console.log(`${CYAN}Step 1: Connect to Temple Wallet${RESET}\n`);
  const pairResult = await pairWallet("Aesthetic Computer");
  
  if (!pairResult?.permissionResponse?.address) {
    console.log(`${RED}❌ Failed to connect wallet${RESET}\n`);
    process.exit(1);
  }
  
  const { client, permissionResponse } = pairResult;
  const walletAddress = permissionResponse.address;
  
  console.log(`\n${GREEN}✓ Wallet connected: ${walletAddress}${RESET}\n`);
  
  // Step 2: Get piece metadata from MongoDB (optional - for display)
  console.log(`${CYAN}Step 2: Preparing keep parameters${RESET}\n`);
  
  // For testing, we'll create minimal metadata
  // In production, this would come from the KidLisp piece in MongoDB
  const testMetadata = {
    artifactUri: stringToBytes(`ipfs://test-artifact-${cleanCode}`),
    attributes: stringToBytes("[]"),
    content_hash: stringToBytes(`test-hash-${cleanCode}-${Date.now()}`),
    content_type: stringToBytes("text/kidlisp"),
    creators: stringToBytes(`["${walletAddress}"]`),
    decimals: stringToBytes("0"),
    description: stringToBytes(`Test keep for $${cleanCode}`),
    displayUri: stringToBytes(`https://aesthetic.computer/$${cleanCode}`),
    formats: stringToBytes("[]"),
    isBooleanAmount: stringToBytes("true"),
    metadata_uri: stringToBytes(""),
    name: stringToBytes(`$${cleanCode}`),
    owner: walletAddress,
    rights: stringToBytes(""),
    shouldPreferSymbol: stringToBytes("false"),
    symbol: stringToBytes(`$${cleanCode}`),
    tags: stringToBytes(`["kidlisp", "aesthetic-computer"]`),
    thumbnailUri: stringToBytes(`https://aesthetic.computer/$${cleanCode}/thumbnail`)
  };
  
  console.log(`${DIM}Piece: $${cleanCode}${RESET}`);
  console.log(`${DIM}Owner: ${walletAddress}${RESET}`);
  console.log(`${DIM}Contract: ${CONTRACT_ADDRESS}${RESET}\n`);
  
  // Step 3: Build Michelson parameters for the keep entrypoint
  // The keep entrypoint expects a complex record type
  const keepParams = {
    prim: "Pair",
    args: [
      { bytes: testMetadata.artifactUri },  // artifactUri
      { prim: "Pair", args: [
        { bytes: testMetadata.attributes },  // attributes
        { prim: "Pair", args: [
          { bytes: testMetadata.content_hash },  // content_hash
          { prim: "Pair", args: [
            { bytes: testMetadata.content_type },  // content_type
            { prim: "Pair", args: [
              { bytes: testMetadata.creators },  // creators
              { prim: "Pair", args: [
                { bytes: testMetadata.decimals },  // decimals
                { prim: "Pair", args: [
                  { bytes: testMetadata.description },  // description
                  { prim: "Pair", args: [
                    { bytes: testMetadata.displayUri },  // displayUri
                    { prim: "Pair", args: [
                      { bytes: testMetadata.formats },  // formats
                      { prim: "Pair", args: [
                        { bytes: testMetadata.isBooleanAmount },  // isBooleanAmount
                        { prim: "Pair", args: [
                          { bytes: testMetadata.metadata_uri },  // metadata_uri
                          { prim: "Pair", args: [
                            { bytes: testMetadata.name },  // name
                            { prim: "Pair", args: [
                              { string: testMetadata.owner },  // owner (address)
                              { prim: "Pair", args: [
                                { bytes: testMetadata.rights },  // rights
                                { prim: "Pair", args: [
                                  { bytes: testMetadata.shouldPreferSymbol },  // shouldPreferSymbol
                                  { prim: "Pair", args: [
                                    { bytes: testMetadata.symbol },  // symbol
                                    { prim: "Pair", args: [
                                      { bytes: testMetadata.tags },  // tags
                                      { bytes: testMetadata.thumbnailUri }  // thumbnailUri
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
  
  // Step 4: Send the operation to Temple
  console.log(`${CYAN}Step 3: Send operation to Temple for signing${RESET}\n`);
  console.log(`${YELLOW}📱 Check your Temple wallet to approve the transaction...${RESET}\n`);
  console.log(`${DIM}Keep fee: 5 XTZ${RESET}\n`);
  
  try {
    const response = await sendContractCall(
      client,
      CONTRACT_ADDRESS,
      "keep",
      keepParams,
      "5000000"  // Keep fee: 5 XTZ in mutez
    );
    
    console.log(`\n${GREEN}🏺 Keep operation submitted!${RESET}\n`);
    if (response.transactionHash) {
      console.log(`${CYAN}View on TzKT:${RESET} https://ghostnet.tzkt.io/${response.transactionHash}`);
    }
  } catch (err) {
    console.log(`\n${RED}❌ Keep failed: ${err.message}${RESET}\n`);
    process.exit(1);
  }
}

main().catch(err => {
  console.error(`${RED}Error: ${err.message}${RESET}`);
  process.exit(1);
});
