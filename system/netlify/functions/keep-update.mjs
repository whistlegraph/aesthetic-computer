// keep-update.mjs - Update on-chain metadata for already-minted Keeps
// 
// POST /api/keep-update - Update token metadata on Tezos
// Requires authentication and admin privileges (for now)

import { authorize, handleFor, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { TezosToolkit } from "@taquito/taquito";
import { InMemorySigner } from "@taquito/signer";

const dev = process.env.CONTEXT === "dev";

// Allow self-signed certs in dev mode
if (dev) {
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
}

// Configuration
const CONTRACT_ADDRESS = process.env.TEZOS_KEEPS_CONTRACT || "KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM";
const NETWORK = process.env.TEZOS_NETWORK || "mainnet";
const RPC_URL = NETWORK === "mainnet" 
  ? "https://mainnet.ecadinfra.com"
  : "https://ghostnet.ecadinfra.com";

// Helper to convert string to bytes (for Tezos metadata)
function stringToBytes(str) {
  return Buffer.from(str, 'utf8').toString('hex');
}

async function getTezosCredentials() {
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "tezos-kidlisp" });
  
  if (!secrets) {
    throw new Error("Tezos KidLisp credentials not found in database");
  }
  
  return {
    address: secrets.address,
    publicKey: secrets.publicKey,
    privateKey: secrets.privateKey,
    network: secrets.network,
  };
}

export default async function handler(event) {
  // Only allow POST
  if (event.httpMethod !== "POST") {
    return {
      statusCode: 405,
      body: JSON.stringify({ error: "Method not allowed" }),
    };
  }

  try {
    // Parse request body
    const body = JSON.parse(event.body || "{}");
    const { piece, tokenId, artifactUri, thumbnailUri, walletAddress } = body;

    if (!piece || !tokenId || !artifactUri) {
      return {
        statusCode: 400,
        body: JSON.stringify({ error: "Missing required fields: piece, tokenId, artifactUri" }),
      };
    }

    const pieceName = piece.replace(/^\$/, "");

    // Check auth - require admin for now
    const user = await authorize(event.headers);
    if (!user) {
      return {
        statusCode: 401,
        body: JSON.stringify({ error: "Please log in first" }),
      };
    }

    const isAdmin = await hasAdmin(user);
    if (!isAdmin) {
      return {
        statusCode: 403,
        body: JSON.stringify({ error: "Admin access required for on-chain updates" }),
      };
    }

    console.log(`🪙 KEEP-UPDATE: Updating token #${tokenId} for $${pieceName}`);
    console.log(`   New artifact: ${artifactUri}`);
    console.log(`   New thumbnail: ${thumbnailUri}`);

    // Get piece data from database
    const database = await connect();
    const collection = database.db.collection("kidlisp");
    const pieceDoc = await collection.findOne({ code: pieceName });

    if (!pieceDoc) {
      await database.disconnect();
      return {
        statusCode: 404,
        body: JSON.stringify({ error: `Piece '$${pieceName}' not found` }),
      };
    }

    // Get author handle
    const authorHandle = pieceDoc.user ? await handleFor(pieceDoc.user) : "@anon";

    // Build metadata
    const tokenName = `$${pieceName}`;
    const description = pieceDoc.source || `A KidLisp piece preserved on Tezos`;
    const tags = [`$${pieceName}`, "KidLisp", "Aesthetic.Computer", "interactive"];
    
    const attributes = [
      { name: "Language", value: "KidLisp" },
      { name: "Code", value: `$${pieceName}` },
      ...(authorHandle && authorHandle !== "@anon" ? [{ name: "Author", value: authorHandle }] : []),
      { name: "Interactive", value: "Yes" },
      { name: "Platform", value: "Aesthetic Computer" },
      { name: "Updated", value: new Date().toISOString().split('T')[0] },
    ];

    // Get Tezos credentials and set up client
    const credentials = await getTezosCredentials();
    const tezos = new TezosToolkit(RPC_URL);
    const signer = new InMemorySigner(credentials.privateKey);
    tezos.setProvider({ signer });

    // Build on-chain token_info
    const tokenInfo = new Map([
      ["name", stringToBytes(tokenName)],
      ["description", stringToBytes(description)],
      ["artifactUri", stringToBytes(artifactUri)],
      ["displayUri", stringToBytes(artifactUri)],
      ["thumbnailUri", stringToBytes(thumbnailUri || artifactUri)],
      ["decimals", stringToBytes("0")],
      ["symbol", stringToBytes("KEEP")],
      ["isBooleanAmount", stringToBytes("true")],
      ["shouldPreferSymbol", stringToBytes("false")],
      ["formats", stringToBytes(JSON.stringify([{
        uri: artifactUri,
        mimeType: "text/html",
        dimensions: { value: "responsive", unit: "viewport" }
      }]))],
      ["tags", stringToBytes(JSON.stringify(tags))],
      ["attributes", stringToBytes(JSON.stringify(attributes))],
      ["creators", stringToBytes(JSON.stringify([credentials.address]))],
      ["rights", stringToBytes("© All rights reserved")],
      ["content_type", stringToBytes("KidLisp")],
      ["content_hash", stringToBytes(pieceName)],
    ]);

    // Call edit_metadata entrypoint
    console.log(`🪙 KEEP-UPDATE: Calling edit_metadata on ${CONTRACT_ADDRESS}...`);
    const contract = await tezos.contract.at(CONTRACT_ADDRESS);
    
    const op = await contract.methodsObject.edit_metadata({
      token_id: parseInt(tokenId),
      token_info: tokenInfo,
    }).send();

    console.log(`🪙 KEEP-UPDATE: Operation hash: ${op.hash}`);
    console.log(`🪙 KEEP-UPDATE: Waiting for confirmation...`);

    await op.confirmation(1);

    console.log(`🪙 KEEP-UPDATE: ✅ Metadata updated successfully!`);

    await database.disconnect();

    return {
      statusCode: 200,
      body: JSON.stringify({
        success: true,
        tokenId,
        opHash: op.hash,
        artifactUri,
        thumbnailUri,
        explorerUrl: NETWORK === "mainnet" 
          ? `https://tzkt.io/${op.hash}`
          : `https://ghostnet.tzkt.io/${op.hash}`,
      }),
    };

  } catch (error) {
    console.error("🪙 KEEP-UPDATE: Error:", error);
    
    let errorMessage = error.message;
    if (error.message?.includes("METADATA_LOCKED")) {
      errorMessage = "Token metadata is locked and cannot be updated";
    }

    return {
      statusCode: 500,
      body: JSON.stringify({ 
        error: errorMessage,
        success: false,
      }),
    };
  }
}
