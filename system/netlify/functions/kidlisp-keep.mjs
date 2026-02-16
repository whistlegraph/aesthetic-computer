// kidlisp-keep.mjs - KidLisp Keeps NFT Status & Analysis API
// 
// GET  /api/kidlisp-keep?piece=cow          - Check piece status and traits
// GET  /api/kidlisp-keep?piece=cow&analyze  - Full static analysis
//
// Minting is handled separately via the oven server or CLI

import { authorize, handleFor, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { analyzeKidLisp, ANALYZER_VERSION } from "../../backend/kidlisp-analyzer.mjs";

const dev = process.env.CONTEXT === "dev";

// Configuration
const CONTRACT_ADDRESS = process.env.TEZOS_KEEPS_CONTRACT || "KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W";
const NETWORK = process.env.TEZOS_NETWORK || "mainnet";

// Convert string to hex bytes (for Tezos)
function stringToBytes(str) {
  return Buffer.from(str, "utf8").toString("hex");
}

// Check if a piece name is already minted via TzKT
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
          objktUrl: `https://${NETWORK === "mainnet" ? "" : "ghostnet."}objkt.com/asset/${CONTRACT_ADDRESS}/${data.value}`,
        };
      }
    }
    return { minted: false };
  } catch (error) {
    console.error("Error checking mint status:", error);
    return { minted: false, error: error.message };
  }
}

// GET: Check piece status and optionally analyze
async function handleGet(event) {
  const pieceName = event.queryStringParameters?.piece?.replace(/^\$/, "");
  const doAnalyze = "analyze" in (event.queryStringParameters || {});
  
  if (!pieceName) {
    return respond(400, { 
      error: "Missing 'piece' parameter",
      usage: {
        status: "GET /api/kidlisp-keep?piece=cow",
        analyze: "GET /api/kidlisp-keep?piece=cow&analyze",
      }
    });
  }
  
  try {
    const database = await connect();
    const collection = database.db.collection("kidlisp-codes");
    
    // Get piece from database
    const piece = await collection.findOne({ code: pieceName });
    await database.disconnect();
    
    if (!piece) {
      return respond(404, { 
        error: "Piece not found",
        piece: pieceName,
        canMint: false,
      });
    }
    
    // Check if already minted
    const mintStatus = await checkMintStatus(pieceName);
    
    // Get owner handle
    let ownerHandle = null;
    let ownerSub = null;
    if (piece.user) {
      ownerSub = piece.user;
      ownerHandle = await handleFor(piece.user);
      if (ownerHandle) ownerHandle = `@${ownerHandle}`;
    }
    
    // Build basic response
    const result = {
      piece: pieceName,
      exists: true,
      owner: ownerHandle || "@anon",
      ownerSub,
      minted: mintStatus.minted,
      canMint: !mintStatus.minted && !!piece.user,
      contract: CONTRACT_ADDRESS,
      network: NETWORK,
    };
    
    // Add mint details if minted
    if (mintStatus.minted) {
      result.tokenId = mintStatus.tokenId;
      result.objktUrl = mintStatus.objktUrl;
    }
    
    // Analyze source code if requested
    if (doAnalyze && piece.source) {
      const analysis = analyzeKidLisp(piece.source);
      result.analysis = {
        version: ANALYZER_VERSION,
        ...analysis,
      };
    } else if (piece.source) {
      // Always include basic metrics
      const lines = piece.source.split("\n");
      result.lineCount = lines.length;
      result.charCount = piece.source.length;
    }
    
    // Add timestamps
    if (piece.timestamp) {
      result.created = new Date(piece.timestamp).toISOString();
    }
    if (piece.updated) {
      result.updated = new Date(piece.updated).toISOString();
    }
    
    return respond(200, result);
    
  } catch (error) {
    console.error("Error checking piece:", error);
    return respond(500, { error: "Failed to check piece status" });
  }
}

// POST: Request minting (authenticated)
// This endpoint validates and queues the mint request
// Actual minting happens via separate service
async function handlePost(event) {
  // Parse body
  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return respond(400, { error: "Invalid JSON body" });
  }
  
  const pieceName = body.piece?.replace(/^\$/, "");
  
  if (!pieceName) {
    return respond(400, { error: "Missing 'piece' in request body" });
  }
  
  // Authenticate user
  const user = await authorize(event.headers);
  if (!user) {
    return respond(401, { error: "Authentication required" });
  }
  
  // Check if user has a handle
  const userHandle = await handleFor(user.sub);
  if (!userHandle) {
    return respond(403, { 
      error: "You need an @handle to mint keeps",
      help: "Visit aesthetic.computer/handle to claim your handle",
    });
  }
  
  // Check admin status for bypass
  const isAdmin = await hasAdmin(user);
  
  try {
    const database = await connect();
    const collection = database.db.collection("kidlisp-codes");
    
    // Get piece from database
    const piece = await collection.findOne({ code: pieceName });
    
    if (!piece) {
      await database.disconnect();
      return respond(404, { error: `Piece '$${pieceName}' not found` });
    }
    
    // Check ownership (unless admin)
    if (!isAdmin && piece.user && piece.user !== user.sub) {
      const ownerHandle = await handleFor(piece.user);
      await database.disconnect();
      return respond(403, { 
        error: "You don't own this piece",
        owner: ownerHandle ? `@${ownerHandle}` : "another user",
      });
    }
    
    // Check if already minted
    const mintStatus = await checkMintStatus(pieceName);
    if (mintStatus.minted) {
      await database.disconnect();
      return respond(409, {
        error: "This piece has already been minted",
        tokenId: mintStatus.tokenId,
        objktUrl: mintStatus.objktUrl,
      });
    }
    
    // Analyze the piece for traits
    const analysis = analyzeKidLisp(piece.source);
    
    await database.disconnect();
    
    // Return mint request details
    // The actual minting will be done by the oven server or CLI
    return respond(200, {
      approved: true,
      piece: pieceName,
      owner: `@${userHandle}`,
      ownerSub: user.sub,
      analysis: {
        version: ANALYZER_VERSION,
        traits: analysis.traits,
        lineCount: analysis.lineCount,
        complexity: analysis.complexity,
        categories: analysis.categoryNames,
      },
      mintEndpoint: `${process.env.OVEN_URL || "https://oven.aesthetic.computer"}/mint-keep`,
      message: "Mint request validated. Use the mintEndpoint to complete minting.",
    });
    
  } catch (error) {
    console.error("Mint validation error:", error);
    return respond(500, { 
      error: "Mint validation failed",
      details: error.message,
    });
  }
}

export async function handler(event, context) {
  console.log(`📥 kidlisp-keep: ${event.httpMethod}`);
  
  if (event.httpMethod === "GET") {
    return handleGet(event);
  } else if (event.httpMethod === "POST") {
    return handlePost(event);
  } else {
    return respond(405, { error: "Method not allowed" });
  }
}
