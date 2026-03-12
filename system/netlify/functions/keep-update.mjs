// keep-update.mjs - Update on-chain metadata for already-minted Keeps
// 
// POST /api/keep-update - Update token metadata on Tezos (streaming SSE)
// Requires authentication and admin privileges (for now)
//
// IMPORTANT: This updates BOTH on-chain token_info AND uploads new off-chain JSON
// The "" key must point to updated IPFS JSON for objkt.com to display correctly

import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { getKeepsContractAddress, LEGACY_KEEPS_CONTRACT } from "../../backend/tezos-keeps-contract.mjs";
import { stream } from "@netlify/functions";
import { TezosToolkit, MichelsonMap } from "@taquito/taquito";
import { InMemorySigner } from "@taquito/signer";

const dev = process.env.CONTEXT === "dev";

// Allow self-signed certs in dev mode
if (dev) {
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
}
const NETWORK = process.env.TEZOS_NETWORK || "mainnet";
const TZKT_API = NETWORK === "mainnet" ? "https://api.tzkt.io/v1" : `https://api.${NETWORK}.tzkt.io/v1`;
const RPC_URL = NETWORK === "mainnet" 
  ? "https://mainnet.ecadinfra.com"
  : "https://ghostnet.ecadinfra.com";

// Helper to convert string to bytes (for Tezos metadata)
function stringToBytes(str) {
  return Buffer.from(str, 'utf8').toString('hex');
}

// SSE format helper
function sse(event, data) {
  return `event: ${event}\ndata: ${JSON.stringify(data)}\n\n`;
}

// Get Pinata credentials from database
async function getPinataCredentials() {
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "pinata" });
  
  if (!secrets) {
    throw new Error("Pinata credentials not found in database");
  }
  
  return {
    apiKey: secrets.apiKey,
    apiSecret: secrets.apiSecret,
  };
}

// Upload JSON metadata to IPFS via Pinata
async function uploadJsonToIPFS(data, name) {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  const response = await fetch("https://api.pinata.cloud/pinning/pinJSONToIPFS", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      pinata_api_key: apiKey,
      pinata_secret_api_key: apiSecret,
    },
    body: JSON.stringify({
      pinataContent: data,
      pinataMetadata: { name },
    }),
  });

  if (!response.ok) {
    throw new Error(`Metadata upload failed: ${response.status}`);
  }

  const result = await response.json();
  return `ipfs://${result.IpfsHash}`;
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

// SSE headers
const headers = {
  "Content-Type": "text/event-stream",
  "Cache-Control": "no-cache",
  "Connection": "keep-alive",
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
  "Access-Control-Allow-Methods": "POST, OPTIONS",
};

export const handler = stream(async (event) => {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 200, headers, body: "" };
  }

  if (event.httpMethod !== "POST") {
    return {
      statusCode: 405,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ error: "Method not allowed" }),
    };
  }

  // Create readable stream for SSE
  const { readable, writable } = new TransformStream();
  const writer = writable.getWriter();
  const encoder = new TextEncoder();

  const send = async (eventType, data) => {
    await writer.write(encoder.encode(sse(eventType, data)));
  };

  let streamClosed = false;
  const closeStream = async () => {
    if (!streamClosed) {
      streamClosed = true;
      await writer.close();
    }
  };

  // Process update
  (async () => {
    let database = null;
    
    try {
      // Parse body
      let body;
      try {
        body = JSON.parse(event.body || "{}");
      } catch {
        await send("error", { error: "Invalid JSON body" });
        return;
      }

      const { piece, tokenId, artifactUri, thumbnailUri, walletAddress, mode } = body;

      if (!piece || tokenId == null || !artifactUri) {
        await send("error", { error: "Missing required fields: piece, tokenId, artifactUri" });
        return;
      }
      
      // mode: "prepare" returns params for client-side wallet signing (preserves artist attribution)
      // mode: undefined = server-side signing (DEPRECATED - breaks objkt.com "Created" tab)
      const isPrepareMode = mode === "prepare";
      
      // allowOwnerEdit: Future flag to let token owners sync (with attribution warning)
      // Contract supports this, but we default to blocking to preserve objkt.com "Created by"
      const allowOwnerEdit = body.allowOwnerEdit === true;

      const pieceName = piece.replace(/^\$/, "");

      await send("progress", { stage: "auth", message: "Checking authorization..." });

      // Resolve contract address dynamically (respects MongoDB config + env)
      const CONTRACT_ADDRESS = await getKeepsContractAddress({ network: NETWORK, fallback: LEGACY_KEEPS_CONTRACT });
      console.log(`🪙 KEEP-UPDATE: Using contract ${CONTRACT_ADDRESS}`);

      // Get piece data from database first
      database = await connect();
      const collection = database.db.collection("kidlisp");
      const pieceDoc = await collection.findOne({ code: pieceName });

      if (!pieceDoc) {
        await send("error", { error: `Piece '$${pieceName}' not found` });
        return;
      }

      // Check if wallet is the current on-chain token owner
      let isOnChainOwner = false;
      if (walletAddress && tokenId != null) {
        try {
          const tokenResponse = await fetch(
            `${TZKT_API}/tokens/balances?token.contract=${CONTRACT_ADDRESS}&token.tokenId=${tokenId}&balance.gt=0`
          );
          const balances = await tokenResponse.json();
          const ownerBalance = balances.find(b => b.account?.address === walletAddress);
          isOnChainOwner = !!ownerBalance;
          console.log(`🪙 KEEP-UPDATE: Wallet ${walletAddress} is on-chain owner: ${isOnChainOwner}`);
        } catch (e) {
          console.warn("🪙 KEEP-UPDATE: Could not verify on-chain ownership:", e.message);
        }
      }

      // Fetch the original minter EARLY - we need this for authorization
      let originalMinter = null;
      if (tokenId != null) {
        try {
          const tzktBase = NETWORK === "mainnet" ? "https://api.tzkt.io" : `https://api.${NETWORK}.tzkt.io`;
          const tokenUrl = `${tzktBase}/v1/tokens?contract=${CONTRACT_ADDRESS}&tokenId=${tokenId}`;
          const tokenResponse = await fetch(tokenUrl);
          if (tokenResponse.ok) {
            const tokens = await tokenResponse.json();
            if (tokens[0]?.firstMinter?.address) {
              originalMinter = tokens[0].firstMinter.address;
              console.log(`🪙 KEEP-UPDATE: Original minter from TzKT: ${originalMinter}`);
            }
          }
        } catch (e) {
          console.warn(`🪙 KEEP-UPDATE: Failed to fetch firstMinter: ${e.message}`);
        }
      }

      // Check AC auth
      const user = await authorize(event.headers);
      
      // Check if user is the piece owner/creator OR an admin
      const isAdmin = user ? await hasAdmin(user) : false;
      const isPieceOwner = user && pieceDoc.user === user.sub;
      
      // Check if wallet matches the original minter (CRITICAL for attribution)
      const isOriginalMinter = walletAddress && originalMinter && walletAddress === originalMinter;
      
      // Authorization rules for metadata sync:
      // 1. Admin: Always allowed (server-side or via AC account)
      // 2. Original creator/minter: Always allowed (preserves objkt "Created by")
      // 3. Token owner: Only if allowOwnerEdit flag is set (changes objkt "Created by")
      //    The contract supports owner edits (v3), but we default-block to preserve attribution
      const canEdit = isAdmin || isOriginalMinter || (isOnChainOwner && allowOwnerEdit);
      
      if (!canEdit) {
        if (isOnChainOwner && !allowOwnerEdit) {
          await send("error", { 
            error: "Token owners cannot sync metadata (would change objkt.com 'Created by'). Only the original creator can sync. Pass allowOwnerEdit:true to override.",
            originalMinter: originalMinter,
            hint: "The contract supports owner edits, but this would reassign the 'Created by' attribution on objkt.com."
          });
        } else if (!user) {
          await send("error", { error: "Please connect the original creator's wallet to sync metadata" });
        } else {
          await send("error", { error: "Only the original creator can sync metadata to preserve attribution" });
        }
        return;
      }
      
      // Warn if owner is editing (will change attribution)
      if (isOnChainOwner && !isOriginalMinter && allowOwnerEdit) {
        console.warn(`🪙 KEEP-UPDATE: Owner ${walletAddress} editing token ${tokenId} - will change objkt attribution!`);
        await send("progress", { stage: "auth", message: "⚠️ Warning: Owner edit will change objkt.com 'Created by'" });
      }

      await send("progress", { stage: "auth", message: "✓ Authorized" });
      await send("progress", { stage: "load", message: `Loading $${pieceName}...` });
      await send("progress", { stage: "load", message: "✓ Piece loaded" });
      await send("progress", { stage: "metadata", message: "Building metadata..." });

      // Build metadata — match keep-prepare-background.mjs format exactly
      const tokenName = `$${pieceName}`;
      const charCount = pieceDoc.source ? pieceDoc.source.length : 0;
      const tags = ["KidLisp"];
      const attributes = [{ name: "Characters", value: String(charCount) }];

      await send("progress", { stage: "metadata", message: "✓ Metadata ready" });
      await send("progress", { stage: "tezos", message: "Connecting to Tezos..." });

      // Set up Tezos client
      const tezos = new TezosToolkit(RPC_URL);
      
      // Only use admin signer for non-prepare mode (deprecated path)
      let credentials = null;
      if (!isPrepareMode) {
        credentials = await getTezosCredentials();
        const signer = new InMemorySigner(credentials.privateKey);
        tezos.setProvider({ signer });
      }

      await send("progress", { stage: "tezos", message: `✓ Connected to ${NETWORK}` });
      await send("progress", { stage: "contract", message: "Loading contract..." });

      // Get contract-specific data from database
      // Use contract-keyed storage: tezos.contracts[CONTRACT_ADDRESS]
      const contractData = pieceDoc.tezos?.contracts?.[CONTRACT_ADDRESS] || {};
      
      // originalMinter was already fetched during auth check above
      // Fallback to DB if TzKT lookup failed earlier
      if (!originalMinter) {
        originalMinter = contractData.minter || contractData.owner || 
                         pieceDoc.tezos?.minter || pieceDoc.tezos?.owner;
      }
      
      // Last resort fallback
      if (!originalMinter) {
        originalMinter = credentials?.address;
        console.warn(`🪙 KEEP-UPDATE: Using kidlisp wallet as fallback minter`);
      }

      // Get the metadataUri from the database (TZIP-16 off-chain JSON)
      // This is critical - the empty string "" key must point to the metadata JSON
      // Otherwise objkt won't resolve artist attribution correctly
      // Try contract-specific first, then legacy flat field
      let metadataUri = contractData.metadataUri || pieceDoc.tezos?.metadataUri;
      
      // If metadataUri not in DB, fetch from on-chain token_metadata bigmap
      // This preserves the original "" key and prevents breaking objkt attribution
      if (!metadataUri && tokenId) {
        try {
          const tzktBase = NETWORK === "mainnet" ? "https://api.tzkt.io" : `https://api.${NETWORK}.tzkt.io`;
          const bigmapUrl = `${tzktBase}/v1/contracts/${CONTRACT_ADDRESS}/bigmaps/token_metadata/keys/${tokenId}`;
          const bigmapResponse = await fetch(bigmapUrl);
          if (bigmapResponse.ok) {
            const bigmapData = await bigmapResponse.json();
            const emptyKeyHex = bigmapData?.value?.token_info?.[""];
            if (emptyKeyHex) {
              // Decode hex to get the original IPFS URI
              metadataUri = Buffer.from(emptyKeyHex, 'hex').toString('utf8');
              console.log(`🪙 KEEP-UPDATE: Found metadataUri from on-chain: ${metadataUri}`);
            }
          }
        } catch (e) {
          console.warn(`🪙 KEEP-UPDATE: Failed to fetch metadataUri from TzKT: ${e.message}`);
        }
      }

      // ═══════════════════════════════════════════════════════════════════
      // BUILD AND UPLOAD NEW OFF-CHAIN JSON METADATA
      // This is CRITICAL - objkt.com reads the "" key to get the full JSON
      // We must upload updated JSON with new artifactUri/thumbnailUri
      // ═══════════════════════════════════════════════════════════════════
      
      await send("progress", { stage: "ipfs", message: "Uploading updated metadata to IPFS..." });
      
      // Build complete off-chain metadata JSON (TZIP-21 compliant)
      const creatorsArray = [originalMinter];

      // v4: Preserve 10% royalty to original creator
      const royalties = {
        decimals: 4,
        shares: {
          [originalMinter]: "1000"  // 10% = 1000/10000 basis points
        }
      };

      const metadataJson = {
        name: tokenName,
        description: pieceDoc.source || "A KidLisp piece preserved on Tezos",
        artifactUri: artifactUri,
        displayUri: artifactUri,
        thumbnailUri: thumbnailUri || artifactUri,
        decimals: 0,
        symbol: pieceName,
        creators: creatorsArray,
        royalties,
        tags,
        attributes,
        formats: [{
          uri: artifactUri,
          mimeType: "text/html",
          dimensions: { value: "responsive", unit: "viewport" },
        }],
      };
      
      // Upload new metadata JSON to IPFS
      const newMetadataUri = await uploadJsonToIPFS(
        metadataJson,
        `$${pieceName}-metadata-updated.json`
      );
      console.log(`🪙 KEEP-UPDATE: Uploaded new metadata JSON: ${newMetadataUri}`);
      
      await send("progress", { stage: "ipfs", message: `✓ Metadata uploaded: ${newMetadataUri.slice(0, 30)}...` });

      // Build on-chain token_info — match keep-prepare-background.mjs format
      const tokenInfo = new MichelsonMap();
      tokenInfo.set("", stringToBytes(newMetadataUri));
      tokenInfo.set("name", stringToBytes(tokenName));
      tokenInfo.set("symbol", stringToBytes(pieceName));
      tokenInfo.set("description", stringToBytes(pieceDoc.source || ""));
      tokenInfo.set("artifactUri", stringToBytes(artifactUri));
      tokenInfo.set("displayUri", stringToBytes(artifactUri));
      tokenInfo.set("thumbnailUri", stringToBytes(thumbnailUri || artifactUri));
      tokenInfo.set("decimals", stringToBytes("0"));
      tokenInfo.set("creators", stringToBytes(JSON.stringify(creatorsArray)));
      tokenInfo.set("royalties", stringToBytes(JSON.stringify(royalties)));
      tokenInfo.set("content_hash", stringToBytes(pieceName));
      tokenInfo.set("metadata_uri", stringToBytes(newMetadataUri));

      const contract = await tezos.contract.at(CONTRACT_ADDRESS);
      
      await send("progress", { stage: "contract", message: "✓ Contract loaded" });

      // ═══════════════════════════════════════════════════════════════════
      // PREPARE MODE: Return Michelson params for client-side wallet signing
      // This preserves artist attribution on objkt.com because the original
      // creator's wallet signs the edit_metadata call, not the admin server.
      // ═══════════════════════════════════════════════════════════════════
      if (isPrepareMode) {
        await send("progress", { stage: "ready", message: "Ready for wallet signature..." });
        
        // Generate the Michelson params for the contract call
        const transferParams = contract.methodsObject.edit_metadata({
          token_id: parseInt(tokenId),
          token_info: tokenInfo,
        }).toTransferParams();
        
        // Update database to clear pendingRebake and store the new metadata URI
        // (the actual on-chain URIs will be updated after client confirms tx)
        await collection.updateOne(
          { code: pieceName },
          { 
            $set: { 
              [`tezos.contracts.${CONTRACT_ADDRESS}.pendingMetadataUri`]: newMetadataUri,
              [`tezos.contracts.${CONTRACT_ADDRESS}.pendingArtifactUri`]: artifactUri,
              [`tezos.contracts.${CONTRACT_ADDRESS}.pendingThumbnailUri`]: thumbnailUri,
            },
          }
        );
        
        await send("prepared", {
          success: true,
          piece: pieceName,
          tokenId,
          contractAddress: CONTRACT_ADDRESS,
          network: NETWORK,
          // Send the Michelson-encoded parameters for Beacon wallet
          michelsonParams: transferParams.parameter,
          entrypoint: "edit_metadata",
          artifactUri,
          thumbnailUri,
          metadataUri: newMetadataUri,
          rpcUrl: RPC_URL,
        });
        return;
      }

      // ═══════════════════════════════════════════════════════════════════
      // SERVER-SIDE SIGNING (DEPRECATED)
      // This path breaks artist attribution on objkt.com because the admin
      // wallet signs the transaction instead of the original creator.
      // TODO: Remove this once client-side signing is confirmed working.
      // ═══════════════════════════════════════════════════════════════════
      console.warn("🪙 KEEP-UPDATE: Using deprecated server-side signing - this will break objkt attribution!");
      await send("progress", { stage: "submit", message: "Submitting transaction..." });

      const op = await contract.methodsObject.edit_metadata({
        token_id: parseInt(tokenId),
        token_info: tokenInfo,
      }).send();

      const opHashShort = op.hash.slice(0, 12) + "...";
      await send("progress", { stage: "submit", message: `✓ Submitted: ${opHashShort}` });
      await send("progress", { stage: "confirm", message: "Waiting for confirmation..." });

      await op.confirmation(1);

      await send("progress", { stage: "confirm", message: "✓ Confirmed on-chain!" });
      await send("progress", { stage: "database", message: "Updating database..." });

      // Clear pendingRebake and update on-chain URIs in the record
      // Use contract-keyed storage: tezos.contracts[CONTRACT_ADDRESS]
      await collection.updateOne(
        { code: pieceName },
        { 
          $set: { 
            [`tezos.contracts.${CONTRACT_ADDRESS}.artifactUri`]: artifactUri,
            [`tezos.contracts.${CONTRACT_ADDRESS}.thumbnailUri`]: thumbnailUri,
            [`tezos.contracts.${CONTRACT_ADDRESS}.metadataUri`]: newMetadataUri,
            [`tezos.contracts.${CONTRACT_ADDRESS}.lastUpdatedAt`]: new Date(),
            [`tezos.contracts.${CONTRACT_ADDRESS}.lastUpdateTxHash`]: op.hash,
          },
          $unset: { pendingRebake: "" }
        }
      );

      await send("progress", { stage: "database", message: "✓ Database updated" });

      const explorerUrl = NETWORK === "mainnet" 
        ? `https://tzkt.io/${op.hash}`
        : `https://ghostnet.tzkt.io/${op.hash}`;

      await send("complete", {
        success: true,
        tokenId,
        opHash: op.hash,
        artifactUri,
        thumbnailUri,
        metadataUri: newMetadataUri,
        explorerUrl,
      });

    } catch (error) {
      console.error("🪙 KEEP-UPDATE: Error:", error);
      
      let errorMessage = error.message;
      if (error.message?.includes("METADATA_LOCKED")) {
        errorMessage = "Token metadata is locked and cannot be updated";
      }
      if (error.message?.includes("FA2_NOT_ADMIN")) {
        errorMessage = "Not authorized to update this token (FA2_NOT_ADMIN)";
      }

      try {
        await send("error", { error: errorMessage });
      } catch (e) {
        // Stream may already be closed
      }
    } finally {
      if (database) {
        try {
          await database.disconnect();
        } catch (e) {
          // Ignore disconnect errors
        }
      }
      await closeStream();
    }
  })();

  return { statusCode: 200, headers, body: readable };
});
