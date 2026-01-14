// keeps/tzkt-client.mjs - TzKT API client for Keeps NFT queries
// Shared functions for checking mints, fetching token info, and ledger lookups
// Used by: keeps-client.mjs, keep.mjs (disk)

import {
  getNetwork,
  getTzktApi,
  getObjktUrl,
  getTzktTokenUrl,
  DEFAULT_NETWORK,
} from "./constants.mjs";

// ═══════════════════════════════════════════════════════════════════════════════
// Utility Functions
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Convert string to hex bytes for TzKT bigmap key lookup
 * @param {string} str - String to convert
 * @returns {string} Hex-encoded bytes
 */
export function stringToBytes(str) {
  return Array.from(new TextEncoder().encode(str))
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
}

// ═══════════════════════════════════════════════════════════════════════════════
// TzKT API Queries
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Check if a piece has already been minted by looking up content_hash bigmap
 * @param {string} piece - Piece name (without $ prefix)
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {Promise<{tokenId: number}|null>} Token ID if minted, null if not
 */
export async function checkIfMinted(piece, network = DEFAULT_NETWORK) {
  const config = getNetwork(network);
  const api = getTzktApi(network);
  const keyBytes = stringToBytes(piece);
  
  const url = `${api}/v1/contracts/${config.contract}/bigmaps/content_hashes/keys/${keyBytes}`;
  
  try {
    const response = await fetch(url);
    if (response.status === 200) {
      const data = await response.json();
      if (data.active) {
        return { tokenId: parseInt(data.value, 10) };
      }
    }
    return null;
  } catch (e) {
    console.warn("🔒 TzKT: Error checking mint status:", e);
    return null;
  }
}

/**
 * Fetch token metadata from TzKT
 * @param {number} tokenId - Token ID to fetch
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {Promise<object>} Token info with metadata, owner, URLs
 */
export async function fetchTokenInfo(tokenId, network = DEFAULT_NETWORK) {
  const config = getNetwork(network);
  const api = getTzktApi(network);
  
  try {
    // Get token metadata
    const metaUrl = `${api}/v1/tokens?contract=${config.contract}&tokenId=${tokenId}`;
    const metaRes = await fetch(metaUrl);
    
    let tokenData = {};
    if (metaRes.ok) {
      const tokens = await metaRes.json();
      if (tokens.length > 0) {
        tokenData = tokens[0];
      }
    }
    
    // Get owner from ledger
    const owner = await fetchLedgerOwner(tokenId, network);
    
    // Parse metadata
    const meta = tokenData.metadata || {};
    
    return {
      tokenId,
      owner,
      name: meta.name,
      description: meta.description,
      artifactUri: meta.artifactUri || meta.artifact_uri,
      thumbnailUri: meta.thumbnailUri || meta.thumbnail_uri || meta.displayUri || meta.display_uri,
      creators: meta.creators,
      attributes: meta.attributes || [],
      mintedAt: tokenData.firstTime || tokenData.lastTime,
      network,
      objktUrl: getObjktUrl(tokenId, network),
      tzktUrl: getTzktTokenUrl(tokenId, network),
    };
  } catch (e) {
    console.error("🔒 TzKT: Error fetching token info:", e);
    return {
      tokenId,
      network,
      objktUrl: getObjktUrl(tokenId, network),
      tzktUrl: getTzktTokenUrl(tokenId, network),
    };
  }
}

/**
 * Fetch token owner from ledger bigmap
 * @param {number} tokenId - Token ID to check
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {Promise<string|null>} Owner address or null
 */
export async function fetchLedgerOwner(tokenId, network = DEFAULT_NETWORK) {
  const config = getNetwork(network);
  const api = getTzktApi(network);
  
  const ledgerUrl = `${api}/v1/contracts/${config.contract}/bigmaps/ledger/keys/${tokenId}`;
  
  try {
    const ledgerRes = await fetch(ledgerUrl);
    if (ledgerRes.ok) {
      const ledgerData = await ledgerRes.json();
      if (ledgerData.active) {
        return ledgerData.value;
      }
    }
    return null;
  } catch (e) {
    console.warn("🔒 TzKT: Error fetching ledger owner:", e);
    return null;
  }
}

/**
 * Search for a token by name (for post-mint token ID lookup)
 * @param {string} tokenName - Token name to search (e.g., "$piece")
 * @param {string} network - "mainnet" or "ghostnet"
 * @param {string} contract - Contract address (optional, uses default)
 * @returns {Promise<number|null>} Token ID if found, null if not
 */
export async function findTokenByName(tokenName, network = DEFAULT_NETWORK, contract = null) {
  const config = getNetwork(network);
  const api = getTzktApi(network);
  const contractAddress = contract || config.contract;
  
  const url = `${api}/v1/tokens?contract=${contractAddress}&metadata.name=${encodeURIComponent(tokenName)}&totalSupply.gt=0&sort.desc=id&limit=1`;
  
  try {
    const res = await fetch(url);
    if (res.ok) {
      const tokens = await res.json();
      if (tokens.length > 0) {
        return tokens[0].tokenId;
      }
    }
    return null;
  } catch (e) {
    console.warn("🔒 TzKT: Error finding token by name:", e);
    return null;
  }
}

/**
 * Check if a token's metadata is locked
 * @param {number} tokenId - Token ID to check
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {Promise<boolean>} True if locked, false otherwise
 */
export async function isMetadataLocked(tokenId, network = DEFAULT_NETWORK) {
  const config = getNetwork(network);
  const api = getTzktApi(network);
  
  const url = `${api}/v1/contracts/${config.contract}/bigmaps/metadata_locked/keys/${tokenId}`;
  
  try {
    const res = await fetch(url);
    if (res.ok) {
      const data = await res.json();
      return data.active && data.value === true;
    }
    return false;
  } catch (e) {
    console.warn("🔒 TzKT: Error checking metadata lock:", e);
    return false;
  }
}

/**
 * Fetch all tokens from the contract with optional filters
 * @param {object} options - Query options
 * @param {string} options.network - "mainnet" or "ghostnet"
 * @param {number} options.limit - Max results (default 100)
 * @param {number} options.offset - Pagination offset
 * @param {string} options.sort - Sort field (default "id")
 * @param {boolean} options.desc - Sort descending (default true)
 * @returns {Promise<Array>} Array of token info objects
 */
export async function fetchAllTokens(options = {}) {
  const {
    network = DEFAULT_NETWORK,
    limit = 100,
    offset = 0,
    sort = "id",
    desc = true,
  } = options;
  
  const config = getNetwork(network);
  const api = getTzktApi(network);
  
  const sortParam = desc ? `sort.desc=${sort}` : `sort.asc=${sort}`;
  const url = `${api}/v1/tokens?contract=${config.contract}&totalSupply.gt=0&${sortParam}&limit=${limit}&offset=${offset}`;
  
  try {
    const res = await fetch(url);
    if (res.ok) {
      const tokens = await res.json();
      return tokens.map(t => ({
        tokenId: t.tokenId,
        name: t.metadata?.name,
        description: t.metadata?.description,
        thumbnailUri: t.metadata?.thumbnailUri || t.metadata?.thumbnail_uri,
        creators: t.metadata?.creators,
        mintedAt: t.firstTime,
        network,
      }));
    }
    return [];
  } catch (e) {
    console.error("🔒 TzKT: Error fetching tokens:", e);
    return [];
  }
}

/**
 * Fetch all burned tokens (totalSupply = 0)
 * @param {object} options - Fetch options
 * @param {string} options.network - "mainnet" or "ghostnet"
 * @param {number} options.limit - Max tokens to fetch
 * @returns {Promise<Array>} Array of burned token info objects
 */
export async function fetchBurnedTokens(options = {}) {
  const {
    network = DEFAULT_NETWORK,
    limit = 50,
  } = options;
  
  const config = getNetwork(network);
  const api = getTzktApi(network);
  
  // totalSupply=0 means the token has been burned
  const url = `${api}/v1/tokens?contract=${config.contract}&totalSupply=0&sort.desc=id&limit=${limit}`;
  
  try {
    const res = await fetch(url);
    if (res.ok) {
      const tokens = await res.json();
      return tokens.map(t => ({
        tokenId: t.tokenId,
        name: t.metadata?.name,
        description: t.metadata?.description,
        thumbnailUri: t.metadata?.thumbnailUri || t.metadata?.thumbnail_uri,
        creators: t.metadata?.creators,
        mintedAt: t.firstTime,
        burnedAt: t.lastTime, // Last activity was the burn
        burned: true,
        network,
      }));
    }
    return [];
  } catch (e) {
    console.error("🔒 TzKT: Error fetching burned tokens:", e);
    return [];
  }
}
