// keeps/constants.mjs - Shared constants for Keeps NFT system
// Single source of truth for contract addresses, networks, and configuration
// Used by: keeps-client.mjs, keep.mjs (disk), kidlisp.com, tezos/keeps.mjs (CLI)

// ═══════════════════════════════════════════════════════════════════════════════
// Feature Flags
// ═══════════════════════════════════════════════════════════════════════════════

// v5 Release Candidate flag - uses mainnet v5 RC contract (v6 will be production)
export const KEEPS_STAGING = true;

// ═══════════════════════════════════════════════════════════════════════════════
// Network Configuration
// ═══════════════════════════════════════════════════════════════════════════════

export const NETWORKS = {
  mainnet: {
    name: "mainnet",
    displayName: KEEPS_STAGING ? "Mainnet (V5 RC)" : "Mainnet",
    contract: "KT1QdGZP8jzqaxXDia3U7DYEqFYhfqGRHido",
    rpc: "https://mainnet.ecadinfra.com",
    tzktApi: "https://api.tzkt.io",
    explorer: "https://tzkt.io",
    objkt: "https://objkt.com",
  },
  ghostnet: {
    name: "ghostnet",
    displayName: "Ghostnet",
    contract: "KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K",
    rpc: "https://ghostnet.ecadinfra.com",
    tzktApi: "https://api.ghostnet.tzkt.io",
    explorer: "https://ghostnet.tzkt.io",
    objkt: "https://ghostnet.objkt.com",
  },
};

// Default network for browser clients
export const DEFAULT_NETWORK = "mainnet";

// ═══════════════════════════════════════════════════════════════════════════════
// Mint Flow Steps
// ═══════════════════════════════════════════════════════════════════════════════

export const MINT_STEPS = [
  { id: "wallet", label: "Connect Wallet", icon: "🔑" },
  { id: "validate", label: "Validate Piece", icon: "✓" },
  { id: "analyze", label: "Analyze Source", icon: "🔍" },
  { id: "thumbnail", label: "Generate Preview", icon: "🖼️" },
  { id: "bundle", label: "Bundle Assets", icon: "📦" },
  { id: "ipfs", label: "Upload to IPFS", icon: "☁️" },
  { id: "metadata", label: "Create Metadata", icon: "📋" },
  { id: "review", label: "Review & Confirm", icon: "👁️" },
  { id: "sign", label: "Sign Transaction", icon: "✍️" },
  { id: "complete", label: "Mint Complete!", icon: "🎉" },
];

export const STEP_STATUS = {
  PENDING: "pending",
  ACTIVE: "active",
  DONE: "done",
  ERROR: "error",
  SKIPPED: "skipped",
};

// ═══════════════════════════════════════════════════════════════════════════════
// Helper Functions
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Get network config by name
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {object} Network configuration
 */
export function getNetwork(network = DEFAULT_NETWORK) {
  return NETWORKS[network] || NETWORKS[DEFAULT_NETWORK];
}

/**
 * Get contract address for a network
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {string} Contract KT1 address
 */
export function getContract(network = DEFAULT_NETWORK) {
  return getNetwork(network).contract;
}

/**
 * Get TzKT API base URL for a network
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {string} API base URL (no trailing slash)
 */
export function getTzktApi(network = DEFAULT_NETWORK) {
  return getNetwork(network).tzktApi;
}

/**
 * Get RPC endpoint for a network
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {string} RPC URL
 */
export function getRpc(network = DEFAULT_NETWORK) {
  return getNetwork(network).rpc;
}

/**
 * Build objkt.com URL for a token
 * @param {number} tokenId - Token ID
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {string} Full objkt URL
 */
export function getObjktUrl(tokenId, network = DEFAULT_NETWORK) {
  const net = getNetwork(network);
  return `${net.objkt}/asset/${net.contract}/${tokenId}`;
}

/**
 * Build TzKT explorer URL for a token
 * @param {number} tokenId - Token ID
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {string} Full TzKT URL
 */
export function getTzktTokenUrl(tokenId, network = DEFAULT_NETWORK) {
  const net = getNetwork(network);
  return `${net.explorer}/${net.contract}/tokens/${tokenId}`;
}

/**
 * Build TzKT explorer URL for the contract
 * @param {string} network - "mainnet" or "ghostnet"
 * @returns {string} Full TzKT contract URL
 */
export function getTzktContractUrl(network = DEFAULT_NETWORK) {
  const net = getNetwork(network);
  return `${net.explorer}/${net.contract}`;
}
