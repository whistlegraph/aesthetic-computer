/**
 * cli-wallet.mjs - Tezos wallet integration for CLI tools
 * 
 * Allows CLI users to:
 * - Connect their Tezos wallet via QR code (Temple/Kukai)
 * - Sign and pay for transactions themselves
 * - Persist wallet connection for future use
 * - Sync wallet address to MongoDB profile
 * 
 * Connection methods:
 * 1. QR Code scanning (Temple Beacon P2P or Kukai WalletConnect)
 * 2. Manual address entry (fallback)
 */

import { TezosToolkit } from "@taquito/taquito";
import { promises as fs } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import readline from "readline";

const __dirname = dirname(fileURLToPath(import.meta.url));

// Storage for wallet session
const WALLET_FILE = join(process.env.HOME, ".ac-tezos-wallet");

// Network config
const NETWORKS = {
  ghostnet: {
    rpc: "https://ghostnet.ecadinfra.com",
    tzkt: "https://api.ghostnet.tzkt.io",
    name: "Ghostnet (Testnet)",
  },
  mainnet: {
    rpc: "https://mainnet.ecadinfra.com",
    tzkt: "https://api.tzkt.io",
    name: "Mainnet",
  },
};

// Terminal colors
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const DIM = '\x1b[2m';
const CYAN = '\x1b[36m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const RED = '\x1b[31m';

let tezos = null;
let connectedAddress = null;
let currentNetwork = "ghostnet";

/**
 * Initialize the Tezos toolkit
 */
export async function init(network = "ghostnet") {
  currentNetwork = network;
  const config = NETWORKS[network];
  
  tezos = new TezosToolkit(config.rpc);
  
  // Try to load saved session
  const session = await loadSession();
  if (session?.address) {
    connectedAddress = session.address;
    currentNetwork = session.network || network;
  }
  
  return { tezos, address: connectedAddress };
}

/**
 * Connect wallet - prompt user to enter their address
 */
export async function connect(network = "ghostnet") {
  currentNetwork = network;
  const config = NETWORKS[network];
  
  console.log(`\n${BOLD}${CYAN}╔════════════════════════════════════════════════════════════════╗${RESET}`);
  console.log(`${BOLD}${CYAN}║  🔷 Connect Tezos Wallet                                       ║${RESET}`);
  console.log(`${BOLD}${CYAN}╚════════════════════════════════════════════════════════════════╝${RESET}\n`);
  
  console.log(`${DIM}Network: ${config.name}${RESET}\n`);
  console.log(`Enter your Tezos wallet address or .tez domain:`);
  console.log(`${DIM}Examples: tz1abc..., jeffrey.tez, or just "jeffrey"${RESET}\n`);
  
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
  
  let input = await new Promise((resolve) => {
    rl.question(`${CYAN}Address/Domain: ${RESET}`, (answer) => {
      rl.close();
      resolve(answer.trim());
    });
  });
  
  let address = input;
  let domain = null;
  
  // Check if it's a domain (doesn't start with tz)
  if (!input.match(/^tz[123]/)) {
    console.log(`${DIM}Resolving domain...${RESET}`);
    const resolved = await resolveDomain(input, network);
    if (resolved) {
      address = resolved;
      domain = input.endsWith('.tez') ? input : `${input}.tez`;
      console.log(`${GREEN}✓${RESET} ${domain} → ${address.slice(0, 8)}...${address.slice(-6)}\n`);
    } else {
      console.log(`${RED}❌ Could not resolve "${input}" to a Tezos address${RESET}`);
      console.log(`${DIM}Make sure the domain exists on ${config.name}${RESET}\n`);
      return null;
    }
  }
  
  // Validate address format
  if (!address.match(/^(tz1|tz2|tz3)[1-9A-HJ-NP-Za-km-z]{33}$/)) {
    console.log(`${RED}❌ Invalid Tezos address format${RESET}\n`);
    return null;
  }
  
  // Verify address exists on chain
  const balance = await fetchBalance(address, network);
  if (balance === null) {
    console.log(`${YELLOW}⚠️  Could not verify address on ${config.name}${RESET}`);
    console.log(`${DIM}The address may be new or network may be unavailable${RESET}\n`);
  }
  
  connectedAddress = address;
  
  // Lookup domain if we didn't already resolve one
  if (!domain) {
    domain = await fetchDomain(address, network);
  }
  
  console.log(`\n${GREEN}✅ Wallet connected!${RESET}`);
  if (domain) {
    console.log(`${CYAN}Domain:${RESET} ${domain}`);
  }
  console.log(`${CYAN}Address:${RESET} ${address}`);
  if (balance !== null) {
    console.log(`${CYAN}Balance:${RESET} ${balance.toFixed(2)} ꜩ`);
  }
  console.log(`${CYAN}Network:${RESET} ${config.name}\n`);
  
  // Save session (include domain if known)
  await saveSession(address, network, domain);
  
  return address;
}

/**
 * Connect wallet via QR code (Temple/Kukai)
 * Uses beacon-node.mjs or walletconnect-node.mjs under the hood
 */
export async function connectViaQR(walletType = "temple", network = "ghostnet") {
  currentNetwork = network;
  const config = NETWORKS[network];
  
  console.log(`\n${BOLD}${CYAN}╔════════════════════════════════════════════════════════════════╗${RESET}`);
  console.log(`${BOLD}${CYAN}║  🔷 Connect Tezos Wallet via QR Code                           ║${RESET}`);
  console.log(`${BOLD}${CYAN}╚════════════════════════════════════════════════════════════════╝${RESET}\n`);
  
  console.log(`${DIM}Wallet: ${walletType === 'temple' ? 'Temple (Beacon P2P)' : 'Kukai (WalletConnect)'}${RESET}`);
  console.log(`${DIM}Network: ${config.name}${RESET}\n`);
  
  let address = null;
  let domain = null;
  
  try {
    if (walletType === "temple") {
      // Use Beacon P2P
      const { pairWallet } = await import("./beacon-node.mjs");
      const result = await pairWallet();
      
      if (result?.permissionResponse) {
        address = result.permissionResponse.address || 
                  result.permissionResponse.account?.address ||
                  result.permissionResponse.accountInfo?.address;
      }
    } else if (walletType === "kukai") {
      // Use WalletConnect
      if (!process.env.WALLETCONNECT_PROJECT_ID) {
        console.log(`${RED}✗ Missing WALLETCONNECT_PROJECT_ID${RESET}`);
        console.log(`${DIM}Get one free at https://cloud.walletconnect.com${RESET}\n`);
        return null;
      }
      
      const { pairWalletWC } = await import("./walletconnect-node.mjs");
      const result = await pairWalletWC();
      
      if (result?.accounts?.length > 0) {
        address = result.accounts[0].address;
      }
    }
    
    if (!address) {
      console.log(`${RED}✗ No address received from wallet${RESET}\n`);
      return null;
    }
    
    connectedAddress = address;
    
    // Lookup domain
    domain = await fetchDomain(address, network);
    
    console.log(`\n${GREEN}✅ Wallet connected via QR!${RESET}`);
    if (domain) {
      console.log(`${CYAN}Domain:${RESET} ${domain}`);
    }
    console.log(`${CYAN}Address:${RESET} ${address}`);
    console.log(`${CYAN}Network:${RESET} ${config.name}\n`);
    
    // Save session
    await saveSession(address, network, domain);
    
    return address;
    
  } catch (err) {
    // Handle user rejection gracefully
    if (err.message?.includes('rejected') || err.message?.includes('User') || err.message?.includes('timeout')) {
      console.log(`\n${YELLOW}⚠️  Connection declined or timed out${RESET}\n`);
      return null;
    }
    console.log(`${RED}✗ QR connection failed: ${err.message}${RESET}\n`);
    return null;
  }
}

/**
 * Disconnect wallet
 */
export async function disconnect() {
  connectedAddress = null;
  await fs.unlink(WALLET_FILE).catch(() => {});
  console.log(`${GREEN}✅ Wallet disconnected${RESET}\n`);
}

/**
 * Get connected address (or null)
 */
export function getAddress() {
  return connectedAddress;
}

/**
 * Check if connected
 */
export function isConnected() {
  return connectedAddress !== null;
}

/**
 * Get Tezos toolkit for operations
 */
export function getTezos() {
  return tezos;
}

/**
 * NOTE: CLI cannot sign transactions directly (no Beacon in Node.js)
 * For now, we use server-side minting where admin pays gas
 * and tokens go to the user's connected address.
 * 
 * Future: Could integrate with Temple CLI or remote signing service
 */

/**
 * Fetch .tez domain for an address (reverse lookup)
 */
export async function fetchDomain(address, network = "ghostnet") {
  try {
    const tzkt = NETWORKS[network].tzkt;
    const res = await fetch(`${tzkt}/v1/domains?address=${address}&reverse=true&select=name`);
    if (res.ok) {
      const data = await res.json();
      if (data && data.length > 0) {
        return data[0];
      }
    }
  } catch (err) {
    // Ignore
  }
  return null;
}

/**
 * Resolve .tez domain to address
 */
export async function resolveDomain(domain, network = "ghostnet") {
  try {
    // Normalize domain - add .tez if not present
    const normalizedDomain = domain.endsWith('.tez') ? domain : `${domain}.tez`;
    
    const tzkt = NETWORKS[network].tzkt;
    const res = await fetch(`${tzkt}/v1/domains?name=${normalizedDomain}&select=address`);
    if (res.ok) {
      const data = await res.json();
      if (data && data.length > 0) {
        return data[0];
      }
    }
  } catch (err) {
    // Ignore
  }
  return null;
}

/**
 * Fetch balance for an address
 */
export async function fetchBalance(address, network = "ghostnet") {
  try {
    const rpc = NETWORKS[network].rpc;
    const res = await fetch(`${rpc}/chains/main/blocks/head/context/contracts/${address}/balance`);
    if (res.ok) {
      const mutez = await res.json();
      return parseInt(mutez) / 1_000_000;
    }
  } catch (err) {
    // Ignore
  }
  return null;
}

/**
 * Save wallet session
 */
async function saveSession(address, network, domain = null) {
  try {
    await fs.writeFile(WALLET_FILE, JSON.stringify({
      address,
      network,
      domain,
      connectedAt: new Date().toISOString(),
    }), "utf8");
  } catch (err) {
    // Ignore
  }
}

/**
 * Load saved session (for display only - actual session is in Beacon)
 */
export async function loadSession() {
  try {
    const data = await fs.readFile(WALLET_FILE, "utf8");
    return JSON.parse(data);
  } catch (err) {
    return null;
  }
}

/**
 * Update user's Tezos address in AC database
 * First tries API endpoint, falls back to direct DB if that fails
 */
export async function updateDatabaseAddress(address, network, token, userId = null) {
  // Try API endpoint first (works when dev server is running)
  try {
    const endpoint = process.env.AC_ENDPOINT || "https://localhost:8888";
    const response = await fetch(`${endpoint}/api/update-tezos-address`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${token}`,
      },
      body: JSON.stringify({ address, network }),
    });
    
    if (response.ok) {
      console.log(`${GREEN}✅ Saved address to AC profile${RESET}`);
      return true;
    }
  } catch (err) {
    // API not available, try direct DB
  }
  
  // Fallback: Direct database update (works in devcontainer)
  if (userId) {
    try {
      const { connect } = await import("../system/backend/database.mjs");
      const { db } = await connect();
      
      await db.collection("users").updateOne(
        { sub: userId },
        {
          $set: {
            "tezos.address": address,
            "tezos.network": network,
            "tezos.connectedAt": new Date(),
          },
        }
      );
      
      console.log(`${GREEN}✅ Saved address to AC profile (direct)${RESET}`);
      return true;
    } catch (err) {
      console.log(`${DIM}⚠️  Could not save to profile: ${err.message}${RESET}`);
    }
  }
  
  return false;
}
