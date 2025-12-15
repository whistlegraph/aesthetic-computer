/**
 * walletconnect-node.mjs - WalletConnect 2.0 client for Tezos
 * 
 * Works with Kukai mobile wallet (and other WC2-compatible wallets)
 * 
 * WalletConnect 2.0 uses:
 * - relay.walletconnect.com as the relay server
 * - X25519 key exchange for encryption
 * - JSON-RPC over websocket
 * - Tezos namespace: "tezos:ghostnet" or "tezos:mainnet"
 * 
 * Note: SDK v2.17.0 has a heartbeat bug that throws after ~30s in Node.js
 * We catch this gracefully - pairing works if wallet scans promptly.
 */

import { SignClient } from "@walletconnect/sign-client";
import qrcode from "qrcode-terminal";

// Gracefully handle WalletConnect heartbeat crash (SDK v2.17.0 bug)
process.on('uncaughtException', (err) => {
  if (err.message?.includes('terminate is not a function')) {
    // Known WalletConnect SDK bug - ignore and continue
    return;
  }
  console.error('Uncaught exception:', err);
  process.exit(1);
});

// ANSI colors
const GREEN = "\x1b[32m";
const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const CYAN = "\x1b[36m";
const DIM = "\x1b[2m";
const BOLD = "\x1b[1m";
const RESET = "\x1b[0m";

// WalletConnect Project ID - get one FREE at https://cloud.walletconnect.com
const PROJECT_ID = process.env.WALLETCONNECT_PROJECT_ID;

if (!PROJECT_ID) {
  console.log(`${RED}✗ No WalletConnect Project ID found${RESET}`);
  console.log(`\n${CYAN}To use WalletConnect 2.0 (for Kukai mobile):${RESET}`);
  console.log(`  1. Go to ${BOLD}https://cloud.walletconnect.com${RESET}`);
  console.log(`  2. Create a free account and project`);
  console.log(`  3. Copy your Project ID`);
  console.log(`  4. Set it: ${DIM}export WALLETCONNECT_PROJECT_ID=your_id_here${RESET}`);
  console.log(`\n${YELLOW}Or use Beacon P2P for Temple wallet instead:${RESET}`);
  console.log(`  ${DIM}node beacon-node.mjs --pair${RESET}\n`);
  process.exit(1);
}

// Tezos network configuration - CAIP-2 format
// Kukai uses simple names: tezos:mainnet or tezos:ghostnet
const TEZOS_NETWORK = process.env.TEZOS_NETWORK || "mainnet";
const TEZOS_CHAIN_ID = `tezos:${TEZOS_NETWORK}`;

// Supported methods for Tezos
const TEZOS_METHODS = [
  "tezos_getAccounts",
  "tezos_send",
  "tezos_sign"
];

// Supported events
const TEZOS_EVENTS = [];

/**
 * WalletConnect 2.0 Client for Tezos
 */
export class WalletConnectClient {
  constructor() {
    this.client = null;
    this.session = null;
  }

  /**
   * Initialize the WalletConnect SignClient
   */
  async init() {
    console.log(`${DIM}Initializing WalletConnect 2.0...${RESET}`);
    
    this.client = await SignClient.init({
      projectId: PROJECT_ID,
      metadata: {
        name: "Aesthetic Computer",
        description: "Creative coding platform",
        url: "https://aesthetic.computer",
        icons: ["https://aesthetic.computer/icon.png"]
      }
    });

    // Set up event listeners
    this.setupListeners();
    
    console.log(`${GREEN}✓${RESET} WalletConnect initialized`);
    return this;
  }

  /**
   * Set up event listeners for session events
   */
  setupListeners() {
    this.client.on("session_event", ({ event }) => {
      console.log(`${CYAN}Session event:${RESET}`, event);
    });

    this.client.on("session_update", ({ topic, params }) => {
      console.log(`${CYAN}Session updated:${RESET}`, topic);
      const { namespaces } = params;
      const session = this.client.session.get(topic);
      this.session = { ...session, namespaces };
    });

    this.client.on("session_delete", () => {
      console.log(`${YELLOW}Session deleted${RESET}`);
      this.session = null;
    });

    this.client.on("session_expire", ({ topic }) => {
      console.log(`${YELLOW}Session expired:${RESET}`, topic);
      this.session = null;
    });
  }

  /**
   * Connect to a wallet
   * Returns URI for QR code display
   */
  async connect() {
    console.log(`${DIM}Creating connection request...${RESET}`);

    const { uri, approval } = await this.client.connect({
      requiredNamespaces: {
        tezos: {
          methods: TEZOS_METHODS,
          chains: [TEZOS_CHAIN_ID],
          events: TEZOS_EVENTS
        }
      }
    });

    if (!uri) {
      throw new Error("No URI returned from connect()");
    }

    console.log(`${GREEN}✓${RESET} Connection URI created`);
    console.log(`${DIM}URI: ${uri.slice(0, 50)}...${RESET}`);

    return { uri, approval };
  }

  /**
   * Wait for session approval from wallet
   */
  async waitForApproval(approval) {
    console.log(`${YELLOW}⏳ Waiting for wallet approval...${RESET}`);
    
    try {
      this.session = await approval();
      console.log(`${GREEN}✓${RESET} Session established!`);
      return this.session;
    } catch (err) {
      console.log(`${RED}✗${RESET} Session rejected: ${err.message}`);
      throw err;
    }
  }

  /**
   * Get accounts from the connected wallet
   */
  getAccounts() {
    if (!this.session) {
      throw new Error("No active session");
    }

    const accounts = this.session.namespaces.tezos?.accounts || [];
    return accounts.map(acc => {
      // Format: "tezos:ghostnet:tz1..."
      const parts = acc.split(":");
      return {
        chain: parts[0],
        network: parts[1],
        address: parts[2]
      };
    });
  }

  /**
   * Request signing a payload
   */
  async signPayload(payload, account) {
    if (!this.session) {
      throw new Error("No active session");
    }

    console.log(`${DIM}Requesting signature...${RESET}`);

    const result = await this.client.request({
      topic: this.session.topic,
      chainId: TEZOS_CHAIN_ID,
      request: {
        method: "tezos_sign",
        params: {
          account: account,
          payload: payload
        }
      }
    });

    return result;
  }

  /**
   * Request sending an operation
   */
  async sendOperation(operations, account) {
    if (!this.session) {
      throw new Error("No active session");
    }

    console.log(`${DIM}Requesting operation...${RESET}`);

    const result = await this.client.request({
      topic: this.session.topic,
      chainId: TEZOS_CHAIN_ID,
      request: {
        method: "tezos_send",
        params: {
          account: account,
          operations: operations
        }
      }
    });

    return result;
  }

  /**
   * Disconnect the session
   */
  async disconnect() {
    if (!this.session) {
      return;
    }

    await this.client.disconnect({
      topic: this.session.topic,
      reason: {
        code: 6000,
        message: "User disconnected"
      }
    });

    this.session = null;
    console.log(`${GREEN}✓${RESET} Disconnected`);
  }
}

/**
 * Display QR code in terminal
 */
export function displayQR(uri) {
  console.log(`\n${BOLD}${CYAN}╔════════════════════════════════════════════════════════════════╗${RESET}`);
  console.log(`${BOLD}${CYAN}║  📱 Scan with Kukai mobile (WalletConnect)                      ║${RESET}`);
  console.log(`${BOLD}${CYAN}╚════════════════════════════════════════════════════════════════╝${RESET}\n`);
  
  // Generate QR synchronously to stdout
  qrcode.generate(uri, { small: true });
  
  console.log(`\n\n${DIM}Waiting for wallet connection...${RESET}\n\n`);
}

/**
 * Full WalletConnect pairing flow
 */
export async function pairWalletWC() {
  console.log(`\n${BOLD}${CYAN}═══════════════════════════════════════════════════════════════════${RESET}`);
  console.log(`${BOLD}${CYAN}  WalletConnect 2.0 Wallet Pairing (Kukai)                         ${RESET}`);
  console.log(`${BOLD}${CYAN}═══════════════════════════════════════════════════════════════════${RESET}\n`);

  // Initialize client
  const client = await new WalletConnectClient().init();

  // Create connection
  const { uri, approval } = await client.connect();

  // Display QR code
  displayQR(uri);

  // Wait for approval
  const session = await client.waitForApproval(approval);

  // Get accounts
  const accounts = client.getAccounts();
  
  console.log(`\n${GREEN}✓ Wallet connected!${RESET}`);
  console.log(`  ${DIM}Network:${RESET} ${TEZOS_NETWORK}`);
  
  if (accounts.length > 0) {
    console.log(`  ${DIM}Accounts:${RESET}`);
    accounts.forEach(acc => {
      console.log(`    - ${acc.address}`);
    });
  }

  return { client, session, accounts };
}

// CLI entry point
const args = process.argv.slice(2);

if (args.includes("--pair") || args.includes("-p")) {
  pairWalletWC()
    .then(({ accounts, client }) => {
      console.log(`\n${GREEN}✓ Pairing complete!${RESET}`);
      if (accounts.length > 0) {
        console.log(`  Address: ${accounts[0].address}`);
      }
      // Give the SDK a moment to settle, then exit cleanly
      setTimeout(() => process.exit(0), 500);
    })
    .catch(err => {
      // Handle user rejection gracefully
      if (err.message?.includes('rejected') || err.message?.includes('User')) {
        console.log(`\n${YELLOW}Connection declined by user${RESET}`);
        process.exit(0);
      }
      console.error(`${RED}Error: ${err.message}${RESET}`);
      process.exit(1);
    });
} else if (args.includes("--help") || args.includes("-h")) {
  console.log(`
${BOLD}WalletConnect 2.0 Client for Tezos${RESET}

${CYAN}Usage:${RESET}
  node walletconnect-node.mjs [options]

${CYAN}Options:${RESET}
  --pair, -p     Start wallet pairing (displays QR code)
  --help, -h     Show this help message

${CYAN}Environment Variables:${RESET}
  WALLETCONNECT_PROJECT_ID   Your WalletConnect project ID
  TEZOS_NETWORK              Network to use (ghostnet/mainnet)

${CYAN}Supported Wallets:${RESET}
  - Kukai mobile
  - Any WalletConnect 2.0 compatible Tezos wallet
`);
} else {
  console.log(`Use --pair to start wallet pairing, or --help for more options`);
}
