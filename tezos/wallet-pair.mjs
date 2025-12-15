#!/usr/bin/env node
/**
 * wallet-pair.mjs - Unified Tezos wallet pairing CLI
 * 
 * Supports two wallet connection methods:
 * 1. Temple Wallet (Beacon P2P via Matrix relay)
 * 2. Kukai Wallet (WalletConnect 2.0)
 * 
 * Usage:
 *   node wallet-pair.mjs              # Interactive menu
 *   node wallet-pair.mjs --temple     # Direct Temple pairing
 *   node wallet-pair.mjs --kukai      # Direct Kukai pairing
 */

import { spawn } from "child_process";
import { fileURLToPath } from "url";
import { dirname, join } from "path";
import readline from "readline";

const __dirname = dirname(fileURLToPath(import.meta.url));

// ANSI colors
const GREEN = "\x1b[32m";
const YELLOW = "\x1b[33m";
const RED = "\x1b[31m";
const CYAN = "\x1b[36m";
const DIM = "\x1b[2m";
const BOLD = "\x1b[1m";
const RESET = "\x1b[0m";

// Wallet configuration
const WALLETS = {
  temple: {
    name: "Temple Wallet",
    protocol: "Beacon P2P",
    script: "beacon-node.mjs",
    args: ["--pair"],
    description: "Most popular Tezos browser & mobile wallet"
  },
  kukai: {
    name: "Kukai Wallet", 
    protocol: "WalletConnect",
    script: "walletconnect-node.mjs",
    args: ["--pair"],
    envRequired: "WALLETCONNECT_PROJECT_ID",
    description: "Tezos wallet with social login support"
  }
};

/**
 * Ask a question and return the answer
 */
function askQuestion(query) {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
  
  return new Promise(resolve => {
    rl.question(query, answer => {
      rl.close();
      resolve(answer.trim());
    });
  });
}

/**
 * Run a wallet pairing script
 */
function runWalletScript(walletType) {
  return new Promise((resolve, reject) => {
    const wallet = WALLETS[walletType];
    if (!wallet) {
      reject(new Error(`Unknown wallet type: ${walletType}`));
      return;
    }

    // Check for required environment variables
    if (wallet.envRequired && !process.env[wallet.envRequired]) {
      console.log(`\n${RED}✗ Missing required: ${wallet.envRequired}${RESET}`);
      if (walletType === "kukai") {
        console.log(`\n${CYAN}To use WalletConnect (Kukai):${RESET}`);
        console.log(`  1. Get a free project ID at ${BOLD}https://cloud.walletconnect.com${RESET}`);
        console.log(`  2. Run: ${DIM}export WALLETCONNECT_PROJECT_ID=your_id_here${RESET}`);
        console.log(`  3. Try again\n`);
      }
      reject(new Error(`Missing ${wallet.envRequired}`));
      return;
    }

    console.log(`\n${CYAN}Starting ${wallet.name} pairing (${wallet.protocol})...${RESET}\n`);

    const scriptPath = join(__dirname, wallet.script);
    const child = spawn("node", [scriptPath, ...wallet.args], {
      stdio: "inherit",
      cwd: __dirname,
      env: process.env
    });
    
    child.on("close", code => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`Pairing exited with code ${code}`));
      }
    });
    
    child.on("error", reject);
  });
}

/**
 * Show interactive menu
 */
async function showMenu() {
  console.clear();
  console.log(`\n${BOLD}════════════════════════════════════════════════════════════════════${RESET}`);
  console.log(`${BOLD}  🎨 Aesthetic Computer - Tezos Wallet Pairing${RESET}`);
  console.log(`${BOLD}════════════════════════════════════════════════════════════════════${RESET}\n`);
  
  console.log(`${CYAN}Choose your wallet:${RESET}\n`);
  console.log(`  ${BOLD}1${RESET} - ${WALLETS.temple.name} ${DIM}(${WALLETS.temple.protocol})${RESET}`);
  console.log(`      ${DIM}${WALLETS.temple.description}${RESET}`);
  console.log();
  console.log(`  ${BOLD}2${RESET} - ${WALLETS.kukai.name} ${DIM}(${WALLETS.kukai.protocol})${RESET}`);
  console.log(`      ${DIM}${WALLETS.kukai.description}${RESET}`);
  console.log();
  console.log(`  ${BOLD}q${RESET} - Quit\n`);
  
  const choice = await askQuestion(`${GREEN}Enter choice (1/2/q): ${RESET}`);
  
  switch (choice.toLowerCase()) {
    case '1':
      return 'temple';
    case '2':
      return 'kukai';
    case 'q':
      console.log(`\n${DIM}Goodbye!${RESET}\n`);
      process.exit(0);
    default:
      console.log(`\n${YELLOW}Invalid choice. Please enter 1, 2, or q.${RESET}\n`);
      return null;
  }
}

/**
 * Main entry point
 */
async function main() {
  const args = process.argv.slice(2);
  
  // Handle direct wallet selection via CLI args
  if (args.includes("--temple") || args.includes("-t")) {
    await runWalletScript("temple");
    return;
  }
  
  if (args.includes("--kukai") || args.includes("-k")) {
    await runWalletScript("kukai");
    return;
  }
  
  if (args.includes("--help") || args.includes("-h")) {
    console.log(`
${BOLD}Tezos Wallet Pairing Tool${RESET}

${CYAN}Usage:${RESET}
  node wallet-pair.mjs              Interactive wallet selection
  node wallet-pair.mjs --temple     Pair with Temple Wallet (Beacon)
  node wallet-pair.mjs --kukai      Pair with Kukai Wallet (WalletConnect)

${CYAN}Supported Wallets:${RESET}
  ${BOLD}Temple${RESET}  - Uses Beacon P2P protocol (Matrix relay)
           Works with Temple browser extension and mobile app
           
  ${BOLD}Kukai${RESET}   - Uses WalletConnect 2.0 protocol
           Requires WALLETCONNECT_PROJECT_ID environment variable
           Get one free at https://cloud.walletconnect.com

${CYAN}Environment Variables:${RESET}
  WALLETCONNECT_PROJECT_ID  Required for Kukai pairing
  TEZOS_NETWORK             Network to use (ghostnet/mainnet)
`);
    return;
  }
  
  // Interactive menu
  let walletType = null;
  while (!walletType) {
    walletType = await showMenu();
  }
  
  await runWalletScript(walletType);
}

// Run
main().catch(err => {
  // Don't show error for expected exits
  if (err.message?.includes('Missing')) {
    process.exit(1);
  }
  console.error(`${RED}Error: ${err.message}${RESET}`);
  process.exit(1);
});
