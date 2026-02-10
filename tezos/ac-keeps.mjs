#!/usr/bin/env node
/**
 * ac-keeps - Interactive CLI for keeping KidLisp pieces on Tezos
 * 
 * Usage:
 *   ac-keeps list              - List your KidLisp pieces
 *   ac-keeps list --top        - List by most popular (hits)
 *   ac-keeps list --recent     - List most recent (default)
 *   ac-keeps keep <code>       - Keep a piece on Ghostnet
 *   ac-keeps status <code>     - Check if already kept
 *   ac-keeps wallet            - Connect/view Tezos wallet
 *   ac-keeps wallet disconnect - Disconnect wallet
 */

import { promises as fs } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { connect } from '../system/backend/database.mjs';
import readline from 'readline';
import * as cliWallet from './cli-wallet.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const TOKEN_FILE = join(process.env.HOME, '.ac-token');

// Contract address - Ghostnet v3
const CONTRACT_ADDRESS = "KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K";
const NETWORK = "ghostnet";
const KEEP_FEE = 5; // XTZ

// Colors
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const DIM = '\x1b[2m';
const CYAN = '\x1b[36m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const RED = '\x1b[31m';
const BLUE = '\x1b[34m';
const MAGENTA = '\x1b[35m';

// Check authentication
async function checkAuth() {
  try {
    const tokenData = await fs.readFile(TOKEN_FILE, 'utf8');
    const tokens = JSON.parse(tokenData);
    
    if (tokens.expires_at && Date.now() > tokens.expires_at) {
      console.log(`${RED}❌ Token expired${RESET}`);
      console.log(`${DIM}Run: ac-login${RESET}\n`);
      process.exit(1);
    }
    
    return tokens;
  } catch (err) {
    console.log(`${RED}❌ Not logged in${RESET}`);
    console.log(`${DIM}Run: ac-login${RESET}\n`);
    process.exit(1);
  }
}

// Get user ID and info from token
async function getUserId(tokens) {
  const { db } = await connect();
  
  // Try to find user by email
  const email = tokens.user?.email;
  if (!email) {
    console.log(`${RED}❌ No email in token${RESET}`);
    process.exit(1);
  }
  
  // Find user by Auth0 sub
  const user = await db.collection('users').findOne({ 
    _id: tokens.user.sub 
  });
  
  if (!user) {
    console.log(`${RED}❌ User not found in database${RESET}`);
    process.exit(1);
  }
  
  // Extract handle from atproto.handle (e.g. "jeffrey.at.aesthetic.computer" -> "jeffrey")
  const fullHandle = user.atproto?.handle || '';
  const handle = fullHandle.replace('.at.aesthetic.computer', '') || email.split('@')[0];
  
  return {
    id: user._id,
    handle,
    email,
    fullHandle
  };
}

// List KidLisp pieces (default sort by top hits)
async function listPieces(userId, userInfo, sortBy = 'top', limit = 50) {
  const { db } = await connect();
  
  const sort = sortBy === 'recent' ? { when: -1 } : { hits: -1 };
  
  const pieces = await db.collection('kidlisp')
    .find({ user: userId })
    .sort(sort)
    .limit(limit)
    .toArray();
  
  const total = await db.collection('kidlisp').countDocuments({ user: userId });
  const keptCount = await db.collection('kidlisp').countDocuments({ user: userId, 'tezos.minted': true });
  
  console.log(`\n${BOLD}${CYAN}╔════════════════════════════════════════════════════════════════╗${RESET}`);
  console.log(`${BOLD}${CYAN}║  🎨 Your KidLisp Pieces${RESET}                                        ${BOLD}${CYAN}║${RESET}`);
  console.log(`${BOLD}${CYAN}╚════════════════════════════════════════════════════════════════╝${RESET}\n`);
  
  // Show user info
  if (userInfo) {
    console.log(`${DIM}👤 ${RESET}${BOLD}@${userInfo.handle}${RESET} ${DIM}(${userInfo.email})${RESET}`);
  }
  console.log(`${DIM}📊 ${total} pieces | ${GREEN}${keptCount} kept${RESET}${DIM} | Showing ${pieces.length} (sorted by ${sortBy})${RESET}\n`);
  
  pieces.forEach((p, i) => {
    const hits = p.hits || 0;
    const code = p.code;
    const preview = colorizeKidlisp(p.source.substring(0, 65).replace(/\n/g, ' '));
    const date = new Date(p.when).toLocaleDateString();
    const minted = p.tezos?.minted ? `${GREEN}✓ kept${RESET}` : `${RED}unkept${RESET}`;
    const url = `https://prompt.ac/$${code}`;
    
    console.log(`${BOLD}${(i+1).toString().padStart(2)}.${RESET} ${YELLOW}$${code}${RESET} ${minted} ${DIM}· 💫 ${hits} hits · ${date}${RESET}`);
    console.log(`    ${preview}${RESET}`);
    console.log(`    ${DIM}${BLUE}${url}${RESET}\n`);
  });
  
  console.log(`${DIM}To keep a piece (5 ꜩ): ${BOLD}ac-keeps keep <code>${RESET}\n`);
}

// CSS color map (subset of common ones)
const CSS_COLORS = {
  red: [255, 0, 0], blue: [0, 0, 255], green: [0, 128, 0], yellow: [255, 255, 0],
  orange: [255, 165, 0], purple: [128, 0, 128], pink: [255, 192, 203], 
  black: [0, 0, 0], white: [255, 255, 255], gray: [128, 128, 128],
  brown: [165, 42, 42], salmon: [250, 128, 114], beige: [245, 245, 220],
  coral: [255, 127, 80], crimson: [220, 20, 60], cyan: [0, 255, 255],
  gold: [255, 215, 0], indigo: [75, 0, 130], lime: [0, 255, 0],
  magenta: [255, 0, 255], maroon: [128, 0, 0], navy: [0, 0, 128],
  olive: [128, 128, 0], teal: [0, 128, 128], violet: [238, 130, 238],
  aqua: [0, 255, 255], azure: [240, 255, 255], chocolate: [210, 105, 30],
  darkred: [139, 0, 0], darkblue: [0, 0, 139], darkgreen: [0, 100, 0],
  deeppink: [255, 20, 147], hotpink: [255, 105, 180], lavender: [230, 230, 250],
  lightblue: [173, 216, 230], lightgreen: [144, 238, 144], lightsteelblue: [176, 196, 222],
  limegreen: [50, 205, 50], mediumseagreen: [60, 179, 113], orangered: [255, 69, 0],
  palegreen: [152, 251, 152], plum: [221, 160, 221], royalblue: [65, 105, 225],
  skyblue: [135, 206, 235], steelblue: [70, 130, 180], tomato: [255, 99, 71],
  turquoise: [64, 224, 208], yellowgreen: [154, 205, 50], orchid: [218, 112, 214],
  fuchsia: [255, 0, 255], tan: [210, 180, 140], sienna: [160, 82, 45],
};
const CSS_COLOR_NAMES = Object.keys(CSS_COLORS);

// Rainbow colors for animated effect
const RAINBOW_COLORS = [[255,0,0], [255,165,0], [255,255,0], [0,128,0], [0,0,255], [75,0,130], [238,130,238]];

// Helper to make RGB ANSI code
function rgb(r, g, b) {
  return `\x1b[38;2;${r};${g};${b}m`;
}

// KidLisp syntax colorizer for terminal (matches kidlisp.mjs style)
// Uses token-based approach to avoid double-processing
function colorizeKidlisp(source) {
  // Tokenize: split into colorizable tokens and preserve spacing/operators
  const tokens = [];
  let remaining = source;
  let rainbowIdx = 0;
  
  // Pattern to match: fade:xxx, rainbow, zebra, color names, $refs, numbers, timing, words, parens, or single chars
  const tokenPattern = /fade:[a-zA-Z0-9:-]+|\brainbow\b|\bzebra\b|\$[a-zA-Z0-9_-]+|\b\d*\.?\d+s!?\b|\b\d+(\.\d+)?\b|"[^"]*"|;[^\n]*|\([a-zA-Z][a-zA-Z0-9-]*|\)|\b[a-zA-Z][a-zA-Z0-9]*\b|./g;
  
  let match;
  while ((match = tokenPattern.exec(source)) !== null) {
    const tok = match[0];
    const lower = tok.toLowerCase();
    
    // fade: expressions - emerald green
    if (tok.startsWith('fade:')) {
      tokens.push(rgb(60, 179, 113) + tok + RESET);
    }
    // rainbow - cycling rainbow colors
    else if (lower === 'rainbow') {
      const c = RAINBOW_COLORS[rainbowIdx % RAINBOW_COLORS.length];
      rainbowIdx++;
      tokens.push(rgb(c[0], c[1], c[2]) + tok + RESET);
    }
    // zebra - inverse video
    else if (lower === 'zebra') {
      tokens.push('\x1b[7m' + tok + RESET);
    }
    // CSS color names
    else if (CSS_COLOR_NAMES.includes(lower)) {
      const c = CSS_COLORS[lower];
      tokens.push(rgb(c[0], c[1], c[2]) + tok + RESET);
    }
    // Piece references ($xxx) - lime green
    else if (tok.startsWith('$')) {
      tokens.push(rgb(50, 205, 50) + tok + RESET);
    }
    // Comments - dim gray
    else if (tok.startsWith(';')) {
      tokens.push(DIM + tok + RESET);
    }
    // String literals - yellow
    else if (tok.startsWith('"')) {
      tokens.push(YELLOW + tok + RESET);
    }
    // Timing patterns (1s, 0.5s) - yellow
    else if (/^\d*\.?\d+s!?$/.test(tok)) {
      tokens.push(YELLOW + tok + RESET);
    }
    // Function calls (open paren + name) - dim paren, yellow name
    else if (tok.startsWith('(') && tok.length > 1) {
      tokens.push(DIM + '(' + RESET + YELLOW + tok.slice(1) + RESET);
    }
    // Numbers - magenta/pink
    else if (/^\d+(\.\d+)?$/.test(tok)) {
      tokens.push(MAGENTA + tok + RESET);
    }
    // Close parens - dim
    else if (tok === ')') {
      tokens.push(DIM + ')' + RESET);
    }
    // Everything else unchanged
    else {
      tokens.push(tok);
    }
  }
  
  return tokens.join('');
}

// Check Keep status
async function checkStatus(userId, code) {
  const { db } = await connect();
  
  const cleanCode = code.replace(/^\$/, '');
  const piece = await db.collection('kidlisp').findOne({ 
    user: userId,
    code: cleanCode
  });
  
  if (!piece) {
    console.log(`${RED}❌ Piece $${cleanCode} not found${RESET}\n`);
    return;
  }
  
  console.log(`\n${BOLD}${CYAN}Keep Status: ${YELLOW}$${cleanCode}${RESET}\n`);
  console.log(`${DIM}${piece.source.substring(0, 80)}...${RESET}\n`);
  
  if (piece.tezos?.minted) {
    console.log(`${GREEN}✅ Already minted as Keep!${RESET}`);
    console.log(`${DIM}Token ID: ${piece.tezos.tokenId}${RESET}`);
    console.log(`${DIM}Transaction: ${piece.tezos.opHash}${RESET}`);
    if (piece.tezos.ipfs) {
      console.log(`${DIM}IPFS: ${piece.tezos.ipfs.artifact}${RESET}`);
    }
  } else {
    console.log(`${YELLOW}⚪ Not yet kept${RESET}`);
    console.log(`${DIM}Run: ${BOLD}ac-keeps keep $${cleanCode}${RESET}\n`);
  }
  console.log('');
}

// Connect wallet command - now with QR option
async function connectWallet(tokens, userId = null, useQR = false) {
  console.log(`\n${BOLD}${CYAN}╔════════════════════════════════════════════════════════════════╗${RESET}`);
  console.log(`${BOLD}${CYAN}║  🔷 Connect Tezos Wallet                                       ║${RESET}`);
  console.log(`${BOLD}${CYAN}╚════════════════════════════════════════════════════════════════╝${RESET}\n`);
  
  try {
    // Initialize and connect
    await cliWallet.init(NETWORK);
    
    // Check if already connected
    let address = cliWallet.getAddress();
    if (address) {
      const balance = await cliWallet.fetchBalance(address, NETWORK);
      const domain = await cliWallet.fetchDomain(address, NETWORK);
      const displayName = domain || `${address.slice(0, 8)}...${address.slice(-6)}`;
      
      console.log(`${GREEN}✅ Already connected${RESET}`);
      console.log(`${CYAN}Address:${RESET} ${displayName}`);
      console.log(`${CYAN}Full:${RESET} ${address}`);
      if (balance !== null) {
        console.log(`${CYAN}Balance:${RESET} ${balance.toFixed(2)} ꜩ`);
      }
      console.log(`${CYAN}Network:${RESET} ${NETWORK}\n`);
      return address;
    }
    
    // Ask user how they want to connect if not specified
    if (!useQR) {
      console.log(`${CYAN}How do you want to connect?${RESET}\n`);
      console.log(`  ${BOLD}1${RESET} - 📱 Scan QR code with mobile wallet (Temple/Kukai)`);
      console.log(`  ${BOLD}2${RESET} - ⌨️  Enter address manually`);
      console.log();
      
      const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
      const choice = await new Promise(resolve => {
        rl.question(`${GREEN}Enter choice (1/2): ${RESET}`, answer => {
          rl.close();
          resolve(answer.trim());
        });
      });
      
      if (choice === '1') {
        useQR = true;
      }
    }
    
    if (useQR) {
      // Show wallet selection for QR
      console.log(`\n${CYAN}Select wallet:${RESET}\n`);
      console.log(`  ${BOLD}1${RESET} - Temple Wallet (Beacon P2P)`);
      console.log(`  ${BOLD}2${RESET} - Kukai Wallet (WalletConnect)`);
      console.log();
      
      const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
      const walletChoice = await new Promise(resolve => {
        rl.question(`${GREEN}Enter choice (1/2): ${RESET}`, answer => {
          rl.close();
          resolve(answer.trim());
        });
      });
      
      const walletType = walletChoice === '2' ? 'kukai' : 'temple';
      address = await cliWallet.connectViaQR(walletType, NETWORK);
    } else {
      // Manual address entry
      address = await cliWallet.connect(NETWORK);
    }
    
    // Update AC database with new address (pass userId for direct DB fallback)
    if (address) {
      await updateMongoDBWallet(userId, address, NETWORK);
      if (tokens?.access_token) {
        await cliWallet.updateDatabaseAddress(address, NETWORK, tokens.access_token, userId);
      }
    }
    
    return address;
    
  } catch (err) {
    console.log(`${RED}❌ Wallet connection failed${RESET}`);
    console.log(`${DIM}${err.message}${RESET}\n`);
    return null;
  }
}

// Update MongoDB with wallet address directly
async function updateMongoDBWallet(userId, address, network) {
  if (!userId) return false;
  
  try {
    const { db } = await connect();
    
    // Fetch domain for this address
    const domain = await cliWallet.fetchDomain(address, network);
    
    await db.collection("users").updateOne(
      { _id: userId },
      {
        $set: {
          "tezos.address": address,
          "tezos.network": network,
          "tezos.domain": domain,
          "tezos.connectedAt": new Date(),
        },
      }
    );
    
    console.log(`${GREEN}✅ Wallet saved to AC profile${RESET}`);
    if (domain) {
      console.log(`${DIM}   Domain: ${domain}${RESET}`);
    }
    console.log(`${DIM}   Address: ${address.slice(0, 12)}...${RESET}\n`);
    return true;
  } catch (err) {
    console.log(`${DIM}⚠️  Could not save to MongoDB: ${err.message}${RESET}`);
    return false;
  }
}

// Show wallet status
async function walletStatus() {
  await cliWallet.init(NETWORK);
  const address = cliWallet.getAddress();
  
  if (!address) {
    console.log(`${YELLOW}⚪ No wallet connected${RESET}`);
    console.log(`${DIM}Run: ${BOLD}ac-keeps wallet${RESET} ${DIM}to connect${RESET}\n`);
    return null;
  }
  
  const balance = await cliWallet.fetchBalance(address, NETWORK);
  const domain = await cliWallet.fetchDomain(address, NETWORK);
  
  console.log(`${GREEN}✅ Wallet connected${RESET}`);
  if (domain) {
    console.log(`${CYAN}Domain:${RESET} ${domain}`);
  }
  console.log(`${CYAN}Address:${RESET} ${address}`);
  if (balance !== null) {
    console.log(`${CYAN}Balance:${RESET} ${balance.toFixed(2)} ꜩ`);
  }
  console.log(`${CYAN}Network:${RESET} ${NETWORK}\n`);
  
  return address;
}

// Disconnect wallet
async function disconnectWallet() {
  await cliWallet.init(NETWORK);
  await cliWallet.disconnect();
  console.log(`${GREEN}✅ Wallet disconnected${RESET}\n`);
}

// Keep a piece (USER PAYS via Beacon wallet)
async function keepPiece(userId, code, tokens) {
  const cleanCode = code.replace(/^\$/, '');
  
  console.log(`\n${BOLD}${CYAN}╔════════════════════════════════════════════════════════════════╗${RESET}`);
  console.log(`${BOLD}${CYAN}║  🏺 Keeping: ${YELLOW}$${cleanCode}${RESET}                                         ${BOLD}${CYAN}║${RESET}`);
  console.log(`${BOLD}${CYAN}╚════════════════════════════════════════════════════════════════╝${RESET}\n`);
  
  // Check piece exists and not already kept
  const { db } = await connect();
  const piece = await db.collection('kidlisp').findOne({ 
    user: userId,
    code: cleanCode
  });
  
  if (!piece) {
    console.log(`${RED}❌ Piece $${cleanCode} not found${RESET}\n`);
    return;
  }
  
  if (piece.tezos?.minted) {
    console.log(`${YELLOW}⚠️  Already kept!${RESET}`);
    console.log(`${DIM}Token ID: ${piece.tezos.tokenId}${RESET}\n`);
    return;
  }
  
  // Initialize wallet and check connection
  await cliWallet.init(NETWORK);
  let walletAddress = cliWallet.getAddress();
  
  if (!walletAddress) {
    console.log(`${YELLOW}⚠️  No wallet connected. Connecting now...${RESET}\n`);
    walletAddress = await connectWallet(tokens, userId);
    if (!walletAddress) {
      return;
    }
  }
  
  // Lookup .tez domain for better UX
  const domain = await cliWallet.fetchDomain(walletAddress, NETWORK);
  const displayAddress = domain 
    ? `${domain} (${walletAddress.slice(0, 8)}...${walletAddress.slice(-6)})`
    : `${walletAddress.slice(0, 8)}...${walletAddress.slice(-6)}`;
  
  console.log(`${DIM}${piece.source.substring(0, 80)}...${RESET}\n`);
  console.log(`${CYAN}📍 Destination:${RESET} ${displayAddress}`);
  console.log(`${CYAN}🌐 Network:${RESET} ${NETWORK}`);
  console.log(`${CYAN}💰 Fee:${RESET} Sponsored by AC (testnet)${RESET}`);
  console.log('');
  
  // Confirmation
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
  
  const confirmed = await new Promise((resolve) => {
    rl.question(`${YELLOW}Keep this piece? (y/n): ${RESET}`, (answer) => {
      rl.close();
      resolve(answer.toLowerCase() === 'y' || answer.toLowerCase() === 'yes');
    });
  });
  
  if (!confirmed) {
    console.log(`${DIM}Cancelled${RESET}\n`);
    return;
  }
  
  // Call mint endpoint (server-side minting, token goes to user's address)
  console.log(`\n${CYAN}📡 Minting to ${displayAddress}...${RESET}\n`);
  
  const endpoint = process.env.AC_ENDPOINT || 'https://localhost:8888/api/keep-mint';
  
  const response = await fetch(endpoint, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${tokens.access_token}`,
    },
    body: JSON.stringify({
      piece: cleanCode,
      mode: 'mint' // Server-side mint to user's stored address
    }),
  });
  
  if (!response.ok) {
    console.log(`${RED}❌ Request failed: ${response.status}${RESET}\n`);
    return;
  }
  
  // Stream SSE events
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  
  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    
    const chunk = decoder.decode(value);
    const lines = chunk.split('\n');
    
    for (const line of lines) {
      if (line.startsWith('data: ')) {
        try {
          const data = JSON.parse(line.slice(6));
          
          if (data.type === 'progress') {
            console.log(`${CYAN}▸${RESET} ${data.data?.stage || ''}: ${data.data?.message || ''}`);
          } else if (data.type === 'complete') {
            console.log(`\n${GREEN}🏺 KEPT!${RESET}\n`);
            console.log(`${CYAN}Token ID:${RESET} ${data.data.tokenId}`);
            console.log(`${CYAN}Owner:${RESET} ${displayAddress}`);
            console.log(`${CYAN}Contract:${RESET} ${data.data.contract}`);
            console.log(`${CYAN}Transaction:${RESET} ${data.data.opHash}`);
            console.log(`${CYAN}View:${RESET} ${data.data.objktUrl}\n`);
          } else if (data.type === 'error') {
            console.log(`${RED}❌ Error: ${data.data?.error || 'Unknown error'}${RESET}\n`);
            return;
          }
        } catch (e) {
          // Ignore parse errors
        }
      }
    }
  }
}

// Interactive mode
async function interactive(userId, tokens, userInfo) {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
  
  console.log(`\n${BOLD}${MAGENTA}╔════════════════════════════════════════════════════════════════╗${RESET}`);
  console.log(`${BOLD}${MAGENTA}║  🏳️ AC Keeps - Interactive Mode                               ║${RESET}`);
  console.log(`${BOLD}${MAGENTA}╚════════════════════════════════════════════════════════════════╝${RESET}\n`);
  
  // Show user info
  console.log(`${DIM}👤 ${RESET}${BOLD}@${userInfo.handle}${RESET} ${DIM}(${userInfo.email})${RESET}\n`);
  
  // Show wallet status on start
  await cliWallet.init(NETWORK);
  const walletAddr = cliWallet.getAddress();
  if (walletAddr) {
    const domain = await cliWallet.fetchDomain(walletAddr, NETWORK);
    const display = domain || `${walletAddr.slice(0, 8)}...${walletAddr.slice(-6)}`;
    console.log(`${GREEN}🔷 Wallet:${RESET} ${display}\n`);
  } else {
    console.log(`${YELLOW}⚪ No wallet connected${RESET} ${DIM}(run: wallet)${RESET}\n`);
  }
  
  console.log(`${DIM}Commands: list, top, keep <code>, status <code>, wallet, exit${RESET}\n`);
  
  const prompt = () => {
    rl.question(`${CYAN}keeps>${RESET} `, async (input) => {
      const [cmd, ...args] = input.trim().split(/\s+/);
      
      switch (cmd) {
        case 'list':
          await listPieces(userId, userInfo, 'recent', 20);
          break;
        case 'top':
          await listPieces(userId, userInfo, 'top', 20);
          break;
        case 'keep':
          if (args.length === 0) {
            console.log(`${RED}Usage: keep <code>${RESET}\n`);
          } else {
            await keepPiece(userId, args[0], tokens);
          }
          break;
        case 'wallet':
          if (args[0] === 'disconnect') {
            await disconnectWallet();
          } else if (args[0] === 'qr') {
            await connectWallet(tokens, userId, true);
          } else {
            await connectWallet(tokens, userId);
          }
          break;
        case 'status':
          if (args.length === 0) {
            console.log(`${RED}Usage: status <code>${RESET}\n`);
          } else {
            await checkStatus(userId, args[0]);
          }
          break;
        case 'exit':
        case 'quit':
          console.log(`${DIM}Goodbye!${RESET}\n`);
          rl.close();
          process.exit(0);
          return;
        case '':
          break;
        default:
          console.log(`${RED}Unknown command: ${cmd}${RESET}\n`);
          console.log(`${DIM}Commands: list, top, keep <code>, status <code>, wallet [qr], exit${RESET}\n`);
      }
      
      prompt();
    });
  };
  
  prompt();
}

// Main
(async () => {
  const tokens = await checkAuth();
  const userInfo = await getUserId(tokens);
  const userId = userInfo.id;
  
  const args = process.argv.slice(2);
  const command = args[0];
  
  if (!command) {
    // No command - enter interactive mode
    await interactive(userId, tokens, userInfo);
    return;
  }
  
  switch (command) {
    case 'list':
      const sortBy = args.includes('--recent') ? 'recent' : 'top';
      const limit = parseInt(args.find(a => a.match(/^\d+$/))) || 50;
      await listPieces(userId, userInfo, sortBy, limit);
      break;
      
    case 'keep':
      if (args.length < 2) {
        console.log(`${RED}Usage: ac-keeps keep <code>${RESET}\n`);
        process.exit(1);
      }
      await keepPiece(userId, args[1], tokens);
      break;
      
    case 'wallet':
      if (args[1] === 'disconnect') {
        await disconnectWallet();
      } else if (args[1] === 'status') {
        await walletStatus();
      } else if (args[1] === 'qr' || args.includes('--qr')) {
        // QR code scanning
        await connectWallet(tokens, userId, true);
      } else {
        await connectWallet(tokens, userId);
      }
      break;
      
    case 'status':
      if (args.length < 2) {
        console.log(`${RED}Usage: ac-keeps status <code>${RESET}\n`);
        process.exit(1);
      }
      await checkStatus(userId, args[1]);
      break;
      
    default:
      console.log(`${RED}Unknown command: ${command}${RESET}\n`);
      console.log(`${DIM}Usage:${RESET}`);
      console.log(`  ac-keeps              - Interactive mode`);
      console.log(`  ac-keeps list         - List recent pieces`);
      console.log(`  ac-keeps list --top   - List by popularity`);
      console.log(`  ac-keeps keep <code>  - Keep a piece on Tezos`);
      console.log(`  ac-keeps wallet       - Connect Tezos wallet`);
      console.log(`  ac-keeps wallet disconnect - Disconnect wallet`);
      console.log(`  ac-keeps status <code> - Check status`);
      console.log('');
      process.exit(1);
  }
  
  process.exit(0);
})();
