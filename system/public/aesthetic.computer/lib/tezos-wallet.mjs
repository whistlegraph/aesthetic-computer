// tezos-wallet.mjs - Client-side Tezos wallet connection for Aesthetic Computer
// Uses Beacon SDK via Taquito for wallet connection

import { TezosToolkit } from "@taquito/taquito";
import { BeaconWallet } from "@taquito/beacon-wallet";

// Network configuration
const NETWORKS = {
  mainnet: {
    rpc: "https://mainnet.ecadinfra.com",
    name: "mainnet",
  },
  ghostnet: {
    rpc: "https://ghostnet.ecadinfra.com", 
    name: "ghostnet",
  },
};

// Current network (can be switched)
let currentNetwork = "mainnet"; // Default to mainnet

// Singleton instances
let tezos = null;
let wallet = null;
let connectedAddress = null;

/**
 * Initialize the Tezos toolkit and wallet
 * @param {string} network - 'mainnet' or 'ghostnet'
 */
export async function init(network = "mainnet") {
  currentNetwork = network;
  const config = NETWORKS[network];
  
  tezos = new TezosToolkit(config.rpc);
  
  wallet = new BeaconWallet({
    name: "Aesthetic Computer",
    preferredNetwork: config.name,
    appUrl: "https://aesthetic.computer",
    iconUrl: "https://aesthetic.computer/aesthetic-computer-logo.png",
  });
  
  tezos.setWalletProvider(wallet);
  
  // Check for existing connection
  const activeAccount = await wallet.client.getActiveAccount();
  if (activeAccount) {
    connectedAddress = activeAccount.address;
    console.log("🔗 Tezos wallet reconnected:", connectedAddress);
  }
  
  return { tezos, wallet, address: connectedAddress };
}

/**
 * Connect to a Tezos wallet (opens Beacon popup)
 * @returns {Promise<string>} The connected wallet address
 */
export async function connect() {
  if (!wallet) {
    await init(currentNetwork);
  }
  
  try {
    await wallet.requestPermissions({
      network: { type: NETWORKS[currentNetwork].name },
    });
    
    connectedAddress = await wallet.getPKH();
    console.log("✅ Tezos wallet connected:", connectedAddress);
    
    return connectedAddress;
  } catch (error) {
    console.error("❌ Wallet connection failed:", error);
    throw error;
  }
}

/**
 * Disconnect the current wallet
 */
export async function disconnect() {
  if (wallet) {
    await wallet.client.clearActiveAccount();
    connectedAddress = null;
    console.log("🔌 Tezos wallet disconnected");
  }
}

/**
 * Get the currently connected address
 * @returns {string|null} The connected address or null
 */
export function getAddress() {
  return connectedAddress;
}

/**
 * Check if a wallet is connected
 * @returns {boolean}
 */
export function isConnected() {
  return connectedAddress !== null;
}

/**
 * Get the Tezos toolkit instance (for advanced operations)
 * @returns {TezosToolkit}
 */
export function getTezos() {
  return tezos;
}

/**
 * Get the wallet instance
 * @returns {BeaconWallet}
 */
export function getWallet() {
  return wallet;
}

/**
 * Get the current network
 * @returns {string} 'mainnet' or 'ghostnet'
 */
export function getNetwork() {
  return currentNetwork;
}

/**
 * Switch to a different network
 * @param {string} network - 'mainnet' or 'ghostnet'
 */
export async function switchNetwork(network) {
  if (network !== currentNetwork) {
    await disconnect();
    await init(network);
  }
}

/**
 * Get the balance of the connected wallet
 * @returns {Promise<number>} Balance in XTZ
 */
export async function getBalance() {
  if (!connectedAddress) {
    throw new Error("No wallet connected");
  }
  
  const balance = await tezos.tz.getBalance(connectedAddress);
  return balance.toNumber() / 1_000_000; // Convert from mutez to XTZ
}

/**
 * Call a contract method using the connected wallet
 * @param {string} contractAddress - The contract address
 * @param {string} method - The method name
 * @param {object} params - The method parameters
 * @param {number} amount - Amount of XTZ to send (optional)
 * @returns {Promise<object>} The operation result
 */
export async function callContract(contractAddress, method, params, amount = 0) {
  if (!connectedAddress) {
    throw new Error("No wallet connected");
  }
  
  const contract = await tezos.wallet.at(contractAddress);
  
  const op = await contract.methodsObject[method](params).send({
    amount,
    mutez: false, // amount is in XTZ, not mutez
  });
  
  console.log(`📤 Transaction submitted: ${op.opHash}`);
  
  // Wait for confirmation
  await op.confirmation(1);
  
  console.log(`✅ Transaction confirmed: ${op.opHash}`);
  
  return {
    hash: op.opHash,
    confirmed: true,
  };
}

/**
 * Mint a Keep NFT using the connected wallet
 * @param {object} params - Minting parameters from keep-mint endpoint
 * @returns {Promise<object>} The minting result
 */
export async function mintKeep(contractAddress, mintParams) {
  if (!connectedAddress) {
    throw new Error("No wallet connected - please connect your Tezos wallet first");
  }
  
  const contract = await tezos.wallet.at(contractAddress);
  
  // The keep method takes all the metadata parameters
  const op = await contract.methodsObject.keep({
    ...mintParams,
    owner: stringToBytes(connectedAddress), // Set the connected wallet as owner
  }).send({
    amount: 5, // 5 XTZ mint fee
    mutez: false,
  });
  
  console.log(`🎨 Keep mint submitted: ${op.opHash}`);
  
  await op.confirmation(1);
  
  console.log(`✅ Keep minted successfully: ${op.opHash}`);
  
  return {
    hash: op.opHash,
    confirmed: true,
    owner: connectedAddress,
  };
}

// Helper: Convert string to hex bytes (for Tezos)
function stringToBytes(str) {
  return Array.from(new TextEncoder().encode(str))
    .map(b => b.toString(16).padStart(2, '0'))
    .join('');
}

// Export all functions
export default {
  init,
  connect,
  disconnect,
  getAddress,
  isConnected,
  getTezos,
  getWallet,
  getNetwork,
  switchNetwork,
  getBalance,
  callContract,
  mintKeep,
};
