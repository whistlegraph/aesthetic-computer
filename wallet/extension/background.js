// Keeps Wallet - Background Service Worker
// Handles key management, signing, and Tezos operations

import {
  initCrypto,
  generateMnemonic,
  validateMnemonic,
  deriveKeypair,
  importFromPrivateKey,
  encrypt,
  decrypt,
  signOperation as cryptoSignOperation,
  clearSensitiveData,
} from './lib/crypto.mjs';

// State (in-memory, encrypted keys in storage)
let unlockedWallet = null; // { address, publicKey, secretKeyBytes }
let lockTimeout = null;
const LOCK_TIMEOUT_MS = 15 * 60 * 1000; // 15 minutes

// Initialize crypto on load
initCrypto().then(() => {
  console.log('🔐 Keeps Wallet crypto initialized');
});

// Message handler
chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
  handleMessage(message, sender).then(sendResponse);
  return true; // Keep channel open for async response
});

async function handleMessage(message, sender) {
  const { type, payload } = message;
  
  try {
    switch (type) {
      case 'KEEPS_PING':
        return { success: true, version: '0.1.0' };
        
      case 'KEEPS_GET_STATE':
        return await getWalletState();
        
      case 'KEEPS_CREATE_WALLET':
        return await createWallet(payload.password);
        
      case 'KEEPS_IMPORT_WALLET':
        return await importWallet(payload.mnemonic, payload.password);
        
      case 'KEEPS_IMPORT_PRIVATE_KEY':
        return await importPrivateKey(payload.privateKey, payload.password);
        
      case 'KEEPS_UNLOCK':
        return await unlockWallet(payload.password);
        
      case 'KEEPS_LOCK':
        return lockWallet();
        
      case 'KEEPS_GET_ADDRESS':
        return getAddress();
        
      case 'KEEPS_SIGN_OPERATION':
        return await handleSignRequest(payload.operation, sender);
        
      case 'KEEPS_GET_BALANCE':
        return await getBalance(payload.network);
        
      case 'KEEPS_GET_KEEPS':
        return await getKeeps(payload.network);
        
      case 'KEEPS_SET_NETWORK':
        return await setNetwork(payload.network);
        
      default:
        return { error: 'Unknown message type' };
    }
  } catch (error) {
    console.error('Message handler error:', error);
    return { error: error.message };
  }
}

// Wallet State
async function getWalletState() {
  const stored = await chrome.storage.local.get(['encryptedSeed', 'address', 'publicKey', 'network']);
  return {
    exists: !!stored.encryptedSeed,
    unlocked: !!unlockedWallet,
    address: stored.address || null,
    publicKey: stored.publicKey || null,
    network: stored.network || 'ghostnet',
  };
}

// Create new wallet
async function createWallet(password) {
  if (!password || password.length < 8) {
    return { error: 'Password must be at least 8 characters' };
  }
  
  await initCrypto();
  
  // 1. Generate mnemonic
  const mnemonic = generateMnemonic();
  
  // 2. Derive keypair
  const keypair = await deriveKeypair(mnemonic);
  
  // 3. Encrypt mnemonic with password
  const encryptedSeed = await encrypt(mnemonic, password);
  
  // 4. Store encrypted seed and public info
  await chrome.storage.local.set({
    encryptedSeed,
    address: keypair.address,
    publicKey: keypair.publicKey,
    network: 'ghostnet', // Default to testnet
  });
  
  // 5. Keep unlocked
  unlockedWallet = {
    address: keypair.address,
    publicKey: keypair.publicKey,
    secretKeyBytes: keypair.secretKeyBytes,
  };
  resetLockTimeout();
  
  // Return mnemonic to user (they need to back it up!)
  return { 
    success: true, 
    mnemonic, // IMPORTANT: Show this to user ONCE for backup
    address: keypair.address,
  };
}

// Import existing wallet
async function importWallet(mnemonic, password) {
  if (!password || password.length < 8) {
    return { error: 'Password must be at least 8 characters' };
  }
  
  // Clean up mnemonic
  const cleanMnemonic = mnemonic.trim().toLowerCase().replace(/\s+/g, ' ');
  
  // Validate mnemonic
  if (!validateMnemonic(cleanMnemonic)) {
    return { error: 'Invalid seed phrase' };
  }
  
  await initCrypto();
  
  // 1. Derive keypair
  const keypair = await deriveKeypair(cleanMnemonic);
  
  // 2. Encrypt mnemonic with password
  const encryptedSeed = await encrypt(cleanMnemonic, password);
  
  // 3. Store encrypted seed and public info
  await chrome.storage.local.set({
    encryptedSeed,
    address: keypair.address,
    publicKey: keypair.publicKey,
    network: 'ghostnet',
  });
  
  // 4. Keep unlocked
  unlockedWallet = {
    address: keypair.address,
    publicKey: keypair.publicKey,
    secretKeyBytes: keypair.secretKeyBytes,
  };
  resetLockTimeout();
  
  return { 
    success: true, 
    address: keypair.address,
  };
}

// Import wallet from private key
async function importPrivateKey(privateKey, password) {
  if (!password || password.length < 8) {
    return { error: 'Password must be at least 8 characters' };
  }
  
  if (!privateKey || !privateKey.startsWith('edsk')) {
    return { error: 'Invalid private key format' };
  }
  
  try {
    await initCrypto();
    
    // 1. Import keypair from private key
    const keypair = await importFromPrivateKey(privateKey.trim());
    
    // 2. Encrypt private key with password (we store the key directly, no mnemonic)
    const encryptedSeed = await encrypt(privateKey.trim(), password);
    
    // 3. Store encrypted key and public info
    await chrome.storage.local.set({
      encryptedSeed,
      importType: 'privateKey', // Flag so we know to handle differently on unlock
      address: keypair.address,
      publicKey: keypair.publicKey,
      network: 'ghostnet',
    });
    
    // 4. Keep unlocked
    unlockedWallet = {
      address: keypair.address,
      publicKey: keypair.publicKey,
      secretKeyBytes: keypair.secretKeyBytes,
    };
    resetLockTimeout();
    
    return { 
      success: true, 
      address: keypair.address,
    };
  } catch (error) {
    console.error('Import private key error:', error);
    return { error: error.message || 'Failed to import private key' };
  }
}

// Unlock wallet
async function unlockWallet(password) {
  const { encryptedSeed, importType, address, publicKey } = await chrome.storage.local.get([
    'encryptedSeed', 'importType', 'address', 'publicKey'
  ]);
  
  if (!encryptedSeed) {
    return { error: 'No wallet found' };
  }
  
  try {
    await initCrypto();
    
    // Decrypt stored secret
    const decrypted = await decrypt(encryptedSeed, password);
    
    let keypair;
    if (importType === 'privateKey') {
      // It's a private key
      keypair = await importFromPrivateKey(decrypted);
    } else {
      // It's a mnemonic
      keypair = await deriveKeypair(decrypted);
    }
    
    // Verify address matches
    if (keypair.address !== address) {
      return { error: 'Key derivation mismatch' };
    }
    
    // Store in memory
    unlockedWallet = {
      address: keypair.address,
      publicKey: keypair.publicKey,
      secretKeyBytes: keypair.secretKeyBytes,
    };
    
    // Clear decrypted secret from memory (best effort)
    clearSensitiveData(decrypted);
    
    resetLockTimeout();
    
    return { success: true, address };
  } catch (error) {
    return { error: 'Invalid password' };
  }
}

// Lock wallet
function lockWallet() {
  if (unlockedWallet?.secretKeyBytes) {
    clearSensitiveData(unlockedWallet.secretKeyBytes);
  }
  unlockedWallet = null;
  
  if (lockTimeout) {
    clearTimeout(lockTimeout);
    lockTimeout = null;
  }
  
  // Notify popup and content scripts
  chrome.runtime.sendMessage({ type: 'KEEPS_LOCKED' }).catch(() => {});
  
  return { success: true };
}

// Reset auto-lock timeout
function resetLockTimeout() {
  if (lockTimeout) clearTimeout(lockTimeout);
  lockTimeout = setTimeout(() => {
    lockWallet();
  }, LOCK_TIMEOUT_MS);
}

// Get address
function getAddress() {
  return { address: unlockedWallet?.address || null };
}

// Set network
async function setNetwork(network) {
  if (network !== 'mainnet' && network !== 'ghostnet') {
    return { error: 'Invalid network' };
  }
  await chrome.storage.local.set({ network });
  return { success: true, network };
}

// Handle signing request (with confirmation)
async function handleSignRequest(operation, sender) {
  if (!unlockedWallet) {
    return { error: 'Wallet is locked' };
  }
  
  // TODO: Show popup to user for confirmation
  // For now, auto-approve for development
  // In production, this should open a popup window asking user to confirm
  
  console.log('🔏 Signing operation:', operation);
  
  try {
    const signature = await cryptoSignOperation(operation.forgedBytes, unlockedWallet.secretKeyBytes);
    resetLockTimeout();
    return { 
      success: true,
      signature: signature.edsig,
      signedBytes: signature.signedBytes,
    };
  } catch (error) {
    return { error: 'Signing failed: ' + error.message };
  }
}

// Get balance from TzKT
async function getBalance(network) {
  const state = await getWalletState();
  const address = state.address;
  if (!address) return { balance: null };
  
  const net = network || state.network || 'ghostnet';
  const apiBase = net === 'mainnet' 
    ? 'https://api.tzkt.io' 
    : 'https://api.ghostnet.tzkt.io';
    
  try {
    const res = await fetch(`${apiBase}/v1/accounts/${address}/balance`);
    const mutez = await res.json();
    return { balance: mutez / 1000000 };
  } catch (error) {
    return { error: error.message };
  }
}

// Get keeps (NFTs) from TzKT
async function getKeeps(network) {
  const state = await getWalletState();
  const address = state.address;
  if (!address) return { keeps: [] };
  
  const net = network || state.network || 'ghostnet';
  const apiBase = net === 'mainnet'
    ? 'https://api.tzkt.io'
    : 'https://api.ghostnet.tzkt.io';
    
  try {
    // Get FA2 token balances
    const res = await fetch(
      `${apiBase}/v1/tokens/balances?account=${address}&balance.gt=0&token.standard=fa2&limit=100`
    );
    const tokens = await res.json();
    
    // Format keeps with metadata
    const keeps = tokens.map(t => ({
      id: t.token.tokenId,
      contract: t.token.contract.address,
      balance: parseInt(t.balance),
      name: t.token.metadata?.name,
      description: t.token.metadata?.description,
      thumbnailUri: t.token.metadata?.thumbnailUri,
      displayUri: t.token.metadata?.displayUri,
      artifactUri: t.token.metadata?.artifactUri,
    }));
    
    return { keeps };
  } catch (error) {
    return { error: error.message, keeps: [] };
  }
}

console.log('🔐 Keeps Wallet background service worker loaded');
