/**
 * Tezos Integration for store-kidlisp.js
 * 
 * This module provides functions to integrate KidLisp token minting
 * with the existing store-kidlisp.js netlify function.
 */

import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import crypto from 'crypto';

/**
 * Configuration for Tezos integration with environment detection
 */
function getTezosConfig() {
  // Detect environment from aesthetic.computer's CONTEXT variable or NODE_ENV
  const isDev = process.env.CONTEXT === 'dev' || 
                process.env.NODE_ENV === 'development' ||
                process.env.TEZOS_ENVIRONMENT === 'development';
  
  const network = isDev ? 'ghostnet' : 'mainnet';
  
  console.log(`🌍 Tezos environment: ${isDev ? 'DEVELOPMENT (Ghostnet)' : 'PRODUCTION (Mainnet)'}`);
  
  const config = {
    network,
    isDev,
    rpc: isDev 
      ? (process.env.TEZOS_RPC_URL_GHOSTNET || 'https://ghostnet.ecadinfra.com')
      : (process.env.TEZOS_RPC_URL_MAINNET || 'https://mainnet.api.tez.ie'),
    contractAddress: isDev
      ? process.env.KIDLISP_CONTRACT_ADDRESS_GHOSTNET
      : process.env.KIDLISP_CONTRACT_ADDRESS_MAINNET,
    adminPrivateKey: isDev
      ? process.env.CONTRACT_ADMIN_SECRET_KEY_GHOSTNET
      : process.env.CONTRACT_ADMIN_SECRET_KEY_MAINNET,
    minting_fee: process.env.TEZOS_MINTING_FEE || '100000', // in mutez
    default_mint_amount: process.env.DEFAULT_MINT_AMOUNT || '1000000', // 1 token with 6 decimals
  };
  
  if (!config.contractAddress) {
    throw new Error(`KIDLISP_CONTRACT_ADDRESS_${network.toUpperCase()} not configured for ${network}`);
  }
  
  if (!config.adminPrivateKey) {
    throw new Error(`CONTRACT_ADMIN_SECRET_KEY_${network.toUpperCase()} not configured for ${network}`);
  }
  
  return config;
}

/**
 * Initialize Tezos toolkit with admin signer
 */
function initTezos() {
  const config = getTezosConfig();
  
  const tezos = new TezosToolkit(config.rpc);
  tezos.setSignerProvider(new InMemorySigner(config.adminPrivateKey));
  
  console.log(`🔗 Connected to Tezos ${config.network} (${config.rpc})`);
  console.log(`📍 Contract address: ${config.contractAddress}`);
  
  return { tezos, config };
}

/**
 * Generate deterministic Tezos wallet address for a user
 * 
 * @param {string} handle - User's @handle
 * @param {string} userSub - User's authentication sub ID
 * @returns {Promise<{address: string, privateKey: string}>}
 */
export async function generateUserTezosWallet(handle, userSub) {
  try {
    // Create deterministic seed from handle and userSub
    const seed = crypto.createHash('sha256')
      .update(`tezos:${handle}:${userSub}:aesthetic.computer`)
      .digest();
    
    // Convert seed to private key format
    // TODO: Use proper BIP39/BIP44 derivation in production
    const privateKeyBytes = seed.slice(0, 32);
    const privateKey = privateKeyBytes.toString('hex');
    
    // Create temporary signer to derive address
    const tempSigner = new InMemorySigner(privateKey);
    const address = await tempSigner.publicKeyHash();
    
    return { address, privateKey };
  } catch (error) {
    console.error('Error generating Tezos wallet:', error);
    throw new Error('Failed to generate Tezos wallet');
  }
}

/**
 * Create SHA-256 hash of KidLisp code
 * 
 * @param {string} code - KidLisp source code
 * @returns {string} - SHA-256 hash
 */
export function hashKidLispCode(code) {
  return crypto.createHash('sha256')
    .update(code.trim())
    .digest('hex');
}

/**
 * Check if a KidLisp code should generate a meme coin
 * 
 * @param {string} code - KidLisp source code
 * @returns {boolean}
 */
export function shouldMintToken(code) {
  // Criteria for minting tokens:
  // 1. Minimum code length
  // 2. Contains at least one function call
  // 3. Not just comments or whitespace
  
  const trimmed = code.trim();
  
  if (trimmed.length < 10) {
    return false; // Too short
  }
  
  // Check for function calls (parentheses with content)
  const hasFunctionCall = /\([a-zA-Z]/.test(trimmed);
  if (!hasFunctionCall) {
    return false;
  }
  
  // Check if it's mostly comments
  const lines = trimmed.split('\n');
  const codeLines = lines.filter(line => {
    const clean = line.trim();
    return clean.length > 0 && !clean.startsWith(';') && !clean.startsWith('//');
  });
  
  return codeLines.length > 0;
}

/**
 * Create metadata URI for KidLisp token
 * 
 * @param {string} codeHash - Hash of the KidLisp code
 * @param {string} code - Original KidLisp code
 * @param {string} handle - Creator's handle
 * @param {string} nanoidCode - Short code for the piece
 * @returns {string}
 */
export function createTokenMetadataUri(codeHash, code, handle, nanoidCode) {
  // In production, this metadata would be stored on IPFS
  // For now, we'll use aesthetic.computer's API
  return `https://aesthetic.computer/api/kidlisp/metadata/${codeHash}`;
}

/**
 * Mint a new KidLisp token
 * Will check if token already exists before minting
 * 
 * @param {string} code - KidLisp source code
 * @param {string} creator - Creator's handle (without @)
 * @param {Object} metadata - Token metadata
 * @returns {Promise<{tokenId: number, exists: boolean, mintedNew?: boolean}>}
 */
export async function mintKidLispToken(code, creator, metadata = {}) {
  try {
    // First check if token already exists
    const existenceCheck = await checkTokenExists(code);
    
    if (existenceCheck.exists) {
      console.log(`🎯 Token already exists: ID ${existenceCheck.tokenId} on ${existenceCheck.network}`);
      return {
        tokenId: existenceCheck.tokenId,
        exists: true,
        mintedNew: false,
        codeHash: existenceCheck.codeHash,
        network: existenceCheck.network
      };
    }
    
    // If it doesn't exist, mint a new one
    console.log(`🏭 Minting new token on ${existenceCheck.network}...`);
    
    const codeHash = hashKidLispCode(code);
    const { tezos, config } = initTezos();
    const contract = await tezos.contract.at(config.contractAddress);
    
    const creatorWallet = generateWalletFromHandle(creator);
    
    const defaultMetadata = {
      name: `KidLisp #${codeHash.substring(0, 8)}`,
      description: `A unique KidLisp code snippet by @${creator}`,
      symbol: "KIDLISP",
      decimals: 0,
      code: code,
      creator: creator,
      ...metadata
    };
    
    console.log(`� Minting token for @${creator} on ${config.network}`);
    
    const op = await contract.methods.mint_kidlisp_token(
      codeHash,
      creatorWallet.address,
      defaultMetadata
    ).send();
    
    console.log(`⏳ Waiting for confirmation... Operation: ${op.hash}`);
    await op.confirmation();
    
    // Get the token ID from the contract
    const tokenId = await contract.views.get_token_for_hash(codeHash).read();
    
    console.log(`✅ Token minted successfully! Token ID: ${tokenId} on ${config.network}`);
    
    return {
      tokenId,
      exists: false,
      mintedNew: true,
      codeHash,
      creator,
      operationHash: op.hash,
      network: config.network
    };
    
  } catch (error) {
    console.error('Error minting KidLisp token:', error);
    throw error;
  }
}

/**
 * Check if a KidLisp token already exists for given code
 * 
 * @param {string} code - KidLisp source code
 * @returns {Promise<{exists: boolean, tokenId?: number, network?: string}>}
 */
export async function checkTokenExists(code) {
  try {
    const codeHash = hashKidLispCode(code);
    const { tezos, config } = initTezos();
    const contract = await tezos.contract.at(config.contractAddress);
    
    console.log(`🔍 Checking if token exists for hash: ${codeHash.substring(0, 16)}... on ${config.network}`);
    
    const tokenId = await contract.views.get_token_for_hash(codeHash).read();
    
    if (tokenId !== null) {
      console.log(`✅ Token exists: Token ID ${tokenId} on ${config.network}`);
      return {
        exists: true,
        tokenId,
        codeHash,
        network: config.network
      };
    } else {
      console.log(`❌ Token does not exist on ${config.network}`);
      return {
        exists: false,
        codeHash,
        network: config.network
      };
    }
    
  } catch (error) {
    console.error('Error checking token existence:', error);
    // If we can't check, assume it doesn't exist and try to mint
    return {
      exists: false,
      error: error.message
    };
  }
}
export async function getTokenInfo(codeHash) {
  try {
    const { tezos, config } = initTezos();
    const contract = await tezos.contract.at(config.contractAddress);
    
    const tokenId = await contract.views.get_token_for_hash(codeHash).read();
    
    if (tokenId === null) {
      return { exists: false, network: config.network };
    }
    
    const creator = await contract.views.get_creator(tokenId).read();
    
    return {
      exists: true,
      tokenId,
      creator,
      network: config.network
    };
    
  } catch (error) {
    console.error('Error getting token info:', error);
    return { exists: false };
  }
}

/**
 * Integration function to be called from store-kidlisp.js
 * This checks for token existence first, then mints if needed
 * 
 * @param {string} code - KidLisp source code
 * @param {Object} user - User object with sub and handle
 * @param {string} nanoidCode - Generated nanoid code
 * @returns {Promise<Object>} - Token result with existence/minting info
 */
export async function integrateWithKidLispCache(code, user, nanoidCode) {
  // Only attempt token operations if user is authenticated and has a handle
  if (!user?.sub) {
    console.log('🔓 User not authenticated, skipping token operations');
    return { minted: false, reason: 'user_not_authenticated' };
  }
  
  // Try to get user handle from database or user object
  let handle = user.handle || user.sub;
  if (!handle) {
    console.log('🏷️ User has no handle, skipping token operations');
    return { minted: false, reason: 'no_handle' };
  }
  
  // Remove @ prefix if present
  if (handle.startsWith('@')) {
    handle = handle.substring(1);
  }
  
  try {
    console.log(`🔍 Checking token existence for @${handle}...`);
    
    // First check if token already exists
    const existenceCheck = await checkTokenExists(code);
    
    if (existenceCheck.exists) {
      console.log(`🎯 Token already exists: ID ${existenceCheck.tokenId} on ${existenceCheck.network}`);
      return {
        minted: false,
        exists: true,
        tokenId: existenceCheck.tokenId,
        network: existenceCheck.network,
        codeHash: existenceCheck.codeHash,
        reason: `Token already exists for this code (ID: ${existenceCheck.tokenId})`
      };
    }
    
    // Token doesn't exist, mint a new one
    console.log(`🏭 Minting new KidLisp token for @${handle}...`);
    
    const mintResult = await mintKidLispToken(code, handle, {
      name: `KidLisp Code by @${handle}`,
      description: `Unique KidLisp code snippet: ${nanoidCode}`,
      ac_code: nanoidCode
    });
    
    if (mintResult.mintedNew) {
      console.log(`🎉 Token minted for @${handle}: Token ID ${mintResult.tokenId} on ${mintResult.network}`);
      return {
        minted: true,
        tokenId: mintResult.tokenId,
        txHash: mintResult.operationHash,
        creatorAddress: generateWalletFromHandle(handle).address,
        codeHash: mintResult.codeHash,
        network: mintResult.network
      };
    } else {
      // This case handles when token existed but wasn't caught by initial check
      console.log(`🎯 Token already existed: ID ${mintResult.tokenId} on ${mintResult.network}`);
      return {
        minted: false,
        exists: true,
        tokenId: mintResult.tokenId,
        network: mintResult.network,
        codeHash: mintResult.codeHash,
        reason: `Token already existed (ID: ${mintResult.tokenId})`
      };
    }
    
  } catch (error) {
    console.error('💥 Token integration error:', error);
    return {
      minted: false,
      reason: 'integration_error',
      error: error.message
    };
  }
}

export default {
  checkTokenExists,
  mintKidLispToken,
  generateUserTezosWallet,
  hashKidLispCode,
  shouldMintToken,
  getTokenInfo,
  integrateWithKidLispCache
};
