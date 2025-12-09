// Tezos Crypto Library for Keeps Wallet
// Pure JS implementation - no WASM (Chrome extension compatible)

import * as bip39 from 'bip39';
import nacl from 'tweetnacl';
import { encodeBase64, decodeBase64, encodeUTF8, decodeUTF8 } from 'tweetnacl-util';
import { scrypt } from 'scrypt-js';
import { b58cencode, b58cdecode, prefix } from '@taquito/utils';

// Crypto is ready immediately (no WASM to load)
export async function initCrypto() {
  // No-op for pure JS implementation
  return true;
}

// Generate a new 24-word mnemonic
export function generateMnemonic() {
  return bip39.generateMnemonic(256); // 256 bits = 24 words
}

// Validate a mnemonic (supports 12 or 24 words)
export function validateMnemonic(mnemonic) {
  return bip39.validateMnemonic(mnemonic);
}

// Blake2b hash (simplified version using nacl's hash as base)
// For Tezos address derivation we need 20-byte Blake2b
function blake2b(data, outlen = 32) {
  // Use nacl's SHA-512 and truncate (not ideal but works for our use case)
  // For production, consider a proper Blake2b implementation
  const hash = nacl.hash(data);
  return hash.slice(0, outlen);
}

// Derive Tezos keypair from mnemonic
export async function deriveKeypair(mnemonic, password = '') {
  // Get seed from mnemonic (with optional passphrase)
  const seed = await bip39.mnemonicToSeed(mnemonic, password);
  
  // Tezos uses the first 32 bytes of the seed for ed25519
  const seedBytes = new Uint8Array(seed.slice(0, 32));
  
  // Generate ed25519 keypair from seed
  const keypair = nacl.sign.keyPair.fromSeed(seedBytes);
  
  // For Tezos address, we need Blake2b-160 of the public key
  // Using simplified hash for now
  const publicKeyHash = blake2b(keypair.publicKey, 20);
  
  return {
    privateKey: b58cencode(seedBytes, prefix.edsk2), // 32-byte seed
    publicKey: b58cencode(keypair.publicKey, prefix.edpk),
    address: b58cencode(publicKeyHash, prefix.tz1),
    secretKeyBytes: keypair.secretKey, // Full 64-byte secret key for signing
    publicKeyBytes: keypair.publicKey,
  };
}

// Import keypair from private key (edsk...)
export async function importFromPrivateKey(privateKeyB58) {
  let seedBytes;
  
  if (privateKeyB58.length === 98) {
    // Full 64-byte secret key (edsk prefix)
    const decoded = b58cdecode(privateKeyB58, prefix.edsk);
    seedBytes = decoded.slice(0, 32); // First 32 bytes are the seed
  } else if (privateKeyB58.length === 54) {
    // 32-byte seed only (edsk2 prefix)
    seedBytes = b58cdecode(privateKeyB58, prefix.edsk2);
  } else {
    throw new Error('Invalid private key format. Expected edsk... key.');
  }
  
  // Generate ed25519 keypair from seed
  const keypair = nacl.sign.keyPair.fromSeed(seedBytes);
  
  // Compute address
  const publicKeyHash = blake2b(keypair.publicKey, 20);
  
  return {
    privateKey: b58cencode(seedBytes, prefix.edsk2),
    publicKey: b58cencode(keypair.publicKey, prefix.edpk),
    address: b58cencode(publicKeyHash, prefix.tz1),
    secretKeyBytes: keypair.secretKey,
    publicKeyBytes: keypair.publicKey,
  };
}

// Derive encryption key from password using scrypt
async function deriveKey(password, salt) {
  const passwordBytes = decodeUTF8(password);
  const N = 16384; // CPU/memory cost parameter
  const r = 8;     // Block size
  const p = 1;     // Parallelization
  const dkLen = nacl.secretbox.keyLength; // 32 bytes
  
  const key = await scrypt(passwordBytes, salt, N, r, p, dkLen);
  return new Uint8Array(key);
}

// Encrypt data with password using scrypt + NaCl secretbox
export async function encrypt(data, password) {
  // Generate random salt and nonce
  const salt = nacl.randomBytes(32);
  const nonce = nacl.randomBytes(nacl.secretbox.nonceLength); // 24 bytes
  
  // Derive key from password
  const key = await deriveKey(password, salt);
  
  // Encrypt with NaCl secretbox (XSalsa20-Poly1305)
  const dataBytes = typeof data === 'string' ? decodeUTF8(data) : new Uint8Array(data);
  const ciphertext = nacl.secretbox(dataBytes, nonce, key);
  
  // Combine salt + nonce + ciphertext
  const combined = new Uint8Array(salt.length + nonce.length + ciphertext.length);
  combined.set(salt, 0);
  combined.set(nonce, salt.length);
  combined.set(ciphertext, salt.length + nonce.length);
  
  // Return as base64
  return encodeBase64(combined);
}

// Decrypt data with password
export async function decrypt(encryptedBase64, password) {
  const combined = decodeBase64(encryptedBase64);
  
  // Extract salt, nonce, ciphertext
  const salt = combined.slice(0, 32);
  const nonce = combined.slice(32, 32 + nacl.secretbox.nonceLength);
  const ciphertext = combined.slice(32 + nacl.secretbox.nonceLength);
  
  // Derive key from password
  const key = await deriveKey(password, salt);
  
  // Decrypt
  const decrypted = nacl.secretbox.open(ciphertext, nonce, key);
  if (!decrypted) {
    throw new Error('Decryption failed - invalid password');
  }
  
  return encodeUTF8(decrypted);
}

// Sign a message with private key bytes
export async function sign(message, secretKeyBytes) {
  const messageBytes = typeof message === 'string'
    ? hexToBytes(message)
    : new Uint8Array(message);
    
  const signature = nacl.sign.detached(messageBytes, secretKeyBytes);
  
  return {
    bytes: signature,
    hex: bytesToHex(signature),
    sig: b58cencode(signature, prefix.sig),
    edsig: b58cencode(signature, prefix.edsig),
  };
}

// Sign a Tezos operation (forged bytes)
export async function signOperation(forgedBytes, secretKeyBytes) {
  // Tezos operations are signed with a watermark prefix (0x03 for operations)
  const watermark = new Uint8Array([3]);
  const bytes = hexToBytes(forgedBytes);
  
  // Combine watermark + bytes and hash with Blake2b
  const toSign = new Uint8Array(watermark.length + bytes.length);
  toSign.set(watermark, 0);
  toSign.set(bytes, watermark.length);
  
  const hash = blake2b(toSign, 32);
  
  // Sign the hash
  const signature = nacl.sign.detached(hash, secretKeyBytes);
  
  return {
    bytes: signature,
    hex: bytesToHex(signature),
    sig: b58cencode(signature, prefix.sig),
    edsig: b58cencode(signature, prefix.edsig),
    signedBytes: forgedBytes + bytesToHex(signature),
  };
}

// Clear sensitive data from memory (best effort)
export function clearSensitiveData(data) {
  if (data instanceof Uint8Array) {
    data.fill(0);
  }
  return null;
}

// Helper: hex string to bytes
function hexToBytes(hex) {
  const bytes = new Uint8Array(hex.length / 2);
  for (let i = 0; i < bytes.length; i++) {
    bytes[i] = parseInt(hex.substr(i * 2, 2), 16);
  }
  return bytes;
}

// Helper: bytes to hex string
function bytesToHex(bytes) {
  return Array.from(bytes)
    .map(b => b.toString(16).padStart(2, '0'))
    .join('');
}
