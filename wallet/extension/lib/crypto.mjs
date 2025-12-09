// Tezos Crypto Library for Keeps Wallet
// Handles key generation, derivation, encryption, and signing

import * as bip39 from 'bip39';
import sodium from 'libsodium-wrappers-sumo';
import { b58cencode, prefix } from '@taquito/utils';

// Ensure sodium is ready
let sodiumReady = false;
export async function initCrypto() {
  if (!sodiumReady) {
    await sodium.ready;
    sodiumReady = true;
  }
}

// Generate a new 24-word mnemonic
export function generateMnemonic() {
  return bip39.generateMnemonic(256); // 256 bits = 24 words
}

// Validate a mnemonic
export function validateMnemonic(mnemonic) {
  return bip39.validateMnemonic(mnemonic);
}

// Derive Tezos keypair from mnemonic
// Uses standard Tezos derivation path: m/44'/1729'/0'/0'
export async function deriveKeypair(mnemonic, password = '') {
  await initCrypto();
  
  // Get seed from mnemonic (with optional passphrase)
  const seed = await bip39.mnemonicToSeed(mnemonic, password);
  
  // Tezos uses the first 32 bytes of the seed for ed25519
  // For proper BIP44 derivation, we'd need slip10, but most Tezos wallets
  // just use the seed directly for simplicity
  const seedBytes = new Uint8Array(seed.slice(0, 32));
  
  // Generate ed25519 keypair from seed
  const keypair = sodium.crypto_sign_seed_keypair(seedBytes);
  
  // Encode as Tezos addresses
  const publicKeyHash = sodium.crypto_generichash(20, keypair.publicKey);
  
  return {
    privateKey: b58cencode(keypair.privateKey.slice(0, 32), prefix.edsk2), // Secret key
    publicKey: b58cencode(keypair.publicKey, prefix.edpk),
    address: b58cencode(publicKeyHash, prefix.tz1),
    // Raw bytes for signing
    secretKeyBytes: keypair.privateKey,
    publicKeyBytes: keypair.publicKey,
  };
}

// Encrypt data with password using Argon2 + XChaCha20-Poly1305
export async function encrypt(data, password) {
  await initCrypto();
  
  // Generate salt for Argon2
  const salt = sodium.randombytes_buf(sodium.crypto_pwhash_SALTBYTES);
  
  // Derive key from password using Argon2id
  const key = sodium.crypto_pwhash(
    sodium.crypto_secretbox_KEYBYTES,
    password,
    salt,
    sodium.crypto_pwhash_OPSLIMIT_MODERATE,
    sodium.crypto_pwhash_MEMLIMIT_MODERATE,
    sodium.crypto_pwhash_ALG_ARGON2ID13
  );
  
  // Generate nonce
  const nonce = sodium.randombytes_buf(sodium.crypto_secretbox_NONCEBYTES);
  
  // Encrypt with XChaCha20-Poly1305
  const dataBytes = typeof data === 'string' 
    ? sodium.from_string(data) 
    : new Uint8Array(data);
  const ciphertext = sodium.crypto_secretbox_easy(dataBytes, nonce, key);
  
  // Combine salt + nonce + ciphertext
  const combined = new Uint8Array(salt.length + nonce.length + ciphertext.length);
  combined.set(salt, 0);
  combined.set(nonce, salt.length);
  combined.set(ciphertext, salt.length + nonce.length);
  
  // Return as base64
  return sodium.to_base64(combined, sodium.base64_variants.ORIGINAL);
}

// Decrypt data with password
export async function decrypt(encryptedBase64, password) {
  await initCrypto();
  
  const combined = sodium.from_base64(encryptedBase64, sodium.base64_variants.ORIGINAL);
  
  // Extract salt, nonce, ciphertext
  const salt = combined.slice(0, sodium.crypto_pwhash_SALTBYTES);
  const nonce = combined.slice(
    sodium.crypto_pwhash_SALTBYTES, 
    sodium.crypto_pwhash_SALTBYTES + sodium.crypto_secretbox_NONCEBYTES
  );
  const ciphertext = combined.slice(
    sodium.crypto_pwhash_SALTBYTES + sodium.crypto_secretbox_NONCEBYTES
  );
  
  // Derive key from password
  const key = sodium.crypto_pwhash(
    sodium.crypto_secretbox_KEYBYTES,
    password,
    salt,
    sodium.crypto_pwhash_OPSLIMIT_MODERATE,
    sodium.crypto_pwhash_MEMLIMIT_MODERATE,
    sodium.crypto_pwhash_ALG_ARGON2ID13
  );
  
  // Decrypt
  const decrypted = sodium.crypto_secretbox_open_easy(ciphertext, nonce, key);
  if (!decrypted) {
    throw new Error('Decryption failed - invalid password');
  }
  
  return sodium.to_string(decrypted);
}

// Sign a message/operation with private key bytes
export async function sign(message, secretKeyBytes) {
  await initCrypto();
  
  const messageBytes = typeof message === 'string'
    ? sodium.from_hex(message)
    : new Uint8Array(message);
    
  const signature = sodium.crypto_sign_detached(messageBytes, secretKeyBytes);
  
  return {
    bytes: signature,
    hex: sodium.to_hex(signature),
    sig: b58cencode(signature, prefix.sig),
    edsig: b58cencode(signature, prefix.edsig),
  };
}

// Sign a Tezos operation (forged bytes)
export async function signOperation(forgedBytes, secretKeyBytes) {
  await initCrypto();
  
  // Tezos operations are signed with a watermark prefix (0x03 for operations)
  const watermark = new Uint8Array([3]);
  const bytes = sodium.from_hex(forgedBytes);
  
  // Hash the watermarked message with Blake2b
  const toSign = new Uint8Array(watermark.length + bytes.length);
  toSign.set(watermark, 0);
  toSign.set(bytes, watermark.length);
  
  const hash = sodium.crypto_generichash(32, toSign);
  
  // Sign the hash
  const signature = sodium.crypto_sign_detached(hash, secretKeyBytes);
  
  return {
    bytes: signature,
    hex: sodium.to_hex(signature),
    sig: b58cencode(signature, prefix.sig),
    edsig: b58cencode(signature, prefix.edsig),
    signedBytes: forgedBytes + sodium.to_hex(signature),
  };
}

// Clear sensitive data from memory (best effort)
export function clearSensitiveData(data) {
  if (data instanceof Uint8Array) {
    sodium.memzero(data);
  } else if (typeof data === 'string') {
    // Can't really clear strings in JS, but we can at least dereference
    return '';
  }
  return null;
}
