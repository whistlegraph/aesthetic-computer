// Beacon SDK Crypto — uses libsodium-wrappers-sumo for all primitives
// Ed25519 keypair for Beacon identity, crypto_box for encrypted messaging

import _sodium from 'libsodium-wrappers-sumo';

let sodium = null;

async function ensureSodium() {
  if (sodium) return sodium;
  await _sodium.ready;
  sodium = _sodium;
  return sodium;
}

// --- Hex helpers ---

function bytesToHex(bytes) {
  return Array.from(bytes)
    .map((b) => b.toString(16).padStart(2, '0'))
    .join('');
}

function hexToBytes(hex) {
  const bytes = new Uint8Array(hex.length / 2);
  for (let i = 0; i < bytes.length; i++) {
    bytes[i] = parseInt(hex.substr(i * 2, 2), 16);
  }
  return bytes;
}

// --- Beacon keypair (persistent Ed25519, separate from Tezos key) ---

export async function getOrCreateBeaconKeypair() {
  const s = await ensureSodium();

  const stored = await chrome.storage.local.get('beacon_keypair_seed');
  let seed;
  if (stored.beacon_keypair_seed) {
    seed = hexToBytes(stored.beacon_keypair_seed);
  } else {
    seed = s.randombytes_buf(32);
    await chrome.storage.local.set({
      beacon_keypair_seed: bytesToHex(seed),
    });
  }

  const kp = s.crypto_sign_seed_keypair(seed);
  return {
    publicKey: kp.publicKey, // Uint8Array 32 bytes
    secretKey: kp.privateKey, // Uint8Array 64 bytes
  };
}

// --- Key conversion: Ed25519 → Curve25519 (X25519) for crypto_box ---

export async function ed25519PublicKeyToCurve25519(edPk) {
  const s = await ensureSodium();
  return s.crypto_sign_ed25519_pk_to_curve25519(edPk);
}

export async function ed25519SecretKeyToCurve25519(edSk) {
  const s = await ensureSodium();
  return s.crypto_sign_ed25519_sk_to_curve25519(edSk);
}

// --- Sender ID (first 5 bytes of blake2b hash of public key, hex) ---

export async function getSenderId(publicKey) {
  const s = await ensureSodium();
  // publicKey can be Uint8Array or hex string
  const pkBytes =
    typeof publicKey === 'string' ? hexToBytes(publicKey) : publicKey;
  const hash = s.crypto_generichash(5, pkBytes);
  return bytesToHex(hash);
}

// --- Encrypt a Beacon message ---
// Returns hex string of (nonce || ciphertext)
// recipientPublicKey and senderSecretKey are Ed25519 keys — converted internally

export async function encryptBeaconMessage(
  message,
  recipientEdPublicKey,
  senderEdSecretKey,
) {
  const s = await ensureSodium();

  // Convert Ed25519 keys to Curve25519
  const recipientX25519PK =
    await ed25519PublicKeyToCurve25519(recipientEdPublicKey);
  const senderX25519SK =
    await ed25519SecretKeyToCurve25519(senderEdSecretKey);

  // Generate random nonce (24 bytes for crypto_box)
  const nonce = s.randombytes_buf(24);

  // Encode message as UTF-8 bytes
  const msgBytes =
    typeof message === 'string'
      ? new TextEncoder().encode(message)
      : message;

  // Encrypt
  const ciphertext = s.crypto_box_easy(
    msgBytes,
    nonce,
    recipientX25519PK,
    senderX25519SK,
  );

  // Combine nonce + ciphertext and return as hex
  const combined = new Uint8Array(nonce.length + ciphertext.length);
  combined.set(nonce, 0);
  combined.set(ciphertext, nonce.length);

  return bytesToHex(combined);
}

// --- Decrypt a Beacon message ---
// encryptedHex is hex string of (nonce || ciphertext)
// senderPublicKey and recipientSecretKey are Ed25519 keys

export async function decryptBeaconMessage(
  encryptedHex,
  senderEdPublicKey,
  recipientEdSecretKey,
) {
  const s = await ensureSodium();

  const combined = hexToBytes(encryptedHex);

  // Extract nonce (first 24 bytes) and ciphertext
  const nonce = combined.slice(0, 24);
  const ciphertext = combined.slice(24);

  // Convert Ed25519 keys to Curve25519
  const senderX25519PK =
    await ed25519PublicKeyToCurve25519(senderEdPublicKey);
  const recipientX25519SK =
    await ed25519SecretKeyToCurve25519(recipientEdSecretKey);

  // Decrypt
  const decrypted = s.crypto_box_open_easy(
    ciphertext,
    nonce,
    senderX25519PK,
    recipientX25519SK,
  );

  return new TextDecoder().decode(decrypted);
}

export { bytesToHex, hexToBytes };
