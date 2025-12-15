/**
 * beacon-node.mjs - Node.js Beacon P2P client
 * 
 * Direct port of Beacon SDK's Matrix transport to Node.js.
 * Uses the same HTTP REST API that the browser SDK uses.
 * 
 * Matrix Protocol (what Beacon uses):
 * - POST /_matrix/client/r0/login - authenticate
 * - GET /_matrix/client/r0/sync - poll for events (long polling)
 * - POST /_matrix/client/r0/createRoom - create rooms
 * - POST /_matrix/client/r0/rooms/{roomId}/join - join rooms  
 * - PUT /_matrix/client/r0/rooms/{roomId}/send/m.room.message/{txnId} - send messages
 */

import { promises as fs } from "fs";
import { join } from "path";
import { generateKeyPairFromSeed, sign, convertPublicKeyToX25519, convertSecretKeyToX25519 } from "@stablelib/ed25519";
import { clientSessionKeys, serverSessionKeys } from "@stablelib/x25519-session";
import { hash } from "@stablelib/blake2b";
import { encode } from "@stablelib/utf8";
import { secretBox, openSecretBox } from "@stablelib/nacl";
import { randomBytes } from "@stablelib/random";
import sodium from "libsodium-wrappers";
import bs58check from "bs58check";
import qrcode from "qrcode-terminal";
import { getPkhfromPk } from "@taquito/utils";

// Secretbox constants (same as Beacon SDK)
const secretbox_NONCEBYTES = 24;
const secretbox_MACBYTES = 16;

// Storage file for Node.js
const BEACON_STORAGE_FILE = join(process.env.HOME, ".ac-beacon-storage.json");

// Matrix API base path
const MATRIX_API = "/_matrix/client/r0";

// Wait for sodium to initialize
const sodiumReady = sodium.ready;

// Terminal colors
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const DIM = '\x1b[2m';
const CYAN = '\x1b[36m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const RED = '\x1b[31m';

/**
 * Beacon Serializer - bs58check encode/decode JSON (matches @airgap/beacon-core)
 */
class Serializer {
  async serialize(message) {
    const str = JSON.stringify(message);
    return bs58check.encode(Buffer.from(str));
  }
  
  async deserialize(encoded) {
    const decodedBytes = bs58check.decode(encoded);
    const jsonString = Buffer.from(decodedBytes).toString('utf8');
    return JSON.parse(jsonString);
  }
}

/**
 * Encrypt message with symmetric key (matches encryptCryptoboxPayload)
 */
function encryptCryptoboxPayload(message, sharedKey) {
  const nonce = randomBytes(secretbox_NONCEBYTES);
  const messageBytes = Buffer.from(message, 'utf8');
  const ciphertext = secretBox(sharedKey, nonce, messageBytes);
  
  const combined = Buffer.concat([Buffer.from(nonce), Buffer.from(ciphertext)]);
  return toHex(combined);
}

/**
 * Convert bytes to hex string
 */
function toHex(bytes) {
  return Array.from(bytes).map(b => b.toString(16).padStart(2, '0')).join('');
}

/**
 * Convert hex string to Buffer
 */
function fromHex(hex) {
  return Buffer.from(hex, 'hex');
}

/**
 * Get BLAKE2b hash of public key (32 bytes) as hex - matches Beacon SDK getHexHash
 */
function getPublicKeyHash(publicKey) {
  const hashed = hash(publicKey, 32);
  return toHex(hashed);
}

/**
 * Matrix HTTP Client - direct port from Beacon SDK
 */
class MatrixClient {
  constructor(baseUrl) {
    this.baseUrl = baseUrl;
    this.accessToken = null;
    this.syncToken = null;
    this.txnNo = 0;
  }

  async request(method, endpoint, body = null, useAuth = true) {
    const url = `${this.baseUrl}${MATRIX_API}${endpoint}`;
    const headers = { 'Content-Type': 'application/json' };
    if (useAuth && this.accessToken) {
      headers['Authorization'] = `Bearer ${this.accessToken}`;
    }
    
    const options = { method, headers };
    if (body) options.body = JSON.stringify(body);
    
    const response = await fetch(url, options);
    if (!response.ok) {
      const error = await response.json().catch(() => ({}));
      throw new Error(`Matrix API error: ${response.status} - ${error.error || response.statusText}`);
    }
    return response.json();
  }

  /**
   * Login to Matrix server using Beacon's signature-based auth
   */
  async login(userId, password, deviceId) {
    const result = await this.request('POST', '/login', {
      type: 'm.login.password',
      identifier: { type: 'm.id.user', user: userId },
      password,
      device_id: deviceId
    }, false);
    
    this.accessToken = result.access_token;
    return result;
  }

  /**
   * Long-poll for sync events
   */
  async sync(timeout = 30000) {
    let endpoint = '/sync';
    const params = new URLSearchParams();
    if (this.syncToken) params.set('since', this.syncToken);
    if (timeout) params.set('timeout', timeout.toString());
    if (params.toString()) endpoint += '?' + params.toString();
    
    const result = await this.request('GET', endpoint);
    this.syncToken = result.next_batch;
    return result;
  }

  /**
   * Join a room
   */
  async joinRoom(roomId) {
    return this.request('POST', `/rooms/${encodeURIComponent(roomId)}/join`, {});
  }

  /**
   * Send a text message to a room
   */
  async sendMessage(roomId, message) {
    const txnId = `m${Date.now()}.${this.txnNo++}`;
    return this.request('PUT', `/rooms/${encodeURIComponent(roomId)}/send/m.room.message/${txnId}`, {
      msgtype: 'm.text',
      body: message
    });
  }
}

/**
 * Beacon P2P Client - handles pairing and message encryption
 */
class BeaconP2PClient {
  constructor(relayServer = "beacon-node-1.diamond.papers.tech") {
    this.relayServer = relayServer;
    this.matrix = new MatrixClient(`https://${relayServer}`);
    this.keyPair = null;
    this.pairingRequest = null;
  }

  /**
   * Initialize with a new keypair
   */
  async init() {
    await sodiumReady;
    
    // Generate keypair from random seed
    const seed = crypto.randomUUID();
    const seedHash = hash(encode(seed), 32);
    this.keyPair = generateKeyPairFromSeed(seedHash);
    
    return this;
  }

  /**
   * Get public key as hex string
   */
  getPublicKey() {
    return toHex(this.keyPair.publicKey);
  }

  /**
   * Get public key hash (for Matrix user ID)
   */
  getPublicKeyHash() {
    return getPublicKeyHash(this.keyPair.publicKey);
  }

  /**
   * Generate pairing request for QR code
   */
  generatePairingRequest(appName = "Aesthetic Computer") {
    const publicKey = this.getPublicKey();
    const id = crypto.randomUUID();
    
    this.pairingRequest = {
      type: "p2p-pairing-request",
      id,
      name: appName,
      publicKey,
      version: "3",
      relayServer: this.relayServer
    };
    
    const serialized = bs58check.encode(Buffer.from(JSON.stringify(this.pairingRequest)));
    const deepLinkUrl = `tezos://?type=tzip10&data=${serialized}`;
    
    return { pairingRequest: this.pairingRequest, serialized, deepLinkUrl };
  }

  /**
   * Connect to Matrix relay server
   * Uses Beacon's signature-based authentication
   */
  async connect() {
    // Get relay server timestamp for login (Beacon uses /_synapse/client/beacon/info)
    const infoUrl = `https://${this.relayServer}/_synapse/client/beacon/info`;
    const info = await fetch(infoUrl).then(r => r.json());
    
    // Beacon SDK: const time = Math.floor(relayServer.timestamp)
    //             const loginString = `login:${Math.floor(time / (5 * 60))}`
    const time = Math.floor(info.timestamp);
    const loginBucket = Math.floor(time / (5 * 60));
    const loginString = `login:${loginBucket}`;
    
    console.log(`${DIM}Login bucket: ${loginBucket} (timestamp: ${time})${RESET}`);
    
    // Create login signature (same as Beacon SDK)
    const loginDigest = hash(encode(loginString), 32);
    const signature = sign(this.keyPair.secretKey, loginDigest);
    
    // Login with signature
    const userId = this.getPublicKeyHash();
    const password = `ed:${toHex(signature)}:${this.getPublicKey()}`;
    const deviceId = toHex(this.keyPair.publicKey);
    
    console.log(`${DIM}Logging in as @${userId}:${this.relayServer}...${RESET}`);
    await this.matrix.login(userId, password, deviceId);
    console.log(`${GREEN}✓${RESET} Connected to Matrix relay`);
    
    return this;
  }

  /**
   * Wait for wallet pairing response
   */
  async waitForPairing(timeoutMs = 120000) {
    console.log(`${YELLOW}⏳ Waiting for wallet to connect...${RESET}`);
    
    const startTime = Date.now();
    
    while (Date.now() - startTime < timeoutMs) {
      try {
        // Poll for new events (5 second timeout for responsiveness)
        const syncResult = await this.matrix.sync(5000);
        
        // Check for room invites
        if (syncResult.rooms?.invite) {
          for (const [roomId, room] of Object.entries(syncResult.rooms.invite)) {
            console.log(`${GREEN}✓${RESET} Received room invite: ${roomId.slice(0, 20)}...`);
            await this.matrix.joinRoom(roomId);
            console.log(`${GREEN}✓${RESET} Joined room`);
          }
        }
        
        // Check for messages in joined rooms
        if (syncResult.rooms?.join) {
          for (const [roomId, room] of Object.entries(syncResult.rooms.join)) {
            const events = room.timeline?.events || [];
            for (const event of events) {
              if (event.type === 'm.room.message' && event.content?.body) {
                const message = event.content.body;
                
                // Check for channel-open message (pairing response)
                if (message.startsWith('@channel-open:')) {
                  console.log(`${GREEN}✓${RESET} Received pairing response!`);
                  const pairingResponse = await this.decryptPairingResponse(message, event.sender, roomId);
                  return pairingResponse;
                }
              }
            }
          }
        }
      } catch (err) {
        // Sync timeout is normal, continue polling
        if (!err.message.includes('timeout')) {
          console.log(`${DIM}Sync: ${err.message}${RESET}`);
        }
      }
    }
    
    throw new Error("Pairing timeout - no wallet connected");
  }

  /**
   * Decrypt pairing response from wallet using openCryptobox
   * The message is encrypted with sealCryptobox (asymmetric encryption)
   */
  async decryptPairingResponse(message, sender, roomId) {
    // Message format: @channel-open:@<recipientHash>:<relayServer>:<encryptedHex>
    const splits = message.split(':');
    const encryptedHex = splits[splits.length - 1];
    const encrypted = fromHex(encryptedHex);
    
    // Extract sender info
    const senderHash = sender.split(':')[0].slice(1); // Remove @ prefix
    
    console.log(`${DIM}Decrypting pairing response...${RESET}`);
    
    try {
      // Use openCryptobox - the Beacon SDK's asymmetric decryption
      // First 32 bytes are ephemeral public key, rest is ciphertext
      const epk = encrypted.slice(0, 32);
      const ciphertext = encrypted.slice(32);
      
      // Convert our ed25519 keys to x25519 for decryption
      const kxSecretKey = sodium.crypto_sign_ed25519_sk_to_curve25519(this.keyPair.secretKey);
      const kxPublicKey = sodium.crypto_sign_ed25519_pk_to_curve25519(this.keyPair.publicKey);
      
      // Create nonce from BLAKE2b(epk || kxPublicKey)
      const nonceData = new Uint8Array(64);
      nonceData.set(epk, 0);
      nonceData.set(kxPublicKey, 32);
      const nonce = hash(nonceData, 24);
      
      // Decrypt using crypto_box_open (nacl box)
      const decrypted = sodium.crypto_box_open_easy(ciphertext, nonce, epk, kxSecretKey);
      
      // Parse the JSON pairing response
      const jsonStr = new TextDecoder().decode(decrypted);
      const pairingResponse = JSON.parse(jsonStr);
      
      console.log(`${GREEN}✓${RESET} Decrypted pairing response:`);
      console.log(`  ${DIM}Name:${RESET} ${pairingResponse.name}`);
      console.log(`  ${DIM}Public Key:${RESET} ${pairingResponse.publicKey?.slice(0, 16)}...`);
      console.log(`  ${DIM}Relay Server:${RESET} ${pairingResponse.relayServer}`);
      
      // Store the peer info for future communication
      this.peer = {
        ...pairingResponse,
        senderId: sender,
        roomId
      };
      
      // Note: The DApp does NOT send a pairing response back.
      // The wallet considers pairing complete after sending its response.
      // Temple may show "loading" until we send an actual request (like PermissionRequest).
      
      return pairingResponse;
    } catch (err) {
      console.log(`${YELLOW}Decryption failed: ${err.message}${RESET}`);
      // Return basic info even if decryption fails
      return { sender, senderHash, error: err.message };
    }
  }

  /**
   * Send our pairing response back to the wallet
   * This completes the handshake so the wallet knows we received their response
   */
  async sendPairingResponse(walletPairingResponse, roomId) {
    console.log(`${DIM}Sending pairing response back to wallet...${RESET}`);
    
    try {
      // Our response (same structure as what wallet sent)
      const ourResponse = {
        id: this.pairingRequest.id,
        type: "p2p-pairing-response", 
        name: this.pairingRequest.name,
        publicKey: this.getPublicKey(),
        version: this.pairingRequest.version,
        relayServer: this.relayServer
      };
      
      // Encrypt for wallet using sealCryptobox
      const message = JSON.stringify(ourResponse);
      const encryptedMessage = await this.encryptForPeer(message, walletPairingResponse.publicKey);
      
      // Build recipient string for the wallet
      const walletPubKeyHash = getPublicKeyHash(fromHex(walletPairingResponse.publicKey));
      const recipient = `@${walletPubKeyHash}:${walletPairingResponse.relayServer}`;
      
      // Format: @channel-open:<recipient>:<encryptedHex>
      const channelOpenMsg = `@channel-open:${recipient}:${encryptedMessage}`;
      
      // Send to the room
      await this.matrix.sendMessage(roomId, channelOpenMsg);
      console.log(`${GREEN}✓${RESET} Sent pairing response to wallet`);
      
    } catch (err) {
      console.log(`${YELLOW}Warning: Could not send pairing response: ${err.message}${RESET}`);
    }
  }

  /**
   * Encrypt a message for a peer using sealCryptobox (asymmetric)
   */
  async encryptForPeer(message, peerPublicKeyHex) {
    const peerPublicKey = fromHex(peerPublicKeyHex);
    
    // Convert peer's ed25519 public key to x25519
    const peerX25519PublicKey = sodium.crypto_sign_ed25519_pk_to_curve25519(peerPublicKey);
    
    // Generate ephemeral keypair for this message
    const ephemeralKeyPair = sodium.crypto_box_keypair();
    
    // Create nonce from BLAKE2b(ephemeralPublicKey || peerX25519PublicKey)
    const nonceData = new Uint8Array(64);
    nonceData.set(ephemeralKeyPair.publicKey, 0);
    nonceData.set(peerX25519PublicKey, 32);
    const nonce = hash(nonceData, 24);
    
    // Encrypt
    const messageBytes = encode(message);
    const ciphertext = sodium.crypto_box_easy(messageBytes, nonce, peerX25519PublicKey, ephemeralKeyPair.privateKey);
    
    // Prepend ephemeral public key to ciphertext
    const sealed = new Uint8Array(32 + ciphertext.length);
    sealed.set(ephemeralKeyPair.publicKey, 0);
    sealed.set(ciphertext, 32);
    
    return toHex(sealed);
  }

  /**
   * Send a permission request to the wallet
   * This is required after pairing to actually get wallet access
   */
  async sendPermissionRequest() {
    if (!this.peer) throw new Error("Not paired with wallet yet");
    
    console.log(`${DIM}Sending permission request to wallet...${RESET}`);
    
    const senderId = await this.getSenderId();
    
    // Build the permission request (Beacon v2 format)
    const permissionRequest = {
      type: "permission_request",
      version: "2",  // Use v2 for Temple compatibility
      id: crypto.randomUUID(),
      senderId: senderId,
      appMetadata: {
        senderId: senderId,
        name: this.pairingRequest.name
      },
      network: {
        type: "ghostnet"  // or "mainnet"
      },
      scopes: ["operation_request", "sign"]
    };
    
    console.log(`${DIM}Permission request:${RESET}`, JSON.stringify(permissionRequest, null, 2));
    
    // Serialize the message with bs58check (like Beacon SDK Serializer)
    const serializer = new Serializer();
    const serializedMessage = await serializer.serialize(permissionRequest);
    
    console.log(`${DIM}Serialized (bs58check): ${serializedMessage.slice(0, 50)}...${RESET}`);
    
    // Encrypt using symmetric key derived from key exchange
    // DApp uses clientSessionKeys, sends with sharedTx (which Beacon calls .send)
    const encryptedMessage = await this.encryptMessageForPeer(serializedMessage);
    
    console.log(`${DIM}Encrypted message: ${encryptedMessage.slice(0, 50)}...${RESET}`);
    
    // Send to the room
    await this.matrix.sendMessage(this.peer.roomId, encryptedMessage);
    console.log(`${GREEN}✓${RESET} Sent permission request`);
    
    return permissionRequest;
  }

  /**
   * Get our sender ID (hash of public key) - matches Beacon SDK getSenderId
   */
  async getSenderId() {
    // Beacon SDK: hash(publicKey, 5) -> hex
    const pubKeyHash = hash(this.keyPair.publicKey, 5);
    return toHex(pubKeyHash);
  }

  /**
   * Wait for permission response from wallet
   */
  async waitForPermissionResponse(timeoutMs = 120000) {
    console.log(`${YELLOW}⏳ Waiting for wallet approval...${RESET}`);
    
    const startTime = Date.now();
    
    while (Date.now() - startTime < timeoutMs) {
      try {
        const syncResult = await this.matrix.sync(5000);
        
        // Check for messages in joined rooms
        if (syncResult.rooms?.join) {
          for (const [roomId, room] of Object.entries(syncResult.rooms.join)) {
            const events = room.timeline?.events || [];
            for (const event of events) {
              if (event.type === 'm.room.message' && event.content?.body) {
                const message = event.content.body;
                
                // Skip channel-open messages (those are for pairing)
                if (message.startsWith('@channel-open:')) continue;
                
                // Try to decrypt as a permission response
                try {
                  const decrypted = await this.decryptMessageFromPeer(message);
                  if (decrypted) {
                    // decrypted is already parsed (deserialize returns parsed JSON)
                    const parsed = typeof decrypted === 'string' ? JSON.parse(decrypted) : decrypted;
                    console.log(`${GREEN}✓${RESET} Received message from wallet:`, parsed.type);
                    
                    if (parsed.type === 'permission_response') {
                      return parsed;
                    } else if (parsed.type === 'acknowledge') {
                      console.log(`${DIM}Received acknowledge message${RESET}`);
                    } else if (parsed.type === 'disconnect') {
                      throw new Error('Wallet rejected/disconnected');
                    } else if (parsed.type === 'error') {
                      throw new Error(`Wallet error: ${parsed.errorType || parsed.message}`);
                    }
                  }
                } catch (decryptErr) {
                  // Not a message we can decrypt, or parse error
                  if (!decryptErr.message.includes('decrypt') && !decryptErr.message.includes('Payload too short')) {
                    console.log(`${DIM}Message parsing: ${decryptErr.message}${RESET}`);
                  }
                }
              }
            }
          }
        }
      } catch (err) {
        if (!err.message.includes('timeout')) {
          console.log(`${DIM}Sync: ${err.message}${RESET}`);
        }
      }
    }
    
    throw new Error("Permission request timeout - wallet did not respond");
  }

  /**
   * Decrypt a message from the wallet using symmetric encryption
   * Wallet uses serverSessionKeys to send (so we receive with client's receive key)
   * But Beacon SDK: DApp creates CryptoBoxServer to receive from wallet!
   */
  async decryptMessageFromPeer(encryptedHex) {
    const encrypted = fromHex(encryptedHex);
    const peerPublicKey = fromHex(this.peer.publicKey);
    
    // Check minimum payload length (nonce + mac)
    if (encrypted.length < secretbox_NONCEBYTES + secretbox_MACBYTES) {
      throw new Error('Payload too short');
    }
    
    // Convert keys to x25519
    const myX25519Public = convertPublicKeyToX25519(this.keyPair.publicKey);
    const myX25519Secret = convertSecretKeyToX25519(this.keyPair.secretKey);
    const peerX25519Public = convertPublicKeyToX25519(peerPublicKey);
    
    // Beacon SDK: DApp uses createCryptoBoxServer to RECEIVE messages
    // (opposite of sending where we use createCryptoBoxClient)
    const sessionKeys = serverSessionKeys(
      { publicKey: myX25519Public, secretKey: myX25519Secret },
      peerX25519Public
    );
    const sharedKey = sessionKeys.receive;
    
    // Extract nonce (first 24 bytes) and ciphertext
    const nonce = encrypted.slice(0, secretbox_NONCEBYTES);
    const ciphertext = encrypted.slice(secretbox_NONCEBYTES);
    
    // Decrypt
    const decrypted = openSecretBox(sharedKey, nonce, ciphertext);
    if (!decrypted) {
      throw new Error('Decryption failed');
    }
    
    // The decrypted content is a serialized (bs58check) string
    const serializedStr = new TextDecoder().decode(decrypted);
    
    // Deserialize from bs58check
    const serializer = new Serializer();
    return await serializer.deserialize(serializedStr);
  }

  /**
   * Send an operation request to the wallet (e.g., contract call)
   * @param {Object} operationDetails - The operation to execute
   * @param {string} operationDetails.kind - "transaction", "origination", etc.
   * @param {string} operationDetails.destination - Contract address
   * @param {string} operationDetails.amount - Amount in mutez (string)
   * @param {Object} operationDetails.parameters - Michelson parameters
   * @param {string} operationDetails.fee - Fee in mutez (optional)
   * @param {string} operationDetails.gasLimit - Gas limit (optional)
   * @param {string} operationDetails.storageLimit - Storage limit (optional)
   */
  async sendOperationRequest(operationDetails) {
    console.log(`${DIM}Sending operation request to wallet...${RESET}`);
    
    const senderId = await this.getSenderId();
    
    // Tezos operations use snake_case: gas_limit, storage_limit
    // Temple iOS has issues estimating, so we provide generous defaults
    const ops = Array.isArray(operationDetails) ? operationDetails : [operationDetails];
    const normalizedOps = ops.map(op => {
      const cleanOp = { ...op };
      // Remove any camelCase variants
      delete cleanOp.gasLimit;
      delete cleanOp.storageLimit;
      // Set snake_case values with generous estimates for contract calls
      // These are based on typical FA2 minting operations
      cleanOp.fee = op.fee || "200000";              // 0.2 XTZ 
      cleanOp.gas_limit = op.gas_limit || "50000";   // 50k gas
      cleanOp.storage_limit = op.storage_limit || "5000";  // 5k storage bytes
      return cleanOp;
    });
    
    // Build the operation request (Beacon v2 format)
    const operationRequest = {
      type: "operation_request",
      version: "2",
      id: crypto.randomUUID(),
      senderId: senderId,
      network: {
        type: "ghostnet",
        rpcUrl: "https://ghostnet.ecadinfra.com"
      },
      operationDetails: normalizedOps,
      sourceAddress: this.permissionResponse?.address || this.permissionResponse?.publicKey
    };
    
    console.log(`${DIM}Operation request:${RESET}`, JSON.stringify(operationRequest, null, 2));
    
    // Serialize the message
    const serializer = new Serializer();
    const serializedMessage = await serializer.serialize(operationRequest);
    
    // Encrypt using symmetric key derived from key exchange
    const encryptedMessage = await this.encryptMessageForPeer(serializedMessage);
    
    // Send to the room
    await this.matrix.sendMessage(this.peer.roomId, encryptedMessage);
    console.log(`${GREEN}✓${RESET} Sent operation request`);
    
    return operationRequest;
  }

  /**
   * Wait for operation response from wallet
   */
  async waitForOperationResponse(timeoutMs = 120000) {
    console.log(`${YELLOW}⏳ Waiting for wallet to sign operation...${RESET}`);
    
    const startTime = Date.now();
    
    while (Date.now() - startTime < timeoutMs) {
      try {
        const syncResult = await this.matrix.sync(5000);
        
        // Check for messages in joined rooms
        if (syncResult.rooms?.join) {
          for (const [roomId, room] of Object.entries(syncResult.rooms.join)) {
            const events = room.timeline?.events || [];
            for (const event of events) {
              if (event.type === 'm.room.message' && event.content?.body) {
                const message = event.content.body;
                
                // Skip channel-open messages
                if (message.startsWith('@channel-open:')) continue;
                
                try {
                  const decrypted = await this.decryptMessageFromPeer(message);
                  if (decrypted) {
                    const parsed = typeof decrypted === 'string' ? JSON.parse(decrypted) : decrypted;
                    console.log(`${GREEN}✓${RESET} Received message from wallet:`, parsed.type);
                    
                    if (parsed.type === 'operation_response') {
                      return parsed;
                    } else if (parsed.type === 'acknowledge') {
                      console.log(`${DIM}Received acknowledge message${RESET}`);
                    } else if (parsed.type === 'error') {
                      throw new Error(`Wallet error: ${parsed.errorType || parsed.message || 'Operation rejected'}`);
                    }
                  }
                } catch (decryptErr) {
                  if (!decryptErr.message.includes('decrypt') && !decryptErr.message.includes('Payload too short')) {
                    console.log(`${DIM}Message parsing: ${decryptErr.message}${RESET}`);
                  }
                }
              }
            }
          }
        }
      } catch (err) {
        if (!err.message.includes('timeout')) {
          console.log(`${DIM}Sync: ${err.message}${RESET}`);
        }
      }
    }
    
    throw new Error("Operation request timeout - wallet did not respond");
  }

  /**
   * Encrypt a message for the peer using symmetric encryption (after pairing)
   * Uses shared secret derived from X25519 key exchange
   * Matches Beacon SDK: createCryptoBoxClient + encryptCryptoboxPayload
   */
  async encryptMessageForPeer(message) {
    const peerPublicKey = fromHex(this.peer.publicKey);
    
    // Convert ed25519 keys to x25519 using @stablelib
    const myX25519Public = convertPublicKeyToX25519(this.keyPair.publicKey);
    const myX25519Secret = convertSecretKeyToX25519(this.keyPair.secretKey);
    const peerX25519Public = convertPublicKeyToX25519(peerPublicKey);
    
    // Derive shared key using clientSessionKeys (DApp is client, wallet is server)
    // This matches Beacon SDK's createCryptoBoxClient
    const sessionKeys = clientSessionKeys(
      { publicKey: myX25519Public, secretKey: myX25519Secret },
      peerX25519Public
    );
    
    // Use .send for sending (DApp sends with client's send key)
    const sharedKey = sessionKeys.send;
    
    console.log(`${DIM}Shared key (first 8 bytes): ${toHex(sharedKey.slice(0, 8))}...${RESET}`);
    
    // Encrypt with secretbox (nonce || ciphertext)
    return encryptCryptoboxPayload(message, sharedKey);
  }
}

/**
 * Display QR code in terminal
 */
export function displayQR(data, small = true) {
  console.log(`\n${BOLD}${CYAN}╔════════════════════════════════════════════════════════════════╗${RESET}`);
  console.log(`${BOLD}${CYAN}║  📱 Scan with Temple Wallet (Beacon P2P)                       ║${RESET}`);
  console.log(`${BOLD}${CYAN}╚════════════════════════════════════════════════════════════════╝${RESET}\n`);
  
  // Generate QR synchronously to stdout
  qrcode.generate(data, { small });
  
  console.log(`\n\n${DIM}Waiting for wallet connection...${RESET}\n\n`);
}

/**
 * Full P2P pairing flow
 */
export async function pairWallet(appName = "Aesthetic Computer") {
  console.log(`\n${BOLD}${CYAN}═══════════════════════════════════════════════════════════════════${RESET}`);
  console.log(`${BOLD}${CYAN}  Beacon P2P Wallet Pairing                                        ${RESET}`);
  console.log(`${BOLD}${CYAN}═══════════════════════════════════════════════════════════════════${RESET}\n`);
  
  // Create P2P client
  const client = await new BeaconP2PClient().init();
  
  // Generate pairing request
  const { pairingRequest, serialized, deepLinkUrl } = client.generatePairingRequest(appName);
  
  console.log(`${GREEN}✓${RESET} Generated pairing request:`);
  console.log(`  ${DIM}ID:${RESET} ${pairingRequest.id}`);
  console.log(`  ${DIM}Name:${RESET} ${pairingRequest.name}`);
  console.log(`  ${DIM}Public Key:${RESET} ${pairingRequest.publicKey.slice(0, 16)}...`);
  console.log(`  ${DIM}Relay:${RESET} ${pairingRequest.relayServer}`);
  
  // Display QR code
  displayQR(deepLinkUrl);
  
  // Connect to Matrix relay
  await client.connect();
  
  // Wait for wallet to respond
  const response = await client.waitForPairing(120000);
  
  if (response) {
    console.log(`\n${GREEN}✓ Wallet paired successfully!${RESET}`);
    console.log(`  ${DIM}Name:${RESET} ${response.name}`);
    console.log(`  ${DIM}Public Key:${RESET} ${response.publicKey?.slice(0, 16)}...`);
    
    // Send permission request to complete the flow
    // This makes the wallet show the permission approval screen
    await client.sendPermissionRequest();
    
    // Wait for user to approve in wallet
    try {
      const permissionResponse = await client.waitForPermissionResponse(120000);
      console.log(`\n${GREEN}✓ Permission granted!${RESET}`);
      
      // Debug: show full response structure
      console.log(`${DIM}Full response:${RESET}`, JSON.stringify(permissionResponse, null, 2));
      
      // Get public key from response
      const publicKey = permissionResponse.publicKey || 
                        permissionResponse.account?.publicKey ||
                        permissionResponse.accountInfo?.publicKey;
      
      // Address might be in response, or derive from public key
      let address = permissionResponse.address || 
                    permissionResponse.account?.address ||
                    permissionResponse.accountInfo?.address;
      
      // Derive address from public key if not provided directly
      if (!address && publicKey) {
        try {
          address = getPkhfromPk(publicKey);
          console.log(`${DIM}(Address derived from public key)${RESET}`);
        } catch (e) {
          console.log(`${DIM}Could not derive address: ${e.message}${RESET}`);
        }
      }
      
      // Store the derived address in the response for callers
      permissionResponse.address = address;
      
      console.log(`  ${DIM}Address:${RESET} ${address || '(unknown)'}`);
      console.log(`  ${DIM}Public Key:${RESET} ${publicKey?.slice(0, 16)}...`);
      console.log(`  ${DIM}Network:${RESET} ${permissionResponse.network?.type}`);
      console.log(`  ${DIM}Scopes:${RESET} ${permissionResponse.scopes?.join(', ')}`);
      
      // Store permission response in client for operation requests
      client.permissionResponse = permissionResponse;
      
      return { client, response, permissionResponse };
    } catch (permErr) {
      console.log(`\n${RED}✗ Permission denied or timeout: ${permErr.message}${RESET}`);
      return { client, response, error: permErr.message };
    }
  } else {
    console.log(`\n${RED}✗ Pairing failed${RESET}`);
    return null;
  }
}

/**
 * Quick test of QR generation only (no Matrix connection)
 */
export async function testQR() {
  console.log(`\n${BOLD}Testing Beacon QR code generation...${RESET}\n`);
  
  const client = await new BeaconP2PClient().init();
  const { pairingRequest, serialized, deepLinkUrl } = client.generatePairingRequest();
  
  console.log(`${GREEN}✓${RESET} Generated pairing request:`);
  console.log(`  ID: ${pairingRequest.id}`);
  console.log(`  Name: ${pairingRequest.name}`);
  console.log(`  Public Key: ${pairingRequest.publicKey.slice(0, 16)}...`);
  console.log(`  Version: ${pairingRequest.version}`);
  console.log(`  Relay: ${pairingRequest.relayServer}`);
  console.log(`  Serialized: ${serialized.slice(0, 30)}...`);
  console.log(`  Length: ${serialized.length} chars`);
  console.log(`\n${CYAN}Deep Link URL for QR:${RESET}`);
  console.log(`  ${deepLinkUrl.slice(0, 60)}...\n`);
  
  // Display QR with deep link URL
  displayQR(deepLinkUrl);
  
  return { pairingRequest, serialized, deepLinkUrl };
}

/**
 * Send a contract call operation via a connected Beacon client
 * @param {BeaconP2PClient} client - Connected Beacon client
 * @param {string} contractAddress - Contract to call
 * @param {string} entrypoint - Entrypoint name
 * @param {Object} args - Michelson arguments
 * @param {string} amount - Amount in XTZ (optional, default "0")
 */
export async function sendContractCall(client, contractAddress, entrypoint, args, amountMutez = "0") {
  if (!client.permissionResponse?.address) {
    throw new Error("Wallet not connected - run pairWallet first");
  }
  
  // Amount is already in mutez
  const amountStr = String(amountMutez);
  const amountXtz = (parseFloat(amountStr) / 1_000_000).toFixed(2);
  
  console.log(`\n${BOLD}${CYAN}═══════════════════════════════════════════════════════════════════${RESET}`);
  console.log(`${BOLD}${CYAN}  Sending Contract Call via Temple                                 ${RESET}`);
  console.log(`${BOLD}${CYAN}═══════════════════════════════════════════════════════════════════${RESET}\n`);
  
  console.log(`${DIM}Contract:${RESET} ${contractAddress}`);
  console.log(`${DIM}Entrypoint:${RESET} ${entrypoint}`);
  console.log(`${DIM}Amount:${RESET} ${amountXtz} ꜩ (${amountStr} mutez)`);
  console.log(`${DIM}Sender:${RESET} ${client.permissionResponse.address}`);
  
  const operation = {
    kind: "transaction",
    destination: contractAddress,
    amount: amountStr,
    parameters: {
      entrypoint: entrypoint,
      value: args
    }
  };
  
  // Send operation request
  await client.sendOperationRequest(operation);
  
  // Wait for response
  const response = await client.waitForOperationResponse(120000);
  
  if (response.transactionHash) {
    console.log(`\n${GREEN}✓ Transaction confirmed!${RESET}`);
    console.log(`${DIM}Operation hash:${RESET} ${response.transactionHash}`);
    return response;
  } else {
    console.log(`\n${GREEN}✓ Operation response received${RESET}`);
    console.log(`${DIM}Response:${RESET}`, JSON.stringify(response, null, 2));
    return response;
  }
}

// Export the client class
export { BeaconP2PClient };

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const args = process.argv.slice(2);
  
  if (args.includes('--pair') || args.includes('-p')) {
    pairWallet().catch(err => {
      console.error(`${RED}Error: ${err.message}${RESET}`);
      process.exit(1);
    });
  } else {
    testQR().catch(console.error);
  }
}
