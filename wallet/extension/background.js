// Keeps Wallet - Background Service Worker
// Handles key management, signing, Tezos operations, and Beacon protocol

import {
  initCrypto,
  generateMnemonic,
  validateMnemonic,
  deriveKeypair,
  importFromPrivateKey,
  encrypt,
  decrypt,
  signOperation as cryptoSignOperation,
  sign as cryptoSign,
  clearSensitiveData,
} from './lib/crypto.mjs';

import {
  getOrCreateBeaconKeypair,
  getSenderId,
  encryptBeaconMessage,
  decryptBeaconMessage,
  bytesToHex,
  hexToBytes,
} from './lib/beacon-crypto.mjs';

// State (in-memory, encrypted keys in storage)
let unlockedWallet = null; // { address, publicKey, secretKeyBytes }
let lockTimeout = null;
const LOCK_TIMEOUT_MS = 15 * 60 * 1000; // 15 minutes

// Beacon pending requests — requestId -> { request, peerPublicKey, senderTabId, resolve }
const pendingBeaconRequests = new Map();

// Initialize crypto on load
initCrypto().then(() => {
  console.log('Keeps Wallet crypto initialized');
});

// Prefer opening the UI in the side panel (new extension UX)
async function enableSidePanel() {
  if (!chrome.sidePanel?.setPanelBehavior) return;
  try {
    await chrome.sidePanel.setPanelBehavior({ openPanelOnActionClick: true });
    await chrome.sidePanel.setOptions({
      path: chrome.runtime.getURL('popup/popup.html'),
      enabled: true,
    });
  } catch (err) {
    console.warn('Side panel setup failed', err);
  }
}

// Open UI via side panel when available; otherwise fall back to popup window.
async function openUiForClick(tab) {
  if (chrome.sidePanel?.open) {
    try {
      await chrome.sidePanel.open({ windowId: tab?.windowId });
      return;
    } catch (err) {
      console.warn('Side panel open failed, falling back to popup window', err);
    }
  }

  // Fallback: open small popup window with the same UI
  chrome.windows.create({
    url: chrome.runtime.getURL('popup/popup.html'),
    type: 'popup',
    width: 420,
    height: 640,
  });
}

chrome.runtime.onInstalled.addListener(() => {
  enableSidePanel();
});

chrome.runtime.onStartup?.addListener(() => {
  enableSidePanel();
});

// Open UI on browser action click
chrome.action.onClicked.addListener((tab) => {
  openUiForClick(tab);
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

      // --- Beacon protocol ---
      case 'BEACON_MESSAGE':
        return await handleBeaconMessage(payload, sender);

      case 'BEACON_CONFIRM':
        return await handleBeaconConfirm(payload);

      case 'BEACON_GET_PENDING':
        return await handleBeaconGetPending(payload);

      default:
        return { error: 'Unknown message type' };
    }
  } catch (error) {
    console.error('Message handler error:', error);
    return { error: error.message };
  }
}

// ============================================================
// Beacon Protocol
// ============================================================

async function handleBeaconMessage(msgFromContent, sender) {
  // msgFromContent is the raw postMessage data from the page
  // It has: { target: "toExtension", payload: ..., encryptedPayload?: ..., targetId?: ... }
  const data = msgFromContent;

  // --- Pairing request (unencrypted, has publicKey + name + type fields in payload) ---
  if (
    data.payload &&
    typeof data.payload === 'object' &&
    data.payload.publicKey &&
    data.payload.name
  ) {
    return await handleBeaconPairing(data.payload, sender);
  }

  // --- Encrypted message ---
  if (data.encryptedPayload) {
    return await handleBeaconEncryptedMessage(
      data.encryptedPayload,
      sender,
    );
  }

  // Unknown Beacon message shape — ignore
  return null;
}

async function handleBeaconPairing(pairingRequest, sender) {
  // pairingRequest: { id, name, publicKey (hex), version, icon?, appUrl?, type? }
  const beaconKp = await getOrCreateBeaconKeypair();
  const ourPublicKeyHex = bytesToHex(beaconKp.publicKey);
  const ourSenderId = await getSenderId(beaconKp.publicKey);

  // Store dApp as a peer
  const peerPublicKeyBytes = hexToBytes(pairingRequest.publicKey);
  const peerSenderId = await getSenderId(peerPublicKeyBytes);

  const peerData = {
    name: pairingRequest.name,
    publicKey: pairingRequest.publicKey,
    senderId: peerSenderId,
    version: pairingRequest.version || '2',
    icon: pairingRequest.icon || null,
    appUrl: pairingRequest.appUrl || null,
    connectedAt: Date.now(),
  };

  // Save peer
  const stored = await chrome.storage.local.get('beacon_peers');
  const peers = stored.beacon_peers || {};
  peers[peerSenderId] = peerData;
  await chrome.storage.local.set({ beacon_peers: peers });

  // Build pairing response
  const pairingResponse = {
    target: 'toPage',
    payload: {
      type: 'postmessage-pairing-response',
      id: pairingRequest.id,
      name: 'Keeps Wallet',
      publicKey: ourPublicKeyHex,
      version: pairingRequest.version || '2',
      extensionId: 'keeps-wallet',
      senderId: ourSenderId,
    },
  };

  return { postToPage: pairingResponse };
}

async function handleBeaconEncryptedMessage(encryptedHex, sender) {
  const beaconKp = await getOrCreateBeaconKeypair();
  const ourSenderId = await getSenderId(beaconKp.publicKey);

  // We need to find which peer sent this. Try all known peers.
  const stored = await chrome.storage.local.get('beacon_peers');
  const peers = stored.beacon_peers || {};

  let decryptedJson = null;
  let matchedPeerSenderId = null;
  let matchedPeerPublicKey = null;

  for (const [peerId, peer] of Object.entries(peers)) {
    try {
      const peerPkBytes = hexToBytes(peer.publicKey);
      const plaintext = await decryptBeaconMessage(
        encryptedHex,
        peerPkBytes,
        beaconKp.secretKey,
      );
      decryptedJson = plaintext;
      matchedPeerSenderId = peerId;
      matchedPeerPublicKey = peer.publicKey;
      break;
    } catch {
      // Wrong peer, try next
    }
  }

  if (!decryptedJson) {
    console.warn('Beacon: could not decrypt message from any known peer');
    return null;
  }

  let beaconMsg;
  try {
    beaconMsg = JSON.parse(decryptedJson);
  } catch {
    console.error('Beacon: decrypted payload is not valid JSON');
    return null;
  }

  const peerPkBytes = hexToBytes(matchedPeerPublicKey);

  // Route based on Beacon message type
  switch (beaconMsg.type) {
    case 'permission_request':
      return await handleBeaconPermissionRequest(
        beaconMsg,
        peerPkBytes,
        matchedPeerSenderId,
        beaconKp,
        ourSenderId,
        sender,
      );

    case 'operation_request':
      return await handleBeaconOperationRequest(
        beaconMsg,
        peerPkBytes,
        matchedPeerSenderId,
        beaconKp,
        ourSenderId,
        sender,
      );

    case 'sign_payload_request':
      return await handleBeaconSignPayloadRequest(
        beaconMsg,
        peerPkBytes,
        matchedPeerSenderId,
        beaconKp,
        ourSenderId,
        sender,
      );

    case 'disconnect':
      return await handleBeaconDisconnect(matchedPeerSenderId);

    default:
      console.warn('Beacon: unhandled message type', beaconMsg.type);
      return null;
  }
}

// --- Send an encrypted Beacon response back to the page via content script ---

async function sendBeaconResponseToTab(tabId, responseObj, peerPkBytes, beaconKp) {
  const responseJson = JSON.stringify(responseObj);
  const encryptedHex = await encryptBeaconMessage(
    responseJson,
    peerPkBytes,
    beaconKp.secretKey,
  );

  chrome.tabs.sendMessage(tabId, {
    type: 'BEACON_RESPONSE',
    data: {
      target: 'toPage',
      encryptedPayload: encryptedHex,
    },
  });
}

// --- Send an unencrypted acknowledge immediately ---

async function sendBeaconAcknowledge(tabId, requestId, ourSenderId) {
  // Acknowledge is sent unencrypted as a postMessage
  // Actually, per Beacon protocol, acknowledge is encrypted too.
  // But for simplicity and compatibility, we send it encrypted.
  // We'll handle it in the request handlers.
}

// --- Build a Beacon error response ---

function buildBeaconError(requestId, errorType, ourSenderId) {
  return {
    type: 'error',
    id: requestId,
    version: '2',
    senderId: ourSenderId,
    errorType: errorType,
  };
}

// --- Permission Request ---

async function handleBeaconPermissionRequest(
  request,
  peerPkBytes,
  peerSenderId,
  beaconKp,
  ourSenderId,
  sender,
) {
  const tabId = sender.tab?.id;

  // Send encrypted acknowledge
  if (tabId) {
    const ack = {
      type: 'acknowledge',
      id: request.id,
      version: '2',
      senderId: ourSenderId,
    };
    await sendBeaconResponseToTab(tabId, ack, peerPkBytes, beaconKp);
  }

  // Check if wallet is unlocked
  if (!unlockedWallet) {
    if (tabId) {
      const err = buildBeaconError(request.id, 'NOT_GRANTED_ERROR', ourSenderId);
      await sendBeaconResponseToTab(tabId, err, peerPkBytes, beaconKp);
    }
    return null;
  }

  // Store pending request and open confirmation popup
  const requestId = request.id;
  const stored = await chrome.storage.local.get('beacon_peers');
  const peers = stored.beacon_peers || {};
  const peerInfo = peers[peerSenderId] || {};

  return new Promise((resolve) => {
    pendingBeaconRequests.set(requestId, {
      requestType: 'permission_request',
      request,
      peerPublicKey: bytesToHex(peerPkBytes),
      peerSenderId,
      peerName: peerInfo.name || 'Unknown dApp',
      peerIcon: peerInfo.icon || null,
      beaconKpPublicKey: bytesToHex(beaconKp.publicKey),
      tabId,
      ourSenderId,
      resolve,
    });

    // Open confirmation popup
    chrome.windows.create({
      url: chrome.runtime.getURL(
        `confirm.html?requestId=${encodeURIComponent(requestId)}`,
      ),
      type: 'popup',
      width: 400,
      height: 500,
      focused: true,
    });
  });
}

// --- Operation Request ---

async function handleBeaconOperationRequest(
  request,
  peerPkBytes,
  peerSenderId,
  beaconKp,
  ourSenderId,
  sender,
) {
  const tabId = sender.tab?.id;

  // Send encrypted acknowledge
  if (tabId) {
    const ack = {
      type: 'acknowledge',
      id: request.id,
      version: '2',
      senderId: ourSenderId,
    };
    await sendBeaconResponseToTab(tabId, ack, peerPkBytes, beaconKp);
  }

  // Check if wallet is unlocked
  if (!unlockedWallet) {
    if (tabId) {
      const err = buildBeaconError(request.id, 'NOT_GRANTED_ERROR', ourSenderId);
      await sendBeaconResponseToTab(tabId, err, peerPkBytes, beaconKp);
    }
    return null;
  }

  const requestId = request.id;
  const stored = await chrome.storage.local.get('beacon_peers');
  const peers = stored.beacon_peers || {};
  const peerInfo = peers[peerSenderId] || {};

  return new Promise((resolve) => {
    pendingBeaconRequests.set(requestId, {
      requestType: 'operation_request',
      request,
      peerPublicKey: bytesToHex(peerPkBytes),
      peerSenderId,
      peerName: peerInfo.name || 'Unknown dApp',
      peerIcon: peerInfo.icon || null,
      beaconKpPublicKey: bytesToHex(beaconKp.publicKey),
      tabId,
      ourSenderId,
      resolve,
    });

    chrome.windows.create({
      url: chrome.runtime.getURL(
        `confirm.html?requestId=${encodeURIComponent(requestId)}`,
      ),
      type: 'popup',
      width: 400,
      height: 600,
      focused: true,
    });
  });
}

// --- Sign Payload Request ---

async function handleBeaconSignPayloadRequest(
  request,
  peerPkBytes,
  peerSenderId,
  beaconKp,
  ourSenderId,
  sender,
) {
  const tabId = sender.tab?.id;

  // Send encrypted acknowledge
  if (tabId) {
    const ack = {
      type: 'acknowledge',
      id: request.id,
      version: '2',
      senderId: ourSenderId,
    };
    await sendBeaconResponseToTab(tabId, ack, peerPkBytes, beaconKp);
  }

  // Check if wallet is unlocked
  if (!unlockedWallet) {
    if (tabId) {
      const err = buildBeaconError(request.id, 'NOT_GRANTED_ERROR', ourSenderId);
      await sendBeaconResponseToTab(tabId, err, peerPkBytes, beaconKp);
    }
    return null;
  }

  const requestId = request.id;
  const stored = await chrome.storage.local.get('beacon_peers');
  const peers = stored.beacon_peers || {};
  const peerInfo = peers[peerSenderId] || {};

  return new Promise((resolve) => {
    pendingBeaconRequests.set(requestId, {
      requestType: 'sign_payload_request',
      request,
      peerPublicKey: bytesToHex(peerPkBytes),
      peerSenderId,
      peerName: peerInfo.name || 'Unknown dApp',
      peerIcon: peerInfo.icon || null,
      beaconKpPublicKey: bytesToHex(beaconKp.publicKey),
      tabId,
      ourSenderId,
      resolve,
    });

    chrome.windows.create({
      url: chrome.runtime.getURL(
        `confirm.html?requestId=${encodeURIComponent(requestId)}`,
      ),
      type: 'popup',
      width: 400,
      height: 500,
      focused: true,
    });
  });
}

// --- Disconnect ---

async function handleBeaconDisconnect(peerSenderId) {
  const stored = await chrome.storage.local.get('beacon_peers');
  const peers = stored.beacon_peers || {};
  delete peers[peerSenderId];
  await chrome.storage.local.set({ beacon_peers: peers });
  return null;
}

// --- Get pending request details (for confirm.html) ---

async function handleBeaconGetPending(payload) {
  const { requestId } = payload;
  const pending = pendingBeaconRequests.get(requestId);
  if (!pending) {
    return { error: 'No pending request found' };
  }

  return {
    requestType: pending.requestType,
    peerName: pending.peerName,
    peerIcon: pending.peerIcon,
    request: pending.request,
    walletAddress: unlockedWallet?.address || null,
  };
}

// --- Handle confirmation from confirm.html ---

async function handleBeaconConfirm(payload) {
  const { requestId, approved } = payload;
  const pending = pendingBeaconRequests.get(requestId);
  if (!pending) {
    return { error: 'No pending request found' };
  }

  pendingBeaconRequests.delete(requestId);

  const beaconKp = await getOrCreateBeaconKeypair();
  const peerPkBytes = hexToBytes(pending.peerPublicKey);

  if (!approved) {
    // Send error response: user rejected
    if (pending.tabId) {
      const err = buildBeaconError(
        pending.request.id,
        'NOT_GRANTED_ERROR',
        pending.ourSenderId,
      );
      await sendBeaconResponseToTab(
        pending.tabId,
        err,
        peerPkBytes,
        beaconKp,
      );
    }
    if (pending.resolve) pending.resolve(null);
    return { success: true };
  }

  // --- Approved: build and send appropriate response ---
  let response;

  switch (pending.requestType) {
    case 'permission_request':
      response = await buildPermissionResponse(pending);
      break;
    case 'operation_request':
      response = await buildOperationResponse(pending);
      break;
    case 'sign_payload_request':
      response = await buildSignPayloadResponse(pending);
      break;
    default:
      if (pending.resolve) pending.resolve(null);
      return { error: 'Unknown request type' };
  }

  // If the response is an error (e.g., operation failed), send as error
  if (response && response.type === 'error') {
    if (pending.tabId) {
      await sendBeaconResponseToTab(
        pending.tabId,
        response,
        peerPkBytes,
        beaconKp,
      );
    }
    if (pending.resolve) pending.resolve(null);
    return { success: true };
  }

  // Send encrypted response back to the dApp
  if (pending.tabId && response) {
    await sendBeaconResponseToTab(
      pending.tabId,
      response,
      peerPkBytes,
      beaconKp,
    );
  }

  if (pending.resolve) pending.resolve(null);
  return { success: true };
}

// --- Build permission response ---

async function buildPermissionResponse(pending) {
  if (!unlockedWallet) {
    return buildBeaconError(
      pending.request.id,
      'NOT_GRANTED_ERROR',
      pending.ourSenderId,
    );
  }

  const storedState = await chrome.storage.local.get('network');
  const network = storedState.network || 'ghostnet';

  return {
    type: 'permission_response',
    id: pending.request.id,
    publicKey: unlockedWallet.publicKey, // edpk... base58
    network: { type: network },
    scopes: ['sign', 'operation_request'],
    appMetadata: {
      senderId: pending.peerSenderId,
      name: pending.peerName,
    },
    version: '2',
    senderId: pending.ourSenderId,
  };
}

// --- Build operation response ---

async function buildOperationResponse(pending) {
  if (!unlockedWallet) {
    return buildBeaconError(
      pending.request.id,
      'NOT_GRANTED_ERROR',
      pending.ourSenderId,
    );
  }

  try {
    const request = pending.request;
    const storedState = await chrome.storage.local.get('network');
    const network = storedState.network || 'ghostnet';

    // Get the RPC node URL
    const rpcUrl =
      network === 'mainnet'
        ? 'https://mainnet.api.tez.ie'
        : 'https://ghostnet.teztnets.com';

    // Forge the operations via RPC
    const operationContents = request.operationDetails;
    const sourceAddress = unlockedWallet.address;

    // Get counter and branch
    const [headRes, counterRes] = await Promise.all([
      fetch(`${rpcUrl}/chains/main/blocks/head/header`),
      fetch(
        `${rpcUrl}/chains/main/blocks/head/context/contracts/${sourceAddress}/counter`,
      ),
    ]);

    const head = await headRes.json();
    const currentCounter = parseInt(await counterRes.json());
    const branch = head.hash;

    // Build the operations array with proper counter, source, etc.
    let counter = currentCounter;
    const contents = operationContents.map((op) => {
      counter++;
      const content = {
        kind: op.kind,
        source: sourceAddress,
        counter: String(counter),
        fee: op.fee || '0',
        gas_limit: op.gas_limit || '10600',
        storage_limit: op.storage_limit || '300',
      };

      if (op.kind === 'transaction') {
        content.destination = op.destination;
        content.amount = op.amount || '0';
        if (op.parameters) {
          content.parameters = op.parameters;
        }
      } else if (op.kind === 'origination') {
        content.balance = op.balance || '0';
        content.script = op.script;
      } else if (op.kind === 'delegation') {
        if (op.delegate) content.delegate = op.delegate;
      }

      return content;
    });

    // Forge via RPC
    const forgeRes = await fetch(
      `${rpcUrl}/chains/main/blocks/head/helpers/forge/operations`,
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ branch, contents }),
      },
    );

    const forgedBytes = (await forgeRes.json()).replace(/^"|"$/g, '');

    // Sign the forged operation
    const signature = await cryptoSignOperation(
      forgedBytes,
      unlockedWallet.secretKeyBytes,
    );

    // Inject the signed operation
    const signedOp = signature.signedBytes;
    const injectRes = await fetch(
      `${rpcUrl}/injection/operation`,
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(signedOp),
      },
    );

    const opHash = (await injectRes.json()).replace(/^"|"$/g, '');

    resetLockTimeout();

    return {
      type: 'operation_response',
      id: pending.request.id,
      transactionHash: opHash,
      version: '2',
      senderId: pending.ourSenderId,
    };
  } catch (error) {
    console.error('Beacon operation error:', error);
    return {
      type: 'error',
      id: pending.request.id,
      version: '2',
      senderId: pending.ourSenderId,
      errorType: 'TRANSACTION_INVALID_ERROR',
      errorData: [{ kind: 'temporary', id: error.message }],
    };
  }
}

// --- Build sign payload response ---

async function buildSignPayloadResponse(pending) {
  if (!unlockedWallet) {
    return buildBeaconError(
      pending.request.id,
      'NOT_GRANTED_ERROR',
      pending.ourSenderId,
    );
  }

  try {
    const request = pending.request;
    const payloadToSign = request.payload;

    // Sign the payload (it comes as a hex string from the dApp)
    const signature = await cryptoSign(
      payloadToSign,
      unlockedWallet.secretKeyBytes,
    );

    resetLockTimeout();

    return {
      type: 'sign_payload_response',
      id: pending.request.id,
      signingType: request.signingType || 'raw',
      signature: signature.edsig,
      version: '2',
      senderId: pending.ourSenderId,
    };
  } catch (error) {
    console.error('Beacon sign payload error:', error);
    return {
      type: 'error',
      id: pending.request.id,
      version: '2',
      senderId: pending.ourSenderId,
      errorType: 'SIGNATURE_TYPE_NOT_SUPPORTED',
    };
  }
}

// ============================================================
// Existing Wallet Operations (unchanged)
// ============================================================

// Wallet State
async function getWalletState() {
  const stored = await chrome.storage.local.get([
    'encryptedSeed',
    'address',
    'publicKey',
    'network',
  ]);
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

  const mnemonic = generateMnemonic();
  const keypair = await deriveKeypair(mnemonic);
  const encryptedSeed = await encrypt(mnemonic, password);

  await chrome.storage.local.set({
    encryptedSeed,
    address: keypair.address,
    publicKey: keypair.publicKey,
    network: 'ghostnet',
  });

  unlockedWallet = {
    address: keypair.address,
    publicKey: keypair.publicKey,
    secretKeyBytes: keypair.secretKeyBytes,
  };
  resetLockTimeout();

  return {
    success: true,
    mnemonic,
    address: keypair.address,
  };
}

// Import existing wallet
async function importWallet(mnemonic, password) {
  if (!password || password.length < 8) {
    return { error: 'Password must be at least 8 characters' };
  }

  const cleanMnemonic = mnemonic.trim().toLowerCase().replace(/\s+/g, ' ');
  if (!validateMnemonic(cleanMnemonic)) {
    return { error: 'Invalid seed phrase' };
  }

  await initCrypto();

  const keypair = await deriveKeypair(cleanMnemonic);
  const encryptedSeed = await encrypt(cleanMnemonic, password);

  await chrome.storage.local.set({
    encryptedSeed,
    address: keypair.address,
    publicKey: keypair.publicKey,
    network: 'ghostnet',
  });

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

    const keypair = await importFromPrivateKey(privateKey.trim());
    const encryptedSeed = await encrypt(privateKey.trim(), password);

    await chrome.storage.local.set({
      encryptedSeed,
      importType: 'privateKey',
      address: keypair.address,
      publicKey: keypair.publicKey,
      network: 'ghostnet',
    });

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
  const { encryptedSeed, importType, address, publicKey } =
    await chrome.storage.local.get([
      'encryptedSeed',
      'importType',
      'address',
      'publicKey',
    ]);

  if (!encryptedSeed) {
    return { error: 'No wallet found' };
  }

  try {
    await initCrypto();

    const decrypted = await decrypt(encryptedSeed, password);

    let keypair;
    if (importType === 'privateKey') {
      keypair = await importFromPrivateKey(decrypted);
    } else {
      keypair = await deriveKeypair(decrypted);
    }

    if (keypair.address !== address) {
      return { error: 'Key derivation mismatch' };
    }

    unlockedWallet = {
      address: keypair.address,
      publicKey: keypair.publicKey,
      secretKeyBytes: keypair.secretKeyBytes,
    };

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

  console.log('Signing operation:', operation);

  try {
    const signature = await cryptoSignOperation(
      operation.forgedBytes,
      unlockedWallet.secretKeyBytes,
    );
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
  const apiBase =
    net === 'mainnet'
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
  const apiBase =
    net === 'mainnet'
      ? 'https://api.tzkt.io'
      : 'https://api.ghostnet.tzkt.io';

  try {
    const res = await fetch(
      `${apiBase}/v1/tokens/balances?account=${address}&balance.gt=0&token.standard=fa2&limit=100`,
    );
    const tokens = await res.json();

    const keeps = tokens.map((t) => ({
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

console.log('Keeps Wallet background service worker loaded');
