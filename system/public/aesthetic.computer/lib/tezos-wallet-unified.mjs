// tezos-wallet-unified.mjs - Unified Tezos wallet connection for Aesthetic Computer
// Uses Beacon SDK for all wallets (Temple, Kukai, etc.) via universal wallet picker.

const NETWORKS = {
  mainnet: {
    rpc: "https://mainnet.ecadinfra.com",
    name: "mainnet",
    displayName: "Mainnet",
  },
  ghostnet: {
    rpc: "https://ghostnet.ecadinfra.com",
    name: "ghostnet",
    displayName: "Ghostnet",
  },
};

let currentNetwork = "mainnet";
let walletState = {
  connected: false,
  address: null,
  balance: null,
  network: "mainnet",
  domain: null,
  walletType: null,
};

let tezosWallet = null;
let beaconClient = null;
let beaconNetwork = null;
let beaconSDK = null;

const SESSION_KEY = "ac:tezos:session";

// ═══════════════════════════════════════════════════════════════════════════
// Beacon SDK — NOTE: v4.0.12 is stable; v4.6.3+ has IndexedDB issues
// ═══════════════════════════════════════════════════════════════════════════

async function loadBeacon(network = currentNetwork) {
  if (!beaconSDK) {
    beaconSDK = await import("https://esm.sh/@airgap/beacon-sdk@4.0.12?bundle");
  }
  const { DAppClient, NetworkType } = beaconSDK;
  const networkType = network === "mainnet" ? NetworkType.MAINNET : NetworkType.GHOSTNET;

  if (!beaconClient || beaconNetwork !== network) {
    if (beaconClient) {
      try { await beaconClient.destroy(); } catch {}
    }
    beaconClient = new DAppClient({
      name: "Aesthetic Computer",
      preferredNetwork: networkType,
    });
    beaconNetwork = network;
  }
  return beaconClient;
}

function buildWallet(client, address, publicKey, network) {
  const rpcUrl = NETWORKS[network]?.rpc;
  return {
    _client: client,
    _rpcUrl: rpcUrl,
    _network: network,
    _publicKey: publicKey,
    pkh: () => address,
    sign: async (payload) => {
      const result = await client.requestSignPayload({
        signingType: "raw",
        payload,
      });
      return result.signature;
    },
    sendOperations: async (operations) => {
      const result = await client.requestOperation({
        operationDetails: operations,
      });
      return result.transactionHash;
    },
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// Unified Wallet API
// ═══════════════════════════════════════════════════════════════════════════

export async function init(network = "mainnet") {
  currentNetwork = network;
  walletState.network = network;
  await restoreSession();
  return walletState;
}

export async function restoreSession() {
  try {
    const savedSession = localStorage.getItem(SESSION_KEY);
    if (!savedSession) return false;

    const session = JSON.parse(savedSession);
    const network = session.network || currentNetwork;
    console.log("🔷 Restoring wallet session:", session.address?.slice(0, 8) + "...");

    const client = await loadBeacon(network);
    const activeAccount = await client.getActiveAccount();

    if (activeAccount?.address) {
      const address = activeAccount.address;
      tezosWallet = buildWallet(client, address, activeAccount.publicKey, network);
      walletState = {
        connected: true,
        address,
        network,
        walletType: "beacon",
        balance: null,
        domain: null,
      };
      console.log("✅ Beacon wallet restored:", address.slice(0, 8) + "...");

      fetchBalance(address, network).then((bal) => {
        walletState.balance = bal;
      });
      return true;
    } else {
      console.log("Beacon has no active account, clearing stale session");
      clearSession();
      return false;
    }
  } catch (err) {
    console.warn("restoreSession error:", err.message);
    clearSession();
  }
  return false;
}

export async function connect(network = currentNetwork) {
  console.log(`🔷 Connecting wallet via Beacon SDK on ${network}...`);

  try {
    const { PermissionScope } = beaconSDK || await import("https://esm.sh/@airgap/beacon-sdk@4.0.12?bundle");
    const client = await loadBeacon(network);

    const permissions = await client.requestPermissions({
      scopes: [PermissionScope.SIGN, PermissionScope.OPERATION_REQUEST],
    });

    const address = permissions.address;
    console.log("🔷 Wallet connected:", address);

    tezosWallet = buildWallet(client, address, permissions.publicKey, network);
    walletState = {
      connected: true,
      address,
      network,
      walletType: "beacon",
      balance: null,
      domain: null,
    };

    saveSession(address, network, "beacon");

    fetchBalance(address, network).then((bal) => {
      walletState.balance = bal;
    });

    return address;
  } catch (err) {
    console.error("🔷 Wallet connection error:", err?.message || err);
    throw new Error(err?.message || "Wallet connection cancelled");
  }
}

export async function disconnect() {
  if (beaconClient) {
    try { await beaconClient.clearActiveAccount(); } catch {}
  }

  tezosWallet = null;
  walletState = {
    connected: false,
    address: null,
    balance: null,
    network: currentNetwork,
    domain: null,
    walletType: null,
  };

  clearSession();
  console.log("🔌 Wallet disconnected");
}

export function getState() {
  return { ...walletState };
}

export function getAddress() {
  return walletState.address;
}

export function isConnected() {
  return walletState.connected;
}

export function getNetwork() {
  return walletState.network;
}

export async function sendOperations(operations) {
  if (!tezosWallet) throw new Error("No wallet connected");
  return await tezosWallet.sendOperations(operations);
}

export async function callContract(contractAddress, entrypoint, params, amount = 0) {
  if (!tezosWallet) throw new Error("No wallet connected");

  const operation = {
    kind: "transaction",
    destination: contractAddress,
    amount: String(Math.floor(amount * 1_000_000)),
    parameters: {
      entrypoint,
      value: params,
    },
  };

  return await sendOperations([operation]);
}

// ═══════════════════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════════════════

function saveSession(address, network, walletType) {
  try {
    localStorage.setItem(SESSION_KEY, JSON.stringify({ address, network, walletType }));
  } catch {}
}

function clearSession() {
  try { localStorage.removeItem(SESSION_KEY); } catch {}
}

async function fetchBalance(address, network = currentNetwork) {
  try {
    const rpc = NETWORKS[network]?.rpc;
    const res = await fetch(`${rpc}/chains/main/blocks/head/context/contracts/${address}/balance`);
    if (res.ok) {
      const mutez = await res.json();
      return parseInt(mutez, 10) / 1_000_000;
    }
  } catch {}
  return null;
}

export default {
  init,
  restoreSession,
  connect,
  disconnect,
  getState,
  getAddress,
  isConnected,
  getNetwork,
  sendOperations,
  callContract,
};
