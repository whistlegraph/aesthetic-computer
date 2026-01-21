// tezos-wallet-unified.mjs - Unified Tezos wallet connection for Aesthetic Computer
// Supports Temple Wallet (direct postMessage) and Beacon SDK (for Kukai/other wallets)
// Mirrors the approach in bios.mjs for consistency across aesthetic.computer and kidlisp.com

// Network configuration
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

// State
let currentNetwork = "mainnet";
let walletState = {
  connected: false,
  address: null,
  balance: null,
  network: "mainnet",
  domain: null,
  walletType: null, // "temple" | "kukai" | "beacon"
};

let tezosWallet = null; // Wallet object with sendOperations
let beaconClient = null; // Beacon DAppClient for Kukai/other wallets
let beaconNetwork = null;
let templeClient = null; // Temple client for direct communication

// Session storage key
const SESSION_KEY = "ac:tezos:session";

// ═══════════════════════════════════════════════════════════════════════════
// Temple Wallet (Direct PostMessage Communication)
// ═══════════════════════════════════════════════════════════════════════════

function createTempleClient() {
  return {
    _reqId: 0,
    _pending: new Map(),
    _initialized: false,

    init() {
      if (this._initialized) return;
      window.addEventListener("message", this._handleMessage.bind(this));
      this._initialized = true;
    },

    _handleMessage(event) {
      if (event.source !== window || event.origin !== window.origin) return;
      const data = event.data || {};

      if (data.type === "TEMPLE_PAGE_RESPONSE") {
        const { payload, reqId } = data;
        if (this._pending.has(reqId)) {
          const { resolve, reject } = this._pending.get(reqId);
          this._pending.delete(reqId);
          if (payload?.error) {
            reject(new Error(payload.error.message || payload.error));
          } else {
            resolve(payload);
          }
        }
      }

      if (data.type === "TEMPLE_PAGE_ERROR_RESPONSE") {
        const { payload, reqId } = data;
        if (this._pending.has(reqId)) {
          const { reject } = this._pending.get(reqId);
          this._pending.delete(reqId);
          reject(new Error(payload?.message || "Temple error"));
        }
      }
    },

    async request(payload) {
      const reqId = ++this._reqId;
      return new Promise((resolve, reject) => {
        this._pending.set(reqId, { resolve, reject });

        // Timeout after 60 seconds
        setTimeout(() => {
          if (this._pending.has(reqId)) {
            this._pending.delete(reqId);
            reject(new Error("Temple request timed out"));
          }
        }, 60000);

        window.postMessage(
          {
            type: "TEMPLE_PAGE_REQUEST",
            payload,
            reqId,
          },
          window.origin
        );
      });
    },

    async isAvailable() {
      try {
        const result = await Promise.race([
          this.request({ type: "GET_CURRENT_PERMISSION" }),
          new Promise((_, reject) =>
            setTimeout(() => reject(new Error("Temple not responding")), 1000)
          ),
        ]);
        return true;
      } catch {
        return false;
      }
    },

    async getCurrentPermission() {
      try {
        return await this.request({ type: "GET_CURRENT_PERMISSION" });
      } catch {
        return null;
      }
    },

    async requestPermission(network = "mainnet") {
      return await this.request({
        type: "CONNECT",
        network: network === "mainnet" ? "mainnet" : "ghostnet",
        appMeta: {
          name: "Aesthetic Computer",
        },
      });
    },

    async disconnect() {
      try {
        await this.request({ type: "REMOVE_PERMISSION" });
      } catch {
        // Ignore disconnect errors
      }
    },
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// Beacon SDK (For Kukai and other wallets)
// ═══════════════════════════════════════════════════════════════════════════

async function loadBeaconSDK() {
  if (beaconClient) return beaconClient;

  try {
    // Use stable v4.0.12 that doesn't have IndexedDB metrics issues
    const beacon = await import(
      "https://esm.sh/@airgap/beacon-sdk@4.0.12?bundle"
    );
    const { DAppClient, NetworkType } = beacon;

    const networkType =
      currentNetwork === "mainnet" ? NetworkType.MAINNET : NetworkType.GHOSTNET;

    beaconClient = new DAppClient({
      name: "Aesthetic Computer",
      preferredNetwork: networkType,
    });
    beaconNetwork = currentNetwork;

    console.log("🥝 Beacon SDK loaded");
    return beaconClient;
  } catch (e) {
    console.error("Failed to load Beacon SDK:", e);
    throw new Error("Failed to load wallet SDK");
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// Unified Wallet API
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Initialize the wallet system
 * @param {string} network - 'mainnet' or 'ghostnet'
 */
export async function init(network = "mainnet") {
  currentNetwork = network;
  walletState.network = network;

  // Initialize Temple client for potential use
  if (!templeClient) {
    templeClient = createTempleClient();
    templeClient.init();
  }

  // Try to restore existing session
  await restoreSession();

  return walletState;
}

/**
 * Restore wallet session from localStorage
 */
export async function restoreSession() {
  try {
    const savedSession = localStorage.getItem(SESSION_KEY);
    if (!savedSession) return false;

    const session = JSON.parse(savedSession);
    console.log(
      "🔷 Restoring wallet session:",
      session.address?.slice(0, 8) + "...",
      "type:",
      session.walletType
    );

    const rpcUrl = NETWORKS[session.network || currentNetwork]?.rpc;

    if (session.walletType === "temple") {
      // Verify Temple is still available
      if (!templeClient) {
        templeClient = createTempleClient();
        templeClient.init();
      }

      const available = await templeClient.isAvailable();
      if (available) {
        tezosWallet = createTempleWalletObject(session.address, rpcUrl, session.network);
        walletState = {
          connected: true,
          address: session.address,
          network: session.network || currentNetwork,
          walletType: "temple",
          balance: null,
          domain: null,
        };
        console.log("✅ Temple wallet restored");

        // Fetch balance in background
        fetchBalance(session.address, session.network).then((bal) => {
          walletState.balance = bal;
        });

        return true;
      } else {
        console.log("Temple extension not available, clearing stale session");
        clearSession();
        return false;
      }
    } else if (session.walletType === "kukai" || session.walletType === "beacon") {
      // For Beacon wallets, mark as needing reconnection for signing
      tezosWallet = {
        _rpcUrl: rpcUrl,
        _network: session.network,
        _needsReconnect: true,
        pkh: () => session.address,
        sendOperations: async () => {
          throw new Error("Wallet session expired - please reconnect");
        },
      };
      walletState = {
        connected: true,
        address: session.address,
        network: session.network || currentNetwork,
        walletType: session.walletType,
        balance: null,
        domain: null,
      };
      console.log("🥝 Beacon wallet session restored (signing requires reconnection)");

      // Fetch balance in background
      fetchBalance(session.address, session.network).then((bal) => {
        walletState.balance = bal;
      });

      return true;
    }
  } catch (err) {
    console.warn("restoreSession error:", err.message);
    clearSession();
  }
  return false;
}

/**
 * Create a Temple wallet object for operations
 */
function createTempleWalletObject(address, rpcUrl, network) {
  return {
    _client: templeClient,
    _rpcUrl: rpcUrl,
    _network: network,
    pkh: () => address,
    sign: async (payload) => {
      const result = await templeClient.request({
        type: "SIGN_REQUEST",
        payload,
        sourcePkh: address,
      });
      return result?.signature;
    },
    sendOperations: async (operations) => {
      const result = await templeClient.request({
        type: "OPERATION_REQUEST",
        sourcePkh: address,
        opParams: operations,
      });
      return result?.opHash;
    },
  };
}

/**
 * Connect to a Tezos wallet
 * @param {string} network - 'mainnet' or 'ghostnet'
 * @param {object} options - { walletType: 'temple' | 'kukai' | 'beacon' }
 * @returns {Promise<string>} Connected wallet address
 */
export async function connect(network = currentNetwork, options = {}) {
  const walletType = options.walletType || "temple";
  const rpcUrl = NETWORKS[network]?.rpc;

  console.log(`🔷 Attempting ${walletType} wallet connection on ${network}...`);

  if (walletType === "kukai" || walletType === "beacon") {
    return connectBeacon(network, rpcUrl);
  }

  // Default: Temple wallet via direct postMessage
  return connectTemple(network, rpcUrl);
}

/**
 * Connect via Temple Wallet
 */
async function connectTemple(network, rpcUrl) {
  if (!templeClient) {
    templeClient = createTempleClient();
    templeClient.init();
  }

  const available = await templeClient.isAvailable();
  if (!available) {
    throw new Error("Temple Wallet not found. Install it from templewallet.com");
  }

  try {
    // Check for existing permission first
    const existingPerm = await templeClient.getCurrentPermission();
    let address;

    if (existingPerm?.pkh) {
      address = existingPerm.pkh;
      console.log("🔷 Using existing Temple permission:", address.slice(0, 8) + "...");
    } else {
      // Request new permission
      const perm = await templeClient.requestPermission(network);
      address = perm?.pkh;
      if (!address) {
        throw new Error("Temple connection cancelled");
      }
      console.log("🔷 New Temple permission granted:", address.slice(0, 8) + "...");
    }

    // Create wallet object
    tezosWallet = createTempleWalletObject(address, rpcUrl, network);

    // Update state
    walletState = {
      connected: true,
      address,
      network,
      walletType: "temple",
      balance: null,
      domain: null,
    };

    // Save session
    saveSession(address, network, "temple");

    // Fetch balance in background
    fetchBalance(address, network).then((bal) => {
      walletState.balance = bal;
    });

    console.log("✅ Temple wallet connected:", address);
    return address;
  } catch (err) {
    console.error("🔷 Temple connection error:", err?.message || err);
    throw new Error(err?.message || "Temple connection cancelled");
  }
}

/**
 * Connect via Beacon SDK (Kukai, etc.)
 */
async function connectBeacon(network, rpcUrl) {
  await loadBeaconSDK();

  try {
    const beacon = await import("https://esm.sh/@airgap/beacon-sdk@4.0.12?bundle");
    const { NetworkType, PermissionScope } = beacon;

    const networkType =
      network === "mainnet" ? NetworkType.MAINNET : NetworkType.GHOSTNET;

    // Recreate client if network changed
    if (beaconNetwork !== network) {
      if (beaconClient) {
        try {
          await beaconClient.destroy();
        } catch {}
      }
      const { DAppClient } = beacon;
      beaconClient = new DAppClient({
        name: "Aesthetic Computer",
        preferredNetwork: networkType,
      });
      beaconNetwork = network;
    }

    // Request permissions
    const permissions = await beaconClient.requestPermissions({
      scopes: [PermissionScope.SIGN, PermissionScope.OPERATION_REQUEST],
    });

    const address = permissions.address;
    console.log("🥝 Beacon wallet connected:", address);

    // Create wallet object
    tezosWallet = {
      _client: beaconClient,
      _rpcUrl: rpcUrl,
      _network: network,
      _publicKey: permissions.publicKey,
      pkh: () => address,
      sign: async (payload) => {
        const result = await beaconClient.requestSignPayload({
          signingType: "raw",
          payload,
        });
        return result.signature;
      },
      sendOperations: async (operations) => {
        const result = await beaconClient.requestOperation({
          operationDetails: operations,
        });
        return result.transactionHash;
      },
    };

    // Update state
    walletState = {
      connected: true,
      address,
      network,
      walletType: "beacon",
      balance: null,
      domain: null,
    };

    // Save session
    saveSession(address, network, "beacon");

    // Fetch balance in background
    fetchBalance(address, network).then((bal) => {
      walletState.balance = bal;
    });

    console.log("✅ Beacon wallet connected:", address);
    return address;
  } catch (err) {
    console.error("🥝 Beacon connection error:", err?.message || err);
    throw new Error(err?.message || "Wallet connection cancelled");
  }
}

/**
 * Disconnect the current wallet
 */
export async function disconnect() {
  if (walletState.walletType === "temple" && templeClient) {
    try {
      await templeClient.disconnect();
    } catch {}
  } else if (beaconClient) {
    try {
      await beaconClient.clearActiveAccount();
    } catch {}
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

/**
 * Get the current wallet state
 */
export function getState() {
  return { ...walletState };
}

/**
 * Get the connected address
 */
export function getAddress() {
  return walletState.address;
}

/**
 * Check if wallet is connected
 */
export function isConnected() {
  return walletState.connected;
}

/**
 * Get the current network
 */
export function getNetwork() {
  return walletState.network;
}

/**
 * Send operations (for contract calls)
 * @param {Array} operations - Array of operation objects
 * @returns {Promise<string>} Operation hash
 */
export async function sendOperations(operations) {
  if (!tezosWallet) {
    throw new Error("No wallet connected");
  }

  if (tezosWallet._needsReconnect) {
    throw new Error("Wallet session expired - please reconnect");
  }

  return await tezosWallet.sendOperations(operations);
}

/**
 * Call a contract method
 * @param {string} contractAddress
 * @param {string} entrypoint
 * @param {object} params - Michelson params
 * @param {number} amount - Amount in XTZ (optional)
 * @returns {Promise<string>} Operation hash
 */
export async function callContract(contractAddress, entrypoint, params, amount = 0) {
  if (!tezosWallet) {
    throw new Error("No wallet connected");
  }

  const operation = {
    kind: "transaction",
    destination: contractAddress,
    amount: String(Math.floor(amount * 1_000_000)), // Convert XTZ to mutez
    parameters: {
      entrypoint,
      value: params,
    },
  };

  return await sendOperations([operation]);
}

// ═══════════════════════════════════════════════════════════════════════════
// Helper Functions
// ═══════════════════════════════════════════════════════════════════════════

function saveSession(address, network, walletType) {
  try {
    localStorage.setItem(
      SESSION_KEY,
      JSON.stringify({ address, network, walletType })
    );
  } catch {}
}

function clearSession() {
  try {
    localStorage.removeItem(SESSION_KEY);
  } catch {}
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

// ═══════════════════════════════════════════════════════════════════════════
// Exports
// ═══════════════════════════════════════════════════════════════════════════

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
