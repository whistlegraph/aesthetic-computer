// keeps-client.mjs - Shared client for Keeps NFT minting
// Used by both kidlisp.com and AC prompt (keep.mjs)
// Provides wallet connection, mint flow, and state management

import TezosWallet from "./tezos-wallet.mjs";
import {
  NETWORKS,
  MINT_STEPS,
  STEP_STATUS,
  KEEPS_STAGING,
  DEFAULT_NETWORK,
  getNetwork,
  getObjktUrl,
  getTzktTokenUrl,
  getTzktContractUrl,
} from "./keeps/constants.mjs";

// Re-export constants for backward compatibility
export { NETWORKS, MINT_STEPS, STEP_STATUS, KEEPS_STAGING };

// ═══════════════════════════════════════════════════════════════════════════════
// KeepsClient class - Main interface for minting
// ═══════════════════════════════════════════════════════════════════════════════

export class KeepsClient {
  constructor(options = {}) {
    this.network = options.network || DEFAULT_NETWORK;
    this.config = getNetwork(this.network);
    this.walletAddress = null;
    this.state = this.createInitialState();
    this.listeners = new Set();
    this.abortController = null;
  }

  // Create fresh state for a mint flow
  createInitialState() {
    return {
      phase: "idle", // idle, connecting, minting, complete, error
      steps: MINT_STEPS.map(s => ({
        ...s,
        status: STEP_STATUS.PENDING,
        detail: null,
        time: null,
        startedAt: null,
      })),
      piece: null,
      sourceCode: null,
      walletAddress: null,
      preparedData: null,
      txHash: null,
      tokenId: null,
      error: null,
      startTime: null,
      alreadyMinted: null, // Existing token info if already minted
    };
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // Event System
  // ═══════════════════════════════════════════════════════════════════════════

  // Subscribe to state changes
  subscribe(callback) {
    this.listeners.add(callback);
    return () => this.listeners.delete(callback);
  }

  // Emit state change to all listeners
  emit() {
    const state = this.getState();
    this.listeners.forEach(cb => cb(state));
  }

  // Get current state (immutable copy)
  getState() {
    return JSON.parse(JSON.stringify(this.state));
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // Step Management
  // ═══════════════════════════════════════════════════════════════════════════

  setStep(id, status, detail = null) {
    const step = this.state.steps.find(s => s.id === id);
    if (!step) return;

    // Track timing
    if (status === STEP_STATUS.ACTIVE && step.status !== STEP_STATUS.ACTIVE) {
      step.startedAt = Date.now();
    }
    
    step.status = status;
    if (detail !== null) step.detail = detail;
    step.time = this.getElapsedTime();

    // Console logging for debugging
    const icons = { done: "✓", error: "✗", active: "►", pending: "○" };
    console.log(`🔒 KEEP [${step.time}] ${icons[status] || "?"} ${step.label}${detail ? " — " + detail : ""}`);

    this.emit();
  }

  getElapsedTime() {
    if (!this.state.startTime) return "0s";
    const elapsed = (Date.now() - this.state.startTime) / 1000;
    return elapsed < 10 ? elapsed.toFixed(1) + "s" : Math.floor(elapsed) + "s";
  }

  getActiveStep() {
    return this.state.steps.find(s => s.status === STEP_STATUS.ACTIVE);
  }

  hasError() {
    return this.state.steps.some(s => s.status === STEP_STATUS.ERROR);
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // Wallet Operations
  // ═══════════════════════════════════════════════════════════════════════════

  async connectWallet() {
    this.state.phase = "connecting";
    this.emit();

    try {
      // Initialize wallet if needed
      await TezosWallet.init(this.network);

      // Check for existing connection
      let address = TezosWallet.getAddress();
      
      if (!address) {
        // Request new connection (opens Beacon popup)
        address = await TezosWallet.connect();
      }

      if (!address) {
        throw new Error("Wallet connection cancelled");
      }

      this.walletAddress = address;
      this.state.walletAddress = address;
      this.state.phase = "idle";
      this.emit();

      console.log("🔒 KEEP: Wallet connected:", address);
      return address;

    } catch (error) {
      this.state.phase = "error";
      this.state.error = error.message;
      this.emit();
      throw error;
    }
  }

  async disconnectWallet() {
    await TezosWallet.disconnect();
    this.walletAddress = null;
    this.state.walletAddress = null;
    this.emit();
    console.log("🔒 KEEP: Wallet disconnected");
  }

  isWalletConnected() {
    return TezosWallet.isConnected();
  }

  getWalletAddress() {
    return TezosWallet.getAddress();
  }

  async getWalletBalance() {
    return await TezosWallet.getBalance();
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // Check if Already Minted
  // ═══════════════════════════════════════════════════════════════════════════

  async checkIfMinted(piece) {
    const keyBytes = this.stringToBytes(piece);
    const url = `https://api.${this.network}.tzkt.io/v1/contracts/${this.config.contract}/bigmaps/content_hashes/keys/${keyBytes}`;

    try {
      const response = await fetch(url);
      if (response.status === 200) {
        const data = await response.json();
        if (data.active) {
          const tokenId = data.value;
          const tokenInfo = await this.fetchTokenInfo(tokenId);
          return tokenInfo;
        }
      }
      return null;
    } catch (e) {
      console.warn("🔒 KEEP: Error checking mint status:", e);
      return null;
    }
  }

  async fetchTokenInfo(tokenId) {
    try {
      // Get token metadata
      const metaUrl = `https://api.${this.network}.tzkt.io/v1/tokens?contract=${this.config.contract}&tokenId=${tokenId}`;
      const metaRes = await fetch(metaUrl);
      
      let tokenData = {};
      if (metaRes.ok) {
        const tokens = await metaRes.json();
        if (tokens.length > 0) tokenData = tokens[0];
      }

      // Get owner from ledger
      const ledgerUrl = `https://api.${this.network}.tzkt.io/v1/contracts/${this.config.contract}/bigmaps/ledger/keys/${tokenId}`;
      const ledgerRes = await fetch(ledgerUrl);
      let ownerAddress = null;
      if (ledgerRes.ok) {
        const ledgerData = await ledgerRes.json();
        if (ledgerData.active) ownerAddress = ledgerData.value;
      }

      const meta = tokenData.metadata || {};
      
      return {
        tokenId,
        owner: ownerAddress,
        name: meta.name,
        description: meta.description,
        artifactUri: meta.artifactUri || meta.artifact_uri,
        thumbnailUri: meta.thumbnailUri || meta.thumbnail_uri || meta.displayUri,
        creators: meta.creators,
        mintedAt: tokenData.firstTime || tokenData.lastTime,
        network: this.network,
        objktUrl: `https://${this.config.objkt}/asset/${this.config.contract}/${tokenId}`,
        tzktUrl: `https://${this.config.explorer}/${this.config.contract}/tokens/${tokenId}`,
      };
    } catch (e) {
      console.error("🔒 KEEP: Error fetching token info:", e);
      return { tokenId, network: this.network };
    }
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // Mint Flow
  // ═══════════════════════════════════════════════════════════════════════════

  async startMint(piece, options = {}) {
    // Reset state
    this.state = this.createInitialState();
    this.state.piece = piece;
    this.state.phase = "minting";
    this.state.startTime = Date.now();
    this.abortController = new AbortController();
    this.emit();

    try {
      // === STEP 1: Connect Wallet ===
      this.setStep("wallet", STEP_STATUS.ACTIVE, "Checking wallet...");
      
      if (!this.walletAddress) {
        this.setStep("wallet", STEP_STATUS.ACTIVE, "Opening wallet...");
        await this.connectWallet();
      }
      
      if (!this.walletAddress) {
        this.setStep("wallet", STEP_STATUS.ERROR, "Connection cancelled");
        return { success: false, error: "Wallet connection cancelled" };
      }
      
      this.setStep("wallet", STEP_STATUS.DONE, this.formatAddress(this.walletAddress));
      this.state.walletAddress = this.walletAddress;

      // === STEP 2: Check if already minted ===
      this.setStep("validate", STEP_STATUS.ACTIVE, `Checking $${piece}...`);
      
      const existing = await this.checkIfMinted(piece);
      if (existing) {
        this.state.alreadyMinted = existing;
        this.setStep("validate", STEP_STATUS.DONE, `Already minted as #${existing.tokenId}`);
        this.state.phase = "complete";
        this.emit();
        return { success: true, alreadyMinted: true, ...existing };
      }

      // === STEPS 2-7: Server Preparation via SSE ===
      const prepResult = await this.runServerPreparation(piece, options);
      
      if (!prepResult.success) {
        return prepResult;
      }

      // === STEP 8: Review (handled by UI) ===
      this.setStep("review", STEP_STATUS.ACTIVE, "Ready for review");
      this.state.preparedData = prepResult.data;
      this.emit();

      // The UI should call confirmMint() when user confirms
      return { success: true, needsConfirmation: true, preparedData: prepResult.data };

    } catch (error) {
      this.state.phase = "error";
      this.state.error = error.message;
      this.emit();
      return { success: false, error: error.message };
    }
  }

  async runServerPreparation(piece, options = {}) {
    this.setStep("validate", STEP_STATUS.ACTIVE, `Validating $${piece}...`);

    try {
      const response = await fetch("/api/keep-mint", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          ...(options.token ? { "Authorization": `Bearer ${options.token}` } : {}),
        },
        body: JSON.stringify({
          piece,
          walletAddress: this.walletAddress,
          network: this.network,
        }),
        signal: this.abortController?.signal,
      });

      if (!response.ok) {
        this.setStep("validate", STEP_STATUS.ERROR, `Server error ${response.status}`);
        return { success: false, error: `Server error ${response.status}` };
      }

      // Parse SSE events
      const text = await response.text();
      const events = text.split('\n\n').filter(e => e.trim());
      
      let preparedData = null;

      for (const event of events) {
        const lines = event.split('\n');
        let eventType = null, eventData = null;

        for (const line of lines) {
          if (line.startsWith('event: ')) eventType = line.slice(7);
          else if (line.startsWith('data: ')) {
            try { eventData = JSON.parse(line.slice(6)); } catch {}
          }
        }

        if (!eventType || !eventData) continue;

        // Map SSE events to steps
        switch (eventType) {
          case "validate":
            this.setStep("validate", 
              eventData.success ? STEP_STATUS.DONE : STEP_STATUS.ERROR,
              eventData.message);
            if (eventData.success) {
              this.state.sourceCode = eventData.source;
            }
            break;

          case "analyze":
            this.setStep("analyze",
              eventData.success ? STEP_STATUS.DONE : STEP_STATUS.ERROR,
              eventData.message);
            break;

          case "thumbnail":
            if (eventData.progress) {
              this.setStep("thumbnail", STEP_STATUS.ACTIVE, eventData.message);
            } else {
              this.setStep("thumbnail",
                eventData.success ? STEP_STATUS.DONE : STEP_STATUS.ERROR,
                eventData.message);
            }
            break;

          case "bundle":
            this.setStep("bundle",
              eventData.success ? STEP_STATUS.DONE : STEP_STATUS.ERROR,
              eventData.message);
            break;

          case "ipfs":
            this.setStep("ipfs",
              eventData.success ? STEP_STATUS.DONE : STEP_STATUS.ERROR,
              eventData.message);
            break;

          case "metadata":
            this.setStep("metadata",
              eventData.success ? STEP_STATUS.DONE : STEP_STATUS.ERROR,
              eventData.message);
            break;

          case "ready":
            preparedData = eventData;
            break;

          case "error":
            const activeStep = this.getActiveStep();
            if (activeStep) {
              this.setStep(activeStep.id, STEP_STATUS.ERROR, eventData.message);
            }
            return { success: false, error: eventData.message };
        }
      }

      if (!preparedData) {
        return { success: false, error: "Server preparation incomplete" };
      }

      return { success: true, data: preparedData };

    } catch (error) {
      if (error.name === "AbortError") {
        return { success: false, error: "Cancelled" };
      }
      throw error;
    }
  }

  async confirmMint() {
    if (!this.state.preparedData) {
      throw new Error("No prepared data - run startMint first");
    }

    this.setStep("review", STEP_STATUS.DONE, "Confirmed");
    this.setStep("sign", STEP_STATUS.ACTIVE, "Waiting for signature...");

    try {
      const result = await TezosWallet.mintKeep(
        this.config.contract,
        this.state.preparedData.mintParams
      );

      this.state.txHash = result.hash;
      this.setStep("sign", STEP_STATUS.DONE, result.hash.slice(0, 12) + "...");

      // === STEP 10: Complete ===
      this.setStep("complete", STEP_STATUS.DONE, "NFT minted!");
      this.state.phase = "complete";
      this.emit();

      // Get token ID from blockchain
      // (In production, would query the contract for the new token)
      
      return { success: true, txHash: result.hash };

    } catch (error) {
      this.setStep("sign", STEP_STATUS.ERROR, error.message);
      this.state.phase = "error";
      this.emit();
      return { success: false, error: error.message };
    }
  }

  cancel() {
    if (this.abortController) {
      this.abortController.abort();
    }
    this.state.phase = "idle";
    this.emit();
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // Utilities
  // ═══════════════════════════════════════════════════════════════════════════

  stringToBytes(str) {
    return Array.from(new TextEncoder().encode(str))
      .map(b => b.toString(16).padStart(2, '0'))
      .join('');
  }

  formatAddress(address) {
    if (!address) return "";
    return address.slice(0, 6) + "..." + address.slice(-4);
  }

  // Get URLs for external services
  getExplorerUrl(txHash) {
    return `https://${this.config.explorer}/${txHash}`;
  }

  getObjktUrl(tokenId) {
    return `https://${this.config.objkt}/asset/${this.config.contract}/${tokenId}`;
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Singleton instance for simple usage
// ═══════════════════════════════════════════════════════════════════════════════

let defaultClient = null;

export function getKeepsClient(network = "mainnet") {
  if (!defaultClient || defaultClient.network !== network) {
    defaultClient = new KeepsClient({ network });
  }
  return defaultClient;
}

// Default export
export default {
  KeepsClient,
  getKeepsClient,
  NETWORKS,
  MINT_STEPS,
  STEP_STATUS,
};
