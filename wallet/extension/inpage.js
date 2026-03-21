// Keeps Wallet - Inpage Script
// Creates window.keeps API for dApps to interact with

(function() {
  'use strict';
  
  // Request ID counter for matching responses
  let requestId = 0;
  const pendingRequests = new Map();
  
  // Listen for responses from content script
  window.addEventListener('message', (event) => {
    if (event.source !== window) return;
    
    const { type, requestId: resId, payload } = event.data;
    if (!type?.endsWith('_RESPONSE')) return;
    
    const pending = pendingRequests.get(resId);
    if (pending) {
      pendingRequests.delete(resId);
      if (payload?.error) {
        pending.reject(new Error(payload.error));
      } else {
        pending.resolve(payload);
      }
    }
  });
  
  // Handle lock events
  window.addEventListener('message', (event) => {
    if (event.data?.type === 'KEEPS_LOCKED') {
      window.dispatchEvent(new CustomEvent('keeps:locked'));
    }
  });
  
  // Send message to extension and wait for response
  function sendMessage(type, payload = {}) {
    return new Promise((resolve, reject) => {
      const id = ++requestId;
      pendingRequests.set(id, { resolve, reject });
      
      // Timeout after 30 seconds
      setTimeout(() => {
        if (pendingRequests.has(id)) {
          pendingRequests.delete(id);
          reject(new Error('Request timeout'));
        }
      }, 30000);
      
      window.postMessage({ type, requestId: id, payload }, '*');
    });
  }
  
  // Public API
  window.keeps = {
    // Check if extension is installed
    async isInstalled() {
      try {
        const res = await sendMessage('KEEPS_PING');
        return res.success === true;
      } catch {
        return false;
      }
    },
    
    // Get wallet state
    async getState() {
      return sendMessage('KEEPS_GET_STATE');
    },
    
    // Get connected address (null if locked/no wallet)
    async getAddress() {
      const res = await sendMessage('KEEPS_GET_ADDRESS');
      return res.address;
    },
    
    // Request connection (will prompt unlock if needed)
    async connect() {
      const state = await this.getState();
      if (!state.exists) {
        throw new Error('No wallet found. Please create one in the extension.');
      }
      if (!state.unlocked) {
        throw new Error('Wallet is locked. Please unlock in the extension.');
      }
      return state.address;
    },
    
    // Get balance
    async getBalance(network) {
      return sendMessage('KEEPS_GET_BALANCE', { network });
    },
    
    // Get keeps (NFTs)
    async getKeeps(network) {
      return sendMessage('KEEPS_GET_KEEPS', { network });
    },
    
    // Request signing an operation
    async sign(operation) {
      return sendMessage('KEEPS_SIGN_OPERATION', { operation });
    },
    
    // Events
    onLocked(callback) {
      window.addEventListener('keeps:locked', callback);
      return () => window.removeEventListener('keeps:locked', callback);
    },
  };
  
  // Announce availability
  window.dispatchEvent(new CustomEvent('keeps:ready'));
  console.log('🔐 Keeps Wallet API available at window.keeps');
})();
