// Keeps Wallet - Content Script
// Relays Beacon SDK postMessage protocol AND existing KEEPS_* custom protocol

const EXTENSION_ID = 'keeps-wallet';
const WALLET_NAME = 'Keeps Wallet';
// Data URI for a small purple diamond icon (inline to avoid network requests)
const ICON_URL =
  'data:image/svg+xml;base64,' +
  btoa(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64">' +
      '<rect width="64" height="64" rx="12" fill="#cd5c9b"/>' +
      '<text x="32" y="44" text-anchor="middle" font-size="36" fill="white">K</text>' +
      '</svg>',
  );

// Inject the inpage script that creates window.keeps
const script = document.createElement('script');
script.src = chrome.runtime.getURL('inpage.js');
script.onload = () => script.remove();
(document.head || document.documentElement).appendChild(script);

// --- Beacon Protocol Relay ---

window.addEventListener('message', async (event) => {
  // Only accept messages from same window
  if (event.source !== window) return;
  const msg = event.data;
  if (!msg) return;

  // --- Beacon: toExtension messages ---
  if (msg.target === 'toExtension') {
    // Ping → respond with pong immediately (discovery)
    if (msg.payload === 'ping') {
      window.postMessage(
        {
          target: 'toPage',
          payload: 'pong',
          sender: {
            id: EXTENSION_ID,
            name: WALLET_NAME,
            iconURL: ICON_URL,
          },
        },
        '*',
      );
      return;
    }

    // Forward everything else (pairing requests, encrypted messages) to background
    try {
      const response = await chrome.runtime.sendMessage({
        type: 'BEACON_MESSAGE',
        payload: msg,
      });
      if (response) {
        // Background may return a direct response to post to page
        if (response.postToPage) {
          window.postMessage(response.postToPage, '*');
        }
      }
    } catch (error) {
      console.error('Keeps Wallet: Beacon relay error', error);
    }
    return;
  }

  // --- Existing KEEPS_* custom protocol ---
  if (msg.type && msg.type.startsWith('KEEPS_')) {
    try {
      const response = await chrome.runtime.sendMessage(msg);
      window.postMessage(
        {
          type: msg.type + '_RESPONSE',
          requestId: msg.requestId,
          payload: response,
        },
        '*',
      );
    } catch (error) {
      window.postMessage(
        {
          type: msg.type + '_RESPONSE',
          requestId: msg.requestId,
          payload: { error: error.message },
        },
        '*',
      );
    }
  }
});

// --- Listen for messages FROM background (extension → page) ---
chrome.runtime.onMessage.addListener((message) => {
  if (message.type === 'BEACON_RESPONSE') {
    window.postMessage(message.data, '*');
  }
  if (message.type === 'KEEPS_LOCKED') {
    window.postMessage({ type: 'KEEPS_LOCKED' }, '*');
  }
});

console.log('Keeps Wallet content script loaded');
