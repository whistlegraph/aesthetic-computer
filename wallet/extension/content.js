// Keeps Wallet - Content Script
// Injects the wallet API into web pages

// Inject the inpage script that creates window.keeps
const script = document.createElement('script');
script.src = chrome.runtime.getURL('inpage.js');
script.onload = () => script.remove();
(document.head || document.documentElement).appendChild(script);

// Relay messages between page and background
window.addEventListener('message', async (event) => {
  // Only accept messages from same window
  if (event.source !== window) return;
  
  // Only handle keeps wallet messages
  if (!event.data?.type?.startsWith('KEEPS_')) return;
  
  // Forward to background and relay response back
  try {
    const response = await chrome.runtime.sendMessage(event.data);
    window.postMessage({
      type: event.data.type + '_RESPONSE',
      requestId: event.data.requestId,
      payload: response,
    }, '*');
  } catch (error) {
    window.postMessage({
      type: event.data.type + '_RESPONSE',
      requestId: event.data.requestId,
      payload: { error: error.message },
    }, '*');
  }
});

// Listen for lock events from background
chrome.runtime.onMessage.addListener((message) => {
  if (message.type === 'KEEPS_LOCKED') {
    window.postMessage({ type: 'KEEPS_LOCKED' }, '*');
  }
});

console.log('🔐 Keeps Wallet content script loaded');
