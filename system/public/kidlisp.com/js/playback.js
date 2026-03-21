/**
 * KidLisp Editor - Playback Controller
 * 
 * Handles play/pause/stop logic and iframe communication.
 */

import { state, setState, actions, events } from './state.js';
import * as ff1 from './ff1.js';

// ============================================
// IFRAME COMMUNICATION
// ============================================

export function sendToIframe(type, data = {}) {
  if (!state.previewIframe?.contentWindow) {
    console.warn('⚠️ No iframe contentWindow available');
    return false;
  }
  
  state.previewIframe.contentWindow.postMessage({
    type,
    ...data
  }, state.aestheticUrl);
  
  return true;
}

export function sendCode(code, createCode = false) {
  return sendToIframe('kidlisp-reload', {
    code,
    createCode,
    authToken: state.token || null
  });
}

export function sendPause() {
  return sendToIframe('kidlisp-pause');
}

export function sendResume() {
  return sendToIframe('kidlisp-resume');
}

export function sendStop() {
  return sendToIframe('kidlisp-stop');
}

export function sendTheme(theme) {
  return sendToIframe('kidlisp-theme', { theme });
}

// ============================================
// PLAYBACK ACTIONS
// ============================================

export function play() {
  if (!state.editor) return;
  
  const code = state.editor.getValue().trim();
  if (!code) {
    console.warn('⚠️ No code to play');
    return;
  }
  
  // Handle FF1 platform separately
  if (ff1.isFF1Platform()) {
    playOnFF1(code);
    return;
  }
  
  // Only create a NEW code if we don't already have one stored
  const existingCode = localStorage.getItem('kidlisp-current-code');
  const shouldCreateCode = !existingCode;
  
  console.log('▶️ Starting playback', existingCode ? `with existing code: ${existingCode}` : 'will create new code');
  
  sendCode(code, shouldCreateCode);
  actions.play();
  
  updatePlaybackUI(true, false);
}

/**
 * Play code on FF1 Art Computer
 */
async function playOnFF1(code) {
  // Check FF1 configuration
  if (!ff1.isConfigured()) {
    showToast('FF1 not configured - open settings to add your device ID', 'warning');
    openFF1Settings();
    return;
  }
  
  console.log('🖼️ Playing on FF1 Art Computer');
  
  // First, send code to iframe to create/get code ID
  const existingCode = localStorage.getItem('kidlisp-current-code');
  const shouldCreateCode = !existingCode;
  
  // Send to iframe (this triggers code creation on Aesthetic.Computer)
  sendCode(code, shouldCreateCode);
  
  // Wait for code ID from iframe response
  // The iframe will post back with the code ID
  await waitForCodeId();
  
  const codeId = localStorage.getItem('kidlisp-current-code');
  if (!codeId) {
    showToast('Failed to create code - please try again', 'error');
    return;
  }
  
  // Send to FF1 device
  try {
    await ff1.sendKidLispCode(codeId);
    showToast('Sent to FF1! 🖼️', 'success');
    actions.play();
    updatePlaybackUI(true, false);
  } catch (e) {
    console.error('❌ FF1 error:', e);
    showToast(`FF1 error: ${e.message}`, 'error');
  }
}

/**
 * Wait for the iframe to respond with a code ID
 */
function waitForCodeId(timeout = 5000) {
  return new Promise((resolve, reject) => {
    const startCode = localStorage.getItem('kidlisp-current-code');
    
    // Check if we already have a code ID
    if (startCode) {
      resolve(startCode);
      return;
    }
    
    let elapsed = 0;
    const interval = setInterval(() => {
      const codeId = localStorage.getItem('kidlisp-current-code');
      if (codeId && codeId !== startCode) {
        clearInterval(interval);
        resolve(codeId);
      }
      elapsed += 100;
      if (elapsed >= timeout) {
        clearInterval(interval);
        reject(new Error('Timeout waiting for code ID'));
      }
    }, 100);
  });
}

/**
 * Open FF1 settings panel
 */
function openFF1Settings() {
  // Emit event for settings panel to handle
  events.dispatchEvent(new CustomEvent('openFF1Settings'));
}

/**
 * Show toast notification (helper)
 */
function showToast(message, type = 'info') {
  // Use the global toast system if available
  if (window.showToast) {
    window.showToast(message, type);
  } else {
    console.log(`[${type}] ${message}`);
  }
}

export function pause() {
  console.log('⏸️ Pausing playback');
  sendPause();
  actions.pause();
  updatePlaybackUI(true, true);
}

export function resume() {
  console.log('▶️ Resuming playback');
  sendResume();
  actions.resume();
  updatePlaybackUI(true, false);
}

export function stop() {
  console.log('⏹️ Stopping playback');
  sendStop();
  actions.stop();
  
  // Clear localStorage code identifier
  localStorage.removeItem('kidlisp-current-code');
  
  updatePlaybackUI(false, false);
  
  // Reset title
  document.title = 'KidLisp.com';
}

export function togglePlayPause() {
  if (!state.isPlaying) {
    play();
  } else if (state.isPaused) {
    resume();
  } else {
    pause();
  }
}

// ============================================
// UI UPDATES
// ============================================

function updatePlaybackUI(isPlaying, isPaused) {
  const sendButton = document.getElementById('send-button');
  const stopButton = document.getElementById('stop-button');
  const editorContainer = document.getElementById('kidlisp-editor');
  
  if (!sendButton || !stopButton) return;
  
  if (isPlaying) {
    sendButton.style.right = '64px';
    stopButton.style.display = 'flex';
    editorContainer?.classList.add('live-running');
    
    if (isPaused) {
      sendButton.classList.remove('playing');
    } else {
      sendButton.classList.add('playing');
    }
  } else {
    sendButton.classList.remove('playing');
    sendButton.style.right = '8px';
    stopButton.style.display = 'none';
    editorContainer?.classList.remove('live-running');
  }
}

// ============================================
// BUTTON INITIALIZATION
// ============================================

export function initPlaybackControls() {
  const sendButton = document.getElementById('send-button');
  const stopButton = document.getElementById('stop-button');
  
  if (sendButton) {
    sendButton.addEventListener('click', () => {
      if (sendButton.disabled) {
        console.log('⏸️ Play button clicked but disabled');
        return;
      }
      togglePlayPause();
    });
  }
  
  if (stopButton) {
    stopButton.addEventListener('click', (e) => {
      if (stopButton.disabled) {
        console.log('⏸️ Stop button clicked but disabled');
        return;
      }
      e.stopPropagation();
      e.preventDefault();
      stop();
    });
  }
  
  // Listen for platform changes to stop playback
  events.addEventListener('platformChange', () => {
    if (state.isPlaying) {
      stop();
    }
  });
  
  console.log('🎮 Playback controls initialized');
}

// ============================================
// ENABLE/DISABLE CONTROLS
// ============================================

export function enableControls() {
  const sendButton = document.getElementById('send-button');
  const stopButton = document.getElementById('stop-button');
  
  if (sendButton) {
    sendButton.disabled = false;
    sendButton.style.opacity = '1';
    sendButton.style.cursor = 'pointer';
  }
  
  if (stopButton) {
    stopButton.disabled = false;
    stopButton.style.opacity = '1';
    stopButton.style.cursor = 'pointer';
  }
  
  console.log('🔓 Controls enabled');
}

export function disableControls() {
  const sendButton = document.getElementById('send-button');
  const stopButton = document.getElementById('stop-button');
  
  if (sendButton) {
    sendButton.disabled = true;
    sendButton.style.opacity = '0.4';
    sendButton.style.cursor = 'not-allowed';
  }
  
  if (stopButton) {
    stopButton.disabled = true;
    stopButton.style.opacity = '0.4';
    stopButton.style.cursor = 'not-allowed';
  }
  
  console.log('🔒 Controls disabled');
}

// Make stop globally available for backward compatibility
window.stopPlayback = stop;
