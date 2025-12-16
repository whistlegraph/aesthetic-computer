/**
 * KidLisp Editor - Playback Controller
 * 
 * Handles play/pause/stop logic and iframe communication.
 */

import { state, setState, actions, events } from './state.js';

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
  
  // Only create a NEW code if we don't already have one stored
  const existingCode = localStorage.getItem('kidlisp-current-code');
  const shouldCreateCode = !existingCode;
  
  console.log('▶️ Starting playback', existingCode ? `with existing code: ${existingCode}` : 'will create new code');
  
  sendCode(code, shouldCreateCode);
  actions.play();
  
  updatePlaybackUI(true, false);
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
  document.title = 'KidLisp - transmedia pattern programming for everybody';
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
