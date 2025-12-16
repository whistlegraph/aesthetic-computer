/**
 * KidLisp Editor - Centralized State Management
 * 
 * All application state in one place with reactive updates via events.
 */

import { sendTheme } from './playback.js';

// ============================================
// STATE OBJECT
// ============================================

export const state = {
  // Editor
  editor: null,           // Monaco editor instance
  code: '',               // Current editor content
  
  // Playback
  isPlaying: false,
  isPaused: false,
  lastCode: '',           // Track last code for auto-reload
  
  // Platform
  currentPlatform: localStorage.getItem('kidlisp-platform') || 'aesthetic-computer',
  
  // Auth (AC Login)
  auth0Client: null,
  user: null,
  token: null,            // acToken
  handle: null,           // @username
  
  // Keeps (Tezos/Blockchain)
  tezos: null,            // TezosToolkit instance
  wallet: null,           // BeaconWallet instance
  walletAddress: null,
  isConnecting: false,
  isMinting: false,
  currentMintCode: null,
  
  // UI State
  theme: localStorage.getItem('kidlisp-theme') || 'auto',
  language: localStorage.getItem('kidlisp-lang') || 'en',
  
  // Layout
  panelStates: {},
  isMobile: window.innerWidth <= 768,
  
  // Iframe communication
  previewIframe: null,
  iframeReady: false,
  
  // URLs
  isDev: window.location.hostname === 'localhost' || window.location.hostname.includes('local.'),
  aestheticUrl: null,     // Set based on isDev
};

// Set aesthetic URL based on environment
state.aestheticUrl = state.isDev ? 'https://localhost:8888' : 'https://aesthetic.computer';

// ============================================
// EVENT SYSTEM
// ============================================

export const events = new EventTarget();

/**
 * Update state and emit change event
 */
export function setState(key, value) {
  const old = state[key];
  if (old === value) return; // No change
  
  state[key] = value;
  
  events.dispatchEvent(new CustomEvent('stateChange', {
    detail: { key, old, value }
  }));
  
  // Also emit specific event for this key
  events.dispatchEvent(new CustomEvent(`state:${key}`, {
    detail: { old, value }
  }));
  
  console.log(`📦 State: ${key} =`, value);
}

/**
 * Subscribe to all state changes
 */
export function onStateChange(callback) {
  events.addEventListener('stateChange', (e) => callback(e.detail));
  return () => events.removeEventListener('stateChange', callback);
}

/**
 * Subscribe to specific state key changes
 */
export function onState(key, callback) {
  const handler = (e) => callback(e.detail.value, e.detail.old);
  events.addEventListener(`state:${key}`, handler);
  return () => events.removeEventListener(`state:${key}`, handler);
}

// ============================================
// ACTIONS (State mutations with side effects)
// ============================================

export const actions = {
  // Playback actions
  play() {
    if (!state.editor) return;
    setState('isPlaying', true);
    setState('isPaused', false);
  },
  
  pause() {
    setState('isPaused', true);
  },
  
  resume() {
    setState('isPaused', false);
  },
  
  stop() {
    setState('isPlaying', false);
    setState('isPaused', false);
    setState('lastCode', '');
  },
  
  // Platform actions
  setPlatform(platform) {
    const old = state.currentPlatform;
    if (old === platform) return;
    
    // Stop current playback when switching platforms
    if (state.isPlaying) {
      actions.stop();
    }
    
    setState('currentPlatform', platform);
    localStorage.setItem('kidlisp-platform', platform);
    
    events.dispatchEvent(new CustomEvent('platformChange', {
      detail: { platform, old }
    }));
  },
  
  // Theme actions
  setTheme(theme) {
    setState('theme', theme);
    localStorage.setItem('kidlisp-theme', theme);
    document.documentElement.setAttribute('data-theme', theme);
    sendTheme(theme); // Sync theme to iframe
  },
  
  // Language actions
  setLanguage(lang) {
    setState('language', lang);
    localStorage.setItem('kidlisp-lang', lang);
    events.dispatchEvent(new CustomEvent('languageChange', { detail: { lang } }));
  },
  
  // Auth actions
  setUser(user, token, handle) {
    setState('user', user);
    setState('token', token);
    setState('handle', handle);
  },
  
  clearUser() {
    setState('user', null);
    setState('token', null);
    setState('handle', null);
  },
  
  // Wallet actions
  setWallet(address) {
    setState('walletAddress', address);
    setState('isConnecting', false);
  },
  
  clearWallet() {
    setState('walletAddress', null);
    setState('wallet', null);
  },
  
  // Code actions
  setCode(code) {
    setState('code', code);
  },
};

// ============================================
// SELECTORS (Computed state)
// ============================================

export const selectors = {
  isLoggedIn: () => state.user !== null,
  hasWallet: () => state.walletAddress !== null,
  canMint: () => state.user !== null && state.walletAddress !== null && state.code.trim().length > 0,
  isLivePreview: () => state.currentPlatform === 'aesthetic-computer' || state.currentPlatform === 'ff1',
};

// ============================================
// PERSISTENCE
// ============================================

// Restore persisted state on load
export function restorePersistedState() {
  const theme = localStorage.getItem('kidlisp-theme');
  if (theme) {
    document.documentElement.setAttribute('data-theme', theme);
    state.theme = theme;
  }
  
  const lang = localStorage.getItem('kidlisp-lang');
  if (lang) {
    state.language = lang;
  }
  
  const platform = localStorage.getItem('kidlisp-platform');
  if (platform) {
    state.currentPlatform = platform;
  }
}

// Auto-restore on module load
restorePersistedState();
