/**
 * KidLisp Editor - Unified Event Handling
 * 
 * Replaces scattered addEventListener calls with a single delegated system.
 */

import { state, actions, events as stateEvents } from './state.js';

// ============================================
// DROPDOWN MANAGEMENT
// ============================================

const openDropdowns = new Set();

export function closeAllDropdowns() {
  // Platform selector
  const platformSelector = document.getElementById('platform-selector');
  if (platformSelector) platformSelector.classList.remove('open');
  
  // Language dropdown
  const langDropdown = document.getElementById('language-dropdown');
  if (langDropdown) langDropdown.style.display = 'none';
  
  // User menu
  const headerUserMenu = document.getElementById('header-user-menu');
  if (headerUserMenu) headerUserMenu.classList.remove('open');
  
  openDropdowns.clear();
}

export function toggleDropdown(id) {
  const wasOpen = openDropdowns.has(id);
  closeAllDropdowns();
  
  if (!wasOpen) {
    openDropdowns.add(id);
    return true;
  }
  return false;
}

// ============================================
// ACTION HANDLERS
// ============================================

const actionHandlers = {
  // Dropdowns
  'toggle-platform': (e, el) => {
    e.stopPropagation();
    const selector = document.getElementById('platform-selector');
    if (toggleDropdown('platform')) {
      selector?.classList.add('open');
    }
  },
  
  'toggle-language': (e, el) => {
    e.stopPropagation();
    const dropdown = document.getElementById('language-dropdown');
    if (toggleDropdown('language')) {
      dropdown.style.display = 'block';
    }
  },
  
  'toggle-user-menu': (e, el) => {
    e.stopPropagation();
    const menu = document.getElementById('header-user-menu');
    if (toggleDropdown('user-menu')) {
      menu?.classList.add('open');
    }
  },
  
  // Platform selection
  'select-platform': (e, el) => {
    e.stopPropagation();
    const platform = el.dataset.platform;
    const isComingSoon = el.classList.contains('coming-soon');
    const isExperimental = el.classList.contains('experimental');
    
    if (isComingSoon) {
      showToast(`${el.querySelector('.platform-opt-name')?.textContent} support coming soon!`, 'info');
      return;
    }
    
    if (isExperimental) {
      showToast(`${el.querySelector('.platform-opt-name')?.textContent} is experimental - features may be limited`, 'warning');
    }
    
    actions.setPlatform(platform);
    closeAllDropdowns();
    
    // Update UI
    updatePlatformUI(platform, el);
  },
  
  // Language selection
  'select-language': (e, el) => {
    e.stopPropagation();
    const lang = el.dataset.lang;
    if (lang) {
      actions.setLanguage(lang);
      closeAllDropdowns();
    }
  },
  
  // Theme toggle
  'toggle-theme': (e, el) => {
    const current = state.theme;
    const newTheme = current === 'dark' ? 'light' : 'dark';
    actions.setTheme(newTheme);
    el.textContent = newTheme === 'dark' ? '●' : '○';
  },
  
  // Auth
  'login': (e, el) => {
    window.acLogin?.();
  },
  
  'logout': (e, el) => {
    window.acLogout?.();
    closeAllDropdowns();
  },
};

// ============================================
// UI UPDATE HELPERS
// ============================================

function updatePlatformUI(platform, optionEl) {
  const platformIcon = document.getElementById('platform-icon');
  const previewTitle = document.getElementById('preview-title');
  
  if (optionEl) {
    const icon = optionEl.querySelector('.platform-opt-icon')?.textContent || '🟣';
    const name = optionEl.querySelector('.platform-opt-name')?.textContent || 'Aesthetic.Computer';
    
    if (platformIcon) platformIcon.textContent = icon;
    if (previewTitle) previewTitle.innerHTML = name.replace('.', '<span class="bounce-dot">.</span>');
    
    // Update active state
    document.querySelectorAll('.platform-option').forEach(o => o.classList.remove('active'));
    optionEl.classList.add('active');
  }
}

// ============================================
// TOAST NOTIFICATIONS
// ============================================

export function showToast(message, type = 'info') {
  const existing = document.querySelector('.kidlisp-toast');
  if (existing) existing.remove();
  
  const toast = document.createElement('div');
  toast.className = `kidlisp-toast toast-${type}`;
  toast.textContent = message;
  
  document.body.appendChild(toast);
  
  requestAnimationFrame(() => {
    toast.style.opacity = '1';
  });
  
  setTimeout(() => {
    toast.style.opacity = '0';
    setTimeout(() => toast.remove(), 300);
  }, 3000);
}

// Make showToast globally available
window.showToast = showToast;

// ============================================
// MAIN EVENT DELEGATION
// ============================================

export function initEventDelegation() {
  // Single click handler for document
  document.addEventListener('click', (e) => {
    // Check for data-action handlers
    const actionEl = e.target.closest('[data-action]');
    if (actionEl) {
      const action = actionEl.dataset.action;
      const handler = actionHandlers[action];
      if (handler) {
        handler(e, actionEl);
        return;
      }
    }
    
    // Close dropdowns when clicking outside
    if (!e.target.closest('.platform-selector, .language-tab, .header-user-menu')) {
      closeAllDropdowns();
    }
  });
  
  // Keyboard shortcuts
  document.addEventListener('keydown', (e) => {
    // Escape closes dropdowns
    if (e.key === 'Escape') {
      closeAllDropdowns();
    }
  });
  
  console.log('🎯 Event delegation initialized');
}

// ============================================
// COMPONENT INITIALIZERS
// ============================================

export function initPlatformSelector() {
  const selector = document.getElementById('platform-selector');
  if (!selector) return;
  
  // Add data-action to make it work with delegation
  selector.setAttribute('data-action', 'toggle-platform');
  
  // Add data-action to options
  document.querySelectorAll('.platform-option').forEach(opt => {
    opt.setAttribute('data-action', 'select-platform');
  });
  
  // Restore saved platform
  const saved = state.currentPlatform;
  const savedOption = document.querySelector(`.platform-option[data-platform="${saved}"]`);
  if (savedOption) {
    updatePlatformUI(saved, savedOption);
  }
  
  console.log('🎯 Platform selector initialized');
}

export function initLanguageDropdown() {
  const langTab = document.getElementById('language-tab');
  if (!langTab) return;
  
  langTab.setAttribute('data-action', 'toggle-language');
  
  document.querySelectorAll('.lang-option').forEach(opt => {
    opt.setAttribute('data-action', 'select-language');
  });
  
  console.log('🌐 Language dropdown initialized');
}

export function initThemeToggle() {
  const toggle = document.getElementById('theme-toggle');
  if (!toggle) return;
  
  toggle.setAttribute('data-action', 'toggle-theme');
  
  // Set initial state
  const theme = state.theme;
  const isDark = theme === 'dark' || 
    (theme === 'auto' && window.matchMedia('(prefers-color-scheme: dark)').matches);
  toggle.textContent = isDark ? '●' : '○';
  
  console.log('🌓 Theme toggle initialized');
}

export function initUserMenu() {
  const loginBtn = document.getElementById('header-login-btn');
  const logoutBtn = document.getElementById('header-logout-btn');
  const userMenu = document.getElementById('header-user-menu');
  
  if (loginBtn) loginBtn.setAttribute('data-action', 'login');
  if (logoutBtn) logoutBtn.setAttribute('data-action', 'logout');
  if (userMenu) userMenu.setAttribute('data-action', 'toggle-user-menu');
  
  console.log('👤 User menu initialized');
}

// ============================================
// INIT ALL
// ============================================

export function initAllComponents() {
  initEventDelegation();
  initPlatformSelector();
  initLanguageDropdown();
  initThemeToggle();
  initUserMenu();
}
