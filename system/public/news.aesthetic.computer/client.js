// News client, 2026.01.15

const config = {
  domain: "aesthetic.us.auth0.com",
  clientId: "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt",
  audience: "https://aesthetic.us.auth0.com/api/v2/",
};

// ===== Internationalization (i18n) =====
const translations = {
  en: {
    'site-title': 'Aesthetic News',
    'log-in': 'Log In',
    'im-new': "I'm New",
    'list': 'List',
    'give': 'Give',
    'commits': 'Commits',
    'report-the-news': 'Report the News',
    'no-posts': 'No posts yet.',
    'respond': 'Respond',
    'comment-guidelines': 'Say something interesting or ask a question. Be nice.',
    'comment-placeholder': 'Respond...',
    'login-prompt-1': 'You need to',
    'login-prompt-2': 'to post.',
    'handle-note': "You'll also need a @handle — get one at",
    'headline': 'Headline',
    'headline-placeholder': "What's the story?",
    'pick-hint': 'pick one or both',
    'source-url': 'Source URL',
    'story': 'Story',
    'story-placeholder': 'Share context, commentary, or tell your own story...',
    'post': 'Post',
    'tagline': 'Be curious. Be kind. Keep it weird.',
    'points': 'points',
    'by': 'by',
    'comments': 'comments',
  },
  da: {
    'site-title': 'Æstetiske Nyheder',
    'log-in': 'Log ind',
    'im-new': 'Jeg er ny',
    'list': 'Liste',
    'give': 'Giv',
    'commits': 'Commits',
    'report-the-news': 'Rapportér Nyheder',
    'no-posts': 'Ingen indlæg endnu.',
    'respond': 'Svar',
    'comment-guidelines': 'Sig noget interessant eller stil et spørgsmål. Vær venlig.',
    'comment-placeholder': 'Svar...',
    'login-prompt-1': 'Du skal',
    'login-prompt-2': 'for at poste.',
    'handle-note': 'Du skal også have et @handle — få et på',
    'headline': 'Overskrift',
    'headline-placeholder': 'Hvad er historien?',
    'pick-hint': 'vælg en eller begge',
    'source-url': 'Kilde URL',
    'story': 'Historie',
    'story-placeholder': 'Del kontekst, kommentarer, eller fortæl din egen historie...',
    'post': 'Post',
    'tagline': 'Vær nysgerrig. Vær venlig. Hold det mærkeligt.',
    'points': 'point',
    'by': 'af',
    'comments': 'kommentarer',
  }
};

let currentLang = localStorage.getItem('news-lang') || 'en';

function setLanguage(lang) {
  if (!translations[lang]) return;
  currentLang = lang;
  localStorage.setItem('news-lang', lang);
  applyTranslations();
  updateLangSelectors();
}

function applyTranslations() {
  const t = translations[currentLang] || translations.en;
  
  // Translate elements with data-i18n attribute
  document.querySelectorAll('[data-i18n]').forEach(el => {
    const key = el.getAttribute('data-i18n');
    if (t[key]) el.textContent = t[key];
  });
  
  // Translate placeholders
  document.querySelectorAll('[data-i18n-placeholder]').forEach(el => {
    const key = el.getAttribute('data-i18n-placeholder');
    if (t[key]) el.placeholder = t[key];
  });
  
  // Update HTML lang attribute
  document.documentElement.lang = currentLang;
}

function updateLangSelectors() {
  const selectors = document.querySelectorAll('.news-lang-selector');
  const labels = { en: 'EN', da: 'DA' };
  const flagClasses = { en: 'fi-gb', da: 'fi-dk' };
  
  selectors.forEach(selector => {
    const langText = selector.querySelector('.lang-text');
    const langFlag = selector.querySelector('[data-lang-flag]');
    
    if (langText) langText.textContent = labels[currentLang] || 'EN';
    if (langFlag) {
      // Update flag class
      langFlag.className = `fi ${flagClasses[currentLang] || 'fi-gb'} lang-flag`;
    }
    
    // Update active state in dropdown
    selector.querySelectorAll('.lang-option').forEach(opt => {
      opt.classList.toggle('active', opt.dataset.lang === currentLang);
    });
  });
}

function initLangSelectors() {
  const selectors = document.querySelectorAll('.news-lang-selector');
  
  selectors.forEach(selector => {
    const trigger = selector.querySelector('.lang-trigger');
    const dropdown = selector.querySelector('.lang-dropdown');
    
    // Toggle dropdown on trigger click
    if (trigger) {
      trigger.addEventListener('click', (e) => {
        e.preventDefault();
        e.stopPropagation();
        // Close other selectors first
        selectors.forEach(s => {
          if (s !== selector) s.classList.remove('open');
        });
        selector.classList.toggle('open');
      });
    }
    
    // Handle option clicks
    if (dropdown) {
      dropdown.addEventListener('click', (e) => {
        const opt = e.target.closest('.lang-option');
        if (opt) {
          e.preventDefault();
          e.stopPropagation();
          const lang = opt.dataset.lang;
          if (lang) {
            setLanguage(lang);
            // Close all selectors
            selectors.forEach(s => s.classList.remove('open'));
          }
        }
      });
    }
  });
  
  // Close on outside click
  document.addEventListener('click', (e) => {
    if (!e.target.closest('.news-lang-selector')) {
      selectors.forEach(s => s.classList.remove('open'));
    }
  });
  
  // Close on Escape
  document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') {
      selectors.forEach(s => s.classList.remove('open'));
    }
  });
  
  // Apply initial language
  applyTranslations();
  updateLangSelectors();
}

// ===== End i18n =====

let auth0Client = null;
let acUser = null;
let acToken = null;
let acHandle = null;
let auth0Initialized = false;
let sessionListenerAttached = false;
let spaInitialized = false;

// DOM elements - initialized after DOM ready
let loginBtn, signupBtn, userMenu, userHandle, logoutBtn;
let userMenuModalOpen = false;

function initDOMRefs() {
  loginBtn = document.getElementById("news-login-btn");
  signupBtn = document.getElementById("news-signup-btn");
  userMenu = document.getElementById("news-user-menu");
  userHandle = document.getElementById("news-user-handle");
  logoutBtn = document.getElementById("news-logout-btn");
  console.log("[news] DOM refs:", { loginBtn, signupBtn, userMenu, logoutBtn });
}

async function hydrateHandleFromEmail(email) {
  if (!email) return;
  try {
    // Use full URL to main API since we're on news.aesthetic.computer subdomain
    const res = await fetch(`https://aesthetic.computer/user?from=${encodeURIComponent(email)}&withHandle=true`);
    const data = await res.json();
    if (data.handle) acHandle = data.handle;
  } catch (error) {
    console.warn("Handle lookup failed", error);
  }
}

function loadSessionCache() {
  try {
    const raw = sessionStorage.getItem('news:vscode-session');
    if (!raw) return null;
    return JSON.parse(raw);
  } catch (error) {
    return null;
  }
}

function saveSessionCache(session) {
  try {
    if (!session) {
      sessionStorage.removeItem('news:vscode-session');
      return;
    }
    sessionStorage.setItem('news:vscode-session', JSON.stringify(session));
  } catch (error) {
    // ignore
  }
}

async function applySession(session) {
  console.log("[news] applySession called:", session);
  if (!session || typeof session !== "object") {
    if (isInVSCode()) {
      const cached = loadSessionCache();
      if (cached && typeof cached === "object") {
        session = cached;
      } else {
        saveSessionCache(null);
      }
    }
  }
  if (!session || typeof session !== "object") {
    // Session cleared - user logged out
    acUser = null;
    acToken = null;
    acHandle = null;
    updateAuthUI();
    return;
  }
  saveSessionCache(session);
  if (session.accessToken) acToken = session.accessToken;

  const label = session.account?.label;
  if (label) {
    if (label.startsWith("@")) {
      acHandle = label.slice(1);
      acUser = { email: label }; // Set user object so we know they're logged in
    } else {
      acUser = { email: label };
      if (label.includes("@")) {
        await hydrateHandleFromEmail(label);
      }
    }
  }

  updateAuthUI();
}

function decodeSessionParam() {
  const params = new URLSearchParams(window.location.search);
  const encoded = params.get("session-aesthetic");
  if (!encoded || encoded === "null") return null;
  try {
    return JSON.parse(atob(decodeURIComponent(encoded)));
  } catch (error) {
    console.warn("Failed to decode session param", error);
    return null;
  }
}

async function initAuth0() {
  if (!window.auth0) return;
  if (auth0Initialized) return;
  auth0Initialized = true;
  
  // Determine correct redirect URI (subdomain vs local path)
  const isNewsDomain = window.location.hostname === 'news.aesthetic.computer';
  const redirectUri = isNewsDomain 
    ? window.location.origin 
    : window.location.origin + '/news.aesthetic.computer';
  
  auth0Client = await auth0.createAuth0Client({
    domain: config.domain,
    clientId: config.clientId,
    authorizationParams: {
      redirect_uri: redirectUri,
      audience: config.audience,
      scope: "openid profile email",
    },
    cacheLocation: 'localstorage',
    useRefreshTokens: true,
    useRefreshTokensFallback: true,
  });

  if (window.location.search.includes("code=") && window.location.search.includes("state=")) {
    try {
      await auth0Client.handleRedirectCallback();
      window.history.replaceState({}, document.title, window.location.pathname);
    } catch (error) {
      console.warn("Auth0 callback error", error);
    }
  }

  let isAuthenticated = await auth0Client.isAuthenticated();
  if (!isAuthenticated) {
    try {
      await auth0Client.getTokenSilently();
      isAuthenticated = await auth0Client.isAuthenticated();
    } catch {
      // ignore
    }
  }

  if (isAuthenticated) {
    acUser = await auth0Client.getUser();
    acToken = await auth0Client.getTokenSilently();
    await hydrateHandleFromEmail(acUser.email);
  }

  updateAuthUI();
}

// Admin handles who can delete/censor content
const ADMIN_HANDLES = ['jeffrey'];

function isAdmin() {
  return acHandle && ADMIN_HANDLES.includes(acHandle);
}

function updateAdminUI() {
  // Show delete buttons for admins and post/comment owners
  const deleteButtons = document.querySelectorAll('.news-admin-delete');
  
  deleteButtons.forEach(btn => {
    // Use data-handle attribute for reliable owner detection
    const itemHandle = btn.dataset.handle || '';
    const isOwner = acHandle && itemHandle === acHandle;
    const canDelete = isAdmin() || isOwner;
    
    btn.style.display = canDelete ? 'inline-flex' : 'none';
  });
}

function updateAuthUI() {
  const reportSection = document.querySelector('.news-report');
  
  if (acUser) {
    if (loginBtn) loginBtn.style.display = "none";
    if (signupBtn) signupBtn.style.display = "none";
    if (userMenu) userMenu.style.display = "flex";
    if (userHandle) userHandle.textContent = acHandle ? `@${acHandle}` : acUser.email;
    if (reportSection) reportSection.classList.add('logged-in');
  } else {
    if (loginBtn) loginBtn.style.display = "inline-flex";
    if (signupBtn) signupBtn.style.display = "inline-flex";
    if (userMenu) userMenu.style.display = "none";
    if (reportSection) reportSection.classList.remove('logged-in');
  }
  
  // Update admin UI (delete buttons)
  updateAdminUI();
  
  // Update inline login button
  const promptLogin = document.getElementById('news-prompt-login');
  if (promptLogin) {
    promptLogin.addEventListener('click', (e) => {
      e.preventDefault();
      acLogin();
    });
  }
}

async function acLogin() {
  console.log("[news] acLogin called, isInVSCode:", isInVSCode());
  
  if (isInVSCode()) {
    // In VS Code, send message to parent to handle login
    window.parent?.postMessage({ type: 'news:login' }, '*');
    window.parent?.postMessage({ type: 'login', tenant: 'aesthetic' }, '*');
    return;
  }
  
  if (!auth0Client) await initAuth0();
  if (!auth0Client) {
    console.error("[news] auth0Client still null after init");
    return;
  }
  console.log("[news] Redirecting to Auth0...");
  await auth0Client.loginWithRedirect({
    authorizationParams: { prompt: "login" },
  });
}

async function acSignup() {
  console.log("[news] acSignup called, isInVSCode:", isInVSCode());
  
  if (isInVSCode()) {
    // In VS Code, send message to parent to handle signup
    window.parent?.postMessage({ type: 'news:signup' }, '*');
    window.parent?.postMessage({ type: 'signup', tenant: 'aesthetic' }, '*');
    return;
  }
  
  if (!auth0Client) await initAuth0();
  if (!auth0Client) return;
  await auth0Client.loginWithRedirect({
    authorizationParams: { screen_hint: "signup" },
  });
}

async function acLogout() {
  console.log("[news] acLogout called, isInVSCode:", isInVSCode());
  
  if (isInVSCode()) {
    // In VS Code, send message to parent to handle logout
    window.parent?.postMessage({ type: 'news:logout' }, '*');
    window.parent?.postMessage({ type: 'logout', tenant: 'aesthetic' }, '*');
    acUser = null;
    acToken = null;
    acHandle = null;
    saveSessionCache(null);
    updateAuthUI();
    return;
  }
  
  if (!auth0Client) return;
  acUser = null;
  acToken = null;
  acHandle = null;
  saveSessionCache(null);
  updateAuthUI();
  
  // Use root URL for logout (must match Auth0 Allowed Logout URLs)
  const isNewsDomain = window.location.hostname === 'news.aesthetic.computer';
  const returnTo = isNewsDomain 
    ? window.location.origin 
    : window.location.origin + '/news.aesthetic.computer';
  
  await auth0Client.logout({ logoutParams: { returnTo } });
}

function attachAuthHandlers() {
  if (loginBtn) loginBtn.addEventListener("click", (e) => {
    e.preventDefault();
    acLogin();
  });
  if (signupBtn) signupBtn.addEventListener("click", (e) => {
    e.preventDefault();
    acSignup();
  });
  if (userHandle) userHandle.addEventListener("click", (e) => {
    e.preventDefault();
    openUserMenuModal();
  });
}

function openUserMenuModal() {
  if (userMenuModalOpen) return;
  userMenuModalOpen = true;
  closeModal();

  const existing = document.getElementById('news-user-menu-modal');
  if (existing) existing.remove();

  const modal = document.createElement('div');
  modal.id = 'news-user-menu-modal';
  modal.className = 'news-user-menu-modal';
  modal.innerHTML = `
    <div class="news-user-menu-backdrop"></div>
    <div class="news-user-menu-panel">
      <div class="news-user-menu-title">${acHandle ? `@${acHandle}` : (acUser?.email || 'account')}</div>
      <button class="news-user-menu-action" type="button" data-action="logout">log out</button>
    </div>
  `;

  document.body.appendChild(modal);

  modal.querySelector('.news-user-menu-backdrop')?.addEventListener('click', closeUserMenuModal);
  modal.querySelector('[data-action="logout"]')?.addEventListener('click', (e) => {
    e.preventDefault();
    closeUserMenuModal();
    acLogout();
  });
  document.addEventListener('keydown', handleUserMenuEscape);
}

function closeUserMenuModal() {
  const modal = document.getElementById('news-user-menu-modal');
  if (modal) modal.remove();
  userMenuModalOpen = false;
  document.removeEventListener('keydown', handleUserMenuEscape);
}

function handleUserMenuEscape(e) {
  if (e.key === 'Escape') closeUserMenuModal();
}

function attachSessionListener() {
  if (sessionListenerAttached) return;
  sessionListenerAttached = true;
  window.addEventListener("message", (event) => {
    if (event.data?.type === "setSession" && event.data?.tenant === "aesthetic") {
      applySession(event.data.session);
    }
  });
}

// Show form message (replaces alert for sandboxed iframes)
function showFormMessage(form, message, isError = true) {
  let msgEl = form.querySelector('.news-form-message');
  if (!msgEl) {
    msgEl = document.createElement('div');
    msgEl.className = 'news-form-message';
    form.insertBefore(msgEl, form.firstChild);
  }
  msgEl.textContent = message;
  msgEl.classList.toggle('error', isError);
  msgEl.classList.toggle('info', !isError);
  msgEl.style.display = 'block';
}

function hideFormMessage(form) {
  const msgEl = form.querySelector('.news-form-message');
  if (msgEl) msgEl.style.display = 'none';
}

async function handleFormSubmit(form, endpoint) {
  hideFormMessage(form);
  
  if (!acToken) {
    showFormMessage(form, "Please log in to post. Click 'Log In' above.");
    return;
  }
  
  if (!acHandle) {
    showFormMessage(form, "You need a @handle to post. Visit aesthetic.computer to set one up.");
    return;
  }
  
  const formData = new FormData(form);
  const body = new URLSearchParams(formData.entries());
  const res = await fetch(endpoint, {
    method: "POST",
    headers: {
      Authorization: `Bearer ${acToken}`,
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body,
  });
  const data = await res.json().catch(() => null);
  
  // Handle rate limit (429 Too Many Requests)
  if (res.status === 429) {
    showFormMessage(form, data?.error || "Rate limit exceeded. Please wait before posting again.");
    return;
  }
  
  if (data?.redirect) {
    // Clear draft on successful submit
    if (form.clearDraft) form.clearDraft();
    window.location.href = data.redirect;
    return;
  }
  if (!res.ok) {
    showFormMessage(form, data?.error || "Request failed");
  } else {
    // Clear draft on successful submit (even without redirect)
    if (form.clearDraft) form.clearDraft();
  }
}

function initForms() {
  document.querySelectorAll("form[data-news-action]").forEach((form) => {
    const action = form.getAttribute("data-news-action");
    form.addEventListener("submit", (e) => {
      e.preventDefault();
      if (action === "submit") {
        handleFormSubmit(form, "/api/news/submit");
      } else if (action === "comment") {
        handleCommentSubmit(form);
      } else if (action === "vote") {
        // Voting disabled
        e.preventDefault();
      } else if (action === "delete") {
        handleDeleteSubmit(form);
      }
    });
  });
}

// Handle comment submit with real-time UI update
async function handleCommentSubmit(form) {
  hideFormMessage(form);
  
  if (!acToken) {
    showFormMessage(form, "Please log in to post. Click 'Log In' above.");
    return;
  }
  
  if (!acHandle) {
    showFormMessage(form, "You need a @handle to post. Visit aesthetic.computer to set one up.");
    return;
  }
  
  const formData = new FormData(form);
  const postCode = formData.get('postCode');
  const text = formData.get('text')?.trim();
  
  if (!text) {
    showFormMessage(form, "Please write something before responding.");
    return;
  }
  
  const body = new URLSearchParams(formData.entries());
  const res = await fetch("/api/news/comment", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${acToken}`,
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body,
  });
  const data = await res.json().catch(() => null);
  
  if (!res.ok) {
    showFormMessage(form, data?.error || "Failed to post comment");
    return;
  }
  
  // Success - add comment to UI without refresh
  const commentsContainer = document.querySelector('.news-comments');
  if (commentsContainer) {
    const commentId = data.commentId || `temp-${Date.now()}`;
    const newComment = document.createElement('div');
    newComment.className = 'news-comment';
    newComment.dataset.commentId = commentId;
    newComment.innerHTML = `
      <div class="news-comment-meta">
        <span><a href="https://aesthetic.computer/${acHandle}" class="news-modal-link news-handle-link" data-modal-url="https://aesthetic.computer/${acHandle}">@${acHandle}</a></span>
        <span>just now</span>
        <form class="news-admin-delete" data-news-action="delete" data-item-type="comment" data-item-id="${commentId}" data-handle="${acHandle}" method="post" action="/api/news/delete" style="display:inline-flex;">
          <input type="hidden" name="itemType" value="comment" />
          <input type="hidden" name="itemId" value="${commentId}" />
          <button type="submit" class="news-delete-btn" title="Delete comment">
            <svg class="news-x-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round" stroke-linejoin="round">
              <line x1="6" y1="6" x2="18" y2="18"/><line x1="18" y1="6" x2="6" y2="18"/>
            </svg>
          </button>
        </form>
      </div>
      <div class="news-comment-body">${escapeHtmlClient(text)}</div>
    `;
    commentsContainer.appendChild(newComment);
    
    // Attach form handler to new delete button
    const deleteForm = newComment.querySelector('form[data-news-action="delete"]');
    if (deleteForm) {
      deleteForm.addEventListener('submit', (e) => {
        e.preventDefault();
        handleDeleteSubmit(deleteForm);
      });
    }
  }
  
  // Clear the textarea
  form.querySelector('textarea[name="text"]').value = '';
  if (form.clearDraft) form.clearDraft();
}

// Handle delete with real-time UI update
async function handleDeleteSubmit(form) {
  const itemType = form.dataset.itemType || form.querySelector('input[name="itemType"]')?.value;
  const itemId = form.dataset.itemId || form.querySelector('input[name="itemId"]')?.value;
  
  if (!confirm("Are you sure you want to delete this?")) {
    return;
  }
  
  if (!acToken) {
    alert("Please log in to delete.");
    return;
  }
  
  const body = new URLSearchParams({ itemType, itemId });
  const res = await fetch("/api/news/delete", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${acToken}`,
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body,
  });
  const data = await res.json().catch(() => null);
  
  if (!res.ok) {
    alert(data?.error || "Failed to delete");
    return;
  }
  
  // Success - remove from UI without refresh
  if (itemType === 'comment') {
    const comment = form.closest('.news-comment');
    if (comment) {
      comment.style.transition = 'opacity 0.3s, transform 0.3s';
      comment.style.opacity = '0';
      comment.style.transform = 'translateX(-20px)';
      setTimeout(() => comment.remove(), 300);
    }
  } else if (itemType === 'post') {
    // For posts, use SPA navigation to homepage (respects localhost path)
    const basePath = window.location.hostname === 'news.aesthetic.computer' ? '' : '/news.aesthetic.computer';
    navigateTo(basePath + '/', { push: true });
  }
}

// Client-side HTML escape
function escapeHtmlClient(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

// ===== VS Code Detection =====
// Check if running inside VS Code webview (sandboxed iframe)
function isInVSCode() {
  // Check for vscode query param or acVSCODE flag set by extension
  const urlParams = new URLSearchParams(window.location.search);
  return urlParams.get('vscode') === 'true' || 
         (window.acVSCODE && window.parent !== window) ||
         window.parent !== window; // Also check if we're in any iframe
}

// Open URL externally - handles VS Code sandboxed iframe case
function openExternal(url) {
  if (isInVSCode()) {
    // In VS Code extension webview, send message to parent to open externally
    window.parent.postMessage({ type: 'openExternal', url }, '*');
    return true;
  }
  // Normal browser - open in new tab
  window.open(url, '_blank', 'noopener');
  return true;
}

// ===== Modal functionality =====
function initModals() {
  // Modal links (List, Weather, Commits) - open in iframe modal
  const modalLinks = document.querySelectorAll('.news-modal-link');
  modalLinks.forEach(link => {
    link.addEventListener('click', (e) => {
      e.preventDefault();
      const url = link.dataset.modalUrl || link.href;
      openModal(url);
    });
  });
  
  // External links (Give) - open in new tab, with VS Code handling
  const externalLinks = document.querySelectorAll('.news-external-link');
  externalLinks.forEach(link => {
    link.addEventListener('click', (e) => {
      e.preventDefault();
      const url = link.href;
      openExternal(url);
    });
  });
}

function openModal(url) {
  // Remove existing modal if any
  const existing = document.getElementById('news-modal');
  if (existing) existing.remove();

  const modal = document.createElement('div');
  modal.id = 'news-modal';
  modal.className = 'news-modal';
  modal.innerHTML = `
    <div class="news-modal-backdrop"></div>
    <div class="news-modal-content">
      <button class="news-modal-close">×</button>
      <iframe src="${url}" frameborder="0"></iframe>
    </div>
  `;
  document.body.appendChild(modal);

  // Close handlers
  modal.querySelector('.news-modal-backdrop').addEventListener('click', closeModal);
  modal.querySelector('.news-modal-close').addEventListener('click', closeModal);
  document.addEventListener('keydown', handleEscape);
}

function closeModal() {
  const modal = document.getElementById('news-modal');
  if (modal) modal.remove();
  document.removeEventListener('keydown', handleEscape);
}

function handleEscape(e) {
  if (e.key === 'Escape') closeModal();
}

// ===== Report Link Login Check =====
function isReportLink(link) {
  if (!link) return false;
  const href = link.getAttribute('href') || '';
  return link.classList.contains('news-report-link') || href.includes('/report');
}

function promptLoginForReport() {
  // Scroll to login button and blink it
  const loginBtn = document.getElementById('news-login-btn');
  if (loginBtn) {
    loginBtn.scrollIntoView({ behavior: 'smooth', block: 'center' });
    
    // Add blink animation after scroll completes
    setTimeout(() => {
      loginBtn.classList.remove('blinking');
      void loginBtn.offsetWidth; // Force reflow to restart animation
      loginBtn.classList.add('blinking');
      
      // Remove class after animation completes
      setTimeout(() => {
        loginBtn.classList.remove('blinking');
      }, 1600); // 4 blinks × 0.4s
    }, 300); // Wait for scroll
  }
}

// ===== Live Updates =====
let liveUpdateInterval = null;
let lastPostTimestamp = Date.now();

function startLiveUpdates() {
  // Only poll on the homepage
  if (window.location.pathname !== '/' && window.location.pathname !== '') return;
  
  // Stop any existing polling
  if (liveUpdateInterval) clearInterval(liveUpdateInterval);
  
  // Initialize timestamp from the newest post on page
  const firstPost = document.querySelector('.news-row');
  if (firstPost) {
    lastPostTimestamp = Date.now(); // Start from now to only show NEW posts
  }
  
  // Poll every 30 seconds
  liveUpdateInterval = setInterval(checkForUpdates, 30000);
}

async function checkForUpdates() {
  try {
    const res = await fetch(`/api/news/updates?since=${lastPostTimestamp}`);
    const data = await res.json();
    
    if (data.newPosts > 0) {
      showNewPostsBanner(data.newPosts);
      lastPostTimestamp = data.latestWhen;
    }
  } catch (err) {
    console.log('[news] Live update check failed:', err.message);
  }
}

function showNewPostsBanner(count) {
  // Remove existing banner
  const existing = document.getElementById('news-live-banner');
  if (existing) existing.remove();
  
  const banner = document.createElement('div');
  banner.id = 'news-live-banner';
  banner.className = 'news-live-banner';
  banner.innerHTML = `
    <span>${count} new ${count === 1 ? 'story' : 'stories'}</span>
    <button onclick="location.reload()">Refresh</button>
  `;
  
  const newsList = document.querySelector('.news-list');
  if (newsList) {
    newsList.insertBefore(banner, newsList.firstChild);
  }
}

// ===== SPA Routing =====
function reinitPage() {
  initDOMRefs();
  attachAuthHandlers();
  initForms();
  initModals();
  initSubmitFormConstraints();
  initLangSelectors();
  updateAuthUI();
  connectToSessionServer();
  startLiveUpdates();
}

function shouldHandleLink(link) {
  if (!link || link.target === '_blank') return false;
  if (link.hasAttribute('download')) return false;
  if (link.classList.contains('news-external-link')) return false;
  if (link.classList.contains('news-modal-link')) return false;
  const rawHref = link.getAttribute('href') || '';
  if (rawHref.startsWith('#')) return false;

  const url = new URL(link.href, window.location.origin);
  if (url.origin !== window.location.origin) return false;
  if (url.pathname === window.location.pathname && url.search === window.location.search && url.hash) return false;
  return true;
}

async function navigateTo(url, { push = true } = {}) {
  const next = new URL(typeof url === 'string' ? url : url.href, window.location.origin);
  const nextUrl = next.href;
  const nextPath = next.pathname + next.search + next.hash;
  console.log('[news] navigateTo called:', nextUrl, 'push:', push);
  if (!nextUrl) return;
  
  const currentPath = window.location.pathname + window.location.search + window.location.hash;
  // For push navigation, skip if same URL. For popstate (push=false), always fetch.
  if (push && nextPath === currentPath) {
    console.log('[news] Skipping - same URL');
    return;
  }

  const currentWrapper = document.querySelector('.news-wrapper');
  
  // Helper to wait for transition end
  const waitForTransition = (el, duration = 150) => new Promise(resolve => {
    const timeout = setTimeout(resolve, duration + 50); // fallback
    el.addEventListener('transitionend', function handler(e) {
      if (e.target === el) {
        clearTimeout(timeout);
        el.removeEventListener('transitionend', handler);
        resolve();
      }
    });
  });

  try {
    closeModal();
    
    // Start fade-out transition
    if (currentWrapper) {
      currentWrapper.classList.add('page-transitioning-out');
    }
    
    console.log('[news] Fetching:', nextPath);
    const res = await fetch(nextPath, { headers: { 'X-Requested-With': 'spa' } });
    console.log('[news] Fetch response:', res.status);
    const html = await res.text();
    console.log('[news] HTML length:', html.length, 'preview:', html.substring(0, 200));
    const doc = new DOMParser().parseFromString(html, 'text/html');
    const nextWrapper = doc.querySelector('.news-wrapper');
    console.log('[news] Wrappers found:', !!nextWrapper, !!currentWrapper);
    console.log('[news] nextWrapper innerHTML length:', nextWrapper?.innerHTML?.length);
    if (!nextWrapper || !currentWrapper) {
      console.log('[news] Falling back to full navigation');
      window.location.href = nextUrl;
      return;
    }
    
    // Wait for fade-out to complete (or timeout if fetch was fast)
    if (currentWrapper.classList.contains('page-transitioning-out')) {
      await waitForTransition(currentWrapper);
    }
    
    // Swap content while hidden
    console.log('[news] Replacing content, old length:', currentWrapper.innerHTML.length, 'new length:', nextWrapper.innerHTML.length);
    currentWrapper.innerHTML = nextWrapper.innerHTML;
    console.log('[news] After replace, current length:', currentWrapper.innerHTML.length);
    
    // Prepare for fade-in
    currentWrapper.classList.remove('page-transitioning-out');
    currentWrapper.classList.add('page-transitioning-in');
    
    // Force reflow to ensure transition triggers
    currentWrapper.offsetHeight;
    
    // Fade in
    currentWrapper.classList.remove('page-transitioning-in');
    
    const newTitle = doc.title || 'Aesthetic News';
    document.title = newTitle;
    if (push) {
      // Cache the HTML content in history state for instant back/forward
      history.pushState({ title: newTitle, url: nextPath, html: nextWrapper.innerHTML }, newTitle, nextPath);
    }
    window.scrollTo(0, 0);
    reinitPage();
  } catch (error) {
    console.error('[news] Navigation error:', error);
    // Clean up transition state on error
    if (currentWrapper) {
      currentWrapper.classList.remove('page-transitioning-out', 'page-transitioning-in');
    }
    window.location.href = nextUrl;
  }
}

function initSpaRouting() {
  if (spaInitialized) return;
  spaInitialized = true;
  
  // Store initial state with title and content
  const initialWrapper = document.querySelector('.news-wrapper');
  const initialPath = window.location.pathname + window.location.search + window.location.hash;
  history.replaceState({ 
    title: document.title, 
    url: initialPath,
    html: initialWrapper ? initialWrapper.innerHTML : null
  }, document.title, initialPath);

  document.addEventListener('click', (e) => {
    const link = e.target.closest('a');
    console.log('[news] Click on link:', link?.href, 'shouldHandle:', shouldHandleLink(link));
    console.log('[news] Current URL at click time:', window.location.href);
    if (!shouldHandleLink(link)) return;
    
    // Prevent default IMMEDIATELY before any async work
    e.preventDefault();
    
    // Check if this is a report link and user is not logged in
    if (isReportLink(link)) {
      const isLoggedIn = acUser || acHandle || acToken;
      console.log('[news] Report link check - isLoggedIn:', isLoggedIn, { acUser, acHandle, acToken: !!acToken });
      if (!isLoggedIn) {
        e.stopPropagation();
        promptLoginForReport();
        return;
      }
    }
    
    console.log('[news] Navigating to:', link.href);
    navigateTo(link.href, { push: true });
  });

  window.addEventListener('popstate', async (e) => {
    // Restore title from state
    if (e.state && e.state.title) {
      document.title = e.state.title;
    }
    
    // If we have cached HTML, use it with transition
    if (e.state && e.state.html) {
      const currentWrapper = document.querySelector('.news-wrapper');
      if (currentWrapper) {
        // Fade out
        currentWrapper.classList.add('page-transitioning-out');
        await new Promise(r => setTimeout(r, 150));
        
        // Swap content
        currentWrapper.innerHTML = e.state.html;
        
        // Prepare for fade-in
        currentWrapper.classList.remove('page-transitioning-out');
        currentWrapper.classList.add('page-transitioning-in');
        currentWrapper.offsetHeight; // force reflow
        currentWrapper.classList.remove('page-transitioning-in');
        
        window.scrollTo(0, 0);
        reinitPage();
        return;
      }
    }
    
    // Fallback: fetch the page content
    const currentPath = window.location.pathname + window.location.search + window.location.hash;
    navigateTo(currentPath, { push: false });
  });
}

// ===== WebSocket Live Reload (dev only) =====
let sessionWs = null;
let reconnectInterval = null;
let connectivityDot = null;
let connectivityLabel = null;

function setConnectivityState(state) {
  if (!connectivityDot) return;
  connectivityDot.className = 'connectivity-dot ' + state;
  if (connectivityLabel) {
    const labels = {
      connected: 'live',
      connecting: 'connecting...',
      waiting: 'reconnecting...',
      receiving: 'live',
      disconnected: 'offline'
    };
    connectivityLabel.textContent = labels[state] || state;
  }
}

function connectToSessionServer() {
  // Only connect in dev mode (when connectivity indicator exists)
  connectivityDot = document.getElementById('connectivity-dot');
  connectivityLabel = document.getElementById('connectivity-label');
  if (!connectivityDot) return; // Not in dev mode

  if (sessionWs && sessionWs.readyState === WebSocket.OPEN) {
    setConnectivityState('connected');
    return;
  }
  if (sessionWs && sessionWs.readyState === WebSocket.CONNECTING) {
    setConnectivityState('connecting');
    return;
  }
  
  if (reconnectInterval) {
    clearInterval(reconnectInterval);
    reconnectInterval = null;
  }
  
  // Determine connection URL
  let connectionUrl = "wss://localhost:8889";
  if (window.location.host === "local.aesthetic.computer" || 
      window.location.host === "news.local.aesthetic.computer") {
    connectionUrl = "wss://session.local.aesthetic.computer";
  } else if (window.location.host === "news.aesthetic.computer") {
    // Production - no live reload needed
    return;
  }
  
  setConnectivityState('connecting');
  console.log("[news] Connecting to session server:", connectionUrl);
  
  try {
    sessionWs = new WebSocket(connectionUrl);
  } catch (error) {
    console.warn("[news] Connection failed:", error.message);
    setConnectivityState('disconnected');
    scheduleReconnect();
    return;
  }

  sessionWs.onopen = () => {
    console.log("[news] Connected to session server");
    setConnectivityState('connected');
  };

  sessionWs.onmessage = (e) => {
    // Blink the dot when receiving a message
    setConnectivityState('receiving');
    setTimeout(() => setConnectivityState('connected'), 400);
    
    try {
      const msg = JSON.parse(e.data);
      
      // Handle reload messages
      if (msg.type === "reload") {
        const piece = msg.content?.piece;
        console.log("[news] Reload message:", piece);
        if (piece === "*refresh*" || piece === "news.aesthetic.computer" || piece === "news") {
          console.log("[news] Reloading page...");
          setTimeout(() => window.location.reload(), 150);
        }
      }
    } catch (error) {
      console.warn("[news] Error parsing message:", error.message);
    }
  };

  sessionWs.onerror = () => {
    console.warn("[news] WebSocket error");
    setConnectivityState('disconnected');
  };

  sessionWs.onclose = () => {
    console.log("[news] Disconnected from session server");
    setConnectivityState('disconnected');
    sessionWs = null;
    scheduleReconnect();
  };
}

function scheduleReconnect() {
  if (!reconnectInterval && connectivityDot) {
    setConnectivityState('waiting');
    reconnectInterval = setInterval(() => {
      if (!sessionWs || sessionWs.readyState === WebSocket.CLOSED) {
        connectToSessionServer();
      }
    }, 2000);
  }
}

// Initialize everything after DOM is ready
reinitPage();
attachSessionListener();
applySession(decodeSessionParam());
initAuth0();
initSpaRouting();

// Submit form constraints - require URL or Thoughts, validate inputs
function initSubmitFormConstraints() {
  const form = document.getElementById('news-submit-form');
  if (!form) return;
  
  const headlineInput = form.querySelector('input[name="title"]');
  const urlInput = form.querySelector('input[name="url"]');
  const textInput = form.querySelector('textarea[name="text"]');
  const submitBtn = form.querySelector('button[type="submit"]');
  const eitherOr = form.querySelector('.news-either-or');
  
  if (!headlineInput || !urlInput || !textInput || !submitBtn) return;
  
  // ===== Draft Storage =====
  const DRAFT_KEY = 'news-draft';
  
  function saveDraft() {
    const draft = {
      title: headlineInput.value,
      url: urlInput.value,
      text: textInput.value,
      savedAt: Date.now()
    };
    try {
      localStorage.setItem(DRAFT_KEY, JSON.stringify(draft));
    } catch (e) {
      console.warn('[news] Failed to save draft:', e);
    }
  }
  
  function loadDraft() {
    try {
      const saved = localStorage.getItem(DRAFT_KEY);
      if (!saved) return;
      const draft = JSON.parse(saved);
      // Only restore if draft is less than 24 hours old
      if (draft.savedAt && Date.now() - draft.savedAt < 24 * 60 * 60 * 1000) {
        if (draft.title) headlineInput.value = draft.title;
        if (draft.url) urlInput.value = draft.url;
        if (draft.text) textInput.value = draft.text;
        console.log('[news] Draft restored');
      } else {
        clearDraft();
      }
    } catch (e) {
      console.warn('[news] Failed to load draft:', e);
    }
  }
  
  function clearDraft() {
    try {
      localStorage.removeItem(DRAFT_KEY);
      console.log('[news] Draft cleared');
    } catch (e) {
      console.warn('[news] Failed to clear draft:', e);
    }
  }
  
  // Load draft on init
  loadDraft();
  
  // Save draft on input (debounced)
  let saveTimeout;
  function debouncedSaveDraft() {
    clearTimeout(saveTimeout);
    saveTimeout = setTimeout(saveDraft, 500);
  }
  
  headlineInput.addEventListener('input', debouncedSaveDraft);
  urlInput.addEventListener('input', debouncedSaveDraft);
  textInput.addEventListener('input', debouncedSaveDraft);
  
  // Expose clearDraft for use after successful submit
  form.clearDraft = clearDraft;
  
  // ===== Auto-fetch title from URL =====
  const autoTitleStatus = document.getElementById('news-auto-title-status');
  let unfurlController = null;
  let lastUnfurledUrl = '';
  let headlineManuallyEdited = false;
  
  // Track if user has manually typed in the headline
  headlineInput.addEventListener('input', () => {
    // If the headline differs from the last auto-filled value, mark as manually edited
    if (headlineInput.dataset.autoTitle && headlineInput.value !== headlineInput.dataset.autoTitle) {
      headlineManuallyEdited = true;
    }
  });
  
  async function unfurlUrl(url) {
    if (!url || url === lastUnfurledUrl) return;
    // Only unfurl valid-looking URLs
    try { new URL(url); } catch { return; }
    
    lastUnfurledUrl = url;
    
    // Cancel any in-flight request
    if (unfurlController) unfurlController.abort();
    unfurlController = new AbortController();
    
    if (autoTitleStatus) {
      autoTitleStatus.textContent = 'Fetching title…';
      autoTitleStatus.className = 'news-auto-title-status loading';
    }
    
    try {
      const res = await fetch(`/api/news/unfurl?url=${encodeURIComponent(url)}`, {
        signal: unfurlController.signal,
      });
      const data = await res.json();
      if (data.title && !headlineManuallyEdited) {
        headlineInput.value = data.title;
        headlineInput.dataset.autoTitle = data.title;
        headlineInput.dispatchEvent(new Event('input', { bubbles: true }));
        if (autoTitleStatus) {
          autoTitleStatus.textContent = '';
          autoTitleStatus.className = 'news-auto-title-status';
        }
      } else if (!data.title) {
        if (autoTitleStatus) {
          autoTitleStatus.textContent = '';
          autoTitleStatus.className = 'news-auto-title-status';
        }
      }
    } catch (e) {
      if (e.name !== 'AbortError') {
        if (autoTitleStatus) {
          autoTitleStatus.textContent = '';
          autoTitleStatus.className = 'news-auto-title-status';
        }
      }
    }
  }
  
  let unfurlTimeout;
  urlInput.addEventListener('input', () => {
    clearTimeout(unfurlTimeout);
    const url = urlInput.value.trim();
    if (url && url.startsWith('http')) {
      unfurlTimeout = setTimeout(() => unfurlUrl(url), 600);
    }
  });
  
  // Also unfurl on paste (immediately, no debounce)
  urlInput.addEventListener('paste', () => {
    setTimeout(() => {
      const url = urlInput.value.trim();
      if (url && url.startsWith('http')) unfurlUrl(url);
    }, 50);
  });
  
  // Add error message elements
  function getOrCreateError(input, id) {
    let err = document.getElementById(id);
    if (!err) {
      err = document.createElement('div');
      err.id = id;
      err.className = 'news-field-error';
      input.parentNode.appendChild(err);
    }
    return err;
  }
  
  const headlineError = getOrCreateError(headlineInput, 'headline-error');
  const urlError = getOrCreateError(urlInput, 'url-error');
  
  function validateUrl(url) {
    if (!url) return { valid: true, error: '' }; // Empty is OK (optional)
    if (!url.startsWith('https://') && !url.startsWith('http://')) {
      return { valid: false, error: 'URL must start with https://' };
    }
    try {
      new URL(url);
      return { valid: true, error: '' };
    } catch {
      return { valid: false, error: 'Invalid URL format' };
    }
  }
  
  function updateConstraints() {
    const headline = headlineInput.value.trim();
    const url = urlInput.value.trim();
    const text = textInput.value.trim();
    
    const hasHeadline = headline.length > 0;
    const hasUrl = url.length > 0;
    const hasText = text.length > 0;
    const urlValidation = validateUrl(url);
    
    // Show/hide errors
    headlineError.textContent = '';
    urlError.textContent = urlValidation.error;
    
    // Valid if: has headline AND (has valid url OR has text)
    const hasContent = (hasUrl && urlValidation.valid) || hasText;
    const isValid = hasHeadline && hasContent;
    
    submitBtn.disabled = !isValid;
    
    if (eitherOr) {
      eitherOr.classList.toggle('has-content', hasText);
    }
  }
  
  // Validate on blur for headline
  headlineInput.addEventListener('blur', () => {
    if (!headlineInput.value.trim()) {
      headlineError.textContent = 'Headline is required';
    }
  });
  
  headlineInput.addEventListener('input', updateConstraints);
  urlInput.addEventListener('input', updateConstraints);
  textInput.addEventListener('input', updateConstraints);
  
  // Prevent submit if invalid
  form.addEventListener('submit', (e) => {
    const headline = headlineInput.value.trim();
    const url = urlInput.value.trim();
    const text = textInput.value.trim();
    const urlValidation = validateUrl(url);
    
    if (!headline) {
      e.preventDefault();
      headlineError.textContent = 'Headline is required';
      headlineInput.focus();
      return;
    }
    
    if (url && !urlValidation.valid) {
      e.preventDefault();
      urlInput.focus();
      return;
    }
    
    if (!url && !text) {
      e.preventDefault();
      urlInput.focus();
      return;
    }
  });
  
  // Initial state
  updateConstraints();
}

// ===== YouTube Timecode Integration =====
let youtubePlayer = null;
let youtubePlayerReady = false;
let timecodeInterval = null;
let isSandboxedMode = false;

function initYouTubePlayer() {
  const embed = document.querySelector('.news-youtube-embed[data-youtube-id]');
  if (!embed) return;
  
  const youtubeId = embed.dataset.youtubeId;
  if (!youtubeId) return;
  
  // Reset state
  youtubePlayer = null;
  youtubePlayerReady = false;
  
  // Check if we're in a sandboxed environment (VS Code webview)
  isSandboxedMode = window.frameElement?.sandbox || 
                    document.featurePolicy && !document.featurePolicy.allowsFeature('autoplay');
  
  if (isSandboxedMode) {
    console.log('[news] Sandboxed environment detected, showing YouTube fallback');
    embed.classList.add('sandboxed');
    // Hide timecode button in sandbox mode
    const timecodeBtn = document.getElementById('insert-timecode-btn');
    if (timecodeBtn) timecodeBtn.style.display = 'none';
    // Fallback is now a link, no extra handling needed - it gracefully degrades
    return;
  }
  
  console.log('[news] Initializing YouTube player for:', youtubeId);
  
  // Load YouTube IFrame API
  if (!window.YT || !window.YT.Player) {
    const tag = document.createElement('script');
    tag.src = 'https://www.youtube.com/iframe_api';
    document.head.appendChild(tag);
    console.log('[news] Loading YouTube IFrame API');
    
    // Set up callback for when API is ready
    window.onYouTubeIframeAPIReady = () => {
      console.log('[news] YouTube API ready');
      createPlayer(youtubeId);
    };
  } else {
    // API already loaded
    createPlayer(youtubeId);
  }
}

function createPlayer(youtubeId) {
  const container = document.querySelector('.news-youtube-embed[data-youtube-id]');
  if (!container) {
    console.log('[news] No YouTube container found');
    return;
  }
  
  // Find the existing iframe
  const iframe = container.querySelector('iframe');
  if (!iframe) {
    console.log('[news] No YouTube iframe found');
    return;
  }
  
  console.log('[news] Creating YouTube player for:', youtubeId);
  
  try {
    // Use the existing iframe with the API
    youtubePlayer = new YT.Player(iframe, {
      events: {
        onReady: (event) => {
          console.log('[news] YouTube player ready!');
          youtubePlayerReady = true;
          updateTimecodeButton();
          startTimecodeUpdater();
        },
        onStateChange: (event) => {
          console.log('[news] YouTube state:', event.data);
          updateTimecodeButton();
        }
      }
    });
  } catch (e) {
    console.error('[news] Failed to create YouTube player:', e);
  }
}

function startTimecodeUpdater() {
  if (timecodeInterval) clearInterval(timecodeInterval);
  timecodeInterval = setInterval(updateTimecodeButton, 500);
}

function formatTime(seconds) {
  const h = Math.floor(seconds / 3600);
  const m = Math.floor((seconds % 3600) / 60);
  const s = Math.floor(seconds % 60);
  if (h > 0) {
    return `${h}:${m.toString().padStart(2, '0')}:${s.toString().padStart(2, '0')}`;
  }
  return `${m}:${s.toString().padStart(2, '0')}`;
}

function parseTimecode(str) {
  // Parse "1:23" or "1:23:45" to seconds
  const parts = str.split(':').map(Number);
  if (parts.length === 2) {
    return parts[0] * 60 + parts[1];
  } else if (parts.length === 3) {
    return parts[0] * 3600 + parts[1] * 60 + parts[2];
  }
  return 0;
}

function updateTimecodeButton() {
  const btn = document.getElementById('insert-timecode-btn');
  if (!btn || !youtubePlayerReady || !youtubePlayer) return;
  
  try {
    const time = youtubePlayer.getCurrentTime?.() || 0;
    btn.textContent = `@ ${formatTime(time)}`;
  } catch (e) {
    // Player not ready
  }
}

function setupTimecodeButton() {
  const btn = document.getElementById('insert-timecode-btn');
  const textarea = document.querySelector('#news-comment-form textarea[name="text"]');
  if (!btn || !textarea) return;
  
  btn.addEventListener('click', () => {
    if (!youtubePlayerReady || !youtubePlayer) {
      alert('Video player not ready yet - try again in a moment');
      return;
    }
    
    try {
      const time = youtubePlayer.getCurrentTime?.() || 0;
      const timecode = formatTime(time);
      
      // Insert at cursor position or append
      const start = textarea.selectionStart;
      const end = textarea.selectionEnd;
      const text = textarea.value;
      const insertion = `[${timecode}] `;
      
      textarea.value = text.substring(0, start) + insertion + text.substring(end);
      textarea.selectionStart = textarea.selectionEnd = start + insertion.length;
      textarea.focus();
    } catch (e) {
      console.error('Failed to get video time:', e);
    }
  });
}

function linkifyTimecodes(container) {
  // Only if there's a YouTube player on the page
  const embed = document.querySelector('.news-youtube-embed[data-youtube-id]');
  if (!embed) return;
  
  const youtubeId = embed.dataset.youtubeId;
  
  const commentBodies = container.querySelectorAll('.news-comment-body');
  commentBodies.forEach(body => {
    // Match [0:00] or [1:23:45] patterns
    const html = body.innerHTML;
    const linked = html.replace(/\[(\d{1,2}:\d{2}(?::\d{2})?)\]/g, (match, time) => {
      if (isSandboxedMode) {
        // In sandbox mode, link directly to YouTube with timestamp
        const seconds = parseTimecode(time);
        return `<a href="https://www.youtube.com/watch?v=${youtubeId}&t=${seconds}s" target="_blank" rel="noopener" class="news-timecode-link">${match}</a>`;
      }
      return `<a href="#" class="news-timecode-link" data-timecode="${time}">${match}</a>`;
    });
    if (linked !== html) {
      body.innerHTML = linked;
    }
  });
  
  // Add click handlers (only for non-sandbox mode)
  if (!isSandboxedMode) {
    container.querySelectorAll('.news-timecode-link').forEach(link => {
      link.addEventListener('click', (e) => {
        e.preventDefault();
        const timecode = link.dataset.timecode;
        if (youtubePlayerReady && youtubePlayer && timecode) {
          const seconds = parseTimecode(timecode);
          youtubePlayer.seekTo(seconds, true);
          youtubePlayer.playVideo();
          
          // Scroll video into view
          embed.scrollIntoView({ behavior: 'smooth', block: 'center' });
        }
      });
    });
  }
}

// Syntax highlight AC commands in backticks or single quotes (e.g., `list`, 'give')
function highlightACCommands(container) {
  const commentBodies = container.querySelectorAll('.news-comment-body, .news-op-body');
  commentBodies.forEach(body => {
    const html = body.innerHTML;
    // Match text in backticks: `command` or single quotes: 'command'
    // For single quotes, require word boundary before to avoid contractions like "I'll"
    // Only highlight the content, not the quotes themselves
    const highlighted = html
      .replace(/`([^`]+)`/g, (match, cmd) => {
        const trimmed = cmd.trim();
        return `\`<a href="#" class="news-ac-command" data-ac-cmd="${trimmed}">${cmd}</a>\``;
      })
      .replace(/(?:^|[\s\(\[\{])'([^']+)'/g, (match, cmd) => {
        const trimmed = cmd.trim();
        // Check if it starts with whitespace/bracket
        const prefix = match.startsWith("'") ? '' : match[0];
        return `${prefix}'<a href="#" class="news-ac-command" data-ac-cmd="${trimmed}">${cmd}</a>'`;
      });
    if (highlighted !== html) {
      body.innerHTML = highlighted;
    }
  });
  
  // Add click handlers for AC commands
  container.querySelectorAll('.news-ac-command').forEach(link => {
    link.addEventListener('click', (e) => {
      e.preventDefault();
      const cmd = link.dataset.acCmd;
      if (cmd) {
        showACModal(cmd);
      }
    });
  });
}

// Show modal with AC piece preview
function showACModal(cmd) {
  // Remove $ prefix if present for the URL
  const piece = cmd.startsWith('$') ? cmd.slice(1) : cmd;
  const acUrl = `https://aesthetic.computer/${piece}`;
  
  // Reuse the existing modal system
  openModal(acUrl);
}

// Hero media (no collapse functionality)
function setupHeroToggle() {
  // Hero media now shows below header, no toggle needed
}

// Initialize YouTube features
document.addEventListener('DOMContentLoaded', () => {
  initYouTubePlayer();
  setupTimecodeButton();
  linkifyTimecodes(document);
  highlightACCommands(document);
  setupHeroToggle();
});

// Also handle SPA navigation
const origRenderPageContent = window.renderPageContent;
if (typeof origRenderPageContent === 'function') {
  window.renderPageContent = function(...args) {
    const result = origRenderPageContent.apply(this, args);
    // Re-init after page content changes
    setTimeout(() => {
      initYouTubePlayer();
      setupTimecodeButton();
      linkifyTimecodes(document);
      highlightACCommands(document);
      setupHeroToggle();
    }, 100);
    return result;
  };
}

// Notify VS Code we're ready to receive session
if (isInVSCode()) {
  window.parent?.postMessage({ type: 'news:ready' }, '*');
}
