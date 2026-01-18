// News client, 2026.01.15

const config = {
  domain: "aesthetic.us.auth0.com",
  clientId: "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt",
  audience: "https://aesthetic.us.auth0.com/api/v2/",
};

let auth0Client = null;
let acUser = null;
let acToken = null;
let acHandle = null;

// DOM elements - initialized after DOM ready
let loginBtn, signupBtn, userMenu, userHandle, logoutBtn;

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

async function applySession(session) {
  console.log("[news] applySession called:", session);
  if (!session || typeof session !== "object") {
    // Session cleared - user logged out
    acUser = null;
    acToken = null;
    acHandle = null;
    updateAuthUI();
    return;
  }
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
    updateAuthUI();
    return;
  }
  
  if (!auth0Client) return;
  acUser = null;
  acToken = null;
  acHandle = null;
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
  if (logoutBtn) logoutBtn.addEventListener("click", (e) => {
    e.preventDefault();
    acLogout();
  });
}

function attachSessionListener() {
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
        handleFormSubmit(form, "/api/news/comment");
      } else if (action === "vote") {
        handleFormSubmit(form, "/api/news/vote");
      }
    });
  });
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
  // In VS Code, open modals externally too (iframes can be problematic)
  if (isInVSCode()) {
    openExternal(url);
    return;
  }
  
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
initDOMRefs();
attachAuthHandlers();
initForms();
initModals();
initSubmitFormConstraints();
attachSessionListener();
applySession(decodeSessionParam());
initAuth0();
connectToSessionServer(); // Start WebSocket connection for live reload

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
      eitherOr.classList.toggle('has-content', hasUrl || hasText);
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

// Notify VS Code we're ready to receive session
if (isInVSCode()) {
  window.parent?.postMessage({ type: 'news:ready' }, '*');
}
