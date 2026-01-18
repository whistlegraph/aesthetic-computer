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
  if (!session || typeof session !== "object") return;
  if (session.accessToken) acToken = session.accessToken;

  const label = session.account?.label;
  if (label) {
    if (label.startsWith("@")) {
      acHandle = label.slice(1);
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
  if (acUser) {
    if (loginBtn) loginBtn.style.display = "none";
    if (signupBtn) signupBtn.style.display = "none";
    if (userMenu) userMenu.style.display = "flex";
    if (userHandle) userHandle.textContent = acHandle ? `@${acHandle}` : acUser.email;
  } else {
    if (loginBtn) loginBtn.style.display = "inline-flex";
    if (signupBtn) signupBtn.style.display = "inline-flex";
    if (userMenu) userMenu.style.display = "none";
  }
}

async function acLogin() {
  console.log("[news] acLogin called, auth0Client:", !!auth0Client);
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
  console.log("[news] acSignup called");
  if (!auth0Client) await initAuth0();
  if (!auth0Client) return;
  await auth0Client.loginWithRedirect({
    authorizationParams: { screen_hint: "signup" },
  });
}

async function acLogout() {
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

async function handleFormSubmit(form, endpoint) {
  if (!acToken) {
    alert("Please log in to post.");
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
    window.location.href = data.redirect;
    return;
  }
  if (!res.ok) {
    alert(data?.error || "Request failed");
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
         (window.acVSCODE && window.parent !== window);
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
attachSessionListener();
applySession(decodeSessionParam());
initAuth0();
connectToSessionServer(); // Start WebSocket connection for live reload
