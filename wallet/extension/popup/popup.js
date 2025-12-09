// Keeps Wallet - Popup Script

// Views
const views = {
  welcome: document.getElementById('view-welcome'),
  locked: document.getElementById('view-locked'),
  main: document.getElementById('view-main'),
  create: document.getElementById('view-create'),
  import: document.getElementById('view-import'),
};

// Elements
const el = {
  network: document.getElementById('network'),
  balance: document.getElementById('balance'),
  balanceUsd: document.getElementById('balance-usd'),
  address: document.getElementById('address'),
  keepsGrid: document.getElementById('keeps-grid'),
  keepsEmpty: document.getElementById('keeps-empty'),
  unlockPassword: document.getElementById('unlock-password'),
  unlockError: document.getElementById('unlock-error'),
  createPassword: document.getElementById('create-password'),
  createPasswordConfirm: document.getElementById('create-password-confirm'),
  createStatus: document.getElementById('create-status'),
  importMnemonic: document.getElementById('import-mnemonic'),
  importPassword: document.getElementById('import-password'),
  importStatus: document.getElementById('import-status'),
};

// State
let currentView = 'welcome';
let tezPrice = null;

// Show a view
function showView(name) {
  Object.values(views).forEach(v => v.classList.remove('active'));
  views[name]?.classList.add('active');
  currentView = name;
}

// Send message to background
async function sendMessage(type, payload = {}) {
  return chrome.runtime.sendMessage({ type, payload });
}

// Initialize
async function init() {
  // Get wallet state
  const state = await sendMessage('KEEPS_GET_STATE');
  
  // Update network badge
  el.network.textContent = state.network?.toUpperCase() || 'GHOSTNET';
  el.network.classList.toggle('mainnet', state.network === 'mainnet');
  
  // Show appropriate view
  if (!state.exists) {
    showView('welcome');
  } else if (!state.unlocked) {
    showView('locked');
  } else {
    showView('main');
    await loadMainView(state);
  }
}

// Load main view data
async function loadMainView(state) {
  // Show address
  el.address.textContent = state.address || 'Unknown';
  
  // Fetch balance
  const balanceRes = await sendMessage('KEEPS_GET_BALANCE', { network: state.network });
  if (balanceRes.balance !== undefined) {
    el.balance.textContent = balanceRes.balance.toFixed(4);
    
    // Fetch TEZ price for USD conversion
    try {
      const priceRes = await fetch('https://api.tzkt.io/v1/quotes/last');
      tezPrice = await priceRes.json();
      const usd = (balanceRes.balance * tezPrice.usd).toFixed(2);
      el.balanceUsd.textContent = `$${usd} USD`;
    } catch (e) {
      el.balanceUsd.textContent = '';
    }
  }
  
  // Fetch keeps
  const keepsRes = await sendMessage('KEEPS_GET_KEEPS', { network: state.network });
  renderKeeps(keepsRes.keeps || []);
}

// Render keeps grid
function renderKeeps(keeps) {
  el.keepsGrid.innerHTML = '';
  
  if (keeps.length === 0) {
    el.keepsEmpty.style.display = 'block';
    return;
  }
  
  el.keepsEmpty.style.display = 'none';
  
  keeps.forEach(keep => {
    const item = document.createElement('div');
    item.className = 'keep-item';
    
    // Try to get thumbnail from metadata
    const thumbnail = keep.metadata?.thumbnailUri || keep.metadata?.displayUri;
    if (thumbnail) {
      const img = document.createElement('img');
      // Convert IPFS URI if needed
      img.src = thumbnail.replace('ipfs://', 'https://ipfs.io/ipfs/');
      img.alt = keep.metadata?.name || `Keep #${keep.id}`;
      item.appendChild(img);
    }
    
    el.keepsGrid.appendChild(item);
  });
}

// Event Listeners

// Welcome view
document.getElementById('btn-create').addEventListener('click', () => {
  showView('create');
});

document.getElementById('btn-import').addEventListener('click', () => {
  showView('import');
});

// Locked view
document.getElementById('btn-unlock').addEventListener('click', async () => {
  const password = el.unlockPassword.value;
  if (!password) return;
  
  const res = await sendMessage('KEEPS_UNLOCK', { password });
  
  if (res.error) {
    el.unlockError.textContent = res.error;
    el.unlockError.style.display = 'block';
  } else {
    el.unlockError.style.display = 'none';
    el.unlockPassword.value = '';
    await init(); // Reload state
  }
});

// Main view - lock button
document.getElementById('btn-lock').addEventListener('click', async () => {
  await sendMessage('KEEPS_LOCK');
  showView('locked');
});

// Create wallet view
document.getElementById('btn-create-confirm').addEventListener('click', async () => {
  const password = el.createPassword.value;
  const confirm = el.createPasswordConfirm.value;
  
  if (!password || password.length < 8) {
    showStatus('create', 'Password must be at least 8 characters', 'error');
    return;
  }
  
  if (password !== confirm) {
    showStatus('create', 'Passwords do not match', 'error');
    return;
  }
  
  const res = await sendMessage('KEEPS_CREATE_WALLET', { password });
  
  if (res.error) {
    showStatus('create', res.error, 'error');
  } else if (res.mnemonic) {
    // TODO: Show mnemonic to user for backup
    showStatus('create', 'Wallet created! (Show seed phrase here)', 'success');
    setTimeout(() => init(), 2000);
  } else {
    showStatus('create', res.message || 'Wallet created', 'success');
  }
});

document.getElementById('btn-create-back').addEventListener('click', () => {
  showView('welcome');
});

// Import wallet view
document.getElementById('btn-import-confirm').addEventListener('click', async () => {
  const mnemonic = el.importMnemonic.value.trim();
  const password = el.importPassword.value;
  
  const words = mnemonic.split(/\s+/);
  if (words.length !== 24) {
    showStatus('import', 'Seed phrase must be 24 words', 'error');
    return;
  }
  
  if (!password || password.length < 8) {
    showStatus('import', 'Password must be at least 8 characters', 'error');
    return;
  }
  
  const res = await sendMessage('KEEPS_IMPORT_WALLET', { mnemonic, password });
  
  if (res.error) {
    showStatus('import', res.error, 'error');
  } else {
    showStatus('import', 'Wallet imported successfully!', 'success');
    setTimeout(() => init(), 2000);
  }
});

document.getElementById('btn-import-back').addEventListener('click', () => {
  showView('welcome');
});

// Helper to show status messages
function showStatus(view, message, type) {
  const statusEl = document.getElementById(`${view}-status`);
  statusEl.textContent = message;
  statusEl.className = `status ${type}`;
  statusEl.style.display = 'block';
}

// Enter key handlers
el.unlockPassword.addEventListener('keyup', (e) => {
  if (e.key === 'Enter') document.getElementById('btn-unlock').click();
});

// Listen for lock events
chrome.runtime.onMessage.addListener((message) => {
  if (message.type === 'KEEPS_LOCKED' && currentView === 'main') {
    showView('locked');
  }
});

// Initialize on load
init();
