// Keeps Wallet — Popup Script
// A living gallery of keeps (generative art NFTs).

// ─── State ───────────────────────────────────────────
let walletState = null; // { exists, unlocked, address, publicKey, network }
let keeps = [];
let balanceVisible = false;
let balanceValue = null;
let currentImportTab = "seed"; // "seed" | "key"
let pendingMnemonic = null; // set after wallet creation, cleared after backup

// ─── Helpers ─────────────────────────────────────────

function sendMessage(type, payload = {}) {
  return chrome.runtime.sendMessage({ type, payload });
}

function ipfsToHttp(uri) {
  if (!uri) return null;
  if (uri.startsWith("ipfs://")) return "https://ipfs.io/ipfs/" + uri.slice(7);
  if (uri.startsWith("https://") || uri.startsWith("http://")) return uri;
  return null;
}

// Show one of the three state panels, with a fade transition.
function showState(name) {
  const states = document.querySelectorAll(".state");
  states.forEach((el) => {
    el.classList.remove("active", "visible");
  });
  const target = document.getElementById("state-" + name);
  if (!target) return;
  target.classList.add("active");
  // Trigger reflow so the opacity transition fires
  void target.offsetWidth;
  target.classList.add("visible");
}

// ─── Initialise ──────────────────────────────────────

async function init() {
  walletState = await sendMessage("KEEPS_GET_STATE");

  if (!walletState.exists) {
    showState("no-wallet");
    return;
  }
  if (!walletState.unlocked) {
    showState("locked");
    document.getElementById("unlock-password").value = "";
    document.getElementById("unlock-password").classList.remove("error");
    // Focus the password field after a brief delay (side-panel readiness)
    setTimeout(() => document.getElementById("unlock-password").focus(), 80);
    return;
  }

  // Unlocked
  showState("unlocked");
  updateNetworkPill();
  loadKeeps();
  loadBalance();
  loadConnectedDapps();
}

// ─── Network ─────────────────────────────────────────

function updateNetworkPill() {
  const pill = document.getElementById("network-pill");
  pill.textContent = walletState.network || "ghostnet";
}

document.getElementById("network-pill").addEventListener("click", async () => {
  const next = walletState.network === "mainnet" ? "ghostnet" : "mainnet";
  const res = await sendMessage("KEEPS_SET_NETWORK", { network: next });
  if (res.success) {
    walletState.network = next;
    updateNetworkPill();
    // Reload data for new network
    renderLoading();
    loadKeeps();
    loadBalance();
  }
});

// ─── Balance ─────────────────────────────────────────

async function loadBalance() {
  const res = await sendMessage("KEEPS_GET_BALANCE", {
    network: walletState.network,
  });
  if (res.balance !== undefined && res.balance !== null) {
    balanceValue = res.balance;
  } else {
    balanceValue = null;
  }
  renderBalanceToggle();
}

function renderBalanceToggle() {
  const btn = document.getElementById("balance-toggle");
  if (balanceVisible && balanceValue !== null) {
    btn.innerHTML = '$<span class="balance-amount">\u{A729} ' + balanceValue.toFixed(2) + "</span>";
  } else {
    btn.textContent = "$";
  }
}

document.getElementById("balance-toggle").addEventListener("click", () => {
  balanceVisible = !balanceVisible;
  renderBalanceToggle();
});

// ─── Keeps ───────────────────────────────────────────

function renderLoading() {
  const container = document.getElementById("keeps-container");
  container.innerHTML = '<div class="loading-indicator"><span class="loading-dot"></span><span class="loading-dot"></span><span class="loading-dot"></span></div>';
}

async function loadKeeps() {
  renderLoading();
  const res = await sendMessage("KEEPS_GET_KEEPS", {
    network: walletState.network,
  });
  keeps = res.keeps || [];
  renderKeeps();
}

function renderKeeps() {
  const container = document.getElementById("keeps-container");
  container.innerHTML = "";

  if (keeps.length === 0) {
    container.innerHTML =
      '<div class="keeps-empty">No keeps yet<a href="https://keeps.kidlisp.com" target="_blank" rel="noopener">Browse keeps</a></div>';
    return;
  }

  const grid = document.createElement("div");
  grid.className = "keeps-grid";

  keeps.forEach((keep, index) => {
    const thumb = document.createElement("div");
    thumb.className = "keep-thumb";

    const imgUrl = ipfsToHttp(keep.thumbnailUri) || ipfsToHttp(keep.displayUri);
    if (imgUrl) {
      const img = document.createElement("img");
      img.src = imgUrl;
      img.alt = keep.name || "Keep #" + keep.id;
      img.loading = "lazy";
      img.onerror = () => {
        // Replace broken image with a colored placeholder
        img.style.display = "none";
      };
      thumb.appendChild(img);
    }

    const nameOverlay = document.createElement("div");
    nameOverlay.className = "keep-name-overlay";
    nameOverlay.textContent = keep.name || "Keep #" + keep.id;
    thumb.appendChild(nameOverlay);

    thumb.addEventListener("click", () => openKeepDetail(keep));
    grid.appendChild(thumb);
  });

  container.appendChild(grid);
}

// ─── Keep Detail Overlay ─────────────────────────────

function openKeepDetail(keep) {
  const overlay = document.getElementById("keep-detail-overlay");
  const img = document.getElementById("keep-detail-image");
  const title = document.getElementById("keep-detail-title");
  const artist = document.getElementById("keep-detail-artist");

  const imgUrl =
    ipfsToHttp(keep.displayUri) || ipfsToHttp(keep.thumbnailUri) || "";
  img.src = imgUrl;
  img.alt = keep.name || "Keep";
  title.textContent = keep.name || "Keep #" + keep.id;
  artist.textContent = keep.description || "";

  overlay.classList.add("open");
  void overlay.offsetWidth;
  overlay.classList.add("visible");
}

function closeKeepDetail() {
  const overlay = document.getElementById("keep-detail-overlay");
  overlay.classList.remove("visible");
  setTimeout(() => overlay.classList.remove("open"), 200);
}

document
  .getElementById("keep-detail-overlay")
  .addEventListener("click", (e) => {
    // Close when clicking the backdrop (not the detail card itself)
    if (e.target === e.currentTarget) {
      closeKeepDetail();
    }
  });

// Also close on Escape
document.addEventListener("keydown", (e) => {
  if (e.key === "Escape") {
    closeKeepDetail();
    closeSeedOverlay();
  }
});

// ─── Lock / Unlock ───────────────────────────────────

document.getElementById("btn-lock").addEventListener("click", async () => {
  await sendMessage("KEEPS_LOCK");
  showState("locked");
  setTimeout(() => document.getElementById("unlock-password").focus(), 80);
});

// Unlock on Enter
document.getElementById("unlock-password").addEventListener("keydown", (e) => {
  if (e.key === "Enter") attemptUnlock();
});

// Unlock on icon click
document.getElementById("btn-unlock").addEventListener("click", attemptUnlock);

async function attemptUnlock() {
  const input = document.getElementById("unlock-password");
  const password = input.value;
  if (!password) return;

  const res = await sendMessage("KEEPS_UNLOCK", { password });
  if (res.error) {
    input.classList.add("error");
    setTimeout(() => input.classList.remove("error"), 400);
    input.value = "";
    input.focus();
  } else {
    input.value = "";
    await init();
  }
}

// Listen for background lock events
chrome.runtime.onMessage.addListener((message) => {
  if (message.type === "KEEPS_LOCKED") {
    showState("locked");
  }
});

// ─── Create Wallet ───────────────────────────────────

document.getElementById("btn-show-create").addEventListener("click", () => {
  document.getElementById("create-form").classList.add("open");
  document.getElementById("import-form").classList.remove("open");
  document.getElementById("create-error").textContent = "";
  setTimeout(() => document.getElementById("create-password").focus(), 100);
});

document.getElementById("btn-create-wallet").addEventListener("click", async () => {
  const pw = document.getElementById("create-password").value;
  const confirm = document.getElementById("create-password-confirm").value;
  const errorEl = document.getElementById("create-error");
  const btn = document.getElementById("btn-create-wallet");

  errorEl.textContent = "";

  if (!pw || pw.length < 8) {
    errorEl.textContent = "Password must be at least 8 characters.";
    return;
  }
  if (pw !== confirm) {
    errorEl.textContent = "Passwords do not match.";
    return;
  }

  btn.disabled = true;
  btn.textContent = "Creating...";

  const res = await sendMessage("KEEPS_CREATE_WALLET", { password: pw });

  btn.disabled = false;
  btn.textContent = "Create Wallet";

  if (res.error) {
    errorEl.textContent = res.error;
    return;
  }

  // Show seed phrase backup overlay
  if (res.mnemonic) {
    pendingMnemonic = res.mnemonic;
    showSeedOverlay(res.mnemonic);
  } else {
    // No mnemonic returned (shouldn't happen, but handle gracefully)
    await init();
  }
});

// Enter to submit create form
document.getElementById("create-password-confirm").addEventListener("keydown", (e) => {
  if (e.key === "Enter") document.getElementById("btn-create-wallet").click();
});

// ─── Import Wallet ───────────────────────────────────

document.getElementById("btn-show-import").addEventListener("click", () => {
  document.getElementById("import-form").classList.add("open");
  document.getElementById("create-form").classList.remove("open");
  document.getElementById("import-error").textContent = "";
  setTimeout(() => document.getElementById("import-mnemonic").focus(), 100);
});

// Import tabs
document.querySelectorAll("[data-import-tab]").forEach((tab) => {
  tab.addEventListener("click", () => {
    currentImportTab = tab.dataset.importTab;
    document.querySelectorAll("[data-import-tab]").forEach((t) => t.classList.remove("active"));
    tab.classList.add("active");
    document.querySelectorAll(".import-tab-content").forEach((c) => c.classList.remove("active"));
    document.getElementById("import-" + currentImportTab + "-tab").classList.add("active");
  });
});

document.getElementById("btn-import-wallet").addEventListener("click", async () => {
  const password = document.getElementById("import-password").value;
  const errorEl = document.getElementById("import-error");
  const btn = document.getElementById("btn-import-wallet");

  errorEl.textContent = "";

  if (!password || password.length < 8) {
    errorEl.textContent = "Password must be at least 8 characters.";
    return;
  }

  btn.disabled = true;
  btn.textContent = "Importing...";

  let res;
  if (currentImportTab === "seed") {
    const mnemonic = document.getElementById("import-mnemonic").value.trim();
    const wordCount = mnemonic.split(/\s+/).filter(Boolean).length;
    if (wordCount !== 12 && wordCount !== 24) {
      errorEl.textContent = "Seed phrase must be 12 or 24 words.";
      btn.disabled = false;
      btn.textContent = "Import Wallet";
      return;
    }
    res = await sendMessage("KEEPS_IMPORT_WALLET", { mnemonic, password });
  } else {
    const privateKey = document.getElementById("import-private-key").value.trim();
    if (!privateKey.startsWith("edsk")) {
      errorEl.textContent = 'Private key must start with "edsk".';
      btn.disabled = false;
      btn.textContent = "Import Wallet";
      return;
    }
    res = await sendMessage("KEEPS_IMPORT_PRIVATE_KEY", { privateKey, password });
  }

  btn.disabled = false;
  btn.textContent = "Import Wallet";

  if (res.error) {
    errorEl.textContent = res.error;
    return;
  }

  await init();
});

// Enter to submit import form
document.getElementById("import-password").addEventListener("keydown", (e) => {
  if (e.key === "Enter") document.getElementById("btn-import-wallet").click();
});

// ─── Seed Phrase Backup Overlay ──────────────────────

function showSeedOverlay(mnemonic) {
  const overlay = document.getElementById("seed-overlay");
  const grid = document.getElementById("seed-grid");

  const words = mnemonic.split(" ");
  grid.innerHTML = "";

  words.forEach((word, i) => {
    const el = document.createElement("div");
    el.className = "seed-word";
    el.innerHTML = '<span class="seed-word-num">' + (i + 1) + ".</span> " + word;
    grid.appendChild(el);
  });

  overlay.classList.add("open");
  void overlay.offsetWidth;
  overlay.classList.add("visible");
}

function closeSeedOverlay() {
  const overlay = document.getElementById("seed-overlay");
  if (!overlay.classList.contains("open")) return;
  overlay.classList.remove("visible");
  pendingMnemonic = null;
  setTimeout(() => {
    overlay.classList.remove("open");
    init();
  }, 200);
}

document.getElementById("seed-dismiss").addEventListener("click", closeSeedOverlay);

// ─── Connected dApps ─────────────────────────────────

async function loadConnectedDapps() {
  // Connected dApps are stored by the beacon handler in background.
  // For now we read from storage; if beacon_peers doesn't exist, show nothing.
  try {
    const { beacon_peers } = await chrome.storage.local.get("beacon_peers");
    const dotsContainer = document.getElementById("dapp-dots");
    dotsContainer.innerHTML = "";

    if (!beacon_peers || Object.keys(beacon_peers).length === 0) return;

    Object.entries(beacon_peers).forEach(([id, peer]) => {
      const dot = document.createElement("div");
      dot.className = "dapp-dot";
      dot.title = peer.name || id;

      const tooltip = document.createElement("div");
      tooltip.className = "dapp-tooltip";
      tooltip.innerHTML =
        '<span>' + (peer.name || id) + '</span><span class="disconnect-link">Disconnect</span>';

      tooltip.querySelector(".disconnect-link").addEventListener("click", async (e) => {
        e.stopPropagation();
        // Remove this peer
        const current = (await chrome.storage.local.get("beacon_peers")).beacon_peers || {};
        delete current[id];
        await chrome.storage.local.set({ beacon_peers: current });
        loadConnectedDapps();
      });

      dot.addEventListener("click", (e) => {
        e.stopPropagation();
        // Toggle tooltip
        const isShown = tooltip.classList.contains("show");
        document.querySelectorAll(".dapp-tooltip").forEach((t) => t.classList.remove("show"));
        if (!isShown) tooltip.classList.add("show");
      });

      dot.appendChild(tooltip);
      dotsContainer.appendChild(dot);
    });
  } catch (e) {
    // Storage access may fail in some contexts; ignore
  }
}

// Close tooltips when clicking elsewhere
document.addEventListener("click", () => {
  document.querySelectorAll(".dapp-tooltip").forEach((t) => t.classList.remove("show"));
});

// ─── Boot ────────────────────────────────────────────

init();
