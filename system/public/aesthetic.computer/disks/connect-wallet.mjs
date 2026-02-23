// Connect Wallet (CLI Flow), 2024.12.14
// Simple wallet connection page for CLI tools.
// Opens from CLI, connects wallet, saves to profile, shows success.

/* #region 📚 README 
  Used by ac-keeps CLI when user runs `wallet connect`.
  
  Flow:
  1. CLI opens browser to /connect-wallet?session=xxx
  2. User clicks to connect Temple/Kukai
  3. Address saved to their AC profile
  4. Page shows success with address
  5. CLI polls profile or reads session result
#endregion */

let walletState = null;
let connecting = false;
let error = null;
let session = null;
let done = false;

const colors = {
  bg: [8, 12, 24],
  primary: [0, 180, 255],
  text: [200, 210, 230],
  textDim: [80, 100, 130],
  positive: [50, 220, 100],
  negative: [255, 80, 100],
};

async function boot({ wallet, wipe, params, net }) {
  wipe(colors.bg);
  
  // Get session ID from params
  session = params[0] || null;
  
  // Check current wallet state
  wallet.sync();
  walletState = wallet.get();
  
  // If already connected, we're done
  if (walletState?.connected) {
    done = true;
    // Save to profile
    await saveToProfile(walletState.address, walletState.network, net);
  }
}

function paint({ wipe, ink, write, screen, ui }) {
  wipe(colors.bg);
  
  const cx = screen.width / 2;
  const cy = screen.height / 2;
  
  // Title
  ink(colors.primary).write("🔷 Connect Tezos Wallet", { x: cx, y: 40, center: "xy" });
  
  if (done && walletState?.connected) {
    // Success state
    ink(colors.positive).write("✅ Wallet Connected!", { x: cx, y: cy - 40, center: "xy" });
    ink(colors.text).write(shortAddress(walletState.address), { x: cx, y: cy, center: "xy" });
    ink(colors.textDim).write("You can close this window", { x: cx, y: cy + 40, center: "xy" });
    ink(colors.textDim).write("and return to the terminal", { x: cx, y: cy + 60, center: "xy" });
  } else if (connecting) {
    // Connecting state
    ink(colors.primary).write("Connecting...", { x: cx, y: cy, center: "xy" });
    ink(colors.textDim).write("Check your wallet", { x: cx, y: cy + 30, center: "xy" });
  } else if (error) {
    // Error state
    ink(colors.negative).write("❌ " + error, { x: cx, y: cy - 20, center: "xy" });
    ui.button("Try Again", { x: cx - 60, y: cy + 30, w: 120 });
  } else {
    // Initial state - show connect button
    ui.button("🔷 Connect Wallet", { x: cx - 80, y: cy - 10, w: 160 });
  }
  
  // Session indicator
  if (session) {
    ink(colors.textDim).write(`Session: ${session.slice(0, 8)}...`, { x: 10, y: screen.height - 20 });
  }
}

async function act({ event: e, wallet, net }) {
  if (e.is("touch")) {
    if (e.button?.label === "🔷 Connect Wallet") {
      await connectWallet(wallet, net);
    } else if (e.button?.label === "Try Again") {
      error = null;
    }
  }
}

async function connectWallet(wallet, net) {
  connecting = true;
  error = null;

  try {
    const network = "mainnet";
    await wallet.connect({ network });

    wallet.sync();
    walletState = wallet.get();

    if (walletState?.connected) {
      await saveToProfile(walletState.address, network, net);
      done = true;
    }
  } catch (err) {
    error = err.message || "Connection failed";
  }

  connecting = false;
}

async function saveToProfile(address, network, net) {
  try {
    // Use the existing bios function if available
    if (typeof window !== 'undefined' && window.acTezos?.updateUserTezosAddress) {
      await window.acTezos.updateUserTezosAddress(address, network);
      console.log("✅ Saved to profile via bios");
    } else {
      // Fallback to API call
      const token = localStorage.getItem("ac-token");
      if (token) {
        const response = await fetch("/api/update-tezos-address", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
          },
          body: JSON.stringify({ address, network }),
        });
        if (response.ok) {
          console.log("✅ Saved to profile via API");
        }
      }
    }
    
    // Also save session result if we have a session ID
    if (session) {
      // Could store in localStorage for CLI to read via polling endpoint
      localStorage.setItem(`ac-wallet-session-${session}`, JSON.stringify({
        address,
        network,
        timestamp: Date.now(),
      }));
    }
  } catch (err) {
    console.log("⚠️ Could not save to profile:", err.message);
  }
}

function shortAddress(addr) {
  if (!addr) return "";
  return `${addr.slice(0, 8)}...${addr.slice(-6)}`;
}

export { boot, paint, act };
