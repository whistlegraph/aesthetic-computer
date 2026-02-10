// Kept, 2024.12.15
// Shows the result of a KidLisp Keep mint operation.

/* #region 📚 README 
  Displays success/failure after minting a Keep NFT.
  Receives data via store["keep:result"] set by prompt.mjs
  Or loads from API when accessed via kept:$piece or kept~$piece
#endregion */

import * as starfield from "./starfield.mjs";

// Keeps contract (mainnet staging v4)
const CONTRACT = "KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W";
const NETWORK = "mainnet";

let result = null;
let viewButton, backButton, txButton, objktButton;
let blinkAlt = false;
let loading = false;

async function boot({ api, hud, store, ui, blink, params, needsPaint }) {
  hud.labelBack();
  
  // Get result from store (set by prompt.mjs before jumping here)
  result = store["keep:result"];
  
  // Also check URL params for piece code (kept~$xyz or kept:$xyz)
  if (!result && params[0]) {
    const pieceCode = params[0].startsWith("$") ? params[0].slice(1) : params[0];
    
    // Try to load from API
    loading = true;
    hud.label(`loading $${pieceCode}...`);
    
    try {
      const networkPrefix = NETWORK === "mainnet" ? "" : "ghostnet.";
      // Search for token with this piece name in metadata
      const tokensRes = await fetch(
        `https://api.${networkPrefix}tzkt.io/v1/tokens?contract=${CONTRACT}&metadata.name.as=*${pieceCode}*&limit=1`
      );
      
      if (tokensRes.ok) {
        const tokens = await tokensRes.json();
        if (tokens.length > 0) {
          const token = tokens[0];
          // Found the token!
          result = {
            success: true,
            piece: pieceCode,
            tokenId: token.tokenId,
            contract: CONTRACT,
            network: NETWORK,
            // Extract owner from token holders if available
            owner: token.holders?.[0]?.address,
          };
        }
      }
    } catch (e) {
      console.warn("Failed to fetch token:", e);
    }
    
    loading = false;
    
    // Fallback if not found
    if (!result) {
      result = { 
        success: false, 
        piece: pieceCode,
        error: `$${pieceCode} has not been kept yet.` 
      };
    }
  }
  
  if (!result) {
    result = { success: false, error: "No mint data found" };
  }
  
  // Set HUD label
  if (result.success) {
    hud.label(`kept $${result.piece}`);
  } else {
    hud.label(`keep failed`);
  }
  
  starfield.boot(api, { stars: 256 });
  starfield.wipe(false);
  
  // Create buttons
  if (result.success) {
    // Only show "View on tzkt" if we have a token ID
    if (result.tokenId !== undefined && result.tokenId !== null) {
      viewButton = new ui.TextButton("tzkt");
      objktButton = new ui.TextButton("objkt");
    }
    if (result.txHash) {
      txButton = new ui.TextButton("Transaction");
    }
  }
  backButton = new ui.TextButton("Back");
  
  blink(120, () => (blinkAlt = !blinkAlt));
  needsPaint();
}

function paint($) {
  const { wipe, ink, screen } = $;
  
  // Starfield background
  const bg = result?.success ? [0, 20, 10] : (loading ? [10, 10, 20] : [30, 10, 10]);
  wipe(...bg);
  starfield.paint($);
  
  const cx = screen.width / 2;
  const cy = screen.height / 2;
  
  if (loading) {
    ink(100, 150, 255).write("Loading...", { x: cx, y: cy, center: "xy" });
    return;
  }
  
  if (result?.success) {
    // Success state
    const titleColor = blinkAlt ? [0, 255, 100] : [100, 255, 150];
    ink(...titleColor).write("KEPT!", { x: cx, y: cy - 60, center: "xy" }, "large");
    
    // Piece name
    ink(0, 200, 255).write(`$${result.piece}`, { x: cx, y: cy - 35, center: "xy" });
    
    // Fee paid (only show if we have it)
    if (result.fee) {
      ink(200, 200, 200).write(`Fee: ${result.fee}ꜩ`, { x: cx, y: cy - 15, center: "xy" });
    }
    
    // Token info if available
    if (result.tokenId !== undefined && result.tokenId !== null) {
      ink(150, 150, 150).write(`Token #${result.tokenId}`, { x: cx, y: cy + 5, center: "xy" });
    }
    
    // Owner address (truncated)
    if (result.owner) {
      const shortAddr = `${result.owner.slice(0, 8)}...${result.owner.slice(-6)}`;
      ink(100, 150, 200).write(`Owner: ${shortAddr}`, { x: cx, y: cy + 25, center: "xy" });
    }
    
    // Buttons row 1: tzkt and objkt side by side
    let btnY = cy + 55;
    if (viewButton && objktButton) {
      viewButton.reposition({ x: cx - 40, y: btnY, center: "xy", screen });
      viewButton.paint($,
        [[20, 60, 40], [0, 200, 100], [0, 255, 150], [20, 60, 40]],
        [[40, 100, 60], [0, 255, 150], [255, 255, 255], [40, 100, 60]]
      );
      
      objktButton.reposition({ x: cx + 40, y: btnY, center: "xy", screen });
      objktButton.paint($,
        [[60, 20, 60], [200, 50, 200], [255, 100, 255], [60, 20, 60]],
        [[100, 40, 100], [255, 100, 255], [255, 255, 255], [100, 40, 100]]
      );
      btnY += 25;
    }
    
    // Transaction button (only if we have txHash)
    if (txButton) {
      txButton.reposition({ x: cx, y: btnY, center: "xy", screen });
      txButton.paint($,
        [[20, 40, 60], [0, 150, 200], [0, 200, 255], [20, 40, 60]],
        [[40, 60, 100], [0, 200, 255], [255, 255, 255], [40, 60, 100]]
      );
    }
    
  } else {
    // Failure state
    const titleColor = blinkAlt ? [255, 50, 50] : [255, 100, 100];
    ink(...titleColor).write("KEEP FAILED", { x: cx, y: cy - 40, center: "xy" }, "large");
    
    // Error message (wrap if long)
    const error = result?.error || "Unknown error";
    const maxWidth = screen.width - 40;
    
    // Simple word wrap
    const words = error.split(" ");
    let lines = [];
    let currentLine = "";
    
    for (const word of words) {
      const testLine = currentLine ? `${currentLine} ${word}` : word;
      if (testLine.length * 6 > maxWidth && currentLine) {
        lines.push(currentLine);
        currentLine = word;
      } else {
        currentLine = testLine;
      }
    }
    if (currentLine) lines.push(currentLine);
    
    // Draw error lines
    let errorY = cy - 10;
    for (const line of lines.slice(0, 4)) { // Max 4 lines
      ink(255, 150, 150).write(line, { x: cx, y: errorY, center: "xy" });
      errorY += 14;
    }
  }
  
  // Back button always shown
  backButton.reposition({ x: cx, y: screen.height - 30, center: "xy", screen });
  backButton.paint(
    $,
    [[40, 40, 40], [150, 150, 150], [200, 200, 200], [40, 40, 40]],
    [[60, 60, 60], [200, 200, 200], [255, 255, 255], [60, 60, 60]]
  );
}

function sim($) {
  starfield.sim($);
}

function act({ event: e, jump, store, send, net }) {
  // Helper to open external URLs
  const openUrl = (url) => {
    if (net?.iframe) {
      send({ type: "post-to-parent", content: { type: "openExternal", url } });
    } else {
      jump(url);
    }
  };
  
  const networkPrefix = (result?.network || NETWORK) === "mainnet" ? "" : "ghostnet.";
  
  // Button interactions
  if (result?.success && viewButton) {
    viewButton.btn.act(e, {
      push: () => {
        // Open tzkt token page
        const url = `https://${networkPrefix}tzkt.io/${result.contract || CONTRACT}/tokens/${result.tokenId}`;
        openUrl(url);
      }
    });
  }
  
  if (result?.success && objktButton) {
    objktButton.btn.act(e, {
      push: () => {
        // Open objkt token page
        const url = `https://${networkPrefix}objkt.com/tokens/${result.contract || CONTRACT}/${result.tokenId}`;
        openUrl(url);
      }
    });
  }
  
  if (result?.success && txButton) {
    txButton.btn.act(e, {
      push: () => {
        // Open transaction page
        const url = `https://${networkPrefix}tzkt.io/${result.txHash}`;
        openUrl(url);
      }
    });
  }
  
  backButton.btn.act(e, {
    push: () => {
      // Clear result and go back to prompt
      delete store["keep:result"];
      jump("prompt");
    }
  });
}

function leave({ store }) {
  // Clean up store data when leaving
  delete store["keep:result"];
}

export { boot, paint, sim, act, leave };

export const nohud = true;
