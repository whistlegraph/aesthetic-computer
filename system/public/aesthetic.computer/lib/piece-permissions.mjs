// piece-permissions.mjs
// Permission prompt system for untrusted pieces requesting sensitive APIs
// Works in both main thread (DOM modal) and worker context (routes to bios.mjs)

// Store granted permissions per piece (session-based)
const grantedPermissions = new Map(); // pieceCode -> Set<permission>

// Pending permission requests waiting for bios.mjs response (worker context)
const pendingRequests = new Map(); // requestId -> { resolve }
let nextRequestId = 0;

const isWorker = typeof document === "undefined";

/**
 * Request permission from the user for a sensitive operation
 * @param {string} pieceCode - The piece requesting permission
 * @param {string} permission - Permission type: 'network', 'auth', 'storage-full', 'upload-external', 'navigate-external'
 * @param {object} details - Additional context for the prompt
 * @returns {Promise<boolean>} - true if granted, false if denied
 */
export async function requestPermission(pieceCode, permission, details = {}) {
  // Check if already granted this session
  const piecePerms = grantedPermissions.get(pieceCode);
  if (piecePerms?.has(permission)) {
    return true;
  }

  // Show permission prompt (routes through bios.mjs in worker context)
  const granted = await showPermissionPrompt(pieceCode, permission, details);

  if (granted) {
    // Store granted permission
    if (!grantedPermissions.has(pieceCode)) {
      grantedPermissions.set(pieceCode, new Set());
    }
    grantedPermissions.get(pieceCode).add(permission);
  }

  return granted;
}

/**
 * Check if a permission has been granted (without prompting)
 */
export function hasPermission(pieceCode, permission) {
  return grantedPermissions.get(pieceCode)?.has(permission) ?? false;
}

/**
 * Revoke a permission for a piece
 */
export function revokePermission(pieceCode, permission) {
  grantedPermissions.get(pieceCode)?.delete(permission);
}

/**
 * Clear all permissions for a piece
 */
export function clearPermissions(pieceCode) {
  grantedPermissions.delete(pieceCode);
}

/**
 * Resolve a pending permission request from bios.mjs (called from disk.mjs)
 */
export function resolvePermissionRequest(requestId, granted) {
  const pending = pendingRequests.get(requestId);
  if (pending) {
    pending.resolve(granted);
    pendingRequests.delete(requestId);
  }
}

/**
 * Show a permission prompt - routes to bios.mjs via postMessage in worker context
 */
async function showPermissionPrompt(pieceCode, permission, details) {
  if (isWorker) {
    // In worker: send request to bios.mjs and wait for response
    const requestId = nextRequestId++;
    return new Promise((resolve) => {
      pendingRequests.set(requestId, { resolve });
      postMessage({
        type: "permission-request",
        content: { requestId, pieceCode, permission, details },
      });
    });
  }

  // In main thread: show DOM modal directly
  return showDOMPermissionPrompt(pieceCode, permission, details);
}

/**
 * Show a DOM-based permission modal (main thread only)
 */
function showDOMPermissionPrompt(pieceCode, permission, details) {
  return new Promise((resolve) => {
    const messages = {
      network: {
        title: "Network Access",
        message: `The piece "${pieceCode}" wants to make network requests.`,
        warning: "This allows the piece to send data to external servers.",
        details: details.url ? `URL: ${details.url}` : null,
      },
      auth: {
        title: "Authentication Access",
        message: `The piece "${pieceCode}" wants to access your authentication tokens.`,
        warning: "This gives the piece access to your account credentials.",
        details: null,
      },
      "storage-full": {
        title: "Full Storage Access",
        message: `The piece "${pieceCode}" wants to access all stored data.`,
        warning: "This allows the piece to read data from other pieces.",
        details: null,
      },
      "upload-external": {
        title: "External Upload",
        message: `The piece "${pieceCode}" wants to upload a file to an external server.`,
        warning: "The file will be sent outside aesthetic.computer.",
        details: details.url ? `Destination: ${details.url}` : null,
      },
      "navigate-external": {
        title: "External Navigation",
        message: `The piece "${pieceCode}" wants to navigate to an external URL.`,
        warning: "You will leave aesthetic.computer.",
        details: details.url ? `URL: ${details.url}` : null,
      },
    };

    const config = messages[permission] || {
      title: "Permission Request",
      message: `The piece "${pieceCode}" wants permission for: ${permission}`,
      warning: null,
      details: null,
    };

    // Create modal
    const modal = document.createElement("div");
    modal.style.cssText = `
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background: rgba(0, 0, 0, 0.8);
      display: flex;
      align-items: center;
      justify-content: center;
      z-index: 100000;
      font-family: sans-serif;
    `;

    const dialog = document.createElement("div");
    dialog.style.cssText = `
      background: rgb(235, 235, 235);
      color: black;
      padding: 24px;
      max-width: 400px;
      border: 2px solid black;
    `;

    const title = document.createElement("h2");
    title.textContent = config.title;
    title.style.cssText = "margin: 0 0 16px 0; font-size: 18px; font-weight: bold;";

    const message = document.createElement("p");
    message.textContent = config.message;
    message.style.cssText = "margin: 0 0 12px 0; line-height: 1.4;";

    dialog.appendChild(title);
    dialog.appendChild(message);

    if (config.warning) {
      const warning = document.createElement("p");
      warning.textContent = config.warning;
      warning.style.cssText = "margin: 0 0 12px 0; color: rgb(200, 0, 0); font-weight: bold;";
      dialog.appendChild(warning);
    }

    if (config.details) {
      const detailsEl = document.createElement("p");
      detailsEl.textContent = config.details;
      detailsEl.style.cssText = "margin: 0 0 12px 0; font-family: monospace; font-size: 12px; word-break: break-all;";
      dialog.appendChild(detailsEl);
    }

    const buttonContainer = document.createElement("div");
    buttonContainer.style.cssText = "display: flex; gap: 12px; margin-top: 20px;";

    const allowButton = document.createElement("button");
    allowButton.textContent = "Allow";
    allowButton.style.cssText = `
      flex: 1;
      padding: 10px;
      background: black;
      color: white;
      border: none;
      cursor: pointer;
      font-size: 14px;
      font-weight: bold;
    `;

    const denyButton = document.createElement("button");
    denyButton.textContent = "Deny";
    denyButton.style.cssText = `
      flex: 1;
      padding: 10px;
      background: white;
      color: black;
      border: 2px solid black;
      cursor: pointer;
      font-size: 14px;
      font-weight: bold;
    `;

    allowButton.addEventListener("click", () => {
      document.body.removeChild(modal);
      resolve(true);
    });

    denyButton.addEventListener("click", () => {
      document.body.removeChild(modal);
      resolve(false);
    });

    buttonContainer.appendChild(allowButton);
    buttonContainer.appendChild(denyButton);
    dialog.appendChild(buttonContainer);
    modal.appendChild(dialog);
    document.body.appendChild(modal);

    // Focus deny button by default (safer)
    denyButton.focus();
  });
}
