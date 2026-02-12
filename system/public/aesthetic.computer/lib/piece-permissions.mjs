// piece-permissions.mjs
// Permission prompt system for untrusted pieces requesting sensitive APIs

// Store granted permissions per piece (session-based)
const grantedPermissions = new Map(); // pieceCode -> Set<permission>

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

  // Show permission prompt
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
 * Show a permission prompt UI
 */
async function showPermissionPrompt(pieceCode, permission, details) {
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
        warning: "⚠️ This gives the piece access to your account credentials.",
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
      const details = document.createElement("p");
      details.textContent = config.details;
      details.style.cssText = "margin: 0 0 12px 0; font-family: monospace; font-size: 12px; word-break: break-all;";
      dialog.appendChild(details);
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
