// restricted-api.mjs
// Creates restricted $ API for untrusted JavaScript pieces

import { requestPermission } from "./piece-permissions.mjs";

/**
 * Create a restricted API for an untrusted piece
 * @param {object} fullApi - The full $ API object
 * @param {object} pieceMetadata - Metadata about the piece (code, trustLevel, etc.)
 * @returns {object} - Restricted $ API
 */
export function createRestrictedApi(fullApi, pieceMetadata) {
  const { code: pieceCode, trustLevel = "untrusted" } = pieceMetadata;

  // If already trusted, return full API
  if (trustLevel === "trusted") {
    return fullApi;
  }

  // Create restricted API
  const restrictedApi = {
    // Copy all safe APIs (rendering, input, utilities)
    ...fullApi,

    // Wrap network APIs with permission checks
    net: createRestrictedNetApi(fullApi.net, pieceCode),

    // Namespace storage per piece
    store: createNamespacedStore(fullApi.store, pieceCode),

    // Block auth APIs (require permission)
    authorize: createPermissionWrapper(
      fullApi.authorize,
      pieceCode,
      "auth",
      "authorize() requires permission"
    ),

    // Wrap upload with external URL check
    upload: createRestrictedUpload(fullApi.upload, pieceCode),

    // Wrap jump with external URL check
    jump: createRestrictedJump(fullApi.jump, pieceCode),

    // Block dynamic imports by not exposing import functionality
    // (pieces can still use static imports in their source)
  };

  // Add metadata flag
  restrictedApi._restricted = true;
  restrictedApi._pieceCode = pieceCode;

  return restrictedApi;
}

/**
 * Create restricted network API that requires permission
 */
function createRestrictedNetApi(netApi, pieceCode) {
  return {
    ...netApi,

    // Wrap preload (fetch wrapper)
    preload: async (url, options) => {
      const granted = await requestPermission(pieceCode, "network", { url });
      if (!granted) {
        throw new Error(
          `Network access denied. The piece "${pieceCode}" requested access to: ${url}`
        );
      }
      return netApi.preload(url, options);
    },

    // Wrap userRequest (authenticated fetch)
    userRequest: async (url, options) => {
      const granted = await requestPermission(pieceCode, "network", { url });
      if (!granted) {
        throw new Error(
          `Network access denied. The piece "${pieceCode}" requested access to: ${url}`
        );
      }
      // Also require auth permission since this uses tokens
      const authGranted = await requestPermission(pieceCode, "auth", {});
      if (!authGranted) {
        throw new Error(
          `Authentication access denied. The piece "${pieceCode}" tried to make an authenticated request.`
        );
      }
      return netApi.userRequest(url, options);
    },

    // Block getToken (direct token access)
    getToken: createPermissionWrapper(
      netApi.getToken,
      pieceCode,
      "auth",
      "getToken() requires permission"
    ),

    // Keep other net APIs that don't involve external requests
    // (signup, login, etc. can stay since they're internal to AC)
  };
}

/**
 * Create namespaced storage that isolates pieces from each other
 */
function createNamespacedStore(fullStore, pieceCode) {
  const namespace = `piece:${pieceCode}:`;

  return {
    async get(key) {
      return await fullStore.get(namespace + key);
    },

    async set(key, value) {
      return await fullStore.set(namespace + key, value);
    },

    async retrieve(key, path) {
      return await fullStore.retrieve(namespace + key, path);
    },

    async delete(key) {
      return await fullStore.delete(namespace + key);
    },

    async keys() {
      // Only return keys in this piece's namespace
      const allKeys = await fullStore.keys();
      return allKeys
        .filter((k) => k.startsWith(namespace))
        .map((k) => k.slice(namespace.length));
    },

    async clear() {
      // Only clear this piece's keys
      const keys = await this.keys();
      for (const key of keys) {
        await this.delete(key);
      }
    },

    // Expose method to request full storage access
    async requestFullAccess() {
      const granted = await requestPermission(pieceCode, "storage-full", {});
      if (granted) {
        // Return the full store
        return fullStore;
      }
      throw new Error("Full storage access denied");
    },
  };
}

/**
 * Wrap upload to check for external URLs
 */
function createRestrictedUpload(uploadFn, pieceCode) {
  return async (file, options = {}) => {
    // If uploading to external URL, require permission
    if (options.url && !isAestheticComputerUrl(options.url)) {
      const granted = await requestPermission(pieceCode, "upload-external", {
        url: options.url,
      });
      if (!granted) {
        throw new Error(
          `External upload denied. The piece "${pieceCode}" tried to upload to: ${options.url}`
        );
      }
    }
    return uploadFn(file, options);
  };
}

/**
 * Wrap jump to check for external URLs
 */
function createRestrictedJump(jumpFn, pieceCode) {
  return async (destination, ...args) => {
    // If jumping to external URL, require permission
    if (
      typeof destination === "string" &&
      (destination.startsWith("http://") ||
        destination.startsWith("https://")) &&
      !isAestheticComputerUrl(destination)
    ) {
      const granted = await requestPermission(
        pieceCode,
        "navigate-external",
        { url: destination }
      );
      if (!granted) {
        throw new Error(
          `External navigation denied. The piece "${pieceCode}" tried to navigate to: ${destination}`
        );
      }
    }
    return jumpFn(destination, ...args);
  };
}

/**
 * Create a wrapper that requires permission before calling the function
 */
function createPermissionWrapper(fn, pieceCode, permission, errorMessage) {
  return async (...args) => {
    const granted = await requestPermission(pieceCode, permission, {});
    if (!granted) {
      throw new Error(`${errorMessage} (piece: ${pieceCode})`);
    }
    return fn(...args);
  };
}

/**
 * Check if a URL is an aesthetic.computer URL (safe)
 */
function isAestheticComputerUrl(url) {
  try {
    const parsed = new URL(url, location.href);
    return (
      parsed.hostname === "aesthetic.computer" ||
      parsed.hostname.endsWith(".aesthetic.computer") ||
      parsed.hostname === location.hostname
    );
  } catch {
    return false;
  }
}

/**
 * Check if a piece should be restricted based on metadata
 */
export function shouldRestrictPiece(pieceMetadata) {
  const { trustLevel, authorSub, anonymous } = pieceMetadata;

  // Always restrict anonymous pieces
  if (anonymous !== false) {
    return true;
  }

  // Check trust level
  if (trustLevel === "trusted") {
    return false;
  }

  // Default to restricted for safety
  return true;
}
