// restricted-api.mjs
// Creates restricted $ API for untrusted JavaScript pieces
// Silently enforces safety boundaries without permission prompts.
// Network, upload, and navigation are allowed (browser security handles those).
// Token/auth access is silently blocked. Storage is namespaced per piece.

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

    // Wrap network APIs - allow requests but block direct token access
    net: createRestrictedNetApi(fullApi.net, pieceCode),

    // Namespace storage per piece
    store: createNamespacedStore(fullApi.store, pieceCode),

    // Silently block auth APIs (no token leaking)
    authorize: async () => {
      console.warn(`🔒 authorize() blocked for untrusted piece: ${pieceCode}`);
      return undefined;
    },

    // Upload and jump pass through (browser security handles external URLs)
    upload: fullApi.upload,
    jump: fullApi.jump,
  };

  // Add metadata flag
  restrictedApi._restricted = true;
  restrictedApi._pieceCode = pieceCode;

  return restrictedApi;
}

/**
 * Create restricted network API - allows requests, blocks token access
 */
function createRestrictedNetApi(netApi, pieceCode) {
  return {
    ...netApi,

    // preload (fetch) is allowed - browser CORS handles security
    preload: netApi.preload,

    // userRequest blocked - it sends auth tokens
    userRequest: async () => {
      console.warn(`🔒 userRequest() blocked for untrusted piece: ${pieceCode} (leaks auth tokens)`);
      return undefined;
    },

    // getToken blocked - direct token access
    getToken: async () => {
      console.warn(`🔒 getToken() blocked for untrusted piece: ${pieceCode}`);
      return undefined;
    },
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
  };
}

/**
 * Check if a piece should be restricted based on metadata
 */
export function shouldRestrictPiece(pieceMetadata) {
  const { trustLevel, anonymous } = pieceMetadata;

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
