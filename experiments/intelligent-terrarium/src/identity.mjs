import { normalizeHandle } from "./core.mjs";

const DEFAULT_USERINFO_URL = "https://aesthetic.us.auth0.com/userinfo";
const DEFAULT_AC_ORIGIN = "https://aesthetic.computer";

export class IdentityError extends Error {
  constructor(code, statusCode, publicMessage) {
    super(publicMessage);
    this.name = "IdentityError";
    this.code = code;
    this.statusCode = statusCode;
    this.publicMessage = publicMessage;
  }
}

function bearerToken(header) {
  if (!header) throw new IdentityError("missing_token", 401, "authorization token required");
  const match = /^Bearer ([^\s]+)$/.exec(String(header));
  if (!match || match[1].length > 8192) {
    throw new IdentityError("invalid_token", 401, "invalid or expired authorization token");
  }
  return match[1];
}

function rejectExpiredJwt(token, now) {
  const parts = token.split(".");
  if (parts.length !== 3) return;
  try {
    const claims = JSON.parse(Buffer.from(parts[1], "base64url").toString("utf8"));
    if (Number.isFinite(claims.exp) && claims.exp * 1000 <= now()) {
      throw new IdentityError("expired_token", 401, "invalid or expired authorization token");
    }
  } catch (error) {
    if (error instanceof IdentityError) throw error;
    // Auth0 userinfo remains authoritative for opaque or unusual token shapes.
  }
}

async function responseJson(response) {
  try {
    return await response.json();
  } catch {
    return null;
  }
}

export function createACIdentityVerifier({
  fetchImpl = globalThis.fetch,
  userInfoUrl = DEFAULT_USERINFO_URL,
  acOrigin = DEFAULT_AC_ORIGIN,
  now = Date.now,
} = {}) {
  if (typeof fetchImpl !== "function") throw new TypeError("identity verifier requires fetch");

  return {
    async verifyAuthorization(authorization) {
      const token = bearerToken(authorization);
      rejectExpiredJwt(token, now);

      let userInfoResponse;
      try {
        userInfoResponse = await fetchImpl(userInfoUrl, {
          headers: { Authorization: `Bearer ${token}` },
          cache: "no-store",
        });
      } catch {
        throw new IdentityError("identity_unavailable", 503, "identity verification unavailable");
      }
      if (userInfoResponse.status === 401 || userInfoResponse.status === 403) {
        throw new IdentityError("invalid_or_expired_token", 401, "invalid or expired authorization token");
      }
      if (!userInfoResponse.ok) {
        throw new IdentityError("identity_unavailable", 503, "identity verification unavailable");
      }
      const claims = await responseJson(userInfoResponse);
      const sub = typeof claims?.sub === "string" ? claims.sub : "";
      if (!sub || sub.length > 512 || /[\u0000-\u001f]/.test(sub)) {
        throw new IdentityError("invalid_identity", 401, "authorization identity is invalid");
      }

      let handleResponse;
      try {
        const handleUrl = new URL(`/handle/${encodeURIComponent(sub)}`, acOrigin);
        handleResponse = await fetchImpl(handleUrl, { cache: "no-store" });
      } catch {
        throw new IdentityError("handle_unavailable", 503, "handle resolution unavailable");
      }
      if (!handleResponse.ok) {
        if (handleResponse.status === 404) {
          throw new IdentityError("handle_required", 403, "an Aesthetic Computer handle is required");
        }
        throw new IdentityError("handle_unavailable", 503, "handle resolution unavailable");
      }
      const handleDocument = await responseJson(handleResponse);
      try {
        return normalizeHandle(handleDocument?.handle);
      } catch {
        throw new IdentityError("handle_required", 403, "an Aesthetic Computer handle is required");
      }
    },
  };
}
