// Profile Stream Publisher, 2026.02.27
// Sends server-side profile events to session-server /profile-event.

import { connect } from "./database.mjs";

const PROFILE_SECRET_CACHE_MS = 60 * 1000;
let profileSecretCacheValue = null;
let profileSecretCacheAt = 0;
let profileSecretLoadPromise = null;

function normalizeHandle(handle) {
  if (!handle) return null;
  const text = `${handle}`.trim();
  if (!text) return null;
  if (!text.startsWith("@")) return null;
  return `@${text.replace(/^@+/, "")}`;
}

function pickProfileStreamSecret(record) {
  if (!record || typeof record !== "object") return null;
  const candidates = [
    record.secret,
    record.token,
    record.profileSecret,
    record.value,
  ];
  for (const raw of candidates) {
    if (!raw) continue;
    const value = `${raw}`.trim();
    if (value) return value;
  }
  return null;
}

async function loadProfileStreamSecretFromMongo() {
  const database = await connect();
  try {
    const record = await database.db
      .collection("secrets")
      .findOne({ _id: "profile-stream" });
    return pickProfileStreamSecret(record);
  } finally {
    await database.disconnect?.();
  }
}

async function resolveProfileStreamSecret() {
  const now = Date.now();
  if (profileSecretCacheAt && now - profileSecretCacheAt < PROFILE_SECRET_CACHE_MS) {
    return profileSecretCacheValue;
  }

  if (profileSecretLoadPromise) return profileSecretLoadPromise;

  profileSecretLoadPromise = (async () => {
    let secret = null;

    try {
      secret = await loadProfileStreamSecretFromMongo();
    } catch (err) {
      console.warn(
        "Profile stream secret read from MongoDB failed:",
        err?.message || err,
      );
    }

    if (!secret) {
      const envSecret = `${process.env.PROFILE_STREAM_SECRET || ""}`.trim();
      secret = envSecret || null;
    }

    profileSecretCacheValue = secret;
    profileSecretCacheAt = Date.now();
    return profileSecretCacheValue;
  })();

  try {
    return await profileSecretLoadPromise;
  } finally {
    profileSecretLoadPromise = null;
  }
}

function profileEventBaseUrl() {
  const configured =
    process.env.SESSION_SERVER_PROFILE_EVENT_URL || process.env.SESSION_SERVER_URL;
  if (configured) return `${configured}`.replace(/\/$/, "");
  return "https://session-server.aesthetic.computer";
}

export async function publishProfileEvent(payload = {}, options = {}) {
  const handle = normalizeHandle(payload.handle);
  if (!handle) return false;

  const timeoutMs = Number(options.timeoutMs) || 1500;
  const body = {
    ...payload,
    handle,
  };

  const headers = {
    "Content-Type": "application/json",
  };

  const profileSecret = await resolveProfileStreamSecret();
  if (profileSecret) {
    headers["x-profile-secret"] = profileSecret;
  }

  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);

  try {
    const response = await fetch(`${profileEventBaseUrl()}/profile-event`, {
      method: "POST",
      headers,
      body: JSON.stringify(body),
      signal: controller.signal,
    });

    if (!response.ok) {
      const text = await response.text().catch(() => "");
      console.warn(
        `Profile event publish failed (${response.status}):`,
        text || response.statusText,
      );
      return false;
    }

    return true;
  } catch (err) {
    const reason = err?.name === "AbortError" ? "timeout" : err?.message;
    console.warn("Profile event publish error:", reason);
    return false;
  } finally {
    clearTimeout(timer);
  }
}
