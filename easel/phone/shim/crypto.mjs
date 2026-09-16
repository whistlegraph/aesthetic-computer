// `randomUUID`, which browsers already have — but only in a secure context.
// Safari over plain http on the LAN is not one, so this falls back rather than
// letting a thread id be the thing that breaks a phone test.

export function randomUUID() {
  if (globalThis.crypto?.randomUUID) return globalThis.crypto.randomUUID();
  const bytes = new Uint8Array(16);
  (globalThis.crypto ?? { getRandomValues: fill }).getRandomValues(bytes);
  bytes[6] = (bytes[6] & 0x0f) | 0x40;
  bytes[8] = (bytes[8] & 0x3f) | 0x80;
  const hex = [...bytes].map((b) => b.toString(16).padStart(2, "0")).join("");
  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`;
}

function fill(array) {
  for (let i = 0; i < array.length; i += 1) array[i] = Math.floor(Math.random() * 256);
  return array;
}

// `ac-session.mjs` is evaluated because `publish.mjs` imports two constants
// from it, so its imports must resolve even though the phone never runs the
// desktop PKCE flow. randomBytes is real (getRandomValues is everywhere);
// createHash is not, because SubtleCrypto is async and nothing here awaits it.
export function randomBytes(size) {
  const bytes = new Uint8Array(size);
  globalThis.crypto.getRandomValues(bytes);
  bytes.toString = (encoding) =>
    encoding === "hex"
      ? [...bytes].map((b) => b.toString(16).padStart(2, "0")).join("")
      : btoa(String.fromCharCode(...bytes));
  return bytes;
}

export function createHash() {
  throw new Error("createHash() is not available in the phone client — use SubtleCrypto");
}

export default { randomUUID, randomBytes, createHash };
