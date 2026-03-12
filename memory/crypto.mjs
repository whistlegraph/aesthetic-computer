// memory/crypto.mjs
// Local app-level encryption helpers for agent memory data.

import { createCipheriv, createDecipheriv, createHash, randomBytes } from "crypto";
import { existsSync } from "fs";
import { chmod, mkdir, readFile, writeFile } from "fs/promises";
import { join } from "path";

const KEY_FILE_NAME = "key.bin";

function parseEnvKey(raw) {
  if (!raw) return null;

  if (raw.startsWith("base64:")) {
    const decoded = Buffer.from(raw.slice("base64:".length), "base64");
    if (decoded.length >= 32) return decoded.subarray(0, 32);
  }

  if (/^[a-fA-F0-9]{64}$/.test(raw)) {
    return Buffer.from(raw, "hex");
  }

  return createHash("sha256").update(raw).digest();
}

async function readOrCreateKeyFile(storeHome) {
  const keyPath = join(storeHome, KEY_FILE_NAME);

  if (existsSync(keyPath)) {
    const file = await readFile(keyPath);
    if (file.length >= 32) return file.subarray(0, 32);
    throw new Error(`agent-memory key file is invalid: ${keyPath}`);
  }

  await mkdir(storeHome, { recursive: true });
  const key = randomBytes(32);
  await writeFile(keyPath, key, { mode: 0o600 });
  try {
    await chmod(keyPath, 0o600);
  } catch {
    // Some environments ignore chmod.
  }
  return key;
}

export async function resolveMemoryKey(storeHome) {
  const envKey = parseEnvKey(process.env.AGENT_MEMORY_KEY || "");
  if (envKey) return envKey;
  return readOrCreateKeyFile(storeHome);
}

export function encryptJSON(payload, key) {
  const iv = randomBytes(12);
  const cipher = createCipheriv("aes-256-gcm", key, iv);
  const plaintext = Buffer.from(JSON.stringify(payload), "utf8");
  const encrypted = Buffer.concat([cipher.update(plaintext), cipher.final()]);
  const authTag = cipher.getAuthTag();

  return {
    version: 1,
    algorithm: "aes-256-gcm",
    iv: iv.toString("base64"),
    authTag: authTag.toString("base64"),
    data: encrypted.toString("base64"),
  };
}

export function decryptJSON(ciphertext, key) {
  if (!ciphertext || ciphertext.algorithm !== "aes-256-gcm") {
    throw new Error("unsupported cipher payload");
  }

  const decipher = createDecipheriv(
    "aes-256-gcm",
    key,
    Buffer.from(ciphertext.iv, "base64")
  );
  decipher.setAuthTag(Buffer.from(ciphertext.authTag, "base64"));

  const decrypted = Buffer.concat([
    decipher.update(Buffer.from(ciphertext.data, "base64")),
    decipher.final(),
  ]);

  return JSON.parse(decrypted.toString("utf8"));
}
