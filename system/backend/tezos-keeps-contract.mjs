import { connect } from "./database.mjs";

const KT1_ADDRESS_RE = /^KT1[1-9A-HJ-NP-Za-km-z]{33}$/;

// Legacy fallback — real v9 production contract (deprecated, all tokens migrated to v11).
export const LEGACY_KEEPS_CONTRACT = "KT1HoPURtwqXy58UYfZYh4ufoaMSNcsb9pCF";

function pickAddressCandidate(value, network = "mainnet") {
  if (!value) return null;

  if (typeof value === "string") {
    return value.trim();
  }

  if (typeof value !== "object" || Array.isArray(value)) {
    return null;
  }

  const orderedKeys = [network, "mainnet", "production", "current", "default"];
  for (const key of orderedKeys) {
    const candidate = value[key];
    if (typeof candidate === "string" && candidate.trim()) {
      return candidate.trim();
    }
  }

  return null;
}

function normalizeKt1(value) {
  if (typeof value !== "string") return null;
  const trimmed = value.trim();
  return KT1_ADDRESS_RE.test(trimmed) ? trimmed : null;
}

function resolveFromSecretDoc(secretDoc, network = "mainnet") {
  if (!secretDoc || typeof secretDoc !== "object") {
    return null;
  }

  const rawCandidates = [
    pickAddressCandidate(secretDoc.keepsContract, network),
    pickAddressCandidate(secretDoc.currentKeepsContract, network),
    pickAddressCandidate(secretDoc.keeps_contract, network),
    pickAddressCandidate(secretDoc.keepsContracts, network),
    pickAddressCandidate(secretDoc.contracts?.keeps, network),
    pickAddressCandidate(secretDoc.keeps?.contract, network),
    pickAddressCandidate(secretDoc.keeps?.contracts, network),
  ];

  for (const raw of rawCandidates) {
    const normalized = normalizeKt1(raw);
    if (normalized) return normalized;
  }

  return null;
}

export async function getKeepsContractAddress(options = {}) {
  const {
    db = null,
    network = "mainnet",
    secretId = "tezos-kidlisp",
    fallback = LEGACY_KEEPS_CONTRACT,
  } = options;

  let database = null;
  const fallbackAddress = normalizeKt1(fallback);

  try {
    const effectiveDb = db || (database = await connect()).db;
    const secretDoc = await effectiveDb.collection("secrets").findOne({ _id: secretId });
    const resolved = resolveFromSecretDoc(secretDoc, network);

    if (resolved) {
      return resolved;
    }

    if (fallbackAddress) {
      console.warn(`⚠️ KEEP: secrets.${secretId} missing keeps contract; using fallback ${fallbackAddress}`);
      return fallbackAddress;
    }

    throw new Error(`Missing valid keeps contract in secrets.${secretId}`);
  } catch (error) {
    if (fallbackAddress) {
      console.warn(`⚠️ KEEP: failed to resolve keeps contract from secrets (${error.message}); using fallback ${fallbackAddress}`);
      return fallbackAddress;
    }
    throw error;
  } finally {
    if (database) {
      await database.disconnect();
    }
  }
}
