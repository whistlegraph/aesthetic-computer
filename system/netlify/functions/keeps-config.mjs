import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { getKeepsContractAddress, LEGACY_KEEPS_CONTRACT } from "../../backend/tezos-keeps-contract.mjs";

const NO_CACHE_HEADERS = {
  "Cache-Control": "no-store, no-cache, must-revalidate, max-age=0",
  "Pragma": "no-cache",
  "Expires": "0",
};

const VERSION_BY_PROFILE = {
  v11: "11.0.0",
  v10: "10.0.0",
  v9: "9.0.0",
  v8: "8.0.0",
  v7: "7.0.0",
  v6: "6.0.0",
  v5: "5.0.0",
  v5rc: "5.0.0-rc",
  v4: "4.0.0",
};

const DEFAULT_MAINNET_V11_CONTRACT =
  (process.env.KEEP_MINT_EXPECTED_CONTRACT || "").trim() ||
  "KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB";

function normalizeNetwork(value) {
  const normalized = String(value || "").trim().toLowerCase();
  if (normalized === "ghostnet" || normalized === "mainnet") return normalized;
  return "mainnet";
}

function pickString(value, network) {
  if (typeof value === "string" && value.trim()) {
    return value.trim();
  }

  if (!value || typeof value !== "object" || Array.isArray(value)) {
    return null;
  }

  const candidate = value[network] || value.mainnet || value.current || value.default;
  return typeof candidate === "string" && candidate.trim() ? candidate.trim() : null;
}

function resolveProfile(secretDoc, network) {
  const raw =
    pickString(secretDoc?.currentKeepsProfile, network) ||
    pickString(secretDoc?.keepsProfile, network) ||
    pickString(secretDoc?.keeps_profile, network) ||
    pickString(secretDoc?.contractProfile, network) ||
    pickString(secretDoc?.keeps?.profile, network);

  return raw ? raw.toLowerCase() : "v11";
}

function resolveVersion(secretDoc, profile, network) {
  const explicit =
    pickString(secretDoc?.currentKeepsVersion, network) ||
    pickString(secretDoc?.keepsVersion, network) ||
    pickString(secretDoc?.keeps_version, network) ||
    pickString(secretDoc?.keeps?.version, network);

  if (explicit) return explicit;
  return VERSION_BY_PROFILE[profile] || null;
}

function rpcForNetwork(network) {
  return network === "mainnet" ? "https://mainnet.ecadinfra.com" : "https://ghostnet.ecadinfra.com";
}

function tzktForNetwork(network) {
  return network === "mainnet" ? "https://tzkt.io" : `https://${network}.tzkt.io`;
}

function objktForNetwork(network) {
  return network === "mainnet" ? "https://objkt.com" : `https://${network}.objkt.com`;
}

function defaultFallbackContract(network = "mainnet") {
  if (network === "mainnet") return DEFAULT_MAINNET_V11_CONTRACT;
  return LEGACY_KEEPS_CONTRACT;
}

function fallbackPayload(network = "mainnet", reason = "fallback") {
  const fallbackContractAddress = defaultFallbackContract(network);
  const fallbackProfile = fallbackContractAddress === LEGACY_KEEPS_CONTRACT ? "legacy" : "v11";
  const fallbackVersion = VERSION_BY_PROFILE[fallbackProfile] || null;
  return {
    contractAddress: fallbackContractAddress,
    network,
    profile: fallbackProfile,
    version: fallbackVersion,
    rpcUrl: rpcForNetwork(network),
    tzktExplorer: tzktForNetwork(network),
    objktBase: objktForNetwork(network),
    source: reason,
  };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const requestedNetwork = normalizeNetwork(event.queryStringParameters?.network || process.env.TEZOS_NETWORK || "mainnet");
  const secretId = (event.queryStringParameters?.secretId || "tezos-kidlisp").trim();

  let database = null;
  try {
    database = await connect();
    const db = database.db;
    const secretDoc = await db.collection("secrets").findOne({ _id: secretId });

    const network = normalizeNetwork(secretDoc?.network || requestedNetwork);
    const contractAddress = await getKeepsContractAddress({
      db,
      network,
      secretId,
      fallback: defaultFallbackContract(network),
    });

    const profile = resolveProfile(secretDoc, network);
    const version = resolveVersion(secretDoc, profile, network);
    const tzktExplorer = tzktForNetwork(network);
    const objktBase = objktForNetwork(network);

    return respond(200, {
      contractAddress,
      network,
      profile,
      version,
      rpcUrl: rpcForNetwork(network),
      tzktExplorer,
      objktBase,
      objktCollectionUrl: `${objktBase}/collections/${contractAddress}`,
      tzktContractUrl: `${tzktExplorer}/${contractAddress}`,
      source: contractAddress === LEGACY_KEEPS_CONTRACT ? "legacy-fallback" : "mongo-secrets",
    }, NO_CACHE_HEADERS);
  } catch (error) {
    console.warn(`⚠️ KEEP-CONFIG: ${error.message}`);
    return respond(200, fallbackPayload(requestedNetwork, "fallback-no-db"), NO_CACHE_HEADERS);
  } finally {
    if (database) {
      await database.disconnect();
    }
  }
}
