import { MongoClient } from "mongodb";

let mongoClient;
let mongoDb;
let cachedIngestToken = "";
let cachedIngestTokenAt = 0;
const INGEST_TOKEN_TTL_MS = 60_000;

function getSecretId() {
  const value = String(process.env.AGENT_MEMORY_SECRET_ID || "").trim();
  return value || "agent-memory";
}

function respond(statusCode, body) {
  return {
    statusCode,
    headers: {
      "content-type": "application/json",
      "cache-control": "no-store",
      "access-control-allow-origin": "*",
      "access-control-allow-methods": "POST, OPTIONS",
      "access-control-allow-headers": "content-type, authorization",
    },
    body: JSON.stringify(body),
  };
}

function getBearerToken(event) {
  const authHeader =
    event?.headers?.authorization ||
    event?.headers?.Authorization ||
    "";

  if (!authHeader) return "";
  const match = authHeader.match(/^Bearer\s+(.+)$/i);
  return match?.[1] || "";
}

function pickString(value) {
  if (typeof value !== "string") return "";
  const trimmed = value.trim();
  return trimmed || "";
}

function tokenFromSecretDoc(secretDoc) {
  const candidates = [
    secretDoc?.ingestToken,
    secretDoc?.token,
    secretDoc?.remoteToken,
    secretDoc?.agentMemoryIngestToken,
    secretDoc?.agentMemoryRemoteToken,
    secretDoc?.agentMemory?.ingestToken,
    secretDoc?.agentMemory?.remoteToken,
  ];

  for (const candidate of candidates) {
    const value = pickString(candidate);
    if (value) return value;
  }
  return "";
}

async function getExpectedToken() {
  if (cachedIngestToken && Date.now() - cachedIngestTokenAt < INGEST_TOKEN_TTL_MS) {
    return cachedIngestToken;
  }

  const db = await getDb();
  const secretDoc = await db.collection("secrets").findOne({ _id: getSecretId() });
  const token = tokenFromSecretDoc(secretDoc);
  cachedIngestToken = token;
  cachedIngestTokenAt = Date.now();
  return token;
}

async function getDb() {
  if (mongoDb) return mongoDb;

  const connectionString =
    process.env.AGENT_MEMORY_MONGODB_CONNECTION_STRING ||
    process.env.MONGODB_CONNECTION_STRING;
  const dbName =
    process.env.AGENT_MEMORY_MONGODB_NAME ||
    process.env.MONGODB_NAME ||
    "aesthetic";

  if (!connectionString) {
    throw new Error("MongoDB connection string is not configured");
  }

  mongoClient = new MongoClient(connectionString, {
    serverSelectionTimeoutMS: 8000,
    connectTimeoutMS: 8000,
  });
  await mongoClient.connect();
  mongoDb = mongoClient.db(dbName);
  return mongoDb;
}

function collectionForKind(kind) {
  switch (kind) {
    case "event":
      return "agent_memory_events";
    case "checkpoint":
      return "agent_memory_checkpoints";
    case "session":
      return "agent_memory_sessions";
    default:
      return "agent_memory_records";
  }
}

async function ensureIndexes(collection, kind) {
  if (kind === "event") {
    await collection.createIndex(
      { session_id: 1, "payload.device_id": 1, "payload.seq": 1 },
      { unique: true, background: true }
    );
    await collection.createIndex({ when: -1 }, { background: true });
    return;
  }

  if (kind === "checkpoint") {
    await collection.createIndex(
      { session_id: 1, "payload.checkpoint_id": 1 },
      { unique: true, background: true }
    );
    await collection.createIndex({ when: -1 }, { background: true });
    return;
  }

  await collection.createIndex({ when: -1 }, { background: true });
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 204,
      headers: {
        "access-control-allow-origin": "*",
        "access-control-allow-methods": "POST, OPTIONS",
        "access-control-allow-headers": "content-type, authorization",
      },
      body: "",
    };
  }

  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Method not allowed" });
  }

  let expectedToken = "";
  try {
    expectedToken = await getExpectedToken();
  } catch (error) {
    return respond(503, {
      error: "Ingest token lookup failed",
      message: error.message,
    });
  }

  if (!expectedToken) {
    return respond(503, {
      error: "Ingest token is not configured",
      secret_id: getSecretId(),
      expected_field: "ingestToken (or token/remoteToken)",
    });
  }

  const token = getBearerToken(event);
  if (!token || token !== expectedToken) {
    return respond(401, { error: "Unauthorized" });
  }

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return respond(400, { error: "Invalid JSON payload" });
  }

  const kind = body.kind || "record";
  const sessionId = body.session_id || body.payload?.session_id || null;
  const payload = body.payload || null;

  if (!payload) {
    return respond(400, { error: "Missing payload" });
  }
  if ((kind === "event" || kind === "checkpoint") && !sessionId) {
    return respond(400, { error: "Missing session_id for event/checkpoint payload" });
  }

  try {
    const db = await getDb();
    const collection = db.collection(collectionForKind(kind));
    await ensureIndexes(collection, kind);

    const doc = {
      kind,
      session_id: sessionId,
      when: new Date(),
      payload,
      source: "agent-memory-ingest",
    };

    if (kind === "event") {
      const filter = {
        session_id: sessionId,
        "payload.device_id": payload.device_id,
        "payload.seq": payload.seq,
      };
      const result = await collection.updateOne(filter, { $setOnInsert: doc }, { upsert: true });
      return respond(200, {
        ok: true,
        kind,
        upserted: !!result.upsertedCount,
        matched: result.matchedCount,
      });
    }

    if (kind === "checkpoint") {
      const filter = {
        session_id: sessionId,
        "payload.checkpoint_id": payload.checkpoint_id,
      };
      const result = await collection.updateOne(filter, { $setOnInsert: doc }, { upsert: true });
      return respond(200, {
        ok: true,
        kind,
        upserted: !!result.upsertedCount,
        matched: result.matchedCount,
      });
    }

    const inserted = await collection.insertOne(doc);
    return respond(200, {
      ok: true,
      kind,
      inserted_id: inserted.insertedId,
    });
  } catch (error) {
    return respond(500, {
      error: "ingest failed",
      message: error.message,
    });
  }
}
