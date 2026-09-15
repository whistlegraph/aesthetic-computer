// Operational mail metadata only. Never pass a letter or provider response here.
import { randomUUID } from "node:crypto";
import { mailErrorCode } from "../../shared/mail-privacy.mjs";

export const mailTrace = () => randomUUID();
const events = new Set([
  "started", "stored", "duplicate", "push", "push_failed", "push_quiet",
  "relay_accepted", "relay_fallback", "failed", "request", "connected",
  "recipient", "rejected", "deferred", "accepted", "disconnected",
  "smtp_response", "ready", "relays_refreshed", "relays_failed", "smtp_error",
]);
const reasons = new Set([
  "relay", "recipient", "lookup", "wire_size", "attachments", "route",
  "dmarc", "rate_pair", "rate_global", "storage", "smtp", "no_devices",
  "push_limit", "unauthorized", "invalid", "not_found", "method", "request",
]);
const counts = ["recipients", "duplicates", "attachments", "bytes", "attempted", "succeeded", "failed", "pruned", "durationMs", "status"];
const uuid = /^[a-f\d]{8}(?:-[a-f\d]{4}){3}-[a-f\d]{12}$/i;
export function mailEvent(fields = {}) {
  if (!events.has(fields.event)) return null;
  const row = { when: new Date(), event: fields.event };
  if (["internal", "smtp-in", "smtp-out", "api", "tell"].includes(fields.transport)) row.transport = fields.transport;
  if (uuid.test(fields.trace || "")) row.trace = fields.trace;
  if (/^[a-f\d]{24}$/i.test(String(fields.letterId || ""))) row.letterId = String(fields.letterId);
  if (["inbox", "count", "download", "read", "send"].includes(fields.action)) row.action = fields.action;
  if (reasons.has(fields.reason)) row.reason = fields.reason;
  if (fields.error) row.error = mailErrorCode({ code: fields.error });
  for (const key of counts) if (Number.isSafeInteger(fields[key]) && fields[key] >= 0) row[key] = fields[key];
  for (const key of ["verified", "tls", "routeSecret", "open"]) if (typeof fields[key] === "boolean") row[key] = fields[key];
  return row;
}

const indexed = new WeakMap();
const pending = new Set();
// Log first so a Mongo outage still leaves an inspectable journal event. A
// bounded asynchronous mirror cannot hold up SMTP or turn delivery into failure.
export function recordMailEvent(database, fields, log = console.log) {
  const row = mailEvent(fields);
  if (!row) return;
  const writeLog = (value) => { try { log(JSON.stringify({ kind: "mail.event", ...value })); } catch {} };
  writeLog(row);
  if (!database?.db) return;
  if (pending.size >= 128) {
    writeLog({ event: "telemetry_unavailable", reason: "queue_full" });
    return;
  }
  const db = database.db;
  const task = Promise.resolve().then(async () => {
    const collection = db.collection("mail-events");
    const client = db.client || db;
    let databases = indexed.get(client);
    if (!databases) { databases = new Map(); indexed.set(client, databases); }
    const name = db.databaseName || "";
    let setup = databases.get(name);
    if (!setup) {
      setup = (async () => {
        await collection.createIndex({ when: 1 }, { expireAfterSeconds: 30 * 86400, maxTimeMS: 1500 });
        await collection.createIndex({ trace: 1, when: 1 }, { maxTimeMS: 1500 });
        await collection.createIndex({ letterId: 1, when: 1 }, { maxTimeMS: 1500 });
      })();
      databases.set(name, setup);
      setup.catch(() => databases.delete(name));
    }
    await setup;
    await collection.insertOne(row, { maxTimeMS: 1500 });
  }).catch((error) => writeLog({ event: "telemetry_unavailable", error: mailErrorCode(error) }));
  pending.add(task);
  task.finally(() => pending.delete(task));
}

// For bounded service shutdown / one-shot scripts; normal requests never wait.
export async function flushMailEvents() {
  let timer;
  await Promise.race([
    Promise.allSettled([...pending]),
    new Promise((resolve) => { timer = setTimeout(resolve, 2000); }),
  ]);
  clearTimeout(timer);
}

// Covers setup/storage errors as well as successful delivery. The same trace
// follows API → delivery → push, or SMTP transaction → recipient copies.
export async function observeMail(transport, options, database, operation) {
  const trace = uuid.test(options.trace || "") ? options.trace : mailTrace();
  const event = (event, fields = {}) => recordMailEvent(database, { ...fields, event, transport, trace });
  event("started");
  try { return await operation({ ...options, trace }, event); }
  catch (error) { event("failed", { error: mailErrorCode(error) }); throw error; }
}
