// Prompt Log, 2026.09.03
// Record every prompt submission (anonymous) so the corpus of what people
// enter — including misses — is searchable and countable.
// POST: fire-and-forget from the prompt piece: { text }
// GET (admin): ?q=substring&limit=N to search raw entries,
//              ?stats=1&since=ISO&limit=N for grouped top entries + totals.

import { existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { connect } from "../../backend/database.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";

const COLLECTION = "prompts";
const MAX_LENGTH = 256;
const REDACTED_HEADS = new Set(["email"]); // arguments may carry PII

let disksDir = null;
try {
  disksDir = join(
    dirname(fileURLToPath(import.meta.url)),
    "../../public/aesthetic.computer/disks",
  );
  if (!existsSync(disksDir)) disksDir = null;
} catch {
  disksDir = null;
}

function pieceOnDisk(slug) {
  if (!disksDir || !/^[a-z0-9-]+$/i.test(slug)) return false;
  return (
    existsSync(join(disksDir, `${slug}.mjs`)) ||
    existsSync(join(disksDir, `${slug}.lisp`))
  );
}

// Classify a submission: what kind of thing was entered, and did it resolve?
// `resolved: false` on kind "word" is the miss corpus — commands the prompt
// handles internally also land there and get filtered during analysis.
async function classify(text, db) {
  const head = text.split(/\s+/)[0];

  if (text.startsWith("(") || text.includes("\n")) {
    return { kind: "kidlisp-source", resolved: true };
  }

  if (/^\$[a-z0-9]+$/i.test(head)) {
    const record = await db
      .collection("kidlisp")
      .findOne({ code: head.slice(1) }, { projection: { _id: 1 } });
    return { kind: "kidlisp-code", resolved: !!record };
  }

  if (head.startsWith("@")) return { kind: "user-piece", resolved: null };

  const slug = head.split(":")[0].split("~")[0];
  if (pieceOnDisk(slug)) return { kind: "piece", resolved: true };

  return { kind: "word", resolved: false };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");

  let database;
  try {
    database = await connect();
  } catch (err) {
    console.error("❌ Database connection failed:", err);
    return respond(500, { error: "Database connection failed" });
  }

  try {
    const prompts = database.db.collection(COLLECTION);

    if (event.httpMethod === "GET") {
      const user = await authorize(event.headers);
      if (!user) {
        await database.disconnect();
        return respond(401, { error: "Authentication required" });
      }
      if (!(await hasAdmin(user))) {
        await database.disconnect();
        return respond(403, { error: "Admin access required" });
      }

      const qs = event.queryStringParameters || {};
      const limit = Math.min(500, Math.max(1, Number(qs.limit || 50)));

      if (qs.stats) {
        const match = {};
        if (qs.since) match.createdAt = { $gte: new Date(qs.since) };
        if (qs.kind) match.kind = qs.kind;
        if (qs.resolved === "true") match.resolved = true;
        if (qs.resolved === "false") match.resolved = false;

        const top = await prompts
          .aggregate([
            { $match: match },
            {
              $group: {
                _id: "$text",
                count: { $sum: 1 },
                kind: { $first: "$kind" },
                resolved: { $first: "$resolved" },
                first: { $min: "$createdAt" },
                last: { $max: "$createdAt" },
              },
            },
            { $sort: { count: -1 } },
            { $limit: limit },
          ])
          .toArray();

        const totals = await prompts
          .aggregate([
            { $match: match },
            { $group: { _id: "$kind", count: { $sum: 1 } } },
            { $sort: { count: -1 } },
          ])
          .toArray();

        await database.disconnect();
        return respond(200, { top, totals });
      }

      const query = {};
      if (qs.q) {
        const escaped = qs.q.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
        query.text = { $regex: escaped, $options: "i" };
      }
      if (qs.kind) query.kind = qs.kind;

      const entries = await prompts
        .find(query)
        .sort({ createdAt: -1 })
        .limit(limit)
        .toArray();

      await database.disconnect();
      return respond(200, { entries });
    }

    if (event.httpMethod === "POST") {
      const body = JSON.parse(event.body || "{}");
      let text = (body.text || "").trim().slice(0, MAX_LENGTH);

      if (!text) {
        await database.disconnect();
        return respond(400, { error: "text required" });
      }

      const head = text.split(/\s+/)[0].toLowerCase();
      if (REDACTED_HEADS.has(head) && text.length > head.length) {
        text = `${head} [redacted]`;
      }

      const { kind, resolved } = await classify(text, database.db);

      const now = new Date();
      try {
        await prompts.createIndex({ createdAt: -1 });
        await prompts.createIndex({ text: 1 });
        await prompts.createIndex({ kind: 1, resolved: 1 });
      } catch (indexErr) {
        // Indexes already exist - that's fine
      }

      await prompts.insertOne({
        text,
        kind,
        resolved,
        day: now.toISOString().split("T")[0],
        createdAt: now,
      });

      await database.disconnect();
      return respond(200, { success: true });
    }

    await database.disconnect();
    return respond(405, { error: "Method not allowed" });
  } catch (err) {
    console.error("❌ prompt-log error:", err);
    if (database) await database.disconnect();
    return respond(500, { error: "Internal server error" });
  }
}
