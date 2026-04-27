// ac-device, 2026.04.27
// Hardware-fingerprint → slot registry for AC Native OS laptops.
//
// Each ac-native machine derives a stable hardware fingerprint from
// DMI fields (product_serial + system_uuid + board_serial, sha256'd
// to 16 hex chars). The fingerprint maps to a curated slot like
// "ac0", "ac1", … so jas can refer to specific upcycled machines by
// stable model numbers across reflashes and reinstalls.
//
// GET  ?fp=<fp>        Public lookup. Returns 200 { slot, model,
//                      assignedAt, assignedBy, notes } or 404 when
//                      the fingerprint isn't in the registry.
// GET  ?list=1         Admin-only. Returns 200 { devices: [...] }
//                      sorted by slot index.
// POST {fp, slot?,     Admin-only. Assigns or re-assigns a slot. If
//       model?,         slot is omitted, auto-picks the next free
//       notes?}         "ac<N>" (lowest unused integer). Returns
//                      the resulting document.
//
// Collection: "ac-devices". _id is the fingerprint string. Schema:
//   { _id, slot, model?, notes?, assignedAt, assignedBy }

import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

function nextFreeSlot(existing) {
  const taken = new Set();
  for (const d of existing) {
    const m = /^ac(\d+)$/.exec(d.slot || "");
    if (m) taken.add(parseInt(m[1], 10));
  }
  for (let i = 0; i < 1024; i++) {
    if (!taken.has(i)) return `ac${i}`;
  }
  return null; // shouldn't ever hit
}

export async function handler(event) {
  if (event.httpMethod === "GET") {
    const params = event.queryStringParameters || {};
    const list = params.list === "1" || params.list === "true";
    const fp = params.fp;

    const database = await connect();
    try {
      const col = database.db.collection("ac-devices");

      if (list) {
        const user = await authorize(event.headers);
        if (!user) return respond(401, { message: "unauthorized" });
        const isAdmin = await hasAdmin(user, "aesthetic");
        if (!isAdmin) return respond(403, { message: "admin only" });
        const devices = await col.find({}).toArray();
        devices.sort((a, b) => {
          const ai = parseInt((a.slot || "").replace(/^ac/, ""), 10);
          const bi = parseInt((b.slot || "").replace(/^ac/, ""), 10);
          return (isFinite(ai) ? ai : 1e9) - (isFinite(bi) ? bi : 1e9);
        });
        return respond(200, { devices });
      }

      if (!fp) return respond(400, { message: "missing fp parameter" });
      const doc = await col.findOne({ _id: fp });
      if (!doc) return respond(404, { message: "Device not registered" });
      return respond(200, {
        slot: doc.slot,
        model: doc.model || null,
        assignedAt: doc.assignedAt,
        assignedBy: doc.assignedBy,
        notes: doc.notes || null,
      });
    } finally {
      await database.disconnect();
    }
  }

  if (event.httpMethod === "POST") {
    const user = await authorize(event.headers);
    if (!user) return respond(401, { message: "unauthorized" });
    const isAdmin = await hasAdmin(user, "aesthetic");
    if (!isAdmin) return respond(403, { message: "admin only" });

    let body;
    try {
      body = JSON.parse(event.body);
    } catch {
      return respond(400, { message: "Invalid JSON" });
    }

    const { fp, slot: requestedSlot, model, notes } = body;
    if (!fp || typeof fp !== "string" || fp.length < 8) {
      return respond(400, { message: "missing or invalid fp" });
    }
    if (requestedSlot && !/^ac\d+$/.test(requestedSlot)) {
      return respond(400, { message: "slot must match /^ac\\d+$/" });
    }

    const database = await connect();
    try {
      const col = database.db.collection("ac-devices");

      let slot = requestedSlot;
      if (!slot) {
        const all = await col.find({}, { projection: { slot: 1 } }).toArray();
        slot = nextFreeSlot(all);
        if (!slot) return respond(500, { message: "no free slot available" });
      } else {
        // If reassigning a slot already used by a different fp, refuse —
        // explicit move requires deleting the old document first.
        const existing = await col.findOne({ slot });
        if (existing && existing._id !== fp) {
          return respond(409, {
            message: `slot ${slot} already assigned to another device (${existing._id})`,
          });
        }
      }

      const now = new Date();
      const doc = {
        _id: fp,
        slot,
        assignedAt: now,
        assignedBy: user.sub,
      };
      if (model && typeof model === "string") doc.model = model;
      if (notes && typeof notes === "string") doc.notes = notes;

      await col.replaceOne({ _id: fp }, doc, { upsert: true });
      return respond(200, doc);
    } finally {
      await database.disconnect();
    }
  }

  if (event.httpMethod === "DELETE") {
    const user = await authorize(event.headers);
    if (!user) return respond(401, { message: "unauthorized" });
    const isAdmin = await hasAdmin(user, "aesthetic");
    if (!isAdmin) return respond(403, { message: "admin only" });
    const fp = event.queryStringParameters?.fp;
    if (!fp) return respond(400, { message: "missing fp" });
    const database = await connect();
    try {
      const col = database.db.collection("ac-devices");
      const r = await col.deleteOne({ _id: fp });
      return respond(200, { deleted: r.deletedCount === 1 });
    } finally {
      await database.disconnect();
    }
  }

  return respond(405, { message: "Method Not Allowed" });
}
