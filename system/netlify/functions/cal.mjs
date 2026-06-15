// cal, 26.06.15
// Calendar CRUD + iCalendar (RFC 5545) feed endpoint.
//
// Stores one document per event in the "calendar" collection:
//   { user, handle, uid, title, start, end, tz, allDay, rrule,
//     note, visibility, when, seq }
// - `user`       Auth0 sub (owner key)
// - `handle`     denormalized @handle (or email) captured at write time
// - `uid`        stable id, also the iCal UID (crypto.randomUUID())
// - `start`/`end` ISO UTC strings
// - `tz`         IANA timezone (e.g. "America/New_York")
// - `allDay`     boolean
// - `rrule`      RFC 5545 recurrence string, or null
// - `visibility` "private" | "public"
// - `when`       server Date (creation time)
// - `seq`        revision integer (starts at 0, $inc on PUT)
//
// HTTP contract:
//   GET    /api/cal?from=<ISO>&to=<ISO>     AUTH  — caller's own events overlapping [from,to]
//   GET    /api/cal?code=<ac25xxxxx>        PUBLIC — canonical per-user public feed (permahandle)
//   GET    /api/cal?handle=<name>           PUBLIC — public feed via mutable handle alias
//          ...add &format=ics to either public GET → text/calendar VCALENDAR body
//   POST   /api/cal                         AUTH  — create an event
//   PUT    /api/cal                         AUTH  — update one of your events by uid
//   DELETE /api/cal?uid=<uid>               AUTH  — delete one of your events by uid
//   OPTIONS                                 — CORS preflight

import crypto from "crypto";
import {
  authorize,
  getHandleOrEmail,
  handleFromPermahandle,
  userIDFromHandle,
} from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

// The only fields a stored event document is allowed to carry. Public reads
// project through this list so we never leak internal/handle-doc fields.
const EVENT_PROJECTION = {
  _id: 0,
  user: 1,
  handle: 1,
  uid: 1,
  title: 1,
  start: 1,
  end: 1,
  tz: 1,
  allDay: 1,
  rrule: 1,
  note: 1,
  visibility: 1,
  when: 1,
  seq: 1,
};

// 📅 Default to the current calendar month when from/to are omitted.
function currentMonthRange() {
  const now = new Date();
  const from = new Date(Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), 1));
  const to = new Date(Date.UTC(now.getUTCFullYear(), now.getUTCMonth() + 1, 1));
  return { from: from.toISOString(), to: to.toISOString() };
}

// Build an overlap filter for [from,to] over string ISO `start`/`end` fields.
// An event overlaps the window when it starts before `to` and ends after `from`.
// (ISO 8601 UTC strings sort lexicographically, so plain string comparison works.)
function overlapFilter(from, to) {
  return { start: { $lt: to }, end: { $gt: from } };
}

// 🗓️ Minimal RFC 5545 serializer (no npm deps).

// Escape TEXT values per spec basics: backslash, comma, semicolon, newline.
function icsEscape(value) {
  return String(value ?? "")
    .replace(/\\/g, "\\\\")
    .replace(/\n/g, "\\n")
    .replace(/,/g, "\\,")
    .replace(/;/g, "\\;");
}

// Fold long content lines at 75 octets, continuation lines begin with a space.
function icsFold(line) {
  if (line.length <= 75) return line;
  const parts = [];
  let rest = line;
  parts.push(rest.slice(0, 75));
  rest = rest.slice(75);
  while (rest.length > 74) {
    parts.push(" " + rest.slice(0, 74));
    rest = rest.slice(74);
  }
  if (rest.length) parts.push(" " + rest);
  return parts.join("\r\n");
}

// Format an ISO instant as a UTC iCal date-time stamp: 20260615T143000Z.
function icsDateTime(iso) {
  const d = new Date(iso);
  return d.toISOString().replace(/[-:]/g, "").replace(/\.\d{3}Z$/, "Z");
}

// Format an ISO instant as a floating iCal date: 20260615 (for all-day events).
function icsDate(iso) {
  const d = new Date(iso);
  const y = d.getUTCFullYear();
  const m = String(d.getUTCMonth() + 1).padStart(2, "0");
  const day = String(d.getUTCDate()).padStart(2, "0");
  return `${y}${m}${day}`;
}

// Serialize a list of event docs into a VCALENDAR body.
function eventsToICS(events, handleLabel) {
  const stamp = icsDateTime(new Date().toISOString());
  const lines = [
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    "PRODID:-//aesthetic.computer//cal//EN",
    "CALSCALE:GREGORIAN",
  ];
  if (handleLabel) lines.push("X-WR-CALNAME:" + icsEscape(handleLabel));

  for (const ev of events) {
    lines.push("BEGIN:VEVENT");
    lines.push("UID:" + icsEscape(ev.uid));
    lines.push("DTSTAMP:" + stamp);
    lines.push("SEQUENCE:" + (Number.isInteger(ev.seq) ? ev.seq : 0));

    if (ev.allDay) {
      // VALUE=DATE form — no time component, no Z.
      lines.push("DTSTART;VALUE=DATE:" + icsDate(ev.start));
      if (ev.end) lines.push("DTEND;VALUE=DATE:" + icsDate(ev.end));
    } else {
      lines.push("DTSTART:" + icsDateTime(ev.start));
      if (ev.end) lines.push("DTEND:" + icsDateTime(ev.end));
    }

    if (ev.title) lines.push("SUMMARY:" + icsEscape(ev.title));
    if (ev.note) lines.push("DESCRIPTION:" + icsEscape(ev.note));
    if (ev.rrule) lines.push("RRULE:" + ev.rrule); // RRULE is structured, not TEXT-escaped.
    lines.push("END:VEVENT");
  }

  lines.push("END:VCALENDAR");
  // Fold each line, join with CRLF per spec.
  return lines.map(icsFold).join("\r\n") + "\r\n";
}

export async function handler(event, context) {
  // CORS preflight.
  if (event.httpMethod === "OPTIONS") {
    return respond(200, "", {
      "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS",
    });
  }

  const method = event.httpMethod;
  if (!["GET", "POST", "PUT", "DELETE"].includes(method)) {
    return respond(405, { message: "Method Not Allowed" });
  }

  const params = event.queryStringParameters || {};
  const database = await connect();

  try {
    const collection = database.db.collection("calendar");

    // Create indexes once (best-effort).
    await collection.createIndex({ user: 1, start: 1 }).catch(() => {});
    await collection
      .createIndex({ user: 1, uid: 1 }, { unique: true })
      .catch(() => {});

    // ───────────────────────────────────────────────────────────── GET ──
    if (method === "GET") {
      const code = params.code;
      const handleParam = params.handle;
      const wantsICS = params.format === "ics";

      // 🌍 PUBLIC feed (by permahandle code or by handle alias).
      if (code || handleParam) {
        let sub;
        let handleStr;

        if (code) {
          // Canonical per-user feed via permahandle (ac25xxxxx).
          const resolved = await handleFromPermahandle(code, database);
          if (!resolved) {
            return respond(404, { message: "Not found." });
          }
          sub = resolved.sub;
          handleStr = "@" + resolved.handle;
        } else {
          // Mutable handle alias → sub.
          const clean = handleParam.replace(/^@/, "");
          sub = await userIDFromHandle(clean, database);
          if (!sub) {
            return respond(404, { message: "Not found." });
          }
          handleStr = "@" + clean;
        }

        // Public reads NEVER return private events, and project only the
        // allowed event fields (never the resolved handle doc).
        const events = await collection
          .find({ user: sub, visibility: "public" })
          .project(EVENT_PROJECTION)
          .sort({ start: 1 })
          .toArray();

        if (wantsICS) {
          return respond(200, eventsToICS(events, handleStr), {
            "Content-Type": "text/calendar; charset=utf-8",
          });
        }

        return respond(200, { handle: handleStr, code: code || null, events });
      }

      // 🔐 AUTH feed (caller's own events overlapping [from,to]).
      const user = await authorize(event.headers);
      if (!user?.sub) {
        return respond(401, { message: "Authorization failure." });
      }

      let { from, to } = params;
      if (!from || !to) {
        const range = currentMonthRange();
        from = from || range.from;
        to = to || range.to;
      }

      const events = await collection
        .find({ user: user.sub, ...overlapFilter(from, to) })
        .project(EVENT_PROJECTION)
        .sort({ start: 1 })
        .toArray();

      return respond(200, { events });
    }

    // ──────────────────────────────────────────────────────────── POST ──
    if (method === "POST") {
      const user = await authorize(event.headers);
      if (!user?.sub) {
        return respond(401, { message: "Authorization failure." });
      }

      let body;
      try {
        body = JSON.parse(event.body || "{}");
      } catch {
        return respond(400, { message: "Invalid JSON body." });
      }

      if (!body.title || !body.start || !body.end) {
        return respond(400, { message: "Missing title, start, or end." });
      }

      const handle = await getHandleOrEmail(user.sub);

      const doc = {
        user: user.sub,
        handle: handle || null,
        uid: crypto.randomUUID(),
        title: body.title,
        start: body.start,
        end: body.end,
        tz: body.tz || null,
        allDay: body.allDay === true,
        rrule: body.rrule || null,
        note: body.note || "",
        visibility: body.visibility === "public" ? "public" : "private",
        when: new Date(),
        seq: 0,
      };

      await collection.insertOne(doc);

      // Return the event with the same projection shape (no _id).
      const { _id, ...eventOut } = doc; // eslint-disable-line no-unused-vars
      return respond(200, { event: eventOut });
    }

    // ───────────────────────────────────────────────────────────── PUT ──
    if (method === "PUT") {
      const user = await authorize(event.headers);
      if (!user?.sub) {
        return respond(401, { message: "Authorization failure." });
      }

      let body;
      try {
        body = JSON.parse(event.body || "{}");
      } catch {
        return respond(400, { message: "Invalid JSON body." });
      }

      if (!body.uid) {
        return respond(400, { message: "Missing uid." });
      }

      // Only allow updating known mutable fields — never user/handle/uid/when/seq.
      const updatable = [
        "title",
        "start",
        "end",
        "tz",
        "allDay",
        "rrule",
        "note",
        "visibility",
      ];
      const set = {};
      for (const key of updatable) {
        if (body[key] === undefined) continue;
        if (key === "allDay") set.allDay = body.allDay === true;
        else if (key === "visibility")
          set.visibility = body.visibility === "public" ? "public" : "private";
        else set[key] = body[key];
      }

      // Update ONLY the caller's own event; bump revision; return the new doc.
      const result = await collection.findOneAndUpdate(
        { user: user.sub, uid: body.uid },
        { $set: set, $inc: { seq: 1 } },
        { returnDocument: "after", projection: EVENT_PROJECTION },
      );

      // Driver compatibility: some versions wrap in { value }, others return the doc.
      const updated = result?.value ?? result;
      if (!updated || !updated.uid) {
        return respond(404, { message: "Not found." });
      }

      return respond(200, { event: updated });
    }

    // ────────────────────────────────────────────────────────── DELETE ──
    if (method === "DELETE") {
      const user = await authorize(event.headers);
      if (!user?.sub) {
        return respond(401, { message: "Authorization failure." });
      }

      const uid = params.uid;
      if (!uid) {
        return respond(400, { message: "Missing uid." });
      }

      const result = await collection.deleteOne({ user: user.sub, uid });
      if (result.deletedCount === 0) {
        return respond(404, { message: "Not found." });
      }

      return respond(200, { ok: true });
    }
  } catch (error) {
    return respond(500, { message: error?.message || String(error) });
  } finally {
    await database.disconnect();
  }
}
