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
//   POST   /api/cal { import:{url|ics} }    AUTH  — import an external .ics feed
//   PUT    /api/cal                         AUTH  — update one of your events by uid
//   DELETE /api/cal?uid=<uid>               AUTH  — delete one of your events by uid
//   OPTIONS                                 — CORS preflight
//
// Imported events get uid = "ics:" + <source UID> so a re-import upserts
// (idempotent) rather than duplicating; they are always stored visibility:"private".

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

// 📥 Minimal RFC 5545 VCALENDAR parser (no npm deps).
//
// Supports, per VEVENT: UID, SUMMARY, DTSTART, DTEND, RRULE, DESCRIPTION, plus
// the VALUE=DATE (all-day) and TZID=... property parameters. It does NOT do
// full IANA-timezone math: a TZID date-time is read as a "floating" wall-clock
// time and stamped as UTC (good enough to place an event on the right day; the
// minute may be off by the source's UTC offset). `...Z` UTC and VALUE=DATE
// all-day forms are exact. VTIMEZONE blocks, VALARMs, attendees, attachments,
// and multi-value/structured params beyond the above are ignored.

const MAX_ICS_BYTES = 5 * 1024 * 1024; // 5MB guard.

// Unfold RFC 5545 folded content lines: a line beginning with a space or tab is
// a continuation of the previous line (the leading whitespace is consumed).
function unfoldICS(text) {
  const raw = text.replace(/\r\n/g, "\n").replace(/\r/g, "\n").split("\n");
  const out = [];
  for (const line of raw) {
    if ((line.startsWith(" ") || line.startsWith("\t")) && out.length) {
      out[out.length - 1] += line.slice(1);
    } else {
      out.push(line);
    }
  }
  return out;
}

// Unescape a TEXT value per spec: \\ \, \; and \n / \N (newline).
function icsUnescape(value) {
  return String(value ?? "")
    .replace(/\\n/gi, "\n")
    .replace(/\\,/g, ",")
    .replace(/\\;/g, ";")
    .replace(/\\\\/g, "\\");
}

// Split a content line into { name, params, value }. The name+params half is
// everything before the first unescaped colon; params are ;KEY=VALUE pairs.
function parseICSLine(line) {
  const colon = line.indexOf(":");
  if (colon === -1) return null;
  const head = line.slice(0, colon);
  const value = line.slice(colon + 1);
  const segs = head.split(";");
  const name = segs[0].toUpperCase();
  const params = {};
  for (let i = 1; i < segs.length; i++) {
    const eq = segs[i].indexOf("=");
    if (eq === -1) continue;
    params[segs[i].slice(0, eq).toUpperCase()] = segs[i].slice(eq + 1);
  }
  return { name, params, value };
}

// Convert a parsed DTSTART/DTEND ({ params, value }) into an ISO UTC string and
// an allDay flag. Handles three forms:
//   VALUE=DATE  → "YYYYMMDD"                (all-day, midnight UTC)
//   "...Z"      → "YYYYMMDDTHHMMSSZ"        (UTC instant, exact)
//   TZID / bare → "YYYYMMDDTHHMMSS"         (floating wall-clock → stamped UTC)
function icsDateToISO(prop) {
  if (!prop || !prop.value) return null;
  const v = prop.value.trim();
  const isDateValue = prop.params?.VALUE === "DATE" || /^\d{8}$/.test(v);

  if (isDateValue) {
    const m = /^(\d{4})(\d{2})(\d{2})$/.exec(v);
    if (!m) return null;
    const iso = new Date(Date.UTC(+m[1], +m[2] - 1, +m[3], 0, 0, 0)).toISOString();
    return { iso, allDay: true };
  }

  const m = /^(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})(Z)?$/.exec(v);
  if (!m) return null;
  // Both UTC (`Z`) and floating/TZID times are placed onto the UTC clock. For
  // floating/TZID we accept the wall-clock minute as-is (no offset table).
  const iso = new Date(
    Date.UTC(+m[1], +m[2] - 1, +m[3], +m[4], +m[5], +m[6]),
  ).toISOString();
  return { iso, allDay: false };
}

// Parse a whole VCALENDAR body into an array of normalized event objects:
//   { sourceUID, title, start, end, allDay, rrule, note }
// Events missing a UID or a start are returned with those fields null so the
// caller can count them as skipped.
function parseVCalendar(text) {
  const lines = unfoldICS(text);
  const events = [];
  let cur = null;

  for (const line of lines) {
    const upper = line.toUpperCase();
    if (upper === "BEGIN:VEVENT") {
      cur = {
        sourceUID: null,
        title: "",
        dtstart: null,
        dtend: null,
        rrule: null,
        note: "",
      };
      continue;
    }
    if (upper === "END:VEVENT") {
      if (cur) {
        const start = icsDateToISO(cur.dtstart);
        const end = icsDateToISO(cur.dtend);
        events.push({
          sourceUID: cur.sourceUID,
          title: cur.title,
          start: start?.iso || null,
          end: end?.iso || null,
          allDay: start?.allDay || false,
          rrule: cur.rrule,
          note: cur.note,
        });
      }
      cur = null;
      continue;
    }
    if (!cur) continue;

    const parsed = parseICSLine(line);
    if (!parsed) continue;
    switch (parsed.name) {
      case "UID":
        cur.sourceUID = parsed.value.trim();
        break;
      case "SUMMARY":
        cur.title = icsUnescape(parsed.value);
        break;
      case "DESCRIPTION":
        cur.note = icsUnescape(parsed.value);
        break;
      case "DTSTART":
        cur.dtstart = parsed;
        break;
      case "DTEND":
        cur.dtend = parsed;
        break;
      case "RRULE":
        cur.rrule = parsed.value.trim();
        break;
      default:
        break;
    }
  }
  return events;
}

// Fetch + sanity-check an external .ics feed. Accepts webcal:// (rewritten to
// https://). Rejects non-text or oversized payloads. Returns the raw text.
async function fetchICS(url) {
  let target = String(url).trim();
  if (/^webcal:\/\//i.test(target)) target = target.replace(/^webcal:\/\//i, "https://");
  if (!/^https?:\/\//i.test(target)) {
    throw new Error("Import URL must be http(s) or webcal.");
  }

  const res = await fetch(target, {
    headers: { Accept: "text/calendar, text/plain, */*" },
    redirect: "follow",
  });
  if (!res.ok) throw new Error(`Fetch failed (${res.status}).`);

  const len = Number(res.headers.get("content-length") || 0);
  if (len && len > MAX_ICS_BYTES) throw new Error("Calendar too large (>5MB).");

  const text = await res.text();
  if (text.length > MAX_ICS_BYTES) throw new Error("Calendar too large (>5MB).");
  if (!/BEGIN:VCALENDAR/i.test(text)) {
    throw new Error("Not an iCalendar (no VCALENDAR) document.");
  }
  return text;
}

// Upsert parsed events into the `calendar` collection, keyed by { user, uid }
// where uid = "ics:" + sourceUID. Idempotent: re-importing updates in place.
async function importEvents(collection, sub, handle, parsed) {
  let imported = 0;
  let updated = 0;
  let skipped = 0;

  for (const ev of parsed) {
    if (!ev.sourceUID || !ev.start) {
      skipped++;
      continue;
    }
    const uid = "ics:" + ev.sourceUID;
    const result = await collection.updateOne(
      { user: sub, uid },
      {
        $set: {
          handle: handle || null,
          title: ev.title || "(untitled)",
          start: ev.start,
          end: ev.end || ev.start,
          allDay: ev.allDay === true,
          rrule: ev.rrule || null,
          note: ev.note || "",
          visibility: "private",
          when: new Date(),
        },
        $setOnInsert: { user: sub, uid, seq: 0 },
      },
      { upsert: true },
    );
    if (result.upsertedCount > 0) imported++;
    else updated++;
  }

  return { imported, updated, skipped };
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

      // 📥 Import branch: { import: { url?, ics? } }. Detected before the normal
      // create path so an import request never falls through to event creation.
      if (body.import && typeof body.import === "object") {
        const { url, ics } = body.import;
        let text;
        try {
          if (typeof ics === "string" && ics.trim()) {
            if (ics.length > MAX_ICS_BYTES) {
              return respond(400, { message: "Calendar too large (>5MB)." });
            }
            if (!/BEGIN:VCALENDAR/i.test(ics)) {
              return respond(400, {
                message: "Not an iCalendar (no VCALENDAR) document.",
              });
            }
            text = ics;
          } else if (typeof url === "string" && url.trim()) {
            text = await fetchICS(url);
          } else {
            return respond(400, { message: "Provide import.url or import.ics." });
          }
        } catch (err) {
          return respond(400, { message: err?.message || "Import failed." });
        }

        const importHandle = await getHandleOrEmail(user.sub);
        const parsed = parseVCalendar(text);
        const counts = await importEvents(
          collection,
          user.sub,
          importHandle,
          parsed,
        );
        return respond(200, counts);
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
