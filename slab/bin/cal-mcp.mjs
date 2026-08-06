#!/usr/bin/env node
// cal-mcp.mjs — an MCP over AesthetiCal, AC's own calendar. It lets any agent
// LIST, ADD, UPDATE, and DELETE events on @jeffrey's canonical calendar — the
// same `calendar` Mongo collection the `cal` web piece renders and the native
// DateWizard edits — plus subscribe external `.ics` feeds as read-only overlays.
//
// AesthetiCal is the canonical store; a Google Calendar can be wired in as an IN
// feed, but events created HERE are first-class AesthetiCal events (they export
// through the permahandle .ics and the secret subscription token). Invites/RSVP
// aren't a server feature yet — for a cross-person invite, still route through
// Google Calendar. See the aestheticalendar memory.
//
// Auth is the shared AC session: we read the OAuth access token from
// ~/.ac-token (minted by `ac-login`, the same file DateWizard watches) and send
// it as a Bearer to /api/cal, which validates it against Auth0 /userinfo. If the
// access token is expired we silently refresh it via the Auth0 refresh_token
// grant and rewrite the file — the same custom-domain client ac-login uses.
//
// Writes go through Cloudflare, so we send a browser User-Agent (a bare UA gets
// a 403 code 1010) and retry once on the transient 524 a cold Mongo can throw.
//
// Hand-rolled JSON-RPC over stdio, matching the sibling prox-mcp / frame-mcp —
// no SDK, only node builtins + the shared http-front.
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";
import { UA, loadTokens } from "../../toolchain/mcp/ac-token.mjs";

const API = "https://aesthetic.computer/api/cal";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// ── one authed request to /api/cal, browser-UA + one 524 retry ──────────────
async function api(method, { query = "", body } = {}) {
  const { access_token } = await loadTokens();
  const url = query ? `${API}?${query}` : API;
  const init = {
    method,
    headers: {
      Authorization: `Bearer ${access_token}`,
      "User-Agent": UA,
      Accept: "application/json",
      ...(body ? { "Content-Type": "application/json" } : {}),
    },
    ...(body ? { body: JSON.stringify(body) } : {}),
    signal: AbortSignal.timeout(20_000),
  };
  let res;
  for (let attempt = 0; attempt < 2; attempt++) {
    res = await fetch(url, init);
    if (res.status !== 524) break; // cold-Mongo transient — retry once
    await sleep(1200);
  }
  const text = await res.text();
  let json;
  try {
    json = text ? JSON.parse(text) : {};
  } catch {
    json = { message: text.slice(0, 200) };
  }
  if (!res.ok) {
    const hint =
      res.status === 401
        ? " (token rejected — run `ac-login`)"
        : res.status === 403 && /1010/.test(text)
          ? " (Cloudflare blocked the UA)"
          : "";
    throw new Error(`cal ${method} → HTTP ${res.status}: ${json.message || text.slice(0, 120)}${hint}`);
  }
  return json;
}

// ── formatting ───────────────────────────────────────────────────────────────
function fmtEvent(ev) {
  const when = ev.allDay
    ? `${ev.start?.slice(0, 10)} (all day)`
    : `${ev.start} → ${ev.end}`;
  const bits = [
    `• ${ev.title || "(untitled)"}  [${when}]`,
    `    uid: ${ev.uid}   vis: ${ev.visibility || "private"}${ev.rrule ? `   ↻ ${ev.rrule}` : ""}${ev.readOnly ? "   (read-only feed)" : ""}`,
  ];
  if (ev.note) bits.push(`    note: ${String(ev.note).replace(/\s+/g, " ").slice(0, 120)}`);
  return bits.join("\n");
}

// ── tools ─────────────────────────────────────────────────────────────────────
async function toolList({ from, to } = {}) {
  const range = new URLSearchParams();
  if (from) range.set("from", from);
  if (to) range.set("to", to);
  // Own events (the canonical `calendar` collection) + read-only feed overlays
  // (e.g. the connected Google Calendar) — the same two sources DateWizard
  // merges. Feed fetch is best-effort so a slow feed never blocks the list.
  const own = api("GET", { query: range.toString() });
  const feedQ = new URLSearchParams(range);
  feedQ.set("feedEvents", "1");
  const overlay = api("GET", { query: feedQ.toString() }).catch(() => ({ events: [] }));
  const [{ events: mine = [] }, { events: fed = [] }] = await Promise.all([own, overlay]);
  const events = [...mine, ...fed];
  if (!events.length) {
    const label = from || to ? ` in [${from || "…"} → ${to || "…"}]` : " this month";
    return [{ type: "text", text: `(no AesthetiCal events${label})` }];
  }
  events.sort((a, b) => String(a.start).localeCompare(String(b.start)));
  return [{ type: "text", text: `${events.length} event(s):\n\n${events.map(fmtEvent).join("\n")}` }];
}

async function toolAdd(args = {}) {
  const { title, start, end } = args;
  if (!title || !start || !end) {
    throw new Error("`title`, `start`, and `end` are required (start/end ISO 8601, e.g. 2026-07-21T09:00:00-07:00).");
  }
  const body = {
    title,
    start,
    end,
    tz: args.tz,
    allDay: args.allDay === true,
    rrule: args.rrule,
    note: args.note,
    visibility: args.visibility === "public" ? "public" : "private",
  };
  const { event } = await api("POST", { body });
  return [{ type: "text", text: `added to AesthetiCal:\n\n${fmtEvent(event)}` }];
}

async function toolUpdate(args = {}) {
  if (!args.uid) throw new Error("`uid` is required (from cal_list).");
  const body = { uid: args.uid };
  for (const k of ["title", "start", "end", "tz", "allDay", "rrule", "note", "visibility"]) {
    if (args[k] !== undefined) body[k] = args[k];
  }
  if (Object.keys(body).length === 1) throw new Error("nothing to update — pass at least one field besides uid.");
  const { event } = await api("PUT", { body });
  return [{ type: "text", text: `updated:\n\n${fmtEvent(event)}` }];
}

async function toolDelete({ uid } = {}) {
  if (!uid) throw new Error("`uid` is required (from cal_list).");
  await api("DELETE", { query: `uid=${encodeURIComponent(uid)}` });
  return [{ type: "text", text: `deleted event ${uid}.` }];
}

async function toolFeed(args = {}) {
  // No url + no removeId → list the subscribed feeds.
  if (!args.url && !args.removeId) {
    const { feeds = [] } = await api("GET", { query: "feeds=1" });
    if (!feeds.length) return [{ type: "text", text: "(no connected feeds)" }];
    const L = feeds.map((f) => `• ${f.label || "Calendar"}  [${f.kind || "ics"}]  id: ${f.id}\n    ${f.url}`);
    return [{ type: "text", text: `${feeds.length} connected feed(s):\n\n${L.join("\n")}` }];
  }
  if (args.removeId) {
    await api("DELETE", { query: `feedId=${encodeURIComponent(args.removeId)}` });
    return [{ type: "text", text: `removed feed ${args.removeId}.` }];
  }
  const { feed } = await api("POST", {
    body: { feed: { url: args.url, label: args.label, color: args.color } },
  });
  return [{ type: "text", text: `subscribed «${feed.label}» (${feed.kind}) — id ${feed.id}\n    ${feed.url}` }];
}

const TOOLS = [
  {
    name: "cal_list",
    description:
      "List AesthetiCal events — @jeffrey's canonical AC calendar (the same events the `cal` web piece shows and DateWizard edits), plus any read-only feed overlays. With no range, returns the current month. Auth is the shared AC session (~/.ac-token).",
    inputSchema: {
      type: "object",
      properties: {
        from: { type: "string", description: "Range start, ISO 8601 (e.g. 2026-07-01). Omit for the current month." },
        to: { type: "string", description: "Range end, ISO 8601. Omit for the current month." },
      },
    },
  },
  {
    name: "cal_add",
    description:
      "Add an event to AesthetiCal (a first-class AC calendar event, not a Google invite). Note: invites/RSVP are not a server feature yet — to invite another person, create it on Google Calendar instead. Defaults to private visibility; set visibility:public to include it in the shareable permahandle .ics feed.",
    inputSchema: {
      type: "object",
      properties: {
        title: { type: "string", description: "Event title." },
        start: { type: "string", description: "Start, ISO 8601 with offset (e.g. 2026-07-21T09:00:00-07:00). For all-day, a date like 2026-07-21." },
        end: { type: "string", description: "End, ISO 8601 (e.g. 2026-07-21T15:00:00-07:00)." },
        tz: { type: "string", description: "Optional IANA time zone (e.g. America/Los_Angeles)." },
        allDay: { type: "boolean", description: "True for an all-day event." },
        rrule: { type: "string", description: "Optional RFC-5545 RRULE for recurrence (e.g. FREQ=WEEKLY;BYDAY=TU)." },
        note: { type: "string", description: "Optional description/note." },
        visibility: { type: "string", description: "'private' (default) or 'public'." },
      },
      required: ["title", "start", "end"],
    },
  },
  {
    name: "cal_update",
    description:
      "Update one of @jeffrey's AesthetiCal events by uid (from cal_list). Pass only the fields to change; uid/owner/creation are never touched and the revision bumps automatically.",
    inputSchema: {
      type: "object",
      properties: {
        uid: { type: "string", description: "The event uid (from cal_list)." },
        title: { type: "string" },
        start: { type: "string", description: "New start, ISO 8601." },
        end: { type: "string", description: "New end, ISO 8601." },
        tz: { type: "string" },
        allDay: { type: "boolean" },
        rrule: { type: "string" },
        note: { type: "string" },
        visibility: { type: "string", description: "'private' or 'public'." },
      },
      required: ["uid"],
    },
  },
  {
    name: "cal_delete",
    description: "Delete one of @jeffrey's AesthetiCal events by uid (from cal_list). DESTRUCTIVE — removes it from the canonical calendar.",
    inputSchema: {
      type: "object",
      properties: { uid: { type: "string", description: "The event uid (from cal_list)." } },
      required: ["uid"],
    },
  },
  {
    name: "cal_feed",
    description:
      "Manage connected calendar feeds — external .ics subscriptions that overlay in AesthetiCal read-only (e.g. a Google Calendar's secret iCal address). No args lists feeds; pass a url (+optional label, color) to subscribe; pass removeId to unsubscribe.",
    inputSchema: {
      type: "object",
      properties: {
        url: { type: "string", description: "The external .ics URL to subscribe to." },
        label: { type: "string", description: "Optional display label for the feed." },
        color: { type: "string", description: "Optional hex color (e.g. #4285F4)." },
        removeId: { type: "string", description: "Feed id to unsubscribe (from a no-arg list)." },
      },
    },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "cal_list": return toolList(args || {});
    case "cal_add": return toolAdd(args || {});
    case "cal_update": return toolUpdate(args || {});
    case "cal_delete": return toolDelete(args || {});
    case "cal_feed": return toolFeed(args || {});
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "cal-mcp", version: "1.0.0" },
          },
        };
      case "initialized":
      case "notifications/initialized":
        return null;
      case "ping":
        return { jsonrpc: "2.0", id, result: {} };
      case "tools/list":
        return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default:
        return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7774);
if (port) serveHttp({ handleMessage, port, banner: "📅 cal shared daemon" });
else serveStdio({ handleMessage, banner: "📅 cal started (cal_list, cal_add, cal_update, cal_delete, cal_feed)" });
