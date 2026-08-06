#!/usr/bin/env node
// gcal-sync.mjs — one-way AesthetiCal → Google Calendar mirror.
//
// Uses the existing Google Desktop OAuth client in the private vault, but keeps
// a calendar-specific refresh token. Every mirrored Google event carries the
// AesthetiCal UID + revision in private extended properties, making syncs
// idempotent and allowing deletions to propagate without touching manual events.

import { createServer } from "node:http";
import { spawn } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  writeFileSync,
} from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const VAULT = resolve(REPO, "aesthetic-computer-vault/calendar");
const CLIENT_PATH = process.env.GCAL_CLIENT_JSON ||
  resolve(REPO, "aesthetic-computer-vault/youtube/client.json");
const TOKEN_PATH = process.env.GCAL_TOKEN_JSON || resolve(VAULT, "google-token.json");
const STATE_PATH = process.env.GCAL_STATE_JSON || resolve(VAULT, "state.json");
const AC_TOKEN_PATH = process.env.AC_TOKEN_JSON || resolve(homedir(), ".ac-token");
const GOOGLE_API = "https://www.googleapis.com/calendar/v3";
const AC_API = "https://aesthetic.computer/api/cal";
const CALENDAR_NAME = process.env.GCAL_NAME || "AesthetiCal";
const SCOPES = ["https://www.googleapis.com/auth/calendar"];
const SERVICE_MANAGEMENT_SCOPE = "https://www.googleapis.com/auth/service.management";
const GOOGLE_PROJECT_NUMBER = process.env.GCAL_GOOGLE_PROJECT || "839964586768";

const command = process.argv[2];

function die(message) {
  console.error(`✗ ${message}`);
  process.exit(1);
}

function readJSON(path, label) {
  if (!existsSync(path)) die(`${label} not found: ${path}`);
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch (error) {
    die(`${label} is invalid JSON: ${error.message}`);
  }
}

function loadClient() {
  const raw = readJSON(CLIENT_PATH, "Google OAuth client");
  const client = raw.installed || raw.web || raw;
  if (!client.client_id || !client.client_secret) {
    die(`Google OAuth client is missing client_id/client_secret: ${CLIENT_PATH}`);
  }
  return { id: client.client_id, secret: client.client_secret };
}

function loadState() {
  if (!existsSync(STATE_PATH)) return {};
  return readJSON(STATE_PATH, "Google Calendar sync state");
}

function saveJSON(path, value) {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, JSON.stringify(value, null, 2) + "\n", { mode: 0o600 });
}

async function exchangeToken(params) {
  const response = await fetch("https://oauth2.googleapis.com/token", {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams(params),
  });
  const json = await response.json();
  if (!response.ok) die(`Google OAuth token exchange failed (${response.status}): ${json.error_description || json.error}`);
  return json;
}

async function accessToken() {
  const client = loadClient();
  const saved = readJSON(TOKEN_PATH, "Google Calendar OAuth token");
  if (!saved.refresh_token) die(`Calendar refresh token is missing. Run: node ${process.argv[1]} auth`);
  const token = await exchangeToken({
    client_id: client.id,
    client_secret: client.secret,
    refresh_token: saved.refresh_token,
    grant_type: "refresh_token",
  });
  return token.access_token;
}

function openBrowser(url) {
  const executable = process.platform === "darwin" ? "open" :
    process.platform === "win32" ? "start" : "xdg-open";
  spawn(executable, [url], { stdio: "ignore", detached: true }).unref();
}

async function oauthConsent(scopes, message) {
  const client = loadClient();
  let redirectURI;
  const code = await new Promise((resolveCode, rejectCode) => {
    const server = createServer((request, response) => {
      const url = new URL(request.url, "http://localhost");
      if (!url.searchParams.has("code") && !url.searchParams.has("error")) {
        response.writeHead(404).end();
        return;
      }
      const oauthError = url.searchParams.get("error");
      response.writeHead(200, { "Content-Type": "text/html; charset=utf-8" });
      response.end(`<!doctype html><meta charset=utf-8><body style="font-family:monospace;background:#111;color:#aef240;padding:3em"><h2>${oauthError ? "✗ Authorization declined" : "✓ AesthetiCal authorized"}</h2><p>You can close this tab.</p></body>`);
      server.close();
      if (oauthError) rejectCode(new Error(oauthError));
      else resolveCode(url.searchParams.get("code"));
    });

    server.listen(0, "127.0.0.1", () => {
      redirectURI = `http://127.0.0.1:${server.address().port}`;
      const authURL = "https://accounts.google.com/o/oauth2/v2/auth?" + new URLSearchParams({
        client_id: client.id,
        redirect_uri: redirectURI,
        response_type: "code",
        scope: scopes.join(" "),
        access_type: "offline",
        include_granted_scopes: "true",
        prompt: "consent",
        login_hint: "mail@aesthetic.computer",
      });
      console.log(`▸ Opening Google authorization — ${message}`);
      openBrowser(authURL);
    });
  });

  const token = await exchangeToken({
    client_id: client.id,
    client_secret: client.secret,
    code,
    grant_type: "authorization_code",
    redirect_uri: redirectURI,
  });
  return token;
}

async function authorize() {
  const token = await oauthConsent(
    SCOPES,
    "approve Calendar access for mail@aesthetic.computer.",
  );
  if (!token.refresh_token) {
    die("Google returned no refresh token. Revoke the app's prior grant and run auth again.");
  }
  saveJSON(TOKEN_PATH, {
    refresh_token: token.refresh_token,
    obtained: new Date().toISOString(),
    scopes: SCOPES,
    account: "mail@aesthetic.computer",
  });
  console.log(`✓ Calendar OAuth token saved → ${TOKEN_PATH}`);
}

async function enableCalendarAPI() {
  const token = await oauthConsent(
    [SERVICE_MANAGEMENT_SCOPE],
    "approve service management so Calendar API can be enabled for the aesthetic-computer project.",
  );
  const name = `projects/${GOOGLE_PROJECT_NUMBER}/services/calendar-json.googleapis.com`;
  const response = await fetch(`https://serviceusage.googleapis.com/v1/${name}:enable`, {
    method: "POST",
    headers: {
      Authorization: `Bearer ${token.access_token}`,
      "Content-Type": "application/json",
    },
    body: "{}",
  });
  const payload = await response.json();
  if (!response.ok) {
    die(`Service Usage API ${response.status}: ${payload?.error?.message || "enable failed"}`);
  }

  if (payload.name) {
    for (let attempt = 0; attempt < 15; attempt++) {
      await new Promise((resolveDelay) => setTimeout(resolveDelay, 2000));
      const operation = await fetch(`https://serviceusage.googleapis.com/v1/${payload.name}`, {
        headers: { Authorization: `Bearer ${token.access_token}` },
      });
      const result = await operation.json();
      if (!operation.ok) {
        die(`Service Usage operation ${operation.status}: ${result?.error?.message || "poll failed"}`);
      }
      if (result.done) {
        if (result.error) die(`Calendar API enable failed: ${result.error.message}`);
        console.log("✓ Google Calendar API enabled");
        return;
      }
    }
  }
  console.log("✓ Google accepted the Calendar API enable request; propagation may take a minute.");
}

async function googleRequest(token, path, options = {}) {
  const response = await fetch(`${GOOGLE_API}${path}`, {
    method: options.method || "GET",
    headers: {
      Authorization: `Bearer ${token}`,
      ...(options.body ? { "Content-Type": "application/json" } : {}),
    },
    body: options.body ? JSON.stringify(options.body) : undefined,
  });
  const text = await response.text();
  let payload = null;
  if (text) {
    try { payload = JSON.parse(text); }
    catch { payload = text; }
  }
  if (!response.ok) {
    const detail = payload?.error?.message || payload?.message || String(payload || response.statusText);
    const error = new Error(`Google Calendar API ${response.status}: ${detail}`);
    error.status = response.status;
    throw error;
  }
  return payload;
}

async function fetchAestheticalEvents() {
  const saved = readJSON(AC_TOKEN_PATH, "Aesthetic Computer session");
  const token = saved.access_token || saved.token;
  if (!token) die(`Aesthetic Computer session has no access token: ${AC_TOKEN_PATH}`);
  const query = new URLSearchParams({
    from: "2000-01-01T00:00:00Z",
    to: "2100-01-01T00:00:00Z",
  });
  const response = await fetch(`${AC_API}?${query}`, {
    headers: { Authorization: `Bearer ${token}` },
  });
  const payload = await response.json();
  if (!response.ok) die(`AesthetiCal API ${response.status}: ${payload.message || "request failed"}`);
  return payload.events || [];
}

async function allPages(token, path, itemKey) {
  const items = [];
  let pageToken;
  do {
    const separator = path.includes("?") ? "&" : "?";
    const pagePath = pageToken ? `${path}${separator}pageToken=${encodeURIComponent(pageToken)}` : path;
    const payload = await googleRequest(token, pagePath);
    items.push(...(payload?.[itemKey] || []));
    pageToken = payload?.nextPageToken;
  } while (pageToken);
  return items;
}

async function primaryIdentity(token) {
  const primary = await googleRequest(token, "/calendars/primary");
  return { id: primary.id, summary: primary.summary };
}

async function ensureCalendar(token) {
  const state = loadState();
  if (state.calendarId) {
    try {
      const calendar = await googleRequest(token, `/calendars/${encodeURIComponent(state.calendarId)}`);
      return calendar;
    } catch (error) {
      if (error.status !== 404) throw error;
    }
  }

  const listed = await allPages(token, "/users/me/calendarList?maxResults=250", "items");
  let calendar = listed.find((item) =>
    item.accessRole === "owner" &&
    (item.summaryOverride === CALENDAR_NAME || item.summary === CALENDAR_NAME));

  if (!calendar) {
    calendar = await googleRequest(token, "/calendars", {
      method: "POST",
      body: {
        summary: CALENDAR_NAME,
        description: "One-way mirror of AesthetiCal events from aesthetic.computer.",
        timeZone: "America/Los_Angeles",
      },
    });
    console.log(`✓ created Google calendar · ${CALENDAR_NAME}`);
  }

  await googleRequest(
    token,
    `/users/me/calendarList/${encodeURIComponent(calendar.id)}?colorRgbFormat=true`,
    {
      method: "PATCH",
      body: {
        selected: true,
        summaryOverride: CALENDAR_NAME,
        backgroundColor: "#ff6b9d",
      },
    },
  );
  saveJSON(STATE_PATH, {
    ...state,
    calendarId: calendar.id,
    calendarName: CALENDAR_NAME,
    updatedAt: new Date().toISOString(),
  });
  return calendar;
}

function isoDate(value) {
  return new Date(value).toISOString().slice(0, 10);
}

function googleEvent(event) {
  const allDay = event.allDay === true;
  const body = {
    summary: event.title || "(untitled)",
    description: event.note || "",
    visibility: event.visibility === "public" ? "public" : "private",
    start: allDay
      ? { date: isoDate(event.start) }
      : { dateTime: event.start, ...(event.tz ? { timeZone: event.tz } : {}) },
    end: allDay
      ? { date: isoDate(event.end || event.start) }
      : { dateTime: event.end || event.start, ...(event.tz ? { timeZone: event.tz } : {}) },
    extendedProperties: {
      private: {
        aestheticalManaged: "1",
        aestheticalUid: event.uid,
        aestheticalSeq: String(event.seq ?? 0),
      },
    },
  };
  if (event.rrule) body.recurrence = [`RRULE:${String(event.rrule).replace(/^RRULE:/i, "")}`];
  return body;
}

async function managedGoogleEvents(token, calendarId) {
  const query = new URLSearchParams({
    maxResults: "2500",
    showDeleted: "false",
    privateExtendedProperty: "aestheticalManaged=1",
  });
  return allPages(
    token,
    `/calendars/${encodeURIComponent(calendarId)}/events?${query}`,
    "items",
  );
}

async function sync() {
  const token = await accessToken();
  const identity = await primaryIdentity(token);
  if (identity.id.toLowerCase() !== "mail@aesthetic.computer") {
    die(`OAuth belongs to ${identity.id}, not mail@aesthetic.computer. Re-run auth with the correct account.`);
  }

  const calendar = await ensureCalendar(token);
  const source = await fetchAestheticalEvents();
  const existing = await managedGoogleEvents(token, calendar.id);
  const byUID = new Map(existing.map((item) => [item.extendedProperties?.private?.aestheticalUid, item]));
  const sourceUIDs = new Set(source.map((item) => item.uid));
  let created = 0;
  let updated = 0;
  let deleted = 0;
  let unchanged = 0;

  for (const event of source) {
    const mirrored = byUID.get(event.uid);
    const revision = String(event.seq ?? 0);
    if (!mirrored) {
      await googleRequest(token, `/calendars/${encodeURIComponent(calendar.id)}/events`, {
        method: "POST",
        body: googleEvent(event),
      });
      created++;
    } else if (mirrored.extendedProperties?.private?.aestheticalSeq !== revision) {
      await googleRequest(
        token,
        `/calendars/${encodeURIComponent(calendar.id)}/events/${encodeURIComponent(mirrored.id)}`,
        { method: "PATCH", body: googleEvent(event) },
      );
      updated++;
    } else {
      unchanged++;
    }
  }

  for (const mirrored of existing) {
    const uid = mirrored.extendedProperties?.private?.aestheticalUid;
    if (!uid || sourceUIDs.has(uid)) continue;
    await googleRequest(
      token,
      `/calendars/${encodeURIComponent(calendar.id)}/events/${encodeURIComponent(mirrored.id)}`,
      { method: "DELETE" },
    );
    deleted++;
  }

  saveJSON(STATE_PATH, {
    ...loadState(),
    calendarId: calendar.id,
    calendarName: CALENDAR_NAME,
    googleAccount: identity.id,
    lastSyncAt: new Date().toISOString(),
    lastSync: { source: source.length, created, updated, deleted, unchanged },
  });
  console.log(`✓ ${identity.id} · ${CALENDAR_NAME}`);
  console.log(`  source ${source.length} · created ${created} · updated ${updated} · deleted ${deleted} · unchanged ${unchanged}`);
}

async function status() {
  const token = await accessToken();
  const identity = await primaryIdentity(token);
  const state = loadState();
  console.log(`Google account · ${identity.id}`);
  console.log(`Mirror calendar · ${state.calendarName || "not created"}`);
  console.log(`Last sync · ${state.lastSyncAt || "never"}`);
  if (state.lastSync) console.log(`Last result · ${JSON.stringify(state.lastSync)}`);
}

try {
  if (command === "auth") await authorize();
  else if (command === "enable-api") await enableCalendarAPI();
  else if (command === "sync") await sync();
  else if (command === "status") await status();
  else {
    console.log("usage: gcal-sync.mjs auth | enable-api | sync | status");
    process.exitCode = 2;
  }
} catch (error) {
  die(error.message || String(error));
}
