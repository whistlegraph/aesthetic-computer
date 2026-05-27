#!/usr/bin/env node
// yt.mjs — aesthetic.computer YouTube posting pipeline.
//
// Zero-dependency CLI for the AC YouTube channel (signed in as
// mail@aesthetic.computer). Handles the one-time OAuth consent, then
// uploads videos via the YouTube Data API v3 resumable endpoint.
//
// Credentials live in the vault, NOT this repo:
//   aesthetic-computer-vault/youtube/client.json  — OAuth client (Desktop app)
//   aesthetic-computer-vault/youtube/token.json   — saved refresh token
//
// ── one-time setup ───────────────────────────────────────────────────
//  1. console.cloud.google.com → project `aesthetic-computer`
//     → APIs & Services → Library → enable "YouTube Data API v3".
//  2. APIs & Services → Credentials → Create credentials
//     → OAuth client ID → Application type "Desktop app".
//     Download the JSON, save it to
//       aesthetic-computer-vault/youtube/client.json
//  3. node toolchain/youtube/yt.mjs auth
//     → opens a browser, sign in as mail@aesthetic.computer, approve.
//     The refresh token is written to the vault; uploads are scripted
//     from then on (no browser needed again).
//
// ── usage ────────────────────────────────────────────────────────────
//   node toolchain/youtube/yt.mjs auth
//   node toolchain/youtube/yt.mjs whoami
//   node toolchain/youtube/yt.mjs upload <video.mp4> \
//        --title "..." \
//        --description-file <path>   (or --description "...") \
//        --tags a,b,c \
//        --privacy private|unlisted|public   (default: private) \
//        --category 10                       (default: 10 = Music) \
//        --thumbnail <image.jpg>             (optional) \
//        --playlist <playlistId>             (optional) \
//        --made-for-kids                     (default: not for kids)
//
// Every upload also writes a sidecar <video>.youtube.json receipt with
// the resulting videoId + watch URL + the metadata that was sent.

import { createServer } from "node:http";
import { spawn } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, statSync, createReadStream, mkdirSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const VAULT = resolve(REPO, "aesthetic-computer-vault/youtube");
const CLIENT_PATH = process.env.YT_CLIENT_JSON || resolve(VAULT, "client.json");
const TOKEN_PATH = process.env.YT_TOKEN_JSON || resolve(VAULT, "token.json");

const SCOPES = [
  "https://www.googleapis.com/auth/youtube.upload",
  "https://www.googleapis.com/auth/youtube.readonly",
  // Broader scope — needed for `delete` (and future edit / playlist /
  // thumbnail-set operations beyond what upload alone covers).
  "https://www.googleapis.com/auth/youtube",
];

// ── tiny arg parser ──────────────────────────────────────────────────
const argv = process.argv.slice(2);
const cmd = argv.shift();
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  } else positional.push(a);
}

function die(msg) { console.error(`✗ ${msg}`); process.exit(1); }

// ── client secret ────────────────────────────────────────────────────
function loadClient() {
  if (!existsSync(CLIENT_PATH)) {
    die(`OAuth client not found at ${CLIENT_PATH}\n` +
        `  Create a "Desktop app" OAuth client in the GCP project\n` +
        `  'aesthetic-computer' and save the JSON there. See header of this file.`);
  }
  const raw = JSON.parse(readFileSync(CLIENT_PATH, "utf8"));
  const c = raw.installed || raw.web || raw;
  if (!c.client_id || !c.client_secret) die(`${CLIENT_PATH} is missing client_id / client_secret`);
  return { id: c.client_id, secret: c.client_secret };
}

function loadToken() {
  if (!existsSync(TOKEN_PATH)) die(`No saved token. Run: node toolchain/youtube/yt.mjs auth`);
  return JSON.parse(readFileSync(TOKEN_PATH, "utf8"));
}

function saveToken(tok) {
  mkdirSync(VAULT, { recursive: true });
  writeFileSync(TOKEN_PATH, JSON.stringify(tok, null, 2));
}

// ── OAuth ────────────────────────────────────────────────────────────
async function exchangeToken(params) {
  const res = await fetch("https://oauth2.googleapis.com/token", {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams(params),
  });
  const json = await res.json();
  if (!res.ok) die(`token endpoint ${res.status}: ${JSON.stringify(json)}`);
  return json;
}

// Returns a fresh access token, refreshing via the saved refresh_token.
async function accessToken() {
  const client = loadClient();
  const tok = loadToken();
  if (!tok.refresh_token) die(`saved token has no refresh_token — re-run 'auth'`);
  const json = await exchangeToken({
    client_id: client.id,
    client_secret: client.secret,
    refresh_token: tok.refresh_token,
    grant_type: "refresh_token",
  });
  return json.access_token;
}

function openBrowser(url) {
  const cmd = process.platform === "darwin" ? "open"
            : process.platform === "win32" ? "start" : "xdg-open";
  spawn(cmd, [url], { stdio: "ignore", detached: true }).unref();
}

async function doAuth() {
  const client = loadClient();
  // Loopback redirect — Desktop-app OAuth clients allow http://localhost.
  const code = await new Promise((res, rej) => {
    const server = createServer((req, resp) => {
      const u = new URL(req.url, "http://localhost");
      if (!u.searchParams.has("code") && !u.searchParams.has("error")) {
        resp.writeHead(404); resp.end(); return;
      }
      const err = u.searchParams.get("error");
      resp.writeHead(200, { "Content-Type": "text/html" });
      resp.end(`<!doctype html><meta charset=utf8><body style="font-family:monospace;background:#111;color:#aef240;padding:3em">` +
        (err ? `<h2>✗ ${err}</h2>` : `<h2>✓ aesthetic.computer YouTube authorized</h2><p>You can close this tab.</p>`) +
        `</body>`);
      server.close();
      if (err) rej(new Error(err)); else res(u.searchParams.get("code"));
    });
    server.listen(0, "127.0.0.1", () => {
      const port = server.address().port;
      const redirect = `http://127.0.0.1:${port}`;
      const authUrl = "https://accounts.google.com/o/oauth2/v2/auth?" + new URLSearchParams({
        client_id: client.id,
        redirect_uri: redirect,
        response_type: "code",
        scope: SCOPES.join(" "),
        access_type: "offline",
        prompt: "consent",
      });
      server._redirect = redirect;
      console.log(`\n▸ Opening browser — sign in as mail@aesthetic.computer and approve.`);
      console.log(`  If it doesn't open, paste this URL:\n\n  ${authUrl}\n`);
      openBrowser(authUrl);
    });
    // stash redirect for the exchange step
    doAuth._serverRef = server;
  });
  const redirect = doAuth._serverRef._redirect;
  const tok = await exchangeToken({
    client_id: client.id,
    client_secret: client.secret,
    code,
    grant_type: "authorization_code",
    redirect_uri: redirect,
  });
  if (!tok.refresh_token) {
    die(`Google returned no refresh_token. Revoke prior access at\n` +
        `  https://myaccount.google.com/permissions  then re-run 'auth'.`);
  }
  saveToken({ refresh_token: tok.refresh_token, obtained: new Date().toISOString(), scopes: SCOPES });
  console.log(`✓ refresh token saved → ${TOKEN_PATH}`);
  await doWhoami();
}

// ── whoami ───────────────────────────────────────────────────────────
async function doWhoami() {
  const at = await accessToken();
  const res = await fetch(
    "https://www.googleapis.com/youtube/v3/channels?part=snippet,statistics&mine=true",
    { headers: { Authorization: `Bearer ${at}` } });
  const json = await res.json();
  if (!res.ok) die(`channels.list ${res.status}: ${JSON.stringify(json)}`);
  const ch = json.items?.[0];
  if (!ch) die(`no channel on this account`);
  console.log(`✓ channel · ${ch.snippet.title}`);
  console.log(`  id        · ${ch.id}`);
  console.log(`  videos    · ${ch.statistics?.videoCount ?? "?"}`);
  console.log(`  subs      · ${ch.statistics?.subscriberCount ?? "?"}`);
  console.log(`  url       · https://www.youtube.com/channel/${ch.id}`);
  return ch;
}

// ── upload (resumable) ───────────────────────────────────────────────
async function doUpload() {
  const file = positional[0];
  if (!file) die(`usage: yt.mjs upload <video> --title "..." [...]`);
  const videoPath = resolve(process.cwd(), file);
  if (!existsSync(videoPath)) die(`video not found: ${videoPath}`);
  if (!flags.title) die(`--title is required`);

  let description = flags.description || "";
  if (flags["description-file"]) {
    const dp = resolve(process.cwd(), flags["description-file"]);
    if (!existsSync(dp)) die(`description file not found: ${dp}`);
    description = readFileSync(dp, "utf8").trimEnd();
  }

  const privacy = (flags.privacy || "private").toLowerCase();
  if (!["private", "unlisted", "public"].includes(privacy)) die(`--privacy must be private|unlisted|public`);

  const tags = flags.tags ? String(flags.tags).split(",").map((t) => t.trim()).filter(Boolean) : undefined;

  const metadata = {
    snippet: {
      title: String(flags.title),
      description,
      categoryId: String(flags.category || "10"), // 10 = Music
      ...(tags ? { tags } : {}),
    },
    status: {
      privacyStatus: privacy,
      selfDeclaredMadeForKids: Boolean(flags["made-for-kids"]),
      embeddable: true,
    },
  };

  const at = await accessToken();
  const size = statSync(videoPath).size;
  console.log(`▸ uploading ${basename(videoPath)} (${(size / 1e6).toFixed(1)} MB) · privacy=${privacy}`);

  // 1. start a resumable session
  const startRes = await fetch(
    "https://www.googleapis.com/upload/youtube/v3/videos?uploadType=resumable&part=snippet,status",
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${at}`,
        "Content-Type": "application/json",
        "X-Upload-Content-Type": "video/*",
        "X-Upload-Content-Length": String(size),
      },
      body: JSON.stringify(metadata),
    });
  if (startRes.status !== 200 && startRes.status !== 201) {
    die(`resumable start ${startRes.status}: ${await startRes.text()}`);
  }
  const uploadUrl = startRes.headers.get("location");
  if (!uploadUrl) die(`no upload URL returned`);

  // 2. PUT the bytes (single shot — these videos are well under any
  //    sane chunk limit; node streams the file body).
  console.log(`▸ transferring bytes…`);
  const putRes = await fetch(uploadUrl, {
    method: "PUT",
    headers: { "Content-Type": "video/*", "Content-Length": String(size) },
    body: createReadStream(videoPath),
    duplex: "half",
  });
  const result = await putRes.json();
  if (!putRes.ok) die(`upload ${putRes.status}: ${JSON.stringify(result)}`);

  const videoId = result.id;
  const watchUrl = `https://youtu.be/${videoId}`;
  const studioUrl = `https://studio.youtube.com/video/${videoId}/edit`;
  console.log(`✓ uploaded · ${videoId}`);
  console.log(`  watch  · ${watchUrl}`);
  console.log(`  studio · ${studioUrl}`);

  // 3. optional thumbnail
  if (flags.thumbnail) {
    const thumbPath = resolve(process.cwd(), flags.thumbnail);
    if (!existsSync(thumbPath)) {
      console.error(`⚠ thumbnail not found: ${thumbPath} — skipped`);
    } else {
      const tRes = await fetch(
        `https://www.googleapis.com/upload/youtube/v3/thumbnails/set?videoId=${videoId}`,
        {
          method: "POST",
          headers: {
            Authorization: `Bearer ${at}`,
            "Content-Type": thumbPath.endsWith(".png") ? "image/png" : "image/jpeg",
          },
          body: readFileSync(thumbPath),
        });
      if (tRes.ok) console.log(`✓ thumbnail set`);
      else console.error(`⚠ thumbnail ${tRes.status}: ${await tRes.text()}`);
    }
  }

  // 4. optional playlist insert
  if (flags.playlist) {
    const pRes = await fetch(
      "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet",
      {
        method: "POST",
        headers: { Authorization: `Bearer ${at}`, "Content-Type": "application/json" },
        body: JSON.stringify({
          snippet: { playlistId: String(flags.playlist), resourceId: { kind: "youtube#video", videoId } },
        }),
      });
    if (pRes.ok) console.log(`✓ added to playlist ${flags.playlist}`);
    else console.error(`⚠ playlist ${pRes.status}: ${await pRes.text()}`);
  }

  // 5. receipt sidecar
  const receipt = {
    videoId, watchUrl, studioUrl,
    uploadedAt: new Date().toISOString(),
    privacy, file: videoPath, bytes: size,
    metadata,
  };
  const sidecar = videoPath.replace(/\.[^.]+$/, "") + ".youtube.json";
  writeFileSync(sidecar, JSON.stringify(receipt, null, 2));
  console.log(`✓ receipt · ${sidecar}`);
}

// ── update a live video's metadata ──────────────────────────────────
// Patch title / description / tags / category / privacy on an existing
// upload. YouTube's videos.update requires a FULL snippet (it replaces,
// not merges) so we first GET the current snippet, then overlay the
// flags onto it, then PUT it back.
//
//   yt.mjs update-meta <videoId> \
//     [--title "..."] [--description "..." | --description-file <path>] \
//     [--tags a,b,c] [--category 10] [--privacy public|unlisted|private]
async function doUpdateMeta() {
  const id = positional[0];
  if (!id) die(`usage: yt.mjs update-meta <videoId> [--title ...] [--description-file ...] [--tags ...] [--privacy ...]`);
  const at = await accessToken();

  const getRes = await fetch(
    `https://www.googleapis.com/youtube/v3/videos?part=snippet,status&id=${encodeURIComponent(id)}`,
    { headers: { Authorization: `Bearer ${at}` } });
  const getJson = await getRes.json();
  if (!getRes.ok) die(`videos.list ${getRes.status}: ${JSON.stringify(getJson)}`);
  const cur = getJson.items?.[0];
  if (!cur) die(`video not found: ${id}`);

  let description = cur.snippet.description;
  if (flags["description-file"]) {
    const dp = resolve(process.cwd(), flags["description-file"]);
    if (!existsSync(dp)) die(`description file not found: ${dp}`);
    description = readFileSync(dp, "utf8").trimEnd();
  } else if (flags.description !== undefined) {
    description = String(flags.description);
  }

  const tags = flags.tags !== undefined
    ? String(flags.tags).split(",").map((t) => t.trim()).filter(Boolean)
    : cur.snippet.tags;

  const snippet = {
    title: flags.title !== undefined ? String(flags.title) : cur.snippet.title,
    description,
    categoryId: flags.category !== undefined ? String(flags.category) : cur.snippet.categoryId,
    ...(tags ? { tags } : {}),
    defaultLanguage: cur.snippet.defaultLanguage,
  };
  Object.keys(snippet).forEach((k) => snippet[k] === undefined && delete snippet[k]);

  const body = { id, snippet };
  let parts = "snippet";
  if (flags.privacy !== undefined) {
    const p = String(flags.privacy).toLowerCase();
    if (!["private", "unlisted", "public"].includes(p)) die(`--privacy must be private|unlisted|public`);
    body.status = { privacyStatus: p, selfDeclaredMadeForKids: cur.status?.selfDeclaredMadeForKids ?? false };
    parts += ",status";
  }

  const putRes = await fetch(
    `https://www.googleapis.com/youtube/v3/videos?part=${parts}`,
    {
      method: "PUT",
      headers: { Authorization: `Bearer ${at}`, "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
  const putJson = await putRes.json();
  if (!putRes.ok) die(`videos.update ${putRes.status}: ${JSON.stringify(putJson)}`);
  console.log(`✓ updated ${id}`);
  console.log(`  title  · ${putJson.snippet.title}`);
  console.log(`  watch  · https://youtu.be/${id}`);
  console.log(`  studio · https://studio.youtube.com/video/${id}/edit`);
}

// ── delete a video ──────────────────────────────────────────────────
// NOTE: requires the broader `youtube` scope (the upload-only scope
// can't delete). If a 403 comes back, re-run `auth` after editing
// SCOPES at the top of this file.
async function doDelete() {
  const id = positional[0];
  if (!id) die(`usage: yt.mjs delete <videoId>`);
  const at = await accessToken();
  const res = await fetch(`https://www.googleapis.com/youtube/v3/videos?id=${encodeURIComponent(id)}`, {
    method: "DELETE",
    headers: { Authorization: `Bearer ${at}` },
  });
  if (res.status === 204) { console.log(`✓ deleted ${id}`); return; }
  const body = await res.text();
  die(`delete ${res.status}: ${body}`);
}

// ── set thumbnail on an existing video ──────────────────────────────
// Useful when the thumbnail upload at video-upload time fails (e.g. the
// source JPG was >2 MB and YouTube rejected it). Run the same call the
// upload path uses, but against an already-published videoId.
async function doThumbnail() {
  const id = positional[0];
  const img = positional[1];
  if (!id || !img) die(`usage: yt.mjs thumbnail <videoId> <image.jpg|png>`);
  const imgPath = resolve(process.cwd(), img);
  if (!existsSync(imgPath)) die(`thumbnail not found: ${imgPath}`);
  const at = await accessToken();
  const res = await fetch(
    `https://www.googleapis.com/upload/youtube/v3/thumbnails/set?videoId=${encodeURIComponent(id)}`,
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${at}`,
        "Content-Type": imgPath.endsWith(".png") ? "image/png" : "image/jpeg",
      },
      body: readFileSync(imgPath),
    });
  if (!res.ok) die(`thumbnail ${res.status}: ${await res.text()}`);
  console.log(`✓ thumbnail set on ${id}`);
  console.log(`  watch  · https://youtu.be/${id}`);
}

// ── dispatch ─────────────────────────────────────────────────────────
const COMMANDS = {
  auth: doAuth,
  whoami: doWhoami,
  upload: doUpload,
  "update-meta": doUpdateMeta,
  thumbnail: doThumbnail,
  delete: doDelete,
};

if (!cmd || !COMMANDS[cmd]) {
  console.log(`yt.mjs — aesthetic.computer YouTube pipeline\n`);
  console.log(`commands:`);
  console.log(`  auth                 one-time OAuth consent (browser)`);
  console.log(`  whoami               print the authorized channel`);
  console.log(`  upload <video> ...   resumable upload (see file header for flags)`);
  process.exit(cmd ? 1 : 0);
}

try {
  await COMMANDS[cmd]();
} catch (e) {
  die(e.stack || e.message || String(e));
}
