#!/usr/bin/env node
// x.mjs — aesthetic.computer X (twitter) management CLI.
//
// Official X API v2 only, signed with OAuth 1.0a user context. That is the
// one auth mode that posts as the account without a refresh dance: four
// static strings, no token expiry, no browser round trip. (OAuth 2.0 PKCE
// also posts, but its tokens expire every two hours and need a stored
// refresh token — not worth it for a mirror cadence.)
//
// Credentials live in the vault, NOT this repo:
//   <aesthetic-computer>/vault/<account>/x.env
//     <PREFIX>_X_API_KEY=...        app consumer key
//     <PREFIX>_X_API_SECRET=...     app consumer secret
//     <PREFIX>_X_ACCESS_TOKEN=...   the account's access token
//     <PREFIX>_X_ACCESS_SECRET=...  the account's access token secret
// Prefix is PROMPTDOTAC / WHISTLEGRAPH. Env vars win over the vault file, so
// `source x.env && x.mjs ...` and a bare `x.mjs ...` both work.
//
// Provisioning walkthrough: toolchain/x/SETUP.md. Nothing here can create the
// developer app — that is a human step at developer.x.com.
//
// usage:
//   node toolchain/x/x.mjs accounts
//   node toolchain/x/x.mjs --as promptdotac me
//   node toolchain/x/x.mjs --as promptdotac search "tezos art -is:retweet"
//   node toolchain/x/x.mjs --as promptdotac budget
//   node toolchain/x/x.mjs --as promptdotac post "hello" [--media img.png] [--alt "..."]
//   node toolchain/x/x.mjs --as promptdotac reply <post-url-or-id> "hello"
//   node toolchain/x/x.mjs --as promptdotac post "hello" --dry-run
//   node toolchain/x/x.mjs --as promptdotac snapshot
//
// X is pay-per-use. Search reserves against a conservative local daily budget;
// the X console's Billing Cycle Cap remains the authoritative global guard.
// Posting tools require confirm:true when driven from MCP.

import { createHmac, randomBytes } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { basename, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(import.meta.dirname, "../..");

const API = "https://api.x.com";
const UPLOAD = "https://upload.twitter.com/1.1/media/upload.json";
const SEARCH_MIN_RESULTS = 10;
const SEARCH_MAX_RESULTS = 100;
const DEFAULT_READ_RATE_USD = 0.005;
const DEFAULT_DAILY_READ_BUDGET_USD = 0.25;
const money = (value) => Number(value.toFixed(6));

// Every account this CLI can drive, and the env-var prefix its vault file
// uses. Adding an account here is the whole registration step.
const ACCOUNTS = {
  promptdotac: "PROMPTDOTAC", // @promptDOTac — the AC brand outlet
  whistlegraph: "WHISTLEGRAPH", // @whistlegraph
};

// ── tiny arg parser ──────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const arg = argv[i];
  if (arg.startsWith("--")) {
    const key = arg.slice(2);
    const next = argv[i + 1];
    if (next === undefined || next.startsWith("--")) flags[key] = true;
    else { flags[key] = next; i++; }
  } else positional.push(arg);
}

// ── credentials ──────────────────────────────────────────────────────
function vaultFile(account) {
  return resolve(root, "vault", account, "x.env");
}

// Read one key: the environment first, then the account's vault file. The
// vault file is plain `KEY=value` lines, same shape as instagram.env.
function secret(account, suffix) {
  const key = `${ACCOUNTS[account]}_X_${suffix}`;
  if (process.env[key]) return process.env[key];
  const path = vaultFile(account);
  if (!existsSync(path)) return undefined;
  const match = readFileSync(path, "utf8").match(
    new RegExp(`^${key}=(.*)$`, "m"),
  );
  return match?.[1].trim().replace(/^["']|["']$/g, "") || undefined;
}

function credentials(account) {
  const creds = {
    apiKey: secret(account, "API_KEY"),
    apiSecret: secret(account, "API_SECRET"),
    accessToken: secret(account, "ACCESS_TOKEN"),
    accessSecret: secret(account, "ACCESS_SECRET"),
  };
  const missing = Object.entries(creds)
    .filter(([, value]) => !value)
    .map(([name]) => name);
  if (missing.length) {
    throw new Error(
      `@${account} is missing ${missing.join(", ")} — see toolchain/x/SETUP.md ` +
        `and fill ${vaultFile(account)}`,
    );
  }
  return creds;
}

function numericSetting(account, suffix, fallback) {
  const accountKey = `${ACCOUNTS[account]}_X_${suffix}`;
  const genericKey = `X_${suffix}`;
  let vaultValue;
  const path = vaultFile(account);
  if (existsSync(path)) {
    const match = readFileSync(path, "utf8").match(
      new RegExp(`^${accountKey}=(.*)$`, "m"),
    );
    vaultValue = match?.[1].trim().replace(/^["']|["']$/g, "") || undefined;
  }
  const raw = process.env[accountKey] ?? process.env[genericKey] ?? vaultValue;
  const value = raw === undefined ? fallback : Number(raw);
  if (!Number.isFinite(value) || value <= 0) {
    throw new Error(`${accountKey} must be a positive number`);
  }
  return value;
}

// ── OAuth 1.0a ───────────────────────────────────────────────────────
// RFC 3986, which is stricter than encodeURIComponent about these four.
const encode = (value) =>
  encodeURIComponent(String(value)).replace(
    /[!*'()]/g,
    (c) => "%" + c.charCodeAt(0).toString(16).toUpperCase(),
  );

// Build the Authorization header. `params` is the signed parameter set: query
// string values, plus form-encoded body values. A JSON body and a multipart
// body are both excluded from the signature — that is per spec, and it is why
// media upload below can post a raw file without any extra ceremony.
export function authorize(method, url, params, creds, fixed) {
  // `fixed` pins the nonce and timestamp so the signature is reproducible;
  // only the spec passes it, against X's published test vector.
  const oauth = {
    oauth_consumer_key: creds.apiKey,
    oauth_nonce: fixed?.nonce ?? randomBytes(16).toString("hex"),
    oauth_signature_method: "HMAC-SHA1",
    oauth_timestamp: fixed?.timestamp ?? Math.floor(Date.now() / 1000).toString(),
    oauth_token: creds.accessToken,
    oauth_version: "1.0",
  };
  const signed = { ...oauth, ...params };
  const base = Object.keys(signed)
    .sort()
    .map((key) => `${encode(key)}=${encode(signed[key])}`)
    .join("&");
  const baseString = [method.toUpperCase(), encode(url), encode(base)].join("&");
  const signingKey = `${encode(creds.apiSecret)}&${encode(creds.accessSecret)}`;
  oauth.oauth_signature = createHmac("sha1", signingKey)
    .update(baseString)
    .digest("base64");
  return (
    "OAuth " +
    Object.keys(oauth)
      .sort()
      .map((key) => `${encode(key)}="${encode(oauth[key])}"`)
      .join(", ")
  );
}

// X reports failures in three different shapes depending on which layer
// rejected you; flatten them so a caller sees one sentence.
export function explain(status, body) {
  let parsed;
  try { parsed = JSON.parse(body); } catch { return `${status}: ${body.slice(0, 400)}`; }
  const detail =
    parsed.detail ||
    parsed.title ||
    parsed.errors?.map((e) => e.message || e.detail).join("; ") ||
    body.slice(0, 400);
  const selfServeReplyRestriction =
    status === 403 && /only reply to or quote posts where you are mentioned or are the author/i.test(detail);
  const hint =
    status === 401
      ? " — check the four vault values, and that the app's user authentication is set to Read and Write"
      : selfServeReplyRestriction
        ? " — X self-serve API accounts may reply only when the original author mentioned or quoted the replying account; make this reply manually in X"
      : status === 403
        ? " — the app likely lacks Write permission, or the access token predates it (regenerate the token after changing permissions)"
        : status === 402
          ? " — the X API is pay-per-use and this account's credit balance is " +
            "empty. Buy credits at console.x.com → Billing → Credits (a payment " +
            "method has to be added first). Reads and media upload still work."
          : status === 429
            ? " — rate limit reached for this endpoint"
            : "";
  return `${status}: ${detail}${hint}`;
}

async function callApi(method, path, { query = {}, json } = {}, creds) {
  const url = `${API}${path}`;
  const search = new URLSearchParams(query).toString();
  const header = authorize(method, url, query, creds);
  const response = await fetch(search ? `${url}?${search}` : url, {
    method,
    headers: {
      Authorization: header,
      ...(json ? { "Content-Type": "application/json" } : {}),
    },
    body: json ? JSON.stringify(json) : undefined,
  });
  const body = await response.text();
  if (!response.ok) throw new Error(explain(response.status, body));
  return body ? JSON.parse(body) : {};
}

// ── media ────────────────────────────────────────────────────────────
const MIME = { png: "image/png", jpg: "image/jpeg", jpeg: "image/jpeg",
  gif: "image/gif", webp: "image/webp", mp4: "video/mp4" };

// v1.1 simple upload, multipart. Still the documented path for images, and
// the media_id it returns is what v2 /2/tweets wants.
async function uploadMedia(path, creds) {
  if (!existsSync(path)) throw new Error(`no such file: ${path}`);
  const extension = path.split(".").pop().toLowerCase();
  const type = MIME[extension];
  if (!type) throw new Error(`unsupported media type .${extension}`);
  const form = new FormData();
  form.append("media", new Blob([readFileSync(path)], { type }), basename(path));
  const response = await fetch(UPLOAD, {
    method: "POST",
    headers: { Authorization: authorize("POST", UPLOAD, {}, creds) },
    body: form,
  });
  const body = await response.text();
  if (!response.ok) throw new Error(explain(response.status, body));
  return JSON.parse(body).media_id_string;
}

// Alt text is a separate v1.1 call and is not optional for us — an image
// without it is an image some readers simply do not get.
async function describeMedia(mediaId, alt, creds) {
  const url = "https://api.x.com/1.1/media/metadata/create.json";
  const response = await fetch(url, {
    method: "POST",
    headers: {
      Authorization: authorize("POST", url, {}, creds),
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ media_id: mediaId, alt_text: { text: alt.slice(0, 1000) } }),
  });
  if (!response.ok) throw new Error(explain(response.status, await response.text()));
}

// ── counting ─────────────────────────────────────────────────────────
// X does not count characters, it weighs them. Every URL is rewritten to
// t.co and costs a flat 23 no matter how long it is, so a naive
// `text.length` rejects posts that would have been fine. (The full rule also
// double-weights CJK and emoji; this handles the URL case, which is the one
// that bites a link-carrying announcement.)
const URL_COST = 23;
export function weigh(text) {
  const urls = text.match(/https?:\/\/\S+/g) || [];
  return urls.reduce(
    (total, url) => total - url.length + URL_COST,
    [...text].length,
  );
}

export function parsePostId(value) {
  const target = String(value || "").trim();
  if (/^\d+$/.test(target)) return target;
  const match = target.match(
    /^https?:\/\/(?:www\.)?(?:x|twitter)\.com\/[^/]+\/status\/(\d+)(?:[/?#].*)?$/i,
  );
  if (match) return match[1];
  throw new Error("reply target must be an X post id or x.com status URL");
}

export function searchBudgetPlan({ spentUsd, maxResults, rateUsd, budgetUsd }) {
  for (const [name, value] of Object.entries(
    { spentUsd, maxResults, rateUsd, budgetUsd },
  )) {
    if (!Number.isFinite(value) || value < 0) throw new Error(`invalid ${name}`);
  }
  const estimatedUsd = money(maxResults * rateUsd);
  const remainingUsd = Math.max(0, budgetUsd - spentUsd);
  if (estimatedUsd > remainingUsd + Number.EPSILON) {
    throw new Error(
      `search blocked: up to $${estimatedUsd.toFixed(3)} would exceed the ` +
        `$${budgetUsd.toFixed(2)} local daily read budget ` +
        `($${spentUsd.toFixed(3)} already reserved)`,
    );
  }
  return { estimatedUsd, remainingUsd: money(remainingUsd - estimatedUsd) };
}

function usagePath(name) {
  return resolve(homedir(), ".local/state/aesthetic-computer/x-api", `${name}.json`);
}

function today() {
  return new Date().toISOString().slice(0, 10);
}

function readUsage(name) {
  const date = today();
  const path = usagePath(name);
  if (!existsSync(path)) return { date, reservedUsd: 0, searches: [] };
  try {
    const state = JSON.parse(readFileSync(path, "utf8"));
    if (state.date !== date) return { date, reservedUsd: 0, searches: [] };
    return {
      date,
      reservedUsd: Number(state.reservedUsd) || 0,
      searches: Array.isArray(state.searches) ? state.searches : [],
    };
  } catch {
    throw new Error(`cannot parse local X usage ledger: ${path}`);
  }
}

function writeUsage(name, state) {
  const path = usagePath(name);
  mkdirSync(resolve(path, ".."), { recursive: true, mode: 0o700 });
  const temporary = `${path}.${process.pid}.tmp`;
  writeFileSync(temporary, `${JSON.stringify(state, null, 2)}\n`, { mode: 0o600 });
  renameSync(temporary, path);
}

function readBudget(name) {
  const rateUsd = numericSetting(name, "READ_USD_PER_POST", DEFAULT_READ_RATE_USD);
  const budgetUsd = numericSetting(
    name,
    "DAILY_READ_BUDGET_USD",
    DEFAULT_DAILY_READ_BUDGET_USD,
  );
  return { rateUsd, budgetUsd };
}

function reserveSearch(name, query, maxResults) {
  const state = readUsage(name);
  const { rateUsd, budgetUsd } = readBudget(name);
  const plan = searchBudgetPlan({
    spentUsd: state.reservedUsd,
    maxResults,
    rateUsd,
    budgetUsd,
  });
  state.reservedUsd = money(state.reservedUsd + plan.estimatedUsd);
  state.searches.push({
    at: new Date().toISOString(),
    query,
    maxResults,
    estimatedUsd: plan.estimatedUsd,
  });
  writeUsage(name, state);
  return { ...plan, rateUsd, budgetUsd, path: usagePath(name) };
}

// ── commands ─────────────────────────────────────────────────────────
function account() {
  const name = flags.as || "promptdotac";
  if (!ACCOUNTS[name]) {
    throw new Error(`unknown account ${name}; use ${Object.keys(ACCOUNTS).join(", ")}`);
  }
  return name;
}

async function commandAccounts() {
  for (const name of Object.keys(ACCOUNTS)) {
    const path = vaultFile(name);
    let ready = true;
    try { credentials(name); } catch { ready = false; }
    console.log(
      `${ready ? "✅" : "❌"} @${name}  vault:${existsSync(path) ? "present" : "missing"}  ${path}`,
    );
  }
}

async function commandMe() {
  const name = account();
  const me = await callApi("GET", "/2/users/me",
    { query: { "user.fields": "public_metrics,description,created_at" } },
    credentials(name));
  const { username, name: display, public_metrics: metrics } = me.data;
  console.log(`@${username} — ${display}`);
  console.log(`  followers: ${metrics.followers_count}`);
  console.log(`  following: ${metrics.following_count}`);
  console.log(`  posts:     ${metrics.tweet_count}`);
}

async function commandPost({ text = positional[1], replyTo = flags["reply-to"] } = {}) {
  const name = account();
  if (!text) throw new Error('usage: post "<text>" [--media path] [--alt "..."]');
  const replyId = replyTo ? parsePostId(replyTo) : undefined;
  const length = weigh(text);
  if (length > 280) {
    throw new Error(`post weighs ${length} characters; X allows 280`);
  }
  if (flags.media && !flags.alt) {
    throw new Error("--alt is required with --media (describe the image)");
  }
  if (flags["dry-run"]) {
    console.log(`— dry run, as @${name} —`);
    if (replyId) console.log(`[reply] https://x.com/i/status/${replyId}`);
    console.log(text);
    if (flags.media) console.log(`[media] ${flags.media} — "${flags.alt}"`);
    console.log(`— ${length}/280 weighted characters —`);
    return;
  }
  const creds = credentials(name);
  const body = { text };
  if (replyId) body.reply = { in_reply_to_tweet_id: replyId };
  if (flags.media) {
    const mediaId = await uploadMedia(flags.media, creds);
    await describeMedia(mediaId, flags.alt, creds);
    body.media = { media_ids: [mediaId] };
    console.log(`uploaded ${flags.media} (${mediaId})`);
  }
  const posted = await callApi("POST", "/2/tweets", { json: body }, creds);
  console.log(`posted: https://x.com/${name}/status/${posted.data.id}`);
}

async function commandReply() {
  const target = positional[1];
  const text = positional[2];
  if (!target || !text) {
    throw new Error('usage: reply <post-url-or-id> "<text>" [--media path] [--alt "..."]');
  }
  return commandPost({ text, replyTo: target });
}

async function commandSearch() {
  const name = account();
  const query = positional[1];
  if (!query) throw new Error('usage: search "<query>" [--max-results 10..100]');
  if ([...query].length > 512) throw new Error("search query exceeds X's 512-character limit");
  const maxResults = Number(flags["max-results"] || SEARCH_MIN_RESULTS);
  if (!Number.isInteger(maxResults) ||
      maxResults < SEARCH_MIN_RESULTS || maxResults > SEARCH_MAX_RESULTS) {
    throw new Error(`--max-results must be an integer from ${SEARCH_MIN_RESULTS} to ${SEARCH_MAX_RESULTS}`);
  }
  const creds = credentials(name);
  const budget = reserveSearch(name, query, maxResults);
  const result = await callApi("GET", "/2/tweets/search/recent", { query: {
    query,
    max_results: maxResults,
    expansions: "author_id",
    "tweet.fields": "author_id,created_at,conversation_id,public_metrics,referenced_tweets",
    "user.fields": "name,username,public_metrics",
  } }, creds);
  const authors = new Map((result.includes?.users || []).map((user) => [user.id, user]));
  const posts = (result.data || []).map((post) => {
    const author = authors.get(post.author_id);
    return {
      id: post.id,
      url: author?.username
        ? `https://x.com/${author.username}/status/${post.id}`
        : `https://x.com/i/status/${post.id}`,
      author: author ? { name: author.name, username: `@${author.username}` } : undefined,
      createdAt: post.created_at,
      text: post.text,
      metrics: post.public_metrics,
      conversationId: post.conversation_id,
      referencedPosts: post.referenced_tweets,
    };
  });
  console.log(JSON.stringify({
    query,
    count: posts.length,
    budget: {
      reservedThisSearchUsd: budget.estimatedUsd,
      reservedTodayUsd: money(budget.budgetUsd - budget.remainingUsd),
      remainingTodayUsd: budget.remainingUsd,
      dailyLimitUsd: budget.budgetUsd,
      assumedReadRateUsd: budget.rateUsd,
    },
    posts,
  }, null, 2));
}

async function commandBudget() {
  const name = account();
  const state = readUsage(name);
  const { rateUsd, budgetUsd } = readBudget(name);
  console.log(JSON.stringify({
    account: `@${name}`,
    date: state.date,
    searches: state.searches.length,
    reservedUsd: state.reservedUsd,
    remainingUsd: money(Math.max(0, budgetUsd - state.reservedUsd)),
    dailyLimitUsd: budgetUsd,
    assumedReadRateUsd: rateUsd,
    ledger: usagePath(name),
    note: "Local estimate only; X Console Billing Cycle Cap is authoritative.",
  }, null, 2));
}

async function commandSnapshot() {
  const name = account();
  const me = await callApi("GET", "/2/users/me",
    { query: { "user.fields": "public_metrics" } }, credentials(name));
  console.log(JSON.stringify({
    date: new Date().toISOString().slice(0, 10),
    handle: `@${me.data.username}`,
    followers: me.data.public_metrics.followers_count,
    following: me.data.public_metrics.following_count,
    posts: me.data.public_metrics.tweet_count,
  }, null, 2));
}

const COMMANDS = { accounts: commandAccounts, me: commandMe,
  search: commandSearch, budget: commandBudget, post: commandPost,
  reply: commandReply, snapshot: commandSnapshot };

// Only dispatch when run as a program; the spec imports this file for its
// signing and counting helpers.
if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const command = COMMANDS[positional[0]];
  if (!command) {
    console.error(`usage: x.mjs [--as ${Object.keys(ACCOUNTS).join("|")}] <${Object.keys(COMMANDS).join("|")}>`);
    process.exit(1);
  }
  command().catch((error) => {
    console.error(error.message || error);
    process.exit(1);
  });
}
