// pulse.mjs — ask a named group of people what they are saying.
//
// A group is a hand-kept list of accounts from one community (security
// reporters, Tezos artists) with their handles on Bluesky, X and Mastodon.
// `pulse` reads each member's recent posts, keeps the ones on topic, and
// returns them as dated rows. `find` proposes who belongs in a group.
//
// The code is public; the groups, questions and saved runs are private
// research and live in the vault under research/social/ (AC_PULSE_HOME
// overrides the location).
//
// Bluesky and Mastodon are read through their public, login-free APIs.
// X goes through toolchain/x/x.mjs, which is pay-per-use and budgeted.

import { spawn } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { vaultRoot } from "../../toolchain/vault/credentials.mjs";

const REPO = resolve(import.meta.dirname, "../..");
const X_CLI = resolve(REPO, "toolchain/x/x.mjs");
const BSKY = "https://api.bsky.app/xrpc";
const MASTODON_HOME = "mastodon.social";
const PLATFORMS = ["bluesky", "x", "mastodon"];
const SEEN_DAYS = 30;

// ---- where the private data lives ----

export function pulseHome(env = process.env) {
  if (env.AC_PULSE_HOME) return resolve(env.AC_PULSE_HOME);
  const vault = vaultRoot({ env });
  if (!vault) throw new Error("no vault found; set AC_VAULT or AC_PULSE_HOME");
  return resolve(vault, "research/social");
}

const slugOf = (value) => String(value || "").toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
const readJson = (path, fallback) => existsSync(path) ? JSON.parse(readFileSync(path, "utf8")) : fallback;
function writeJson(path, value) {
  mkdirSync(resolve(path, ".."), { recursive: true });
  writeFileSync(path, JSON.stringify(value, null, 2) + "\n");
}

// ---- groups ----

const groupPath = (slug, home = pulseHome()) => resolve(home, "groups", `${slugOf(slug)}.json`);

export function readGroup(slug, home = pulseHome()) {
  const group = readJson(groupPath(slug, home), null);
  if (!group) throw new Error(`no group "${slug}"; have: ${listGroups(home).map((g) => g.slug).join(", ") || "none"}`);
  return group;
}

export function listGroups(home = pulseHome()) {
  const dir = resolve(home, "groups");
  if (!existsSync(dir)) return [];
  return readdirSync(dir).filter((f) => f.endsWith(".json")).map((file) => {
    const group = readJson(resolve(dir, file));
    const state = readJson(resolve(home, "state", file), {});
    return { slug: group.slug, title: group.title, about: group.about, members: group.members.length,
      keywords: group.keywords || [], lastRun: state.lastRun || null, lastQuestion: state.lastQuestion || null };
  });
}

// A member is matched by name, so saving the same person again updates them.
export function saveGroup({ slug, title, about, keywords, add = [], remove = [] }, home = pulseHome()) {
  const id = slugOf(slug);
  if (!id) throw new Error("a group needs a slug");
  const now = new Date().toISOString();
  const group = readJson(groupPath(id, home), { slug: id, title: title || id, about: "", keywords: [], members: [], created: now });
  if (title) group.title = title;
  if (about !== undefined) group.about = about;
  if (keywords) group.keywords = keywords;
  const key = (name) => String(name).trim().toLowerCase();
  const gone = new Set(remove.map(key));
  group.members = group.members.filter((m) => !gone.has(key(m.name)));
  for (const member of add) {
    if (!member.name) throw new Error("each member needs a name");
    const clean = Object.fromEntries(Object.entries({
      name: member.name.trim(), role: member.role, bluesky: bare(member.bluesky), x: bare(member.x), mastodon: bare(member.mastodon),
    }).filter(([, v]) => v));
    const at = group.members.findIndex((m) => key(m.name) === key(clean.name));
    if (at >= 0) group.members[at] = { ...group.members[at], ...clean, name: group.members[at].name };
    else group.members.push(clean);
  }
  group.updated = now;
  writeJson(groupPath(id, home), group);
  return group;
}

const bare = (handle) => handle ? String(handle).trim().replace(/^@/, "") : undefined;

// ---- small helpers ----

async function getJson(url, params = {}, { tries = 2, timeout = 15_000 } = {}) {
  const query = new URLSearchParams(Object.entries(params).filter(([, v]) => v !== undefined && v !== null)).toString();
  let last;
  for (let attempt = 0; attempt < tries; attempt++) {
    try {
      const response = await fetch(query ? `${url}?${query}` : url, {
        headers: { accept: "application/json", "user-agent": "pulse/1" }, signal: AbortSignal.timeout(timeout) });
      if (!response.ok) throw new Error(`${response.status} ${new URL(url).host}`);
      return await response.json();
    } catch (error) { last = error; }
  }
  throw last;
}

// Run jobs a few at a time; a failed job yields { error } instead of throwing.
async function pool(items, size, job) {
  const results = new Array(items.length);
  let next = 0;
  await Promise.all(Array.from({ length: Math.min(size, items.length) }, async () => {
    while (next < items.length) {
      const index = next++;
      try { results[index] = await job(items[index]); }
      catch (error) { results[index] = { error: error.message || String(error) }; }
    }
  }));
  return results;
}

const plain = (html) => String(html || "").replace(/<br\s*\/?>/gi, " ").replace(/<\/p>\s*<p>/gi, " ").replace(/<[^>]+>/g, "")
  .replace(/&amp;/g, "&").replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&#39;|&apos;/g, "'").replace(/\s+/g, " ").trim();
const oneLine = (value) => String(value || "").replace(/\s+/g, " ").trim();

// ---- Bluesky ----

function bskyPost(post) {
  const record = post.record || {}, embed = post.embed || {};
  const quoted = embed.record?.value?.text || embed.record?.record?.value?.text;
  const link = embed.external?.title || embed.media?.external?.title;
  return { platform: "bluesky", id: post.uri, url: `https://bsky.app/profile/${post.author.handle}/post/${post.uri.split("/").pop()}`,
    handle: post.author.handle, name: post.author.displayName || post.author.handle, at: record.createdAt,
    text: oneLine(record.text), quoted: oneLine(quoted), link: oneLine(link),
    likes: post.likeCount || 0, reposts: post.repostCount || 0 };
}

export async function bskyFeed(handle, limit = 50) {
  const data = await getJson(`${BSKY}/app.bsky.feed.getAuthorFeed`, { actor: handle, limit, filter: "posts_and_author_threads" });
  return (data.feed || []).filter((item) => !item.reason).map((item) => bskyPost(item.post));
}

export async function bskySearch(q, { since, limit = 25, sort = "top" } = {}) {
  const data = await getJson(`${BSKY}/app.bsky.feed.searchPosts`, { q, sort, limit, since });
  return (data.posts || []).map(bskyPost);
}

async function bskyProfiles(handles) {
  const out = [];
  for (let i = 0; i < handles.length; i += 25) {
    const query = handles.slice(i, i + 25).map((h) => `actors=${encodeURIComponent(h)}`).join("&");
    const response = await fetch(`${BSKY}/app.bsky.actor.getProfiles?${query}`, { signal: AbortSignal.timeout(15_000) });
    if (response.ok) out.push(...((await response.json()).profiles || []));
  }
  return out;
}

export async function bskyPacks(q, limit = 8) {
  const data = await getJson(`${BSKY}/app.bsky.graph.searchStarterPacks`, { q, limit });
  return (data.starterPacks || []).map((pack) => ({ uri: pack.uri, name: pack.record?.name,
    about: oneLine(pack.record?.description), by: pack.creator?.handle, joined: pack.joinedAllTimeCount || 0 }));
}

export async function bskyPackMembers(uri, limit = 100) {
  const pack = await getJson(`${BSKY}/app.bsky.graph.getStarterPack`, { starterPack: uri });
  const list = pack.starterPack?.list?.uri;
  if (!list) throw new Error("that starter pack has no member list");
  const data = await getJson(`${BSKY}/app.bsky.graph.getList`, { list, limit });
  return (data.items || []).map((item) => item.subject.handle);
}

// ---- Mastodon ----

function mastoPost(status) {
  const account = status.account || {};
  const host = new URL(status.url || status.uri).host;
  const handle = account.acct?.includes("@") ? account.acct : `${account.acct}@${host}`;
  return { platform: "mastodon", id: status.uri, url: status.url || status.uri, handle, name: account.display_name || account.username,
    at: status.created_at, text: plain(status.content), quoted: "", link: oneLine(status.card?.title),
    likes: status.favourites_count || 0, reposts: status.reblogs_count || 0 };
}

// handle is user@instance; the instance itself is asked, so no login is needed.
export async function mastoFeed(handle, limit = 40) {
  const [user, host] = bare(handle).split("@");
  if (!host) throw new Error(`mastodon handle needs an instance: ${handle}`);
  const account = await getJson(`https://${host}/api/v1/accounts/lookup`, { acct: user });
  const statuses = await getJson(`https://${host}/api/v1/accounts/${account.id}/statuses`, { limit, exclude_reblogs: true });
  return statuses.map(mastoPost);
}

export async function mastoTag(tag, { host = MASTODON_HOME, limit = 40 } = {}) {
  const statuses = await getJson(`https://${host}/api/v1/timelines/tag/${encodeURIComponent(tag.replace(/^#/, ""))}`, { limit });
  return statuses.map(mastoPost);
}

export async function mastoActors(q, { host = MASTODON_HOME, limit = 10 } = {}) {
  const data = await getJson(`https://${host}/api/v2/search`, { q, type: "accounts", limit });
  return (data.accounts || []).map((a) => ({ platform: "mastodon", handle: a.acct.includes("@") ? a.acct : `${a.acct}@${host}`,
    name: a.display_name || a.username, bio: plain(a.note), followers: a.followers_count || 0 }));
}

// ---- X (paid; every search reserves against the local daily budget) ----

function runX(args, timeout = 120_000) {
  return new Promise((done, reject) => {
    const child = spawn(process.execPath, [X_CLI, ...args], { cwd: REPO, env: process.env, stdio: ["ignore", "pipe", "pipe"] });
    let out = "", err = "";
    child.stdout.on("data", (chunk) => { out += chunk; });
    child.stderr.on("data", (chunk) => { err += chunk; });
    const timer = setTimeout(() => { child.kill("SIGTERM"); reject(new Error("x search timed out")); }, timeout);
    child.on("error", reject);
    child.on("close", (code) => { clearTimeout(timer); code ? reject(new Error((err || out).trim().slice(0, 300) || `x exited ${code}`)) : done(out); });
  });
}

export async function xSearch(query, { max = 20, account = "promptdotac" } = {}) {
  const raw = await runX(["--as", account, "search", query, "--max-results", String(Math.min(100, Math.max(10, max)))]);
  const data = JSON.parse(raw.slice(raw.indexOf("{")));
  const posts = (data.posts || []).map((post) => ({ platform: "x", id: post.url, url: post.url, handle: bare(post.author?.username),
    name: post.author?.name || bare(post.author?.username), at: post.createdAt, text: oneLine(post.text), quoted: "", link: "",
    likes: post.metrics?.like_count || 0, reposts: post.metrics?.retweet_count || 0 }));
  return { posts, budget: data.budget };
}

// X caps a query at 512 characters, so members are packed into as few
// `(from:a OR from:b) (term OR term)` queries as fit.
export function xQueries(handles, terms = [], limit = 512) {
  const topic = terms.length ? ` (${terms.map((t) => /\s/.test(t) ? `"${t}"` : t).join(" OR ")})` : "";
  const tail = `${topic} -is:retweet`;
  const queries = [];
  let batch = [];
  const build = (list) => `(${list.map((h) => `from:${h}`).join(" OR ")})${tail}`;
  for (const handle of handles) {
    if (batch.length && build([...batch, handle]).length > limit) { queries.push(build(batch)); batch = []; }
    batch.push(handle);
  }
  if (batch.length) queries.push(build(batch));
  return queries;
}

// ---- matching and formatting ----

const escapeRe = (s) => s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
export function matcher(terms = []) {
  const clean = terms.map((t) => String(t).trim()).filter(Boolean);
  if (!clean.length) return () => true;
  // Word boundaries keep "nsa" from matching "unsafe"; phrases match as written.
  const re = new RegExp(clean.map((t) => `(?<![\\p{L}\\p{N}])${escapeRe(t)}(?![\\p{L}\\p{N}])`).join("|"), "iu");
  return (post) => re.test(`${post.text} ${post.quoted || ""} ${post.link || ""}`);
}

const stamp = (iso, timeZone) => new Intl.DateTimeFormat("en-US", { timeZone, weekday: "short", day: "numeric", month: "short",
  hour: "2-digit", minute: "2-digit", hour12: false }).format(new Date(iso)).replace(/,/g, "");
const cell = (value) => String(value).replace(/\|/g, "/").replace(/\s+/g, " ");

export function table(rows, { max = 280 } = {}) {
  if (!rows.length) return "(no posts)";
  const lines = ["| When (UTC) | LA time | Who | Where | Post |", "|---|---|---|---|---|"];
  for (const row of rows) {
    const who = row.role ? `${row.name} (${row.role})` : row.name;
    const extra = row.quoted ? ` // quoting: ${row.quoted.slice(0, 120)}` : row.link ? ` // link: ${row.link.slice(0, 100)}` : "";
    const text = (row.text.length > max ? row.text.slice(0, max - 1) + "…" : row.text) + extra + (row.more ? ` (+${row.more} more in thread)` : "");
    lines.push(`| ${stamp(row.at, "UTC")} | ${stamp(row.at, "America/Los_Angeles")} | ${cell(who)} | ${row.platform}${row.also?.length ? ` (+${row.also.join(", ")})` : ""}${row.fresh ? " ·new" : ""} · ${row.likes}♥ | ${cell(text)} — ${row.url} |`);
  }
  return lines.join("\n");
}

// A thread arrives as many posts from one person within a minute or two.
// Keep its first post and count the rest, so one long thread is one row.
export function collapseThreads(rows, gapMs = 120_000) {
  const out = [], open = new Map();
  for (const row of rows) {
    const key = `${row.platform}:${row.handle}`, head = open.get(key);
    if (head && new Date(row.at) - new Date(head.last) <= gapMs) {
      head.row.more = (head.row.more || 0) + 1;
      head.row.likes = Math.max(head.row.likes, row.likes);
      head.row.fresh ||= row.fresh;
      head.row.ids.push(row.id);
      head.last = row.at;
      continue;
    }
    const first = { ...row, ids: [row.id] };
    open.set(key, { row: first, last: row.at });
    out.push(first);
  }
  return out;
}

// The same person often posts the same words on two or three platforms.
// Keep the best-liked copy and name the others on it.
export function mergeCrossPosts(rows, gapMs = 15 * 60_000) {
  const out = [];
  const same = (a, b) => a.name === b.name && a.platform !== b.platform &&
    Math.abs(new Date(a.at) - new Date(b.at)) <= gapMs && a.text.slice(0, 60).toLowerCase() === b.text.slice(0, 60).toLowerCase();
  for (const row of rows) {
    const twin = out.find((kept) => same(kept, row) && !(kept.also || []).includes(row.platform));
    if (!twin) { out.push({ ...row }); continue; }
    const [keep, drop] = row.likes > twin.likes ? [row, twin] : [twin, row];
    Object.assign(twin, { ...keep, ids: [...twin.ids, ...row.ids], fresh: twin.fresh || row.fresh,
      also: [...new Set([...(twin.also || []), drop.platform])].filter((p) => p !== keep.platform) });
  }
  return out;
}

// ---- pulse: read a group ----

export async function pulse({ group: slug, question = "", terms, hours = 24, platforms, all = false, xMax = 20, save = true } = {}, home = pulseHome()) {
  const group = readGroup(slug, home);
  const use = (platforms?.length ? platforms : PLATFORMS).filter((p) => PLATFORMS.includes(p));
  const want = all ? [] : (terms?.length ? terms : group.keywords || []);
  const keep = matcher(want);
  const since = new Date(Date.now() - hours * 3600_000);
  const inWindow = (post) => post.at && new Date(post.at) >= since;
  const byHandle = new Map();
  const tag = (platform, handle, member) => byHandle.set(`${platform}:${String(handle).toLowerCase()}`, member);
  const problems = [], jobs = [];

  for (const member of group.members) {
    if (use.includes("bluesky") && member.bluesky) { tag("bluesky", member.bluesky, member); jobs.push({ member, platform: "bluesky", read: () => bskyFeed(member.bluesky) }); }
    if (use.includes("mastodon") && member.mastodon) { tag("mastodon", member.mastodon, member); jobs.push({ member, platform: "mastodon", read: () => mastoFeed(member.mastodon) }); }
    if (use.includes("x") && member.x) tag("x", member.x, member);
  }

  const read = await pool(jobs, 6, (job) => job.read());
  let posts = [];
  read.forEach((result, index) => {
    if (result.error) problems.push(`${jobs[index].member.name} on ${jobs[index].platform}: ${result.error}`);
    else posts.push(...result);
  });

  let budget = null;
  const xHandles = use.includes("x") ? group.members.filter((m) => m.x).map((m) => m.x) : [];
  for (const query of xQueries(xHandles, want)) {
    try { const found = await xSearch(query, { max: xMax }); posts.push(...found.posts); budget = found.budget; }
    catch (error) { problems.push(`x: ${error.message}`); break; }
  }

  // Mark what was not there on the last look, then remember everything shown.
  const statePath = resolve(home, "state", `${group.slug}.json`);
  const state = readJson(statePath, { seen: {} });
  const now = new Date().toISOString();
  const unique = new Map();
  for (const post of posts) if (inWindow(post) && keep(post)) unique.set(post.id, post);
  const every = [...unique.values()].sort((a, b) => a.at.localeCompare(b.at)).map((post) => {
    const member = byHandle.get(`${post.platform}:${String(post.handle).toLowerCase()}`);
    return { ...post, name: member?.name || post.name, role: member?.role || "", fresh: !state.seen[post.id] };
  });

  const rows = mergeCrossPosts(collapseThreads(every));
  const spoke = new Set(rows.map((row) => row.name));
  const quiet = group.members.filter((m) => !spoke.has(m.name) && PLATFORMS.some((p) => use.includes(p) && m[p])).map((m) => m.name);
  const unreached = group.members.filter((m) => !PLATFORMS.some((p) => use.includes(p) && m[p])).map((m) => m.name);
  const result = { group: group.slug, title: group.title, question, terms: want, hours, platforms: use, at: now,
    rows, quiet, unreached, problems, budget };

  if (save) {
    const cutoff = Date.now() - SEEN_DAYS * 86400_000;
    for (const [id, when] of Object.entries(state.seen)) if (new Date(when).getTime() < cutoff) delete state.seen[id];
    for (const row of rows) for (const id of row.ids) state.seen[id] ??= now;
    writeJson(statePath, { ...state, lastRun: now, lastQuestion: question || state.lastQuestion || null });
    writeJson(resolve(home, "runs", group.slug, `${now.replace(/[:.]/g, "")}.json`), result);
  }
  return result;
}

export function report(result) {
  const fresh = result.rows.filter((row) => row.fresh).length;
  const head = `${result.title} · last ${result.hours}h · ${result.platforms.join(" + ")} · ${result.rows.length} posts (${fresh} new since the last look)` +
    (result.terms.length ? ` · on: ${result.terms.join(", ")}` : " · every post");
  const notes = [
    result.quiet.length && `Quiet on this: ${result.quiet.join(", ")}`,
    result.unreached.length && `No handle on the platforms read: ${result.unreached.join(", ")}`,
    result.problems.length && `Could not read: ${result.problems.join("; ")}`,
    result.budget && `X budget: $${result.budget.reservedTodayUsd} reserved today, $${result.budget.remainingTodayUsd} left`,
  ].filter(Boolean);
  return [head, result.question && `Question: ${result.question}`, "", table(result.rows), "", ...notes].filter((line) => line !== false && line !== undefined).join("\n");
}

// ---- find: who belongs in a group ----

// Candidates come from three places: accounts whose profile matches the topic,
// authors of the best-liked recent posts on it, and Bluesky starter packs
// (other people's hand-made lists). `pack` expands one of those packs.
export async function find({ topic, platforms, limit = 20, pack, x = false } = {}) {
  if (pack) {
    const handles = await bskyPackMembers(pack);
    const profiles = await bskyProfiles(handles);
    return { topic: pack, packs: [], candidates: profiles.map((p) => ({ platform: "bluesky", handle: p.handle, name: p.displayName || p.handle,
      bio: oneLine(p.description).slice(0, 160), followers: p.followersCount || 0, why: ["pack member"] }))
      .sort((a, b) => b.followers - a.followers).slice(0, limit), problems: [] };
  }
  if (!topic) throw new Error("find needs a topic or a pack uri");
  const use = platforms?.length ? platforms : ["bluesky", "mastodon"];
  const found = new Map(), problems = [];
  const note = (candidate, why) => {
    const key = `${candidate.platform}:${candidate.handle.toLowerCase()}`;
    const prior = found.get(key) || { ...candidate, why: [] };
    prior.why.push(why);
    prior.likes = (prior.likes || 0) + (candidate.likes || 0);
    found.set(key, prior);
  };
  // Account search is fuzzy ("p5.js" finds anyone called JS), so a profile
  // only counts when every word of the topic is in its name, handle or bio.
  const words = topic.toLowerCase().split(/\s+/).filter(Boolean);
  const onTopic = (c) => { const hay = `${c.name} ${c.handle} ${c.bio || ""}`.toLowerCase(); return words.every((w) => hay.includes(w)); };
  const week = new Date(Date.now() - 7 * 86400_000).toISOString();
  let packs = [];

  if (use.includes("bluesky")) {
    try {
      const actors = await getJson(`${BSKY}/app.bsky.actor.searchActors`, { q: topic, limit: 25 });
      for (const a of actors.actors || []) {
        const c = { platform: "bluesky", handle: a.handle, name: a.displayName || a.handle, bio: oneLine(a.description).slice(0, 160) };
        if (onTopic(c)) note(c, "profile match");
      }
      for (const post of await bskySearch(topic, { since: week, limit: 50 })) note({ platform: "bluesky", handle: post.handle, name: post.name, likes: post.likes }, "posted on it this week");
      packs = await bskyPacks(topic);
      const profiles = await bskyProfiles([...found.values()].filter((c) => c.platform === "bluesky").map((c) => c.handle));
      for (const p of profiles) {
        const c = found.get(`bluesky:${p.handle.toLowerCase()}`);
        if (c) { c.followers = p.followersCount || 0; c.bio ||= oneLine(p.description).slice(0, 160); }
      }
    } catch (error) { problems.push(`bluesky: ${error.message}`); }
  }
  if (use.includes("mastodon")) {
    try {
      for (const a of await mastoActors(topic, { limit: 15 })) if (onTopic(a)) note({ ...a, bio: a.bio.slice(0, 160) }, "profile match");
      for (const post of await mastoTag(slugOf(topic).replace(/-/g, ""))) note({ platform: "mastodon", handle: post.handle, name: post.name, likes: post.likes }, "posted under the tag");
    } catch (error) { problems.push(`mastodon: ${error.message}`); }
  }
  if (x || use.includes("x")) {
    try {
      // Replies are left out: on X they are mostly promotion under bigger accounts.
      const { posts } = await xSearch(`${topic} -is:retweet -is:reply`, { max: 50 });
      for (const post of posts) note({ platform: "x", handle: post.handle, name: post.name, likes: post.likes }, "posted on it this week");
    } catch (error) { problems.push(`x: ${error.message}`); }
  }

  const candidates = [...found.values()].map((c) => ({ ...c, why: [...new Set(c.why)] }))
    .sort((a, b) => (b.why.length - a.why.length) || ((b.followers || 0) + (b.likes || 0) * 20) - ((a.followers || 0) + (a.likes || 0) * 20))
    .slice(0, limit);
  return { topic, packs, candidates, problems };
}

export function findReport({ topic, packs, candidates, problems }) {
  const lines = [`Candidates for "${topic}" (${candidates.length}):`, "",
    "| Who | Handle | Where | Followers | Why | Bio |", "|---|---|---|---|---|---|",
    ...candidates.map((c) => `| ${cell(c.name)} | ${c.handle} | ${c.platform} | ${c.followers ?? "?"} | ${c.why.join(", ")}${c.likes ? ` (${c.likes}♥)` : ""} | ${cell(c.bio || "")} |`)];
  if (packs.length) lines.push("", "Bluesky starter packs (pass a uri as `pack` to list its members):",
    ...packs.map((p) => `- ${p.name} — by ${p.by}${p.about ? ` — ${p.about.slice(0, 100)}` : ""}\n  ${p.uri}`));
  if (problems.length) lines.push("", `Could not read: ${problems.join("; ")}`);
  return lines.join("\n");
}
