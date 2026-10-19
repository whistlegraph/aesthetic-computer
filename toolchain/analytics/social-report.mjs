// social-report — AC's social accounts for the network report: Instagram
// posts in the window with their insights (official Graph API through
// toolchain/instagram/ig.mjs, vault tokens), Bluesky's public profile and
// posts, and X follower counts (toolchain/x/x.mjs). Read-only. Each account
// fails on its own.
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { join } from "node:path";
import { INSTAGRAM_ACCOUNTS, X_ACCOUNTS, credentialStatus } from "../vault/credentials.mjs";

const pexec = promisify(execFile);
const BSKY = "https://public.api.bsky.app/xrpc";

async function instagram(root, account, hours) {
  if (credentialStatus(`instagram:${account}`).state !== "ready") return { account, unavailable: "not provisioned on this host" };
  try {
    const { stdout } = await pexec(process.execPath, [join(root, "toolchain/instagram/ig.mjs"), "--as", account, "recent", "--hours", String(hours), "--json"],
      { cwd: root, timeout: 120_000, maxBuffer: 4 * 1024 * 1024 });
    return JSON.parse(stdout.slice(stdout.indexOf("{")));
  } catch (error) { return { account, unavailable: String(error.stderr || error.message).trim().split("\n").at(-1).slice(0, 200) }; }
}

async function bluesky(actor, hours) {
  try {
    const get = async path => (await fetch(`${BSKY}/${path}`, { signal: AbortSignal.timeout(15_000) })).json();
    const profile = await get(`app.bsky.actor.getProfile?actor=${actor}`);
    const feed = await get(`app.bsky.feed.getAuthorFeed?actor=${actor}&limit=50&filter=posts_no_replies`);
    const since = Date.now() - hours * 3600_000;
    const posts = (feed.feed || []).map(item => item.post).filter(post => post && Date.parse(post.record?.createdAt) >= since)
      .map(post => ({ at: post.record.createdAt, text: String(post.record.text || "").split("\n")[0].slice(0, 100),
        likes: post.likeCount, reposts: post.repostCount, replies: post.replyCount, quotes: post.quoteCount,
        url: `https://bsky.app/profile/${actor}/post/${post.uri.split("/").at(-1)}` }));
    return { account: `@${profile.handle}`, followers: profile.followersCount, posts: profile.postsCount, recent: posts };
  } catch (error) { return { account: `@${actor}`, unavailable: String(error.message).slice(0, 200) }; }
}

async function x(root, account) {
  if (credentialStatus(`x:${account}`).state !== "ready") return { account, unavailable: "not provisioned on this host" };
  try {
    const { stdout } = await pexec(process.execPath, [join(root, "toolchain/x/x.mjs"), "--as", account, "me"], { cwd: root, timeout: 40_000 });
    const field = name => Number((stdout.match(new RegExp(`${name}:\\s*(\\d+)`)) || [])[1] ?? NaN);
    return { account: stdout.match(/@\w+/)?.[0] || account, followers: field("followers"), following: field("following"), posts: field("posts") };
  } catch (error) { return { account, unavailable: String(error.stderr || error.message).trim().split("\n").at(-1).slice(0, 200) }; }
}

export async function socialReport({ root, hours = 24 }) {
  const [ig, bsky, xs] = await Promise.all([
    Promise.all(Object.keys(INSTAGRAM_ACCOUNTS).map(account => instagram(root, account, hours))),
    bluesky("aesthetic.computer", hours),
    Promise.all(Object.keys(X_ACCOUNTS).map(account => x(root, account))),
  ]);
  return { hours, instagram: ig, bluesky: bsky, x: xs,
    note: "Instagram views/reach come from Meta's insights and lag by minutes to hours. X search is not used here (pay-per-use)." };
}
