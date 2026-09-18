// mail-journey.test, 2026.09.17
// End-to-end proof of the post: a signed-in @handle opens `mail`, sees the
// box the API reports, sends a letter to itself whose `<3` survives the
// server's entity decoding, replies to it from the reply chip, and then
// `laklok` wears a red unread badge in its corner until the box is marked
// read — all read off the running pieces and real screenshots.
//
//   npm run test:mail:e2e           # headless, production
//   npm run test:mail:e2e:local     # against a local `npm run site`
//   AC_HEADED=1 npm run test:mail:e2e
//   AC_MAIL_READONLY=1 ...          # read paths only: no letter sent, nothing marked read
//
// Auth rides ~/.ac-token (`ac-login`); an expired token is refreshed through
// the standard refresh grant, and a token that cannot be refreshed fails the
// run up front rather than guessing. `mail?test=1` and `laklok?test=1` publish
// snapshots on the "ac-mail-test" BroadcastChannel; a target without the
// hook fails the first scenario, which is the point.
//
// Letters are sent to self with a unique marker subject `e2e <timestamp>`
// (printed in the summary) so they can be found afterwards.
//
// Puppeteer needs a Chrome; with none downloaded, point it at the installed
// one: PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome".

import { PNG } from "pngjs";
import { ACSession, CONFIG, scenario, report } from "./ac-harness.mjs";
import { loadTokens, UA } from "../../toolchain/mcp/ac-token.mjs";

const base = new URL(CONFIG.baseURL);
const isLocal = base.hostname === "localhost" || base.hostname === "127.0.0.1";
// A local `npm run site` is self-signed; Node's fetch must accept it too.
if (isLocal) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";

const READONLY = process.env.AC_MAIL_READONLY === "1";
const NUDGE_STATUSES = ["sent", "throttled", "pushed", "no_email", "unsubscribed", "failed", "skipped"];
// paintBadge in disks/common/laklok-tema.mjs: fill [220,30,40], hot [255,60,70].
const BADGE_REDS = ["220,30,40", "255,60,70"];

// ─── the signed-in user ───────────────────────────────────────────────────
let tokens;
try {
  tokens = await loadTokens(); // refreshes when stale, throws when it can't
} catch (e) {
  console.error(`✗ ${e.message}`);
  process.exit(1);
}
if (!tokens.expires_at || Date.now() > tokens.expires_at) {
  console.error(`✗ ~/.ac-token expired at ${new Date(tokens.expires_at || 0).toISOString()} — run ac-login`);
  process.exit(1);
}
const handle = tokens.user?.handle || "jeffrey";
const session = {
  accessToken: tokens.access_token,
  account: { id: tokens.user?.sub, label: tokens.user?.email },
};
const auth = { Authorization: `Bearer ${tokens.access_token}`, "User-Agent": UA };

async function api(method, body) {
  const url = `${CONFIG.baseURL}/api/mail${method === "GET" ? "?count=1" : ""}`;
  const res = await fetch(url, {
    method,
    headers: { ...auth, "Content-Type": "application/json" },
    body: body ? JSON.stringify(body) : undefined,
  });
  let json = null;
  try { json = await res.json(); } catch {}
  return { status: res.status, ...(json || {}) };
}

// ─── the browser ─────────────────────────────────────────────────────────
const ac = await ACSession.open();
await ac.signIn(session);
await ac.page.evaluateOnNewDocument(() => {
  window.acDEBUG = false; // debug dials a local chat-clock; laklok wants the real room
  const snapshots = {};
  let seq = 0;
  const channel = new BroadcastChannel("ac-mail-test");
  channel.onmessage = ({ data }) => {
    if (data?.ready && data.piece) snapshots[data.piece] = { ...data, seq: ++seq };
  };
  window.__acMailTest = (piece) => snapshots[piece] || null;
  window.__acMailSend = (msg) => channel.postMessage(msg);
});

const state = (piece) => ac.page.evaluate((p) => window.__acMailTest?.(p) || null, piece);
const send = (msg) => ac.page.evaluate((m) => window.__acMailSend(m), msg);

async function waitFor(piece, pred, { timeout = 30000, every = 250, label = "condition" } = {}) {
  const t0 = Date.now();
  let last = null;
  while (Date.now() - t0 < timeout) {
    last = await state(piece);
    if (last && pred(last)) return last;
    await ac.wait(every);
  }
  throw new Error(`timed out waiting for ${label}; last ${piece} state: ${JSON.stringify(last)?.slice(0, 600)}`);
}

// Fallback for a boot that came up anonymous: hand it the session the way a
// host page does, repeating until the piece reports it is no longer noauth.
async function postSessionUntilSignedIn(piece) {
  const t0 = Date.now();
  while (Date.now() - t0 < 10000) {
    const s = await state(piece);
    if (s && s.status !== "noauth") return s;
    await ac.page.evaluate((sess) => window.postMessage({ type: "setSession", session: sess }, "*"), session);
    await ac.wait(250);
  }
  return state(piece);
}

// Count badge-red pixels in the top-right corner of the piece canvas, read
// off a real screenshot. The corner cluster (QR, gear, envelope + badge) sits
// inside laklok's 34px top margin; 100 CSS px is generous at any density.
async function badgeReds(name) {
  const rect = await ac.page.evaluate(() => {
    const r = document.querySelector("#aesthetic-computer canvas").getBoundingClientRect();
    return { x: r.x, y: r.y, w: r.width, h: r.height };
  });
  const clip = { x: rect.x + rect.w * 0.6, y: rect.y, width: rect.w * 0.4, height: Math.min(100, rect.h) };
  const bytes = await ac.page.screenshot({ clip, encoding: "binary" });
  const png = PNG.sync.read(Buffer.from(bytes));
  let hits = 0;
  for (let i = 0; i < png.data.length; i += 4) {
    if (BADGE_REDS.includes(`${png.data[i]},${png.data[i + 1]},${png.data[i + 2]}`)) hits++;
  }
  await ac.shot(name);
  return { hits, clip };
}

const marker = Date.now();
const subject = `e2e ${marker}`;
const text = `thank you &lt;3 &amp; <3 e2e-${marker}`;
let letter = null; // the inbox entry for the marker letter
let unreadBefore = 0; // the box's unread count before the e2e letter, restored by cleanup

try {
  await scenario("signed-in inbox matches the API", async (expect) => {
    await ac.boot("mail?test=1");
    let st = await waitFor("mail", (s) => s.status && s.status !== "loading", {
      label: "mail snapshot (is the piece hooked?)",
    });
    if (st.status === "noauth") st = await postSessionUntilSignedIn("mail");
    st = await waitFor("mail", (s) => s.status === "loaded" || s.status === "error", {
      timeout: 20000, label: "mail to load the box",
    });
    await ac.shot("mail-journey/01-inbox");
    expect(st.status === "loaded", `status is loaded (${st.status})`);
    expect(
      (st.addresses || []).includes(`${handle}@aesthetic.computer`),
      `addresses include ${handle}@aesthetic.computer (${JSON.stringify(st.addresses)})`,
    );
    const count = await api("GET");
    expect(count.status === 200, `GET /api/mail?count=1 → ${count.status}`);
    expect(
      count.unread === st.unread && count.total === st.total,
      `piece counts match the API (piece ${st.unread}/${st.total}, api ${count.unread}/${count.total})`,
    );
    unreadBefore = count.unread ?? 0;
  });

  await scenario("a heart survives the send (piece path)", async (expect) => {
    if (READONLY) return expect(true, "skipped (AC_MAIL_READONLY)");
    let apiReply = null;
    const onResponse = async (res) => {
      const req = res.request();
      if (req.method() === "POST" && /\/api\/mail(?:[?#]|$)/.test(res.url())) {
        try { apiReply = await res.json(); } catch { apiReply = { status: `unparsable ${res.status()}` }; }
      }
    };
    ac.page.on("response", onResponse);
    await send({ type: "compose", to: `@${handle}`, subject, text });
    const sentSt = await waitFor("mail", (s) => (s.sent || []).some((l) => l.subject === subject), {
      timeout: 30000, label: `a sent entry titled "${subject}"`,
    });
    ac.page.off("response", onResponse);
    expect(true, `sent list carries "${subject}" (${sentSt.sent.length} sent)`);
    expect(
      apiReply && NUDGE_STATUSES.includes(apiReply.nudge?.status),
      `POST /api/mail replied with a known nudge.status (${JSON.stringify(apiReply?.nudge ?? apiReply)?.slice(0, 200)})`,
    );
    expect(apiReply?.status === "mailed", `API status is mailed (${apiReply?.status})`);

    await send({ type: "refresh" });
    const st = await waitFor("mail", (s) => (s.inbox || []).some((l) => l.subject === subject), {
      timeout: 30000, label: `the letter "${subject}" to land in the inbox`,
    });
    letter = st.inbox.find((l) => l.subject === subject);
    await ac.shot("mail-journey/02-heart");
    expect(letter.text.includes("<3"), `inbox text keeps a literal <3 (${JSON.stringify(letter.text)})`);
    expect(!letter.text.includes("&lt;") && !letter.text.includes("&amp;"), "no entity leaks into the shown text");
    expect(letter.read === false, `the fresh letter is unread (read=${letter.read})`);
  });

  await scenario("the reply chip opens a reply", async (expect) => {
    if (READONLY) return expect(true, "skipped (AC_MAIL_READONLY)");
    expect(!!letter?.id, `have a letter id to reply to (${letter?.id})`);
    if (!letter?.id) return;
    await send({ type: "reply", id: letter.id });
    const st = await waitFor("mail", (s) => s.view === "compose", { timeout: 10000, label: "compose view" });
    await ac.shot("mail-journey/03-reply");
    expect(st.composeTo === `@${handle}`, `composeTo is the sender (${st.composeTo})`);
    expect(
      typeof st.composeSubject === "string" && st.composeSubject.startsWith("Re: "),
      `composeSubject starts with "Re: " (${JSON.stringify(st.composeSubject)})`,
    );
    await send({ type: "view", value: "inbox" });
    const back = await waitFor("mail", (s) => s.view === "inbox", { timeout: 10000, label: "inbox view" });
    expect(back.view === "inbox", "back on the inbox");
  });

  await scenario("laklok wears the unread badge until the box is read", async (expect) => {
    await ac.boot("laklok?test=1");
    // The piece may not be listening yet; ask for a fast poll a few times.
    for (let i = 0; i < 12; i++) {
      await send({ type: "mail-poll", every: 2000 });
      await ac.wait(250);
    }
    let st = await waitFor("laklok", (s) => s.asks >= 1 && (READONLY ? s.mail : (s.mail?.unread ?? 0) >= 1), {
      timeout: 45000, label: "laklok to count unread mail (is the piece hooked and signed in?)",
    });
    await ac.wait(600); // one more paint so the corner holds the badge
    const before = await badgeReds("mail-journey/04-laklok-badge");
    const unread = st.mail.unread;
    expect(
      unread > 0 ? before.hits >= 10 : before.hits < 5,
      `red badge ${unread > 0 ? "painted" : "absent"} top-right for ${unread} unread (${before.hits} badge-red px at ${JSON.stringify(before.clip)})`,
    );
    if (READONLY) return expect(true, "mark-read half skipped (AC_MAIL_READONLY)");
    expect(!!letter?.id, `have the e2e letter's id (${letter?.id})`);
    if (!letter?.id) return;

    // Only the e2e letter is marked read — real letters in the box stay
    // unread, so the badge drops by exactly one and vanishes only when the
    // e2e letter was the last unread one.
    const asks0 = st.asks;
    const want = unread - 1;
    const read = await api("POST", { action: "read", id: letter.id });
    expect(read.status === 200 && read.read === 1, `POST /api/mail {action:"read", id} → ${read.status}, read ${read.read}`);
    st = await waitFor("laklok", (s) => s.asks >= asks0 + 2 && s.mail?.unread === want, {
      timeout: 45000, label: `laklok to re-poll twice (asks ${asks0}) and count ${want} unread`,
    });
    await ac.wait(600);
    const after = await badgeReds("mail-journey/05-laklok-clear");
    expect(
      want > 0 ? after.hits >= 10 : after.hits < 5,
      `badge ${want > 0 ? "still painted" : "gone"} for ${want} unread after reading the e2e letter (${after.hits} badge-red px; asks ${asks0} → ${st.asks})`,
    );
  });

  await scenario("cleanup", async (expect) => {
    if (READONLY) return expect(true, "skipped (AC_MAIL_READONLY)");
    if (letter?.id) {
      const read = await api("POST", { action: "read", id: letter.id });
      expect(read.status === 200, `e2e letter marked read again, idempotent (${read.status})`);
    }
    const count = await api("GET");
    expect(
      count.unread === unreadBefore,
      `the box's unread count is back where it started (${count.unread}, was ${unreadBefore}; ${count.total} total)`,
    );
    console.log(`  ℹ️  e2e letter left in @${handle}'s box, subject "${subject}"`);
  });
} finally {
  await ac.close();
}

console.log(`\nmarker: "${subject}"`);
process.exit(report());
