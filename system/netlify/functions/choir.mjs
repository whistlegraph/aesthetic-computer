// choir, 26.10.12
// The room's HTTP door. Store logic lives in backend/choir.mjs.
//
//   GET    /api/choir                      main chat  ?tag=&before=<when>&limit=
//   GET    /api/choir?news=1               Discover: news items, newest first
//   GET    /api/choir?thread=<id>          one root with its replies
//   GET    /api/choir?tags=1               tags by activity, starters included
//   GET    /api/choir?me=1                 {handle, prefs}            (Bearer)
//   POST   /api/choir  {text,tags,replyTo} a post or a reply          (Bearer)
//   DELETE /api/choir?id=<id>              your own post              (Bearer)
//   PUT    /api/choir?prefs=1 {notifications}                         (Bearer)
//   POST   /api/choir?event=1 {name,props,session,surface}   a trail event
//   GET    /api/choir?events=1&since=<iso>                   the trail (admin)
//   GET    /api/choir?here=1                 {members, here, invited, joined}; with Bearer, counts you as here
//   GET    /api/choir?invites=1 (Bearer)     your invites + how many are left
//   POST   /api/choir?invite=1 {name?}       make an invite code (Bearer, has @name)
//   GET    /api/choir?invite=<code>          who sent it (public, for the choir.ac page)
//   POST   /api/choir?claim=1 {code}         a newcomer redeems an invite (Bearer)
//   POST   /api/choir?upload=1 {name, type, size} → {uploadUrl, url, headers}  (Bearer, has @name)
//          then PUT the file to uploadUrl and post with attachments:[{url,name,type,size,…}]
//   POST   /api/choir?bug=1 {text, image (base64 jpeg), context}  a bug report (Bearer)
//   GET    /api/choir?bugs=1  ·  ?bug=<id>&image=1   the bug list / one screenshot (admin)
//   PUT    /api/choir?bug=<id> {status, note}        open | fixed | wontfix (admin)
//   (filing is for @fifi and admins only — Fía's tester button)
//   POST   /api/choir?report=1 {id, reason}   report a post (Bearer); the moderator is pushed
//   GET    /api/choir?reports=1[&status=open]  the report queue (admin)
//   PUT    /api/choir?report=<id> {status, remove, ban, note}  act on a report (admin)
//          status: open | actioned | dismissed; remove takes the post down, ban stops its author posting
//   POST   /api/choir?terms=1 {version}        agree to choir.ac/terms (Bearer); ?me=1 returns `terms`
//   POST   /api/choir?curate=1 {items:[…]}   x-choir-editor: <CHOIR_EDITOR_SECRET>
//          the jasellite curator adds to Discover and retires closed calls
//
// Sign-in is the aesthetic tenant's: the client uses lib/auth0-otp.mjs
// (email + six digits) and the token is the one `authorize()` already reads.
import { connect } from "../../backend/database.mjs";
import { authorize, handleBySharedEmail, hasAdmin } from "../../backend/authorization.mjs";
import { cleanEvent, record, recent } from "../../backend/choir-events.mjs";
import { noteMember, notifyChoir } from "../../backend/choir-push.mjs";
import { createInvite, myInvites, peekInvite, claimInvite, stats } from "../../backend/choir-invites.mjs";
import { fileReport, resolveReport, acceptTerms, termsOf, REPORT_COLLECTION } from "../../backend/choir-moderation.mjs";
import { sendToTarget } from "../../../shared/push.mjs";
import { respond } from "../../backend/http.mjs";
import { mongoChoirStore, publicPost, curate, greet, UPLOAD_MAX, UPLOAD_TYPES, UPLOAD_BASE } from "../../backend/choir.mjs";
import { S3Client, PutObjectCommand } from "@aws-sdk/client-s3";
import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
import { timingSafeEqual, createHash } from "node:crypto";

let pending;
export const choirStore = () => pending ??= (async () => {
  const { db } = await connect();
  return mongoChoirStore(db.collection("choir-posts"), { tags: db.collection("choir-tags"), prefs: db.collection("choir-prefs") });
})().catch((e) => { pending = null; throw e; });

// The owner is whichever account holds the @handle. An email-code sign-in is
// often a second account beside the one that has it (see handleBySharedEmail).
const linked = new Map(); // sub → { sub, handle } for this process.
async function whoami(headers, db) {
  const user = await authorize(headers);
  if (!user?.sub) return null;
  const at = (h) => (h ? "@" + h.replace(/^@/, "") : null);
  const row = await db.collection("@handles").findOne({ _id: user.sub });
  if (row?.handle) return { sub: user.sub, handle: at(row.handle), user };
  let sibling = linked.get(user.sub);
  if (!sibling) {
    try { sibling = await handleBySharedEmail(user, { db }); } catch { sibling = null; }
    if (sibling) linked.set(user.sub, sibling);
  }
  return sibling ? { sub: sibling.sub, handle: at(sibling.handle), user, via: user.sub } : { sub: user.sub, handle: null, user };
}
const trail = (db, name, props, headers) => record(db, cleanEvent(
  { name, props, surface: "server", session: headers?.["x-choir-session"] }, {})).catch(() => {});

// Constant-time check of the curator's shared secret (vault lith/.env).
function editor(headers) {
  const want = process.env.CHOIR_EDITOR_SECRET || "";
  const got = headers["x-choir-editor"] || headers["X-Choir-Editor"] || "";
  if (want.length < 32 || !got) return false;
  const h = (v) => createHash("sha256").update(v).digest();
  return timingSafeEqual(h(want), h(got));
}

const ok = (v, cache = "no-store") => respond(200, v, { "Cache-Control": cache, "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS" });
const no = (code, error) => respond(code, { error }, { "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS" });

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null, { "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS" });
  const q = event.queryStringParameters || {};
  let store, db;
  try { store = await choirStore(); db = (await connect()).db; }
  catch { return no(503, "The room is unavailable"); }

  try {
    if (event.httpMethod === "GET") {
      if (q.me) {
        const me = await whoami(event.headers || {}, db);
        if (!me) { trail(db, "me:anonymous", {}, event.headers); return no(401, "Sign in"); }
        trail(db, me.handle ? (me.via ? "me:linked" : "me:handle") : "me:no-handle",
          { handle: me.handle || "", conn: me.user.sub.split("|")[0] }, event.headers);
        // Remember the member, then welcome them in the chat the first time they arrive with an @name.
        noteMember(db, me.sub, me.user.sub, me.handle).then(() => me.handle && greet(db.collection("choir-posts"), db.collection("choir-prefs"), me.sub, me.handle))
          .then((row) => {
            if (!row) return;
            trail(db, "member:welcomed", { handle: me.handle }, event.headers);
            notifyChoir(db, { title: "choir 🌼", body: `${me.handle} just joined — say hi!`, thread: "choir", data: { url: "/" } },
              { except: me.sub, channel: "chat" }).catch(() => {});
          }).catch(() => {});
        return ok({ handle: me.handle, prefs: await store.prefsOf(me.sub), terms: await termsOf(db.collection("choir-prefs"), me.sub) });
      }
      if (q.bugs || q.bug) {
        const me = await whoami(event.headers || {}, db);
        if (!me || !(await hasAdmin(me.user))) return no(401, "Admins only");
        const bugs = db.collection("choir-bugs");
        if (q.bug && q.image) {
          const row = await bugs.findOne({ _id: q.bug }, { projection: { image: 1 } });
          if (!row?.image) return no(404, "No screenshot");
          return { statusCode: 200, headers: { "Content-Type": "image/jpeg", "Cache-Control": "private, max-age=3600" }, body: row.image, isBase64Encoded: true };
        }
        const rows = await bugs.find(q.bug ? { _id: q.bug } : {}, { projection: { image: 0 } }).sort({ at: -1 }).limit(100).toArray();
        return ok({ bugs: rows.map((r) => ({ ...r, screenshot: r.hasImage ? `/api/choir?bug=${r._id}&image=1` : null })) });
      }
      if (q.reports) {
        const me = await whoami(event.headers || {}, db);
        if (!me || !(await hasAdmin(me.user))) return no(401, "Admins only");
        const filter = ["open", "actioned", "dismissed"].includes(q.status) ? { status: q.status } : {};
        return ok({ reports: await db.collection(REPORT_COLLECTION).find(filter).sort({ at: -1 }).limit(200).toArray() });
      }
      if (q.events) {
        const me = await whoami(event.headers || {}, db);
        if (!me || !(await hasAdmin(me.user))) return no(401, "Admins only");
        return ok({ events: await recent(db, { since: q.since, limit: Number(q.limit) || 200 }) });
      }
      if (q.tags) return ok({ tags: await store.tagList() }, "public, max-age=30");
      if (q.here) {
        const prefs = db.collection("choir-prefs"), invites = db.collection("choir-invites");
        if (event.headers?.authorization) {
          const me = await whoami(event.headers, db).catch(() => null);
          if (me) await prefs.updateOne({ _id: me.sub }, { $set: { lastSeenAt: new Date().toISOString(), ...(me.handle ? { handle: me.handle } : {}) } }, { upsert: true });
        }
        return ok(await stats(prefs, invites));
      }
      if (q.invite && q.invite !== "1") {
        const p = await peekInvite(db.collection("choir-invites"), q.invite);
        return p ? ok(p, "public, max-age=60") : no(404, "No such invite");
      }
      if (q.invites) {
        const me = await whoami(event.headers || {}, db);
        if (!me) return no(401, "Sign in");
        return ok(await myInvites(db.collection("choir-invites"), me.sub, me.handle));
      }
      if (q.thread) {
        const t = await store.thread(q.thread);
        return t ? ok({ root: publicPost(t.root), replies: t.replies.map(publicPost) }, "public, max-age=5") : no(404, "No such thread");
      }
      const rows = await store.feed({ tag: q.tag, before: q.before, limit: Number(q.limit) || 30, news: !!q.news });
      return ok({ posts: rows.map(publicPost), next: rows.length ? rows.at(-1).when : null }, "public, max-age=5");
    }

    if (event.httpMethod === "POST" && q.curate) {
      if (!editor(event.headers || {})) return no(401, "Editors only");
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      const result = await curate(db.collection("choir-posts"), db.collection("choir-posts-archive"), body.items);
      if (result.added.length) {
        const first = (Array.isArray(body.items) ? body.items : []).find((it) => it?.url === result.added[0]);
        const more = result.added.length - 1;
        notifyChoir(db, {
          title: "New in Discover",
          body: `${first?.title || "Fresh news and open calls"}${more ? ` and ${more} more` : ""}`.slice(0, 180),
          thread: "choir-discover", collapse: "choir-discover", data: { url: "/discover" },
        }, { channel: "discover" }).then((t) => trail(db, "push:discover", t, {})).catch(() => {});
      }
      await record(db, { name: "curate", props: { added: result.added.length, skipped: result.skipped.length, expired: result.expired.length }, surface: "server", session: null, at: new Date() }).catch(() => {});
      return ok(result);
    }

    if (event.httpMethod === "POST" && q.event) {
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      const e = cleanEvent(body);
      if (!e) return no(400, "Bad event");
      await record(db, e);
      return ok({ ok: true });
    }

    const me = await whoami(event.headers || {}, db);
    if (!me) return no(401, "Sign in");

    if (event.httpMethod === "POST" && q.invite) {
      let body = {}; try { body = JSON.parse(event.body || "{}"); } catch {}
      const inv = await createInvite(db.collection("choir-invites"), { member: me.sub, handle: me.handle, name: body.name });
      trail(db, "invite:made", { handle: me.handle || "" }, event.headers);
      return ok(inv);
    }
    if (event.httpMethod === "POST" && q.claim) {
      let body = {}; try { body = JSON.parse(event.body || "{}"); } catch {}
      const r = await claimInvite(db.collection("choir-invites"), db.collection("choir-prefs"), { code: body.code, member: me.sub, handle: me.handle });
      trail(db, "invite:claimed", { handle: me.handle || "", from: r.from }, event.headers);
      return ok(r);
    }

    if (event.httpMethod === "POST" && q.upload) {
      if (!me.handle) return no(403, "Choose your @name first");
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      const type = String(body.type || ""), size = Number(body.size) || 0;
      if (!UPLOAD_TYPES.test(type)) return no(400, "That file type isn't supported");
      if (size <= 0 || size > UPLOAD_MAX) return no(400, "Files can be up to 25 MB");
      if (!process.env.CHOIR_SPACES_KEY) return no(503, "Uploads aren't set up");
      const safe = String(body.name || "file").normalize("NFKD").replace(/[^\w.-]+/g, "-").replace(/^-+|-+$/g, "").slice(-80) || "file";
      const who = me.handle.replace(/^@/, "").replace(/[^\w-]/g, "");
      const key = `u/${who}/${new Date().toISOString().slice(0, 7)}/${[...crypto.getRandomValues(new Uint8Array(6))].map((b) => b.toString(16).padStart(2, "0")).join("")}/${safe}`;
      const s3 = new S3Client({ region: "us-east-1", endpoint: `https://${process.env.CHOIR_SPACES_ENDPOINT || "sfo3.digitaloceanspaces.com"}`,
        credentials: { accessKeyId: process.env.CHOIR_SPACES_KEY, secretAccessKey: process.env.CHOIR_SPACES_SECRET } });
      const headers = { "Content-Type": type, "x-amz-acl": "public-read" };
      const uploadUrl = await getSignedUrl(s3, new PutObjectCommand({ Bucket: process.env.CHOIR_SPACES_BUCKET || "choir", Key: key,
        ContentType: type, ACL: "public-read", ContentLength: size }), { expiresIn: 900 });
      trail(db, "upload:signed", { handle: me.handle, type, size }, event.headers);
      return ok({ uploadUrl, url: UPLOAD_BASE + key, headers, key });
    }

    if (event.httpMethod === "POST" && q.report) {
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      const { report, fresh } = await fileReport(db.collection(REPORT_COLLECTION), db.collection("choir-posts"),
        { postID: String(body.id || ""), reason: body.reason, member: me.sub, handle: me.handle });
      trail(db, "report:filed", { id: report._id, post: report.postID, by: me.handle || "", fresh }, event.headers);
      // The 24-hour promise in choir.ac/terms starts here: tell the moderator's devices.
      if (fresh && process.env.ADMIN_SUB) {
        sendToTarget(db, { user: process.env.ADMIN_SUB, app: "choir" }, {
          title: `🚩 ${me.handle || "someone"} reported ${report.post.handle}`, body: (report.reason || report.post.text || "a post").slice(0, 160),
          thread: "choir-reports", data: { url: `/thread/${report.post.replyTo || report.postID}` },
        }).catch(() => {});
      }
      return ok({ id: report._id, received: true });
    }

    if (event.httpMethod === "PUT" && q.report) {
      if (!(await hasAdmin(me.user))) return no(401, "Admins only");
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      const r = await resolveReport(db.collection(REPORT_COLLECTION), db.collection("choir-posts"), db.collection("choir-prefs"),
        { reportID: q.report, status: body.status, remove: body.remove === true, ban: body.ban === true, note: body.note, by: me.handle || me.sub });
      if (!r) return no(404, "No such report");
      trail(db, "report:resolved", { id: r._id, status: r.status, removed: r.removed, banned: r.banned }, event.headers);
      return ok({ report: r });
    }

    if (event.httpMethod === "POST" && q.terms) {
      let body = {}; try { body = JSON.parse(event.body || "{}"); } catch {}
      const t = await acceptTerms(db.collection("choir-prefs"), me.sub, { version: body.version });
      trail(db, "terms:accepted", { handle: me.handle || "", version: t.termsVersion }, event.headers);
      return ok(t);
    }

    if (event.httpMethod === "PUT" && q.bug) {
      if (!(await hasAdmin(me.user))) return no(401, "Admins only");
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      if (!["open", "fixed", "wontfix"].includes(body.status)) return no(400, "status must be open, fixed or wontfix");
      const r = await db.collection("choir-bugs").updateOne({ _id: String(q.bug) },
        { $set: { status: body.status, updatedAt: new Date(), ...(body.note ? { note: String(body.note).slice(0, 2000) } : {}) } });
      return r.matchedCount ? ok({ id: q.bug, status: body.status }) : no(404, "No such bug");
    }

    if (event.httpMethod === "POST" && q.bug) {
      // Fía's tester button: only her account (and admins, for testing) can file.
      if (me.handle !== "@fifi" && !(await hasAdmin(me.user))) return no(403, "Bug reports are for testers");
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      const text = String(body.text || "").trim().slice(0, 4000);
      const image = typeof body.image === "string" && /^[A-Za-z0-9+/=]+$/.test(body.image) && body.image.length < 4_000_000 ? body.image : null;
      if (!text && !image) return no(400, "Say what happened or include a screenshot");
      const ctx = body.context && typeof body.context === "object" ? body.context : {};
      const clean = (v) => (typeof v === "string" ? v.slice(0, 200) : typeof v === "number" || typeof v === "boolean" ? v : undefined);
      const context = Object.fromEntries(Object.entries(ctx).slice(0, 20).map(([k, v]) => [k.slice(0, 40), clean(v)]).filter(([, v]) => v !== undefined));
      const id = [...crypto.getRandomValues(new Uint8Array(6))].map((b) => b.toString(16).padStart(2, "0")).join("");
      await db.collection("choir-bugs").insertOne({ _id: id, at: new Date(), handle: me.handle, member: me.sub, signedInAs: me.user.sub,
        text, hasImage: !!image, ...(image ? { image } : {}), context, status: "open" });
      trail(db, "bug:filed", { id, handle: me.handle || "", hasImage: !!image }, event.headers);
      // Tell the admins' own choir devices.
      if (process.env.ADMIN_SUB) {
        sendToTarget(db, { user: process.env.ADMIN_SUB, app: "choir" }, {
          title: `🐞 ${me.handle || "someone"} filed a bug`, body: (text || "Screenshot only").slice(0, 160),
          thread: "choir-bugs", data: { url: `/bug/${id}` },
        }).catch(() => {});
      }
      return ok({ id });
    }

    if (event.httpMethod === "POST") {
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      if (!me.handle) { trail(db, "post:no-handle", {}, event.headers); return no(403, "Choose your @name first"); }
      try {
        const row = await store.post(me.sub, me.handle, body);
        trail(db, body.replyTo ? "post:reply" : "post:root", { handle: me.handle, tags: (row.tags || []).join(",") }, event.headers);
        // Everyone else in the room hears about it; replies say where.
        notifyChoir(db, {
          title: body.replyTo ? `${me.handle} replied` : me.handle,
          body: (row.text.replace(/https?:\/\/\S+/g, "🔗") ||
            (row.attachments?.some((a) => a.type.startsWith("audio/")) ? "🎙 voice note" : row.attachments?.length ? "📷 photo" : "")).slice(0, 180),
          thread: "choir", collapse: body.replyTo ? `choir-${row.replyTo}` : undefined,
          data: { url: `/thread/${row.replyTo || row._id}` },
        }, { except: me.sub, channel: "chat" }).then((t) => trail(db, "push:post", t, {})).catch(() => {});
        return ok({ post: publicPost(row) });
      } catch (error) {
        trail(db, "post:refused", { handle: me.handle, why: error.message }, event.headers);
        throw error;
      }
    }
    if (event.httpMethod === "DELETE") {
      return (await store.remove(me.sub, q.id || "")) ? ok({ deleted: true }) : no(404, "Not yours, or not there");
    }
    if (event.httpMethod === "PUT" && q.prefs) {
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      return ok({ prefs: await store.setPrefs(me.sub, body) });
    }
    return no(405, "Not a door");
  } catch (error) {
    // Store errors are written for the person ("Slow down a little"); pass them on.
    return no(error?.message?.match(/Slow down|this hour/) ? 429 : 400, error?.message || "Something broke");
  }
}
