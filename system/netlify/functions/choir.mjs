// choir, 26.10.12
// The room's HTTP door. Store logic lives in backend/choir.mjs.
//
//   GET    /api/choir                      main chat  ?tag=&before=<when>&limit=
//   GET    /api/choir?thread=<id>          one root with its replies
//   GET    /api/choir?tags=1               tags by activity, starters included
//   GET    /api/choir?me=1                 {handle, prefs}            (Bearer)
//   POST   /api/choir  {text,tags,replyTo} a post or a reply          (Bearer)
//   DELETE /api/choir?id=<id>              your own post              (Bearer)
//   PUT    /api/choir?prefs=1 {notifications}                         (Bearer)
//   POST   /api/choir?event=1 {name,props,session,surface}   a trail event
//   GET    /api/choir?events=1&since=<iso>                   the trail (admin)
//
// Sign-in is the aesthetic tenant's: the client uses lib/auth0-otp.mjs
// (email + six digits) and the token is the one `authorize()` already reads.
import { connect } from "../../backend/database.mjs";
import { authorize, handleBySharedEmail, hasAdmin } from "../../backend/authorization.mjs";
import { cleanEvent, record, recent } from "../../backend/choir-events.mjs";
import { respond } from "../../backend/http.mjs";
import { mongoChoirStore, publicPost } from "../../backend/choir.mjs";

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
        return ok({ handle: me.handle, prefs: await store.prefsOf(me.sub) });
      }
      if (q.events) {
        const me = await whoami(event.headers || {}, db);
        if (!me || !(await hasAdmin(me.user))) return no(401, "Admins only");
        return ok({ events: await recent(db, { since: q.since, limit: Number(q.limit) || 200 }) });
      }
      if (q.tags) return ok({ tags: await store.tagList() }, "public, max-age=30");
      if (q.thread) {
        const t = await store.thread(q.thread);
        return t ? ok({ root: publicPost(t.root), replies: t.replies.map(publicPost) }, "public, max-age=5") : no(404, "No such thread");
      }
      const rows = await store.feed({ tag: q.tag, before: q.before, limit: Number(q.limit) || 30 });
      return ok({ posts: rows.map(publicPost), next: rows.length ? rows.at(-1).when : null }, "public, max-age=5");
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

    if (event.httpMethod === "POST") {
      let body;
      try { body = JSON.parse(event.body || "{}"); } catch { return no(400, "Send JSON"); }
      if (!me.handle) { trail(db, "post:no-handle", {}, event.headers); return no(403, "Choose your @name first"); }
      try {
        const row = await store.post(me.sub, me.handle, body);
        trail(db, body.replyTo ? "post:reply" : "post:root", { handle: me.handle, tags: (row.tags || []).join(",") }, event.headers);
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
