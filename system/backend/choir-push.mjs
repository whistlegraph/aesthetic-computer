// choir push, 26.10.12
// Notifications for choir members through the AC network device registry
// (shared/app-devices.mjs rows with app "choir"; shared/push.mjs delivers).
//
// A member can sign in through more than one Auth0 account (an email-code
// account beside the one holding their @handle), and devices are bound to the
// account that signed in. `choir-prefs` rows are keyed by the member (the
// handle's account) and list every sign-in account seen as `subs`, so a device
// maps back to its member: never notify the poster, never notify anyone who
// turned notifications off.
import { sendToTarget } from "../../shared/push.mjs";

export async function noteMember(db, member, signedInAs, handle) {
  await db.collection("choir-prefs").updateOne(
    { _id: member },
    { $addToSet: { subs: { $each: [member, signedInAs].filter(Boolean) } }, ...(handle ? { $set: { handle } } : {}) },
    { upsert: true },
  );
}

// Every choir device user to reach, excluding `except` (a member key).
// `channel`: "chat" or "discover"; a member can silence either.
export async function audience(db, { except, channel } = {}) {
  const devices = await db.collection("app-devices").find({ app: "choir", push: { $exists: true }, user: { $type: "string" } }, { projection: { user: 1 } }).toArray();
  const prefs = await db.collection("choir-prefs").find({}).toArray();
  const memberOf = new Map();
  for (const p of prefs) for (const s of [p._id, ...(p.subs || [])]) memberOf.set(s, p);
  const users = new Set();
  for (const d of devices) {
    const m = memberOf.get(d.user);
    if (m && m._id === except) continue;
    if (!m && d.user === except) continue;
    if (m?.notifications === false) continue;
    if (channel && m?.[channel] === false) continue;
    users.add(d.user);
  }
  return [...users];
}

export async function notifyChoir(db, note, { except, channel, log = () => {} } = {}) {
  const users = await audience(db, { except, channel });
  const totals = { people: users.length, attempted: 0, succeeded: 0, failed: 0, pruned: 0 };
  for (const user of users) {
    const s = await sendToTarget(db, { user, app: "choir" }, note, log);
    for (const k of ["attempted", "succeeded", "failed", "pruned"]) totals[k] += s[k] || 0;
  }
  return totals;
}
