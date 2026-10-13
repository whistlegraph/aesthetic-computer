// choir invites + presence, 26.10.12
// Every member can invite a few friends (Fía, @fifi, without limit); an invite
// is a short code with a choir.ac link. A newcomer who signs up with a code is
// recorded as invited by its sender. Presence is a heartbeat the app sends
// while open; "here" means seen in the last two minutes.

export const INVITES_EACH = 5;
export const UNLIMITED = new Set(["@fifi"]);
const ALPHABET = "ABCDEFGHJKMNPQRSTUVWXYZ23456789"; // no 0/O, 1/I/L

export const inviteUrl = (code) => `https://choir.ac/?invite=${code}`;

export function makeCode(rand = Math.random) {
  let c = ""; for (let i = 0; i < 6; i++) c += ALPHABET[Math.floor(rand() * ALPHABET.length)];
  return c;
}

export function normalizeCode(raw) {
  const c = String(raw || "").toUpperCase().replace(/[^A-Z0-9]/g, "");
  return /^[A-Z0-9]{6}$/.test(c) ? c : null;
}

export async function createInvite(invites, { member, handle, name, now = new Date(), code = makeCode }) {
  if (!handle) throw Error("Choose your @name first");
  const used = await invites.countDocuments({ from: member });
  if (!UNLIMITED.has(handle) && used >= INVITES_EACH) throw Error(`You've used all ${INVITES_EACH} invites`);
  for (let i = 0; i < 20; i++) {
    const row = { _id: code(), from: member, fromHandle: handle, name: String(name || "").trim().slice(0, 60) || null, at: now.toISOString() };
    try { await invites.insertOne(row); return { ...row, url: inviteUrl(row._id), left: UNLIMITED.has(handle) ? null : INVITES_EACH - used - 1 }; }
    catch (e) { if (e.code !== 11000) throw e; }
  }
  throw Error("Couldn't make an invite");
}

export async function myInvites(invites, member, handle) {
  const rows = await invites.find({ from: member }).sort({ at: -1 }).limit(200).toArray();
  return { invites: rows.map((r) => ({ code: r._id, url: inviteUrl(r._id), name: r.name, at: r.at, joined: r.claimedHandle || null })),
    left: UNLIMITED.has(handle) ? null : Math.max(0, INVITES_EACH - rows.length) };
}

export async function peekInvite(invites, raw) {
  const code = normalizeCode(raw); if (!code) return null;
  const r = await invites.findOne({ _id: code });
  return r ? { code, from: r.fromHandle, name: r.name, claimed: !!r.claimedBy } : null;
}

export async function claimInvite(invites, prefs, { code: raw, member, handle, now = new Date() }) {
  const code = normalizeCode(raw); if (!code) throw Error("That code doesn't look right");
  const r = await invites.findOne({ _id: code });
  if (!r) throw Error("No such invite");
  if (r.from === member) throw Error("That's your own invite");
  if (r.claimedBy && r.claimedBy !== member) throw Error("That invite has been used");
  await invites.updateOne({ _id: code, claimedBy: { $in: [null, member] } }, { $set: { claimedBy: member, claimedHandle: handle || null, claimedAt: now.toISOString() } });
  await prefs.updateOne({ _id: member }, { $set: { invitedBy: r.fromHandle, invite: code } }, { upsert: true });
  return { from: r.fromHandle };
}

export async function stats(prefs, invites, { now = Date.now() } = {}) {
  const since = new Date(now - 2 * 60_000).toISOString();
  const [members, here, invited, joined] = await Promise.all([
    prefs.countDocuments({ handle: { $type: "string" } }),
    prefs.countDocuments({ lastSeenAt: { $gt: since } }),
    invites.countDocuments({}),
    invites.countDocuments({ claimedBy: { $type: "string" } }),
  ]);
  return { members, here, invited, joined };
}
