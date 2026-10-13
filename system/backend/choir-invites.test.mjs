import test from "node:test";
import assert from "node:assert/strict";
import { createInvite, myInvites, peekInvite, claimInvite, normalizeCode, INVITES_EACH } from "./choir-invites.mjs";

function collection() {
  const rows = [];
  const matches = (r, q) => Object.entries(q).every(([k, v]) => v && typeof v === "object" && "$in" in v ? v.$in.includes(r[k] ?? null) : r[k] === v);
  return { rows,
    insertOne: async (row) => { if (rows.some((r) => r._id === row._id)) throw Object.assign(Error("dup"), { code: 11000 }); rows.push({ ...row }); },
    countDocuments: async (q) => rows.filter((r) => matches(r, q)).length,
    findOne: async (q) => rows.find((r) => matches(r, q)) || null,
    updateOne: async (q, u, o = {}) => { let r = rows.find((x) => matches(x, q)); if (!r && o.upsert) { r = { ...q }; rows.push(r); } if (r) Object.assign(r, u.$set); return {}; },
    find: (q) => ({ sort: () => ({ limit: () => ({ toArray: async () => rows.filter((r) => matches(r, q)) }) }) }) };
}

test("members get a few invites, Fía is unlimited, codes are claimed once", async () => {
  const inv = collection(), prefs = collection();
  let n = 0; const code = () => ["AAAAAA", "AAAAAA", "BBBBBB", "CCCCCC", "DDDDDD", "EEEEEE", "FFFFFF", "GGGGGG"][n++];
  const a = await createInvite(inv, { member: "m1", handle: "@harry", name: "Lou", code });
  assert.equal(a._id, "AAAAAA"); assert.equal(a.url, "https://choir.ac/?invite=AAAAAA"); assert.equal(a.left, INVITES_EACH - 1);
  const b = await createInvite(inv, { member: "m1", handle: "@harry", code }); assert.equal(b._id, "BBBBBB", "a taken code is retried");
  for (let i = 2; i < INVITES_EACH; i++) await createInvite(inv, { member: "m1", handle: "@harry", code });
  await assert.rejects(createInvite(inv, { member: "m1", handle: "@harry", code }), /used all/);
  for (let i = 0; i < 8; i++) await createInvite(inv, { member: "fia", handle: "@fifi", code: () => "Z" + String(i).padStart(5, "X") });
  assert.equal((await myInvites(inv, "fia", "@fifi")).left, null, "Fía is unlimited");
  assert.deepEqual(await peekInvite(inv, "aaa-aaa"), { code: "AAAAAA", from: "@harry", name: "Lou", claimed: false });
  assert.deepEqual(await claimInvite(inv, prefs, { code: "aaaaaa", member: "m9", handle: "@lou" }), { from: "@harry" });
  await assert.rejects(claimInvite(inv, prefs, { code: "AAAAAA", member: "m8", handle: "@x" }), /been used/);
  await assert.rejects(claimInvite(inv, prefs, { code: "BBBBBB", member: "m1", handle: "@harry" }), /your own/);
  assert.equal(prefs.rows[0].invitedBy, "@harry");
  assert.equal((await myInvites(inv, "m1", "@harry")).invites.find((i) => i.code === "AAAAAA").joined, "@lou");
  assert.equal(normalizeCode("ab1"), null);
});
