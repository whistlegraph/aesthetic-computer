// Auth refusals, 2026.11.21
// When authorize() turns a signed-in account away (an AC account locked for
// deletion, a banned Sotce reader), keep a record, so "did they try again?"
// has an answer without Auth0's log retention or scopes.
//
// One document per account, tenant and UTC day in `auth-refusals`, counting
// attempts, so a tab left open retrying cannot grow the collection. Recording
// never blocks or fails a request.

import { connect } from "./database.mjs";
import { shell } from "./shell.mjs";

export function recordRefusal({ tenant, user, reason }) {
  const sub = user?.sub;
  if (!sub) return;
  shell.log(`🚫 Refused ${tenant} account (${reason}): ${sub}`);
  const now = new Date();
  const day = now.toISOString().slice(0, 10);
  (async () => {
    const { db } = await connect();
    await db.collection("auth-refusals").updateOne(
      { _id: `${tenant}:${sub}:${day}` },
      {
        $inc: { count: 1 },
        $set: { lastAt: now },
        $setOnInsert: {
          tenant, sub, day, reason, firstAt: now,
          ...(typeof user.email === "string" ? { email: user.email } : {}),
        },
      },
      { upsert: true },
    );
  })().catch(() => {});
}
