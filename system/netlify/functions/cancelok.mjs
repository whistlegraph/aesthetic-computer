// cancelok, 26.07.14
// Verdicts, attributed.
//
// A verdict is worthless without a name on it. One person's cancel is taste;
// forty people's cancels are a signal — but only if you can tell them apart,
// because otherwise a single stubborn visitor with a fast thumb IS the crowd.
//
// 1. POST `api/cancelok`   { code, verdict: ok|cancel, dwellMs, taps, failed }
//    Logged in and handled → stored against your handle, one verdict per pad
//    (judging a pad twice overwrites — you're allowed to change your mind).
//    Not handled → we take it as an anonymous usage signal and say so.
//
// 2. GET `api/cancelok`            → the tallies, per pad
// 3. GET `api/cancelok/@handle`    → what one person kept

import { authorize, getHandleOrEmail } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond, pathParams } from "../../backend/http.mjs";

const VERDICTS = ["ok", "cancel"];

export async function handler(event) {
  if (event.httpMethod === "GET") {
    const database = await connect();
    const verdicts = database.db.collection("cancelok");
    const who = pathParams(event.path)[2]; // /api/cancelok/@handle

    if (who) {
      const rows = await verdicts
        .find({ handle: decodeURIComponent(who) })
        .sort({ when: -1 })
        .limit(500)
        .toArray();
      await database.disconnect();
      return respond(200, {
        handle: decodeURIComponent(who),
        kept: rows.filter((r) => r.verdict === "ok").map((r) => r.code),
        cancelled: rows.filter((r) => r.verdict === "cancel").map((r) => r.code),
      });
    }

    // The tallies. `taps` sums because being played is the signal nobody was
    // asked for — a pad with 3 oks and 40 taps beats one with 5 oks and none.
    const tally = await verdicts
      .aggregate([
        {
          $group: {
            _id: "$code",
            ok: { $sum: { $cond: [{ $eq: ["$verdict", "ok"] }, 1, 0] } },
            cancel: { $sum: { $cond: [{ $eq: ["$verdict", "cancel"] }, 1, 0] } },
            taps: { $sum: "$taps" },
            judges: { $addToSet: "$handle" },
          },
        },
        { $sort: { ok: -1 } },
      ])
      .toArray();
    await database.disconnect();

    return respond(200, {
      pads: tally.map((t) => ({
        code: t._id,
        ok: t.ok,
        cancel: t.cancel,
        taps: t.taps,
        judges: t.judges.filter(Boolean).length,
      })),
    });
  }

  if (event.httpMethod !== "POST")
    return respond(405, { message: "Method Not Allowed" });

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return respond(400, { message: "Bad JSON." });
  }

  const { code, verdict } = body;
  if (!code || !VERDICTS.includes(verdict))
    return respond(400, { message: "Need { code, verdict: ok | cancel }." });

  // Auth is a boundary, and an unauthenticated visitor is a normal state here,
  // not an error — they just don't get their name on it.
  let user = null;
  let handle = null;
  try {
    user = await authorize(event.headers);
    if (user?.sub) handle = await getHandleOrEmail(user.sub);
  } catch {
    // no token / bad token → anonymous. Fall through.
  }

  const attributed = Boolean(user?.sub && handle?.startsWith("@"));

  const database = await connect();
  const verdicts = database.db.collection("cancelok");
  await verdicts.createIndex({ code: 1 });
  await verdicts.createIndex({ user: 1, code: 1 });

  const row = {
    code,
    verdict,
    dwellMs: Number(body.dwellMs) || 0,
    taps: Number(body.taps) || 0,
    failed: Boolean(body.failed),
    when: new Date(),
    user: attributed ? user.sub : null,
    handle: attributed ? handle : null,
  };

  if (attributed) {
    // One verdict per person per pad. Changing your mind should REPLACE, not
    // stack — otherwise whoever judges most often wins, and that's turnout,
    // not taste.
    await verdicts.updateOne(
      { user: user.sub, code },
      { $set: row },
      { upsert: true },
    );
  } else {
    await verdicts.insertOne(row);
  }

  await database.disconnect();

  return respond(200, {
    ok: true,
    attributed,
    handle: row.handle,
    message: attributed
      ? `${verdict} — recorded for ${handle}`
      : `${verdict} — counted, but you're not handled, so it's anonymous`,
  });
}
