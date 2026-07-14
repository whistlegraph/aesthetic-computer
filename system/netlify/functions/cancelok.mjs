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
    const who = pathParams(event.path)[2]; // /api/cancelok/@handle | /mine

    // What YOU have already judged. The piece uses this to deal you a deck of
    // things you haven't seen — being asked the same question twice is the
    // fastest way to make someone stop answering honestly.
    // Sessions, newest first. A session is a sitting: what you were shown, in
    // what order, how long you stayed, what your hands did, and what the frame
    // rate was while you decided. The ok/cancel column alone can't tell you that
    // someone bailed on pad 12 because the whole run had gone slow.
    if (who === "sessions") {
      const rows = await verdicts.find({}).sort({ when: -1 }).limit(400).toArray();
      const by = new Map();
      for (const r of rows) {
        const key = r.session || "—";
        if (!by.has(key))
          by.set(key, {
            session: key,
            handle: r.handle || null,
            when: r.when,
            pads: [],
            ok: 0,
            cancel: 0,
            taps: 0,
          });
        const s = by.get(key);
        s.pads.push({
          code: r.code,
          verdict: r.verdict,
          dwellMs: r.dwellMs,
          taps: r.taps,
          fps: r.fps,
          fpsMin: r.fpsMin,
        });
        s[r.verdict === "ok" ? "ok" : "cancel"] += 1;
        s.taps += r.taps || 0;
        if (r.when < s.when) s.when = r.when;
      }
      await database.disconnect();
      return respond(200, {
        sessions: [...by.values()].map((s) => ({
          ...s,
          pads: s.pads.reverse(), // back into the order you actually saw them
          judged: s.pads.length,
          slowest: Math.min(...s.pads.map((p) => p.fpsMin || 999)),
        })),
      });
    }

    if (who === "mine") {
      let mine = [];
      try {
        const user = await authorize(event.headers);
        if (user?.sub)
          mine = (await verdicts.find({ user: user.sub }).toArray()).map((r) => r.code);
      } catch {
        // logged out — you've judged nothing, so you get the whole deck.
      }
      await database.disconnect();
      return respond(200, { judged: mine });
    }

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
    // dwell and fps come along because they're the two things that explain WHY:
    // a pad nobody stays with, and a pad nobody CAN stay with, look identical in
    // the ok/cancel column and are completely different problems.
    const tally = await verdicts
      .aggregate([
        {
          $group: {
            _id: "$code",
            ok: { $sum: { $cond: [{ $eq: ["$verdict", "ok"] }, 1, 0] } },
            cancel: { $sum: { $cond: [{ $eq: ["$verdict", "cancel"] }, 1, 0] } },
            taps: { $sum: "$taps" },
            dwell: { $avg: "$dwellMs" },
            fps: { $avg: "$fps" },
            worstFps: { $min: "$fpsMin" },
            revisits: { $sum: "$revisit" },
            judges: { $addToSet: "$handle" },
            sessions: { $addToSet: "$session" },
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
        dwellMs: Math.round(t.dwell || 0),
        fps: Math.round(t.fps || 0),
        worstFps: Math.round(t.worstFps || 0),
        revisits: t.revisits || 0,
        judges: t.judges.filter(Boolean).length,
        sessions: t.sessions.filter(Boolean).length,
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
    // Real frame rate, on the machine a real person was holding. The headless
    // gate can only prove a pad runs in a datacenter; this is whether it ran for
    // YOU. A pad that stutters on the hardware it's judged on has already been
    // cancelled by physics, and no amount of taste can save it.
    fps: Number(body.fps) || 0,
    fpsMin: Number(body.fpsMin) || 0,
    w: Number(body.w) || 0,
    h: Number(body.h) || 0,
    revisit: Number(body.revisit) || 0, // coming BACK is the strongest keep there is
    session: String(body.session || "").slice(0, 40),
    failed: Boolean(body.failed),
    when: new Date(),
    user: attributed ? user.sub : null,
    handle: attributed ? handle : null,
  };

  await verdicts.createIndex({ session: 1 });

  if (attributed) {
    // One verdict per person per pad. Changing your mind should REPLACE, not
    // stack — otherwise whoever judges most often wins, and that's turnout,
    // not taste. But keep the BEST of the two: if you came back and stayed, that
    // supersedes the time you flicked past it, not the other way around.
    const prior = await verdicts.findOne({ user: user.sub, code });
    const better = !prior || row.verdict === "ok" || prior.verdict !== "ok";
    if (better)
      await verdicts.updateOne({ user: user.sub, code }, { $set: row }, { upsert: true });
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
