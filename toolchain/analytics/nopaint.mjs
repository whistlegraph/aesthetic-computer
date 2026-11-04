// No Paint across its instruments, for nopaint-report.mjs (on Lith) and the
// analytics MCP. Read-only; no database import here so tests can load it.
//
// 1. The No Paint → notepaint / aesthetic.computer sticker split and the
//    notepaint funnel, from first-party visits. Each site's visits are their
//    own aggregate; the two sites' visit IDs are never joined.
// 2. Classic saves: the 2021 Construct game's "Done" button posts to
//    server.nopaint.art/upload-painting → `nopaint-classic-paintings`. The
//    visit collector cannot see these, so they are counted here. Counts only:
//    never painting codes or PNG bytes.
// 3. Teacher notes from the title-screen form → `nopaint-teacher-notes`.

export const CLASSIC_COLLECTION = "nopaint-classic-paintings";
export const TEACHER_COLLECTION = "nopaint-teacher-notes";
export const NOTEPAINT_ACTIONS = ["note_written", "paint_pressed", "paint_signin_asked", "paint_failed", "painting_rejected",
  "canvas_interacted", "painting_edited", "painting_signed", "painting_saved", "piece_made",
  "braincells_sheet_shown", "braincells_limit_reached", "braincells_checkout_opened", "braincells_purchased"];
// Where notepaint visitors from nopaint.art stop, in the order the studio asks.
export const NOTEPAINT_STEPS = ["visits", "interacted", "note_written", "paint_pressed", "paint_signin_asked",
  "braincells_limit_reached", "paint_failed", "painting_edited", "painting_rejected", "painting_signed", "painting_saved", "piece_made"];
const MAX_NOTES = 200;

export function nopaintReportOptions({ hours = 24, end } = {}, now = new Date()) {
  hours = Number(hours);
  const until = end === undefined ? now : new Date(end);
  if (!Number.isFinite(hours) || hours <= 0 || hours > 840 || !Number.isFinite(+until))
    throw new Error("Use --hours 1..840 and optional --end ISO-date");
  return { start: new Date(+until - hours * 3600000), end: until };
}

const count = field => ({ $sum: { $cond: [`$${field}`, 1, 0] } });
const both = (a, b) => ({ $sum: { $cond: [{ $and: [`$${a}`, `$${b}`] }, 1, 0] } });
const utcDay = field => ({ $dateToString: { format: "%Y-%m-%d", date: `$${field}`, timezone: "+00:00" } });
const FROM_NOPAINT = { $in: ["$referrerHost", ["nopaint.art", "www.nopaint.art"]] };

export async function nopaintReport(db, options = {}) {
  const { start, end } = options.start && options.end ? options : nopaintReportOptions(options);
  const range = { $gte: start, $lt: end };
  const aggregate = (name, pipeline) => db.collection(name).aggregate(pipeline, { maxTimeMS: 10000 }).toArray();
  const [clicks, destinations, arrivals, saves, reservations, notes] = await Promise.all([
    aggregate("network-visits", [
      { $match: { property: "nopaint.art", startedAt: range, linkVersion: 1 } },
      { $group: { _id: { automated: "$automated" }, visitsWithLink: { $sum: 1 },
        firstMeasuredVisit: { $min: "$startedAt" }, interacted: count("interacted"),
        notepaintOpened: count("actions.notepaint_opened"),
        // The sticker only shows once the title art is up; a visit that left
        // before it (or never interacted) never saw it. Click-through counts
        // only visits it was shown to.
        adShown: count("actions.notepaint_ad_shown"),
        openedAfterShown: both("actions.notepaint_ad_shown", "actions.notepaint_opened"),
        // The aesthetic.computer arm of the sticker split (from 2026-11-04).
        acAdShown: count("actions.ac_ad_shown"), acOpened: count("actions.ac_opened"),
        acOpenedAfterShown: both("actions.ac_ad_shown", "actions.ac_opened"),
        teacherNoteOpened: count("actions.teacher_note_opened"), teacherNoteSent: count("actions.teacher_note_sent") } },
    ]),
    // Arrivals on each arm's destination with nopaint.art as referrer. Arrival
    // counts are a floor; the two sites' visits are never joined.
    aggregate("network-visits", [
      { $match: { property: { $in: ["notepaint.art", "aesthetic.computer"] }, automated: false,
        startedAt: range, $expr: FROM_NOPAINT } },
      { $group: { _id: "$property", arrivals: { $sum: 1 }, interacted: count("interacted"), engaged: count("engaged"),
        stayed60: { $sum: { $cond: [{ $and: ["$interacted", { $gte: ["$activeSeconds", 60] }] }, 1, 0] } },
        canvas_interacted: count("actions.canvas_interacted"), painting_edited: count("actions.painting_edited"),
        painting_saved: count("actions.painting_saved") } },
    ]),
    aggregate("network-visits", [
      { $match: { property: "notepaint.art", startedAt: range } },
      { $group: { _id: { automated: "$automated", from: { $switch: { branches: [
          { case: FROM_NOPAINT, then: "nopaint.art" },
          { case: { $in: ["$referrerHost", ["notepaint.art", "www.notepaint.art"]] }, then: "notepaint.art (itself)" },
          { case: { $eq: [{ $ifNull: ["$referrerHost", null] }, null] }, then: "direct or unknown" },
        ], default: "elsewhere" } } },
        visits: { $sum: 1 }, interacted: count("interacted"), engaged: count("engaged"),
        ...Object.fromEntries(NOTEPAINT_ACTIONS.map(action => [action, count(`actions.${action}`)])) } },
      { $sort: { visits: -1 } },
    ]),
    // Grouping by day drops each painting's code; the PNG is never projected.
    aggregate(CLASSIC_COLLECTION, [
      { $match: { savedAt: range } },
      { $group: { _id: utcDay("savedAt"), saves: { $sum: 1 }, first: { $min: "$savedAt" }, last: { $max: "$savedAt" } } },
      { $sort: { _id: 1 } },
    ]),
    // Codes are reserved when a painting starts saving. A reservation that
    // never received its PNG keeps `expiresAt` and is deleted by TTL a day
    // later, so for windows reaching back past a day this is a floor.
    aggregate(CLASSIC_COLLECTION, [
      { $match: { createdAt: range } },
      { $group: { _id: null, reserved: { $sum: 1 },
        reservedUnsaved: { $sum: { $cond: [{ $eq: [{ $type: "$expiresAt" }, "date"] }, 1, 0] } } } },
    ]),
    // TODO: no analytics tool returns private free text (SCORE.md), so names,
    // emails and messages stay out; they reach @jeffrey's inbox by mail.
    db.collection(TEACHER_COLLECTION).find({ createdAt: range },
      { projection: { _id: 0, createdAt: 1, school: 1, lang: 1 }, maxTimeMS: 10000 })
      .sort({ createdAt: 1 }).limit(MAX_NOTES + 1).toArray(),
  ]);
  return foldNopaint({ clicks, destinations, arrivals, saves, reservations, notes }, { start, end });
}

const rate = (n, d) => d ? +(100 * n / d).toFixed(2) : null;

export function foldNopaint({ clicks = [], destinations = [], arrivals = [], saves = [], reservations = [], notes = [] }, { start, end }) {
  const human = clicks.find(row => !row._id.automated);
  const fromNopaint = arrivals.find(row => !row._id.automated && row._id.from === "nopaint.art");
  // The Braincells funnel across every human notepaint arrival. The purchase
  // flag is set only after AC confirmed the payment with Stripe and granted it.
  const funnel = Object.fromEntries(["visits", "braincells_sheet_shown", "braincells_limit_reached",
    "braincells_checkout_opened", "braincells_purchased"].map(key => [key,
    arrivals.filter(row => !row._id.automated).reduce((sum, row) => sum + (row[key] || 0), 0)]));
  const arrived = property => destinations.find(row => row._id === property) || {};
  // One row per arm: shown → opened → arrived → engaged → made something.
  // "Made" is the destination's own act: an accepted paint on notepaint, a
  // touch on AC's canvas. Compare engaged per shown for the cleanest read.
  const stickerSplit = ["notepaint", "ac"].map(arm => {
    const shown = arm === "ac" ? human?.acAdShown : human?.adShown;
    const opened = arm === "ac" ? human?.acOpenedAfterShown : human?.openedAfterShown;
    const dest = arrived(arm === "ac" ? "aesthetic.computer" : "notepaint.art");
    const made = arm === "ac" ? dest.canvas_interacted : dest.painting_edited;
    return { arm, shown: shown || 0, opened: opened || 0, clickThroughPercent: rate(opened, shown),
      arrivals: dest.arrivals || 0, engaged: dest.engaged || 0, stayed60: dest.stayed60 || 0, made: made || 0,
      engagedPerShownPercent: rate(dest.engaged, shown), madePerShownPercent: rate(made, shown) };
  });
  const reserved = reservations[0] || {};
  const classicSaves = {
    total: saves.reduce((sum, row) => sum + row.saves, 0),
    perDay: Object.fromEntries(saves.map(row => [row._id, row.saves])),
    firstSavedAt: saves[0]?.first ?? null, lastSavedAt: saves.at(-1)?.last ?? null,
    reserved: reserved.reserved || 0, reservedUnsaved: reserved.reservedUnsaved || 0,
  };
  const teacherNotes = {
    count: Math.min(notes.length, MAX_NOTES), truncated: notes.length > MAX_NOTES,
    notes: notes.slice(0, MAX_NOTES).map(note => ({ day: new Date(note.createdAt).toISOString().slice(0, 10),
      school: note.school || null, lang: note.lang || null })),
    formOpened: human?.teacherNoteOpened || 0, formSent: human?.teacherNoteSent || 0,
  };
  return { start, end,
    unit: "page visits, not unique people; actions are once per visit. Classic saves and teacher notes are records, not visits.",
    note: "Click-through is visits that opened notepaint among visits the sticker was shown to (notepaint_ad_shown, recorded at the visit's first interaction); visits before that action shipped are excluded, not zero. The second rate keeps the older denominator, every visit whose page carried the link. Arrivals group notepaint visits by referrer, which privacy settings can omit, so nopaint.art arrivals are a floor. A notepaint visit spans in-page navigation, so its actions belong to that arrival. The halves are separate aggregates, not joined journeys. Classic saves are the 2021 game's Done button (server.nopaint.art), invisible to visits; days are UTC by savedAt. reservedUnsaved counts codes reserved in the window that never got a painting; TTL deletes them after a day, so it is a floor for older windows. Teacher notes carry school and language only; names, emails and messages are not reported.",
    classicSaves,
    teacherNotes,
    nopaintClickThroughPercent: rate(human?.openedAfterShown, human?.adShown),
    nopaintClickThroughOfAllLinkVisitsPercent: rate(human?.notepaintOpened, human?.visitsWithLink),
    fromNopaintMakingPercent: rate(fromNopaint?.painting_edited, fromNopaint?.visits),
    stickerSplit,
    notepaintFunnelFromNopaint: Object.fromEntries(NOTEPAINT_STEPS.map(key => [key, fromNopaint?.[key] || 0])),
    braincellsFunnel: funnel,
    nopaint: clicks,
    notepaintArrivals: arrivals,
  };
}

// The part of the report that rides along in network_report: everything but
// the raw per-group rows, which stay in nopaint_report.
export function nopaintSummary({ nopaint, notepaintArrivals, ...summary }) {
  return summary;
}
