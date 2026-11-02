import { createHash } from "node:crypto";
import { LAKLOK_PIECES, LAKLOK_PRESENCE } from "../../system/public/aesthetic.computer/lib/laklok-activity.mjs";

export async function mimeJourneyReport(db, { start, end, scoped, handle, limit }) {
  const query = { ...scoped, tenant: "aesthetic", at: { $gte: start, $lt: end }, $or: [
    { piece: "mime", activityVersion: 1, action: { $in: ["mime_opened", "mime_closed", "mime_piece_viewed"] } },
    { piece: { $in: LAKLOK_PIECES }, action: { $in: ["piece_opened", LAKLOK_PRESENCE] } },
  ] };
  if (handle) {
    const users = await db.collection("@handles").find({ handle: handle.replace(/^@/, "") }, { projection: { _id: 1 } }).limit(10).toArray();
    query.user = { $in: users.map(row => row._id).filter(id => !String(id).startsWith("sotce-")) };
  }
  const collection = db.collection("account-activity");
  const aggregate = pipeline => collection.aggregate(pipeline, { maxTimeMS: 10000 }).toArray();
  const isOpen = { $eq: ["$action", "mime_opened"] };
  const count = condition => ({ $sum: { $cond: [condition, 1, 0] } });
  const promptEntry = { $in: ["$entry", ["prompt_swipe", "prompt_wheel", "prompt_key", "prompt_history"]] };
  const grouped = [
    { $match: query }, { $group: { _id: "$user", first: { $min: "$at" }, last: { $max: "$at" },
      mimePromptOpens: count({ $and: [isOpen, promptEntry] }),
      mimePromptSwipes: count({ $and: [isOpen, { $eq: ["$entry", "prompt_swipe"] }] }),
      mimeDirectOpens: count({ $and: [isOpen, { $eq: ["$entry", "direct"] }] }),
      mimeAlreadyOpen: count({ $and: [isOpen, { $eq: ["$entry", "already_open"] }] }),
      mimeReturns: count({ $and: [{ $eq: ["$action", "mime_closed"] }, { $eq: ["$exitReason", "prompt"] }] }),
      mimeFocusedMs: { $sum: { $ifNull: ["$focusedMs", 0] } },
      laklokOpens: count({ $eq: ["$action", "piece_opened"] }),
      laklokVisibleMinutes: count({ $eq: ["$action", LAKLOK_PRESENCE] }),
    } },
  ];
  const [totals = { accounts: 0, mimePromptAccounts: 0, mimeSwipeAccounts: 0, laklokAccounts: 0, both: 0, swipeAndLaklokAccounts: 0 }] = await aggregate([...grouped, { $group: { _id: null,
    accounts: { $sum: 1 }, mimePromptAccounts: count({ $gt: ["$mimePromptOpens", 0] }),
    mimeSwipeAccounts: count({ $gt: ["$mimePromptSwipes", 0] }), laklokAccounts: count({ $gt: ["$laklokOpens", 0] }),
    swipeAndLaklokAccounts: count({ $and: [{ $gt: ["$mimePromptSwipes", 0] }, { $gt: ["$laklokOpens", 0] }] }),
    both: count({ $and: [{ $gt: ["$mimePromptOpens", 0] }, { $gt: ["$laklokOpens", 0] }] }),
  } }, { $project: { _id: 0 } }]);
  const rows = await aggregate([...grouped, { $sort: { last: -1, _id: 1 } }, { $limit: limit + 1 }]);
  const selected = rows.slice(0, limit), users = selected.map(row => row._id);
  const names = new Map((await db.collection("@handles").find({ _id: { $in: users } }, { projection: { handle: 1 } }).toArray()).map(row => [String(row._id), row.handle]));
  const account = user => names.get(String(user)) ? `@${names.get(String(user))}` : `account-${createHash("sha256").update(String(user)).digest("hex").slice(0, 12)}`;
  const content = await aggregate([
    { $match: { ...query, user: { $in: users }, action: "mime_piece_viewed" } },
    { $group: { _id: { user: "$user", contentId: "$contentId", entry: "$entry" }, focusedMs: { $sum: "$focusedMs" }, last: { $max: "$at" } } },
    { $sort: { last: -1 } }, { $limit: 1001 },
  ]);
  return { start, end, totals, truncated: rows.length > limit, contentTruncated: content.length > 1000,
    note: "Signed-in accounts only, not unique people. MIME coverage starts at this rollout; choose a window after deployment for comparison. Prompt swipes, other prompt entries, direct entries and sign-in while already open are distinct. Focused time is foreground exposure, not proof of attention. Laklok counts whole visible minutes, including a wall clock. No anonymous-history joins. Missing events, opt-outs and offline use leave gaps. Account overlap is within the window, not necessarily the same session.",
    accounts: selected.map(({ _id, ...row }) => ({ account: account(_id), ...row })),
    content: content.slice(0, 1000).map(({ _id, ...row }) => ({ account: account(_id.user), contentId: _id.contentId, entry: _id.entry, ...row })),
  };
}
