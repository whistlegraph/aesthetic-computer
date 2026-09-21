// Mongo update pipeline: every phase can arrive first (requests race/retry).
// $literal prevents client strings/objects being interpreted as expressions.
export function bootTelemetryUpdate({ bootId, phase, meta = {}, data = {}, server, now }) {
  if (!["start", "log", "error", "complete"].includes(phase)) return null;
  const literal = (value) => ({ $literal: value });
  const update = [{ $set: {
    _id: { $ifNull: ["$_id", literal(bootId)] },
    bootId: { $ifNull: ["$bootId", literal(bootId)] },
    createdAt: { $ifNull: ["$createdAt", literal(now)] },
    meta: { $ifNull: ["$meta", literal(meta)] },
    server: { $ifNull: ["$server", literal(server)] },
    status: { $ifNull: ["$status", "started"] },
    updatedAt: literal(now),
  } }];
  if (meta?.user) update.push({ $set: { "meta.user": literal(meta.user) } });
  if (phase === "log") {
    const events = Array.isArray(data.events) ? data.events.slice(-500) : [];
    update.push({ $set: { events: { $slice: [
      { $concatArrays: [{ $ifNull: ["$events", []] }, literal(events)] }, -500,
    ] } } });
  } else if (phase === "error") {
    update.push({ $set: {
      // A late error payload retains evidence without undoing a completed boot.
      status: { $cond: [{ $eq: ["$status", "success"] }, "success", "error"] },
      error: literal(data),
    } });
  } else if (phase === "complete") {
    update.push({ $set: {
      status: "success", completedAt: literal(now), summary: literal(data),
    } });
  }
  return update;
}

// Legacy ObjectIds remain untouched. New records use the boot ID as Mongo's
// unique _id, so concurrent first phases cannot insert duplicate documents.
export async function writeBootTelemetry(boots, bootId, update) {
  try {
    return await boots.updateOne({ bootId }, update, { upsert: true });
  } catch (error) {
    if (error?.code !== 11000 || error?.keyValue?._id !== bootId) throw error;
    const result = await boots.updateOne({ _id: bootId, bootId }, update);
    if (result.matchedCount !== 1) throw error;
    return result;
  }
}
