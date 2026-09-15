import { mongoPaintingWips } from "./painting-wips.mjs";
import { createMediaRecord, MediaTypes } from "./media-atproto.mjs";

export async function sealPaintingWip(database, body, user) {
  const { service, paintings } = await mongoPaintingWips(database.db);
  const result = await service.seal(body.wip, user, body.slug);
  if (result.newlyDone) {
    try {
      const record = await paintings.findOne({ code: result.code });
      const atproto = await createMediaRecord(database, MediaTypes.PAINTING, record, { userSub: user?.sub || null });
      if (atproto?.rkey) await paintings.updateOne({ _id: record._id }, { $set: { "atproto.rkey": atproto.rkey } });
    } catch (error) { console.warn("Painting federation:", error.message); }
  }
  return result;
}
