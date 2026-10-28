// One row in `pieces` per @handle piece: the first registration fixes its
// code, slug and feed position; later saves update its bytes. The row is what
// mime.ac's piece stream and the profile count read. Shared by
// /api/register-piece (Aesel's publish) and notepaint's "Make piece".
import {createHash} from 'node:crypto';
import {ObjectId} from 'mongodb';
import {generateUniqueCode} from './generate-short-code.mjs';

export const PIECE_BUCKET = 'user-aesthetic-computer';

export async function registerPiece(pieces, {user, slug, ext, bytes, origin = 'aesel', makeCode = generateUniqueCode, now = new Date()}) {
  const mediaSlug = `piece/${slug}`;
  const existing = await pieces.findOne({user, slug: {$in: [mediaSlug, `${user}/${mediaSlug}`]}});
  // A deterministic id protects concurrent first publications of one name.
  const id = existing?._id || new ObjectId(createHash('sha256').update(`named-piece\0${user}\0${slug}`).digest('hex').slice(0, 24));
  const code = existing?.code || await makeCode(pieces, {mode: 'random', type: 'piece'});
  const revision = createHash('sha256').update(bytes).digest('hex');
  await pieces.updateOne({_id: id}, {
    $setOnInsert: {code, slug: mediaSlug, user, when: now, bucket: PIECE_BUCKET, type: 'piece', name: slug, origin},
    $set: {ext, size: bytes.length, revision, updatedAt: now},
  }, {upsert: true});
  const saved = await pieces.findOne({_id: id});
  return {code: saved.code, id: String(saved._id), discussion: `https://mime.ac/piece_${saved._id}`};
}
