// No Paint uses the existing daily allowance and purchased Braincells wallet.
import {createHash, randomUUID} from 'node:crypto';
import {DAILY_TOKEN_BUDGET, dayKey} from './ai-budget.mjs';
import {reserve, settle} from './easel-paid-credits.mjs';

export const requestKey = (user, id) => createHash('sha256').update(user+'\0'+id.toLowerCase()).digest('hex');
const fail = (status, message) => Object.assign(Error(message), {status});
const collection = 'nopaint-move-requests';

export async function ensureNoPaintBillingIndexes(db) {
  await db.collection(collection).createIndex({status:1, expiresAt:1}, {name:'pending_recovery'});
  await db.collection(collection).createIndex({user:1}, {name:'account_receipts'});
}

export function noPaintBilling(db, {now=()=>new Date(), limit=DAILY_TOKEN_BUDGET}={}) {
  const receipts = db.collection(collection), usage = db.collection('ai-usage');
  const wallet = session => ({updateOne:(filter, update, options={}) =>
    db.collection('ac-credit-wallets').updateOne(filter, update, {...options, session})});
  async function transaction(work) {
    const session = db.client.startSession();
    try { return await session.withTransaction(()=>work(session), {
      readConcern:{level:'snapshot'}, writeConcern:{w:'majority'},
    }); } finally { await session.endSession(); }
  }
  async function finish(id, success) {
    return transaction(async session => {
      const receipt = await receipts.findOne({_id:id}, {session});
      if (!receipt || receipt.status !== 'pending') return false;
      if (receipt.hold) {
        const settled = await settle(receipt.hold, success ? receipt.paid : 0, wallet(session), {now:now()});
        if (success && !settled) throw fail(503, 'The Braincells reservation expired');
      }
      if (!success && receipt.free) await usage.updateOne({_id:receipt.usageId}, {$inc:{tokens:-receipt.free}}, {session});
      if (success) await usage.updateOne({_id:receipt.usageId}, {
        $inc:{asks:1}, $set:{last:now(), lastModel:receipt.model},
      }, {session});
      await receipts.updateOne({_id:id, status:'pending'}, {$set:{
        status:success ? 'complete' : 'failed', charged:success ? receipt.braincells : 0, finishedAt:now(),
      }}, {session});
      return true;
    });
  }
  async function reconcile(user) {
    let count = 0;
    for await (const receipt of receipts.find({status:'pending', expiresAt:{$lte:now()}, ...(user ? {user} : {})})) {
      if (await finish(receipt._id, false)) count++;
    }
    return count;
  }
  async function begin({user, handle, requestId, hash, braincells, model}) {
    if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(requestId || '')) throw fail(400, 'Move ID must be a UUID');
    if (!Number.isSafeInteger(braincells) || braincells < 1) throw fail(400, 'Invalid move price');
    const id = requestKey(user, requestId), at = now(), day = dayKey(at), usageId = handle+':'+day;
    const holdId = randomUUID();
    await reconcile(user);
    return transaction(async session => {
      const previous = await receipts.findOne({_id:id}, {session});
      if (previous) throw fail(409, previous.hash !== hash ? 'Move ID was already used for a different image or settings' : 'Move already submitted; it will not be charged again');
      await usage.updateOne({_id:usageId}, {$setOnInsert:{handle, day, first:at, tokens:0}}, {upsert:true, session});
      const spent = await usage.findOne({_id:usageId}, {session});
      const free = Math.min(braincells, Math.max(0, limit-(Number(spent.tokens)||0))), paid = braincells-free;
      const hold = paid ? await reserve(user, paid, wallet(session), {id:holdId, now:at}) : null;
      if (paid && !hold) throw fail(402, 'Not enough available Braincells for this move. Choose a local model.');
      if (free) await usage.updateOne({_id:usageId}, {$inc:{tokens:free}}, {session});
      await receipts.insertOne({_id:id, user, hash, model, usageId, day, free, paid, hold, braincells,
        status:'pending', startedAt:at, expiresAt:new Date(+at+5*60_000)}, {session});
      return {id, braincells, free, paid};
    });
  }
  return {begin, finish, reconcile};
}
