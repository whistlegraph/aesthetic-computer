import { createHash, randomUUID } from 'node:crypto';
import { SignedDataVerifier, Environment } from '@apple/app-store-server-library';
import { appleRootCertificates } from './apple-roots.mjs';

export const BUNDLE_ID = 'computer.aesthetic.walkieware';
export const PRODUCT_ID = 'computer.aesthetic.walkieware.braincells.1m';
export const CREDITS = 1_000_000;
const uuid = /^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}$/i;
const fail = (status, message) => Object.assign(new Error(message), { status });

// Apple's signedDate orders snapshots, including retries delivered out of order:
// https://developer.apple.com/documentation/appstoreservernotifications/signeddate
export function purchaseState(transaction, notification) {
  const signedDate = (notification || transaction).signedDate;
  if (!Number.isSafeInteger(signedDate) || signedDate < 1) throw fail(400, 'Missing Apple signature date.');
  const type = notification?.notificationType || (transaction.revocationDate != null ? 'REFUND' : 'PURCHASE');
  let refundedCredits = 0;
  if (type === 'REFUND' || type === 'REVOKE') {
    // Server JWS percentages are milliunits (100000 = 100%), not the decimal
    // percentage exposed by the Swift Transaction API. Legacy refunds are full.
    const percent = transaction.revocationPercentage ?? (transaction.revocationType === 'REFUND_PRORATED' ? NaN : 100_000);
    if (!Number.isSafeInteger(percent) || percent < 0 || percent > 100_000) throw fail(400, 'Invalid Apple refund percentage.');
    refundedCredits = Math.round(CREDITS * percent / 100_000);
  }
  return { signedDate, refundedCredits, type };
}

const newer = (path, state) => ({ $or: [
  { [path]: { $exists: false } },
  { [`${path}.signedDate`]: { $lt: state.signedDate } },
  // In the unlikely event of equal timestamps, the larger refund wins.
  { [`${path}.signedDate`]: state.signedDate, [`${path}.refundedCredits`]: { $lt: state.refundedCredits } },
] });

// One wallet update grants/revokes/restores the difference, never a second pack.
// It also handles a refund arriving before redemption without exposing a
// temporarily spendable balance between an increment and a reversal.
export function walletStateUpdate(grant, state) {
  const refund = `refunds.${grant.id}`, marker = `applePurchases.${grant.id}`;
  const grants = { $ifNull: ['$grants', []] };
  const initial = { $cond: [{ $in: [grant.id, grants] }, 0, grant.credits] };
  return {
    filter: { _id: grant.user, ...newer(marker, state) },
    update: [{ $set: {
      balance: { $add: [{ $ifNull: ['$balance', 0] }, initial,
        { $subtract: [{ $ifNull: [`$${refund}`, 0] }, state.refundedCredits] }] },
      grants: { $setUnion: [grants, [grant.id]] },
      [refund]: state.refundedCredits, [marker]: { $literal: state }, updatedAt: '$$NOW',
    } }],
  };
}

export function makeVerifiers({ appAppleId, sandbox = false, online = true, roots = appleRootCertificates() }) {
  if (!Number.isSafeInteger(appAppleId) || appAppleId < 1) throw fail(503, 'App Store purchases are not configured yet.');
  const verifiers = [new SignedDataVerifier(roots, online, Environment.PRODUCTION, BUNDLE_ID, appAppleId)];
  if (sandbox) verifiers.push(new SignedDataVerifier(roots, online, Environment.SANDBOX, BUNDLE_ID, appAppleId));
  return verifiers;
}

export function purchaseGrant(transaction, environment) {
  if (!['Production', 'Sandbox'].includes(environment) || transaction?.environment !== environment) throw fail(400, 'Wrong purchase environment.');
  if (transaction.bundleId !== BUNDLE_ID) throw fail(400, 'Wrong app.');
  if (transaction.productId !== PRODUCT_ID || transaction.type !== 'Consumable') throw fail(400, 'Unknown braincell product.');
  // This app sells one pack per purchase. Never infer quantity from missing data.
  if (transaction.quantity !== 1) throw fail(400, 'Invalid purchase quantity.');
  if (typeof transaction.transactionId !== 'string' || !/^\d{1,30}$/.test(transaction.transactionId)) throw fail(400, 'Invalid transaction ID.');
  if (typeof transaction.appAccountToken !== 'string' || !uuid.test(transaction.appAccountToken)) throw fail(409, 'This purchase is not bound to an AC account.');
  return { id: `apple:whistlegraph:${environment}:${transaction.transactionId}`, transactionId: transaction.transactionId,
    productId: PRODUCT_ID, credits: CREDITS, environment, appAccountToken: transaction.appAccountToken.toLowerCase() };
}

// Every claim has one immutable AC owner before touching their wallet. A crash
// after either write is safe to retry; the wallet grants each claim only once.
export function whistlegraphPurchases({ accounts, purchases, notifications, wallets, verifiers, deletions, tombstones, sandboxUsers = new Set(), now = () => new Date() }) {
  async function decode(method, jws, environment) {
    if (typeof jws !== 'string' || !jws.length || jws.length > 48_000) throw fail(400, 'Missing signed purchase.');
    for (const verifier of verifiers) {
      if (environment && verifier.environment !== environment) continue;
      try { return { payload: await verifier[method](jws), environment: verifier.environment }; } catch {}
    }
    throw fail(401, 'Apple could not verify this purchase.');
  }
  function allowed(user, environment) {
    if (environment === 'Sandbox' && !sandboxUsers.has(user)) throw fail(403, 'This account is not enabled for App Store sandbox testing.');
  }
  async function active(user) {
    const hash = createHash('sha256').update(user).digest('hex');
    const [job, tombstone] = await Promise.all([deletions?.findOne({ _id: user }), tombstones?.findOne({ _id: hash })]);
    if (tombstone || (job && job.state !== 'cancelled')) throw fail(409, 'This AC account is being deleted.');
  }
  async function retire(grant) {
    const userHash = createHash('sha256').update(grant.user).digest('hex');
    const [job, tombstone, saved] = await Promise.all([deletions?.findOne({ _id: grant.user }),
      tombstones?.findOne({ _id: userHash }), purchases.findOne({ _id: grant.id })]);
    if (!tombstone && !saved?.deletedAt && !['running', 'failed', 'complete'].includes(job?.state)) return false;
    // The purge may have swept a collection before this in-flight handler wrote
    // it. Repeat the narrow cleanup after every attempt, including failures.
    await accounts.deleteOne({ _id: grant.user, appAccountToken: grant.appAccountToken });
    await purchases.updateOne({ _id: grant.id, user: grant.user }, {
      $set: { deletedAt: now(), userHash }, $unset: { user: '', appAccountToken: '' },
    });
    await notifications?.deleteMany({ purchaseId: grant.id, user: grant.user });
    await wallets.deleteOne({ _id: grant.user, iapCreationID: { $exists: true }, grants: [], balance: 0 });
    return true;
  }
  async function withGrant(grant, run, notification = false) {
    let result, error;
    try { result = await run(); } catch (cause) { error = cause; }
    if (await retire(grant)) {
      if (notification) return { received: true };
      throw fail(410, 'The account for this purchase was deleted.');
    }
    if (error) throw error;
    return result;
  }
  async function owner(grant, user) {
    const saved = await purchases.findOne({ _id: grant.id });
    if (saved?.deletedAt) {
      if (user) throw fail(410, 'The account for this purchase was deleted.');
      return null;
    }
    const account = await accounts.findOne({ appAccountToken: grant.appAccountToken });
    if (!account || account.deletedAt || (user && account._id !== user)) throw fail(403, 'Sign in to the AC account that bought these braincells.');
    if (user) await active(account._id);
    // Removing a sandbox tester must not prevent reversing their old grant.
    if (user) allowed(account._id, grant.environment);
    else if (grant.environment === 'Sandbox' && !sandboxUsers.has(account._id) && !saved?.sandboxAuthorized)
      throw fail(403, 'This account is not enabled for App Store sandbox testing.');
    return { ...grant, user: account._id };
  }
  async function claim(grant) {
    const { id, ...fields } = grant;
    try { await purchases.insertOne({ _id: id, ...fields,
      sandboxAuthorized: grant.environment === 'Sandbox' && sandboxUsers.has(grant.user), createdAt: now() }); }
    catch (error) { if (error.code !== 11000) throw error; }
    const saved = await purchases.findOne({ _id: id });
    if (!saved || saved.deletedAt || ['user', 'appAccountToken', 'productId', 'credits', 'environment', 'transactionId'].some(key => saved[key] !== grant[key]))
      throw fail(409, 'This transaction already belongs to another purchase.');
    return saved;
  }
  async function apply(grant, state, initialOnly = false) {
    await claim(grant);
    // Save the intended state before delivery. Both documents reject stale
    // snapshots independently, so a crash or concurrent retry cannot regress it.
    await purchases.updateOne({ _id: grant.id, deletedAt: { $exists: false },
      ...(initialOnly ? { state: { $exists: false } } : newer('state', state)) }, { $set: { state } });
    const saved = await purchases.findOne({ _id: grant.id });
    if (!saved?.state || saved.deletedAt) throw fail(410, 'The account for this purchase was deleted.');
    await active(grant.user);
    const iapCreationID = randomUUID();
    try { await wallets.updateOne({ _id: grant.user }, {
      $setOnInsert: { balance: 0, grants: [], createdAt: now(), iapCreationID },
    }, { upsert: true }); } catch (error) { if (error.code !== 11000) throw error; }
    // Check durable deletion state after ensuring the wallet; financial writes
    // never upsert. A purge that removed it cannot be undone by a late update.
    await active(grant.user);
    const account = await accounts.findOne({ _id: grant.user });
    if (!account || account.deletedAt || account.appAccountToken !== grant.appAccountToken) throw fail(410, 'The account for this purchase was deleted.');
    const { filter, update } = walletStateUpdate(grant, saved.state);
    update.push({ $unset: 'iapCreationID' });
    const delivered = await wallets.updateOne(filter, update);
    const wallet = await wallets.findOne({ _id: grant.user });
    const applied = wallet?.applePurchases?.[grant.id];
    if (!applied || applied.signedDate < saved.state.signedDate) throw fail(503, 'Purchase delivery is pending. Reopen Whistlegraph to retry.');
    await purchases.updateOne({ _id: grant.id, deletedAt: { $exists: false } }, { $set: { deliveredAt: now() } });
    return { credited: delivered.modifiedCount === 1, state: applied };
  }
  return {
    async account(user) {
      await active(user);
      const token = randomUUID();
      try { await accounts.insertOne({ _id: user, appAccountToken: token, createdAt: now() }); }
      catch (error) { if (error.code !== 11000) throw error; }
      try { await active(user); } catch (error) {
        // Only remove this attempt's insertion. A restorable account's existing
        // mapping belongs to its earlier request and must survive the grace period.
        if (error.status === 409) await accounts.deleteOne({ _id: user, appAccountToken: token });
        throw error;
      }
      const account = await accounts.findOne({ _id: user });
      if (account?.deletedAt || !uuid.test(account?.appAccountToken || '')) throw fail(503, 'Could not prepare the purchase account.');
      return { appAccountToken: account.appAccountToken };
    },
    async redeem(user, jws) {
      const { payload, environment } = await decode('verifyAndDecodeTransaction', jws);
      const grant = await owner(purchaseGrant(payload, environment), user);
      // A non-revoked client receipt never clears a server refund. Only Apple's
      // ordered REFUND_REVERSED notification may restore refunded credits.
      return withGrant(grant, async () => {
        const { credited, state } = await apply(grant, purchaseState(payload), payload.revocationDate == null);
        if (state.refundedCredits > 0) throw fail(409, 'This purchase was refunded.');
        return { credited, transactionId: grant.transactionId, credits: grant.credits, environment };
      });
    },
    async notification(jws) {
      const { payload, environment } = await decode('verifyAndDecodeNotification', jws);
      if (!['REFUND', 'REVOKE', 'REFUND_REVERSED'].includes(payload.notificationType)) return { received: true };
      if (payload.data?.bundleId !== BUNDLE_ID || payload.data?.environment !== environment) throw fail(400, 'Notification app or environment mismatch.');
      const transaction = await decode('verifyAndDecodeTransaction', payload.data.signedTransactionInfo, environment);
      const grant = await owner(purchaseGrant(transaction.payload, environment));
      if (!grant) return { received: true }; // Anonymized claim; never recreate its wallet.
      const state = purchaseState(transaction.payload, payload);
      if (typeof payload.notificationUUID !== 'string' || !/^[a-f0-9]{8}(-[a-f0-9]{4}){3}-[a-f0-9]{12}$/i.test(payload.notificationUUID)) throw fail(400, 'Invalid notification ID.');
      const id = `${environment}:${payload.notificationUUID}`;
      return withGrant(grant, async () => {
        await claim(grant);
        if (notifications) {
          try { await notifications.insertOne({ _id: id, purchaseId: grant.id, user: grant.user,
            ...state, receivedAt: now(), status: 'pending' }); }
          catch (error) { if (error.code !== 11000) throw error; }
        }
        await apply(grant, state);
        // No success response until the balance and its ordering marker are durable.
        await notifications?.updateOne({ _id: id }, { $set: { status: 'applied', appliedAt: now() } });
        return { received: true };
      }, true);
    },
    async reconcilePending({ limit = 50 } = {}) {
      let applied = 0, pending = 0;
      // These rows are written only after Apple verification and immutable claim
      // creation. Replaying them needs no signing keys or stored receipt payload.
      const rows = await notifications.find({ status: 'pending', $or: [
        { nextAttemptAt: { $exists: false } }, { nextAttemptAt: { $lte: now() } },
      ] }).sort({ receivedAt: 1 }).limit(limit).toArray();
      for (const row of rows) {
        const saved = await purchases.findOne({ _id: row.purchaseId });
        if (!saved || saved.deletedAt) { await notifications.deleteOne({ _id: row._id }); continue; }
        const { _id: id, user, appAccountToken, productId, credits, environment, transactionId } = saved;
        const grant = { id, user, appAccountToken, productId, credits, environment, transactionId };
        try {
          await withGrant(grant, async () => {
            const { signedDate, refundedCredits, type } = row;
            await apply(grant, { signedDate, refundedCredits, type });
            await notifications.updateOne({ _id: row._id }, { $set: { status: 'applied', appliedAt: now() } });
          }, true);
          applied++;
        } catch {
          pending++;
          // Restorable deletion jobs must not occupy the first batch forever.
          await notifications.updateOne({ _id: row._id }, { $set: { nextAttemptAt: new Date(+now() + 15 * 60_000) } });
        }
      }
      return { applied, pending };
    },
  };
}
