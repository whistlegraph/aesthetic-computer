import { whistlegraphPurchases } from './whistlegraph-iap.mjs';
import { connect } from './database.mjs';

export const sandboxAccounts = () => new Set((process.env.WHISTLEGRAPH_IAP_SANDBOX_USERS || '').split(',').map(s => s.trim()).filter(Boolean));

export async function withWhistlegraphPurchases(run, { verifiers = [] } = {}) {
  const connection = await connect();
  try {
    const collection = name => connection.db.collection(name);
    const accounts = collection('whistlegraph-iap-accounts');
    await accounts.createIndex({ appAccountToken: 1 }, { unique: true });
    const notifications = collection('whistlegraph-iap-notifications');
    await notifications.createIndex({ status: 1, nextAttemptAt: 1, receivedAt: 1 });
    return await run(whistlegraphPurchases({ accounts, notifications,
      purchases: collection('whistlegraph-iap-purchases'), wallets: collection('ac-credit-wallets'),
      deletions: collection('account-deletions'), tombstones: collection('account-tombstones'),
      verifiers, sandboxUsers: sandboxAccounts() }));
  } finally { await connection.disconnect(); }
}

// Verified rows carry the desired refund state. Lith retries independently of
// Apple's notification retry window and without storing replayable signed JWS.
export async function reconcileWhistlegraphPurchases() {
  return withWhistlegraphPurchases(service => service.reconcilePending());
}
