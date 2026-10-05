import { createHash, randomBytes, randomUUID } from 'node:crypto';
import { getPkhfromPk, verifySignature, validateAddress } from '@taquito/utils';
import { CREDIT_PACK, fulfillGrant } from './easel-paid-credits.mjs';

export const TEZOS_TREASURY = 'tz1gkf8EexComFBJvjtT1zdsisdah791KwBE'; // aesthetic.tez
export const CONFIRMATIONS = 3;
export const QUOTE_MS = 15 * 60_000;
export const TEZOS_CREDIT_PACKS = Object.freeze([
  Object.freeze({ id:'braincells-600k-v1', amount:300, credits:600_000 }),
  CREDIT_PACK,
]);
const API = 'https://api.tzkt.io/v1';
const digest = value => createHash('sha256').update(value).digest('hex');
export const paymentError = (status, message) => Object.assign(new Error(message), { status });
const publicIntent = i => ({ id:i._id, handle:i.handle, status:i.status, credits:i.credits, usd:i.usd, pack:i.pack || CREDIT_PACK.id,
  network:'mainnet', recipient:i.recipient, sender:i.sender || null, amountMutez:i.amountMutez || null,
  expiresAt:i.expiresAt || null, operationHash:i.operationHash || null, confirmations:CONFIRMATIONS });

let cachedPrice;
export async function displayPrice({ chain = chainJSON, now = new Date() } = {}) {
  if (chain === chainJSON && cachedPrice && +now - Date.parse(cachedPrice.asOf) < 60_000) return cachedPrice;
  const head = await chain('/head');
  quoteAmount(head, now); // Same freshness and network checks as checkout.
  const price = { usdPerTez:head.quoteUsd, braincellsPerUSD:CREDIT_PACK.credits / (CREDIT_PACK.amount / 100),
    asOf:head.timestamp, source:'TzKT', expiresAt:new Date(Date.parse(head.timestamp) + 5 * 60_000).toISOString() };
  if (chain === chainJSON) cachedPrice = price;
  return price;
}

export async function chainJSON(path, fetcher = fetch) {
  const response = await fetcher(`${API}${path}`, { signal:AbortSignal.timeout(15_000) });
  if (!response.ok) throw paymentError(503, 'Tezos verification is unavailable. Your payment can be checked again.');
  return response.json();
}

export function quoteAmount(head, now = new Date(), cents = CREDIT_PACK.amount) {
  if (head.chain !== 'mainnet' || head.chainId !== 'NetXdQprcVkpaWU' || !head.synced || !Number.isFinite(head.quoteUsd) || head.quoteUsd <= 0 ||
      !Number.isSafeInteger(head.level) || !Number.isFinite(Date.parse(head.timestamp)) ||
      Math.abs(+now - Date.parse(head.timestamp)) > 5 * 60_000 || !Number.isSafeInteger(head.quoteLevel) || head.quoteLevel < head.level - 100) {
    throw paymentError(503, 'A fresh Tezos price is unavailable. Try again shortly.');
  }
  if (!TEZOS_CREDIT_PACKS.some(pack => pack.amount === cents)) throw paymentError(400, 'Unknown braincell pack');
  const mutez = Math.ceil((cents / 100) / head.quoteUsd * 1e6);
  if (!Number.isSafeInteger(mutez) || mutez < 1) throw paymentError(503, 'Invalid Tezos quote');
  return String(mutez);
}

export function ownershipPayload(intent) {
  const message = `Tezos Signed Message: Buy AC braincells\nhttps://aesthetic.computer\nCheckout: ${intent._id}\nAccount: @${intent.handle}\nNonce: ${intent.nonce}`;
  const bytes = Buffer.from(message, 'utf8');
  // Micheline string, not a transaction: wallets display the readable message.
  return '0501' + bytes.length.toString(16).padStart(8, '0') + bytes.toString('hex');
}

export function verifyOwnership(intent, { address, publicKey, signature }) {
  try {
    return typeof address === 'string' && /^tz[123]/.test(address) && validateAddress(address) === 3 &&
      getPkhfromPk(publicKey) === address && verifySignature(ownershipPayload(intent), publicKey, signature);
  } catch { return false; }
}

export function matchingPayment(intent, transactions, head) {
  if (head.chain !== 'mainnet' || head.chainId !== 'NetXdQprcVkpaWU' || !head.synced || !Number.isSafeInteger(head.level)) throw paymentError(503, 'Tezos is still syncing');
  const candidates = transactions.filter(tx => tx.status === 'applied' && tx.sender?.address === intent.sender &&
    tx.target?.address === intent.recipient && String(tx.amount) === intent.amountMutez &&
    // TzKT omits initiator for a top-level transfer; internal transfers have a nonce.
    (!tx.initiator || tx.initiator.address === intent.sender) && tx.nonce == null && !tx.parameter && Number.isSafeInteger(tx.id) && Number.isSafeInteger(tx.level) &&
    Date.parse(tx.timestamp) >= Date.parse(intent.quotedAt) && Date.parse(tx.timestamp) <= Date.parse(intent.expiresAt));
  if (!candidates.length) return null;
  const tx = candidates[0];
  return { tx, confirmed:head.level - tx.level + 1 >= CONFIRMATIONS };
}

// Dependencies are collections on one database. A global transaction claim
// fixes its owner before the wallet grant; either write can be safely resumed.
export function tezosPayments({ intents, claims, wallets, chain = chainJSON, verify = verifyOwnership,
  now = () => new Date(), recipient = TEZOS_TREASURY }) {
  async function lookup(secret) {
    if (typeof secret !== 'string' || !/^[a-f0-9]{64}$/.test(secret)) throw paymentError(401, 'Invalid checkout');
    const intent = await intents.findOne({ _id:digest(secret) });
    if (!intent) throw paymentError(404, 'Checkout not found');
    return intent;
  }
  return {
    async create(user, handle) {
      if (!/^tz[123]/.test(recipient) || validateAddress(recipient) !== 3) throw paymentError(503, 'Tezos payments unavailable');
      // Creating an intent never reserves funds or grants credit. Limit abandoned
      // checkouts per account so the authenticated endpoint cannot grow unbounded.
      if (await intents.countDocuments({ user, createdAt:{ $gt:new Date(+now() - 3600_000) } }) >= 20) throw paymentError(429, 'Too many checkouts. Reopen an existing one.');
      const secret = randomBytes(32).toString('hex');
      const intent = { _id:digest(secret), user, handle:handle.replace(/^@/, ''), nonce:randomUUID(),
        status:'created', createdAt:now(), recipient, credits:CREDIT_PACK.credits, usd:CREDIT_PACK.amount / 100 };
      await intents.insertOne(intent);
      return { ...publicIntent(intent), checkoutURL:`https://aesthetic.computer/braincells/#${secret}` };
    },
    async status(secret) {
      const intent = await lookup(secret);
      const expired = !intent.sender && +now() - +new Date(intent.createdAt) > 24 * 3600_000;
      return { ...publicIntent(intent), ...(expired ? { status:'expired' } : {}), payload:ownershipPayload(intent),
        offers:TEZOS_CREDIT_PACKS.map(pack => ({ id:pack.id, credits:pack.credits, usd:pack.amount / 100 })) };
    },
    async quote(secret, proof) {
      const intent = await lookup(secret);
      if (intent.status === 'credited') return publicIntent(intent);
      if (intent.sender) {
        if (intent.sender !== proof.address) throw paymentError(409, 'This checkout belongs to a different wallet');
        if (proof.pack && proof.pack !== (intent.pack || CREDIT_PACK.id)) throw paymentError(409, 'This checkout already has a fixed pack');
        if (+now() > Date.parse(intent.expiresAt)) throw paymentError(409, 'Quote expired. Start a new checkout; do not pay this quote.');
        return publicIntent(intent);
      }
      if (+now() - +new Date(intent.createdAt) > 24 * 3600_000) throw paymentError(409, 'Checkout expired');
      if (!verify(intent, proof)) throw paymentError(403, 'Wallet ownership signature did not verify');
      const pack = TEZOS_CREDIT_PACKS.find(pack => pack.id === (proof.pack ?? CREDIT_PACK.id));
      if (!pack) throw paymentError(400, 'Unknown braincell pack');
      const head = await chain('/head');
      const quotedAt = now();
      const quote = { status:'quoted', sender:proof.address, pack:pack.id, credits:pack.credits, usd:pack.amount / 100,
        amountMutez:quoteAmount(head, quotedAt, pack.amount),
        quotedAt:quotedAt.toISOString(), expiresAt:new Date(+quotedAt + QUOTE_MS).toISOString(), quoteUsd:head.quoteUsd };
      await intents.updateOne({ _id:intent._id, sender:{ $exists:false } }, { $set:quote });
      const saved = await lookup(secret);
      if (saved.sender !== proof.address) throw paymentError(409, 'Another wallet already claimed this checkout');
      if (saved.pack !== pack.id) throw paymentError(409, 'Another quote already fixed the pack for this checkout');
      return publicIntent(saved);
    },
    async confirm(secret, operationHash) {
      const intent = await lookup(secret);
      if (intent.status === 'credited') return publicIntent(intent);
      if (!intent.sender) throw paymentError(409, 'Connect your wallet and get a quote first');
      // On iOS the browser may be suspended before Beacon returns the hash.
      // Recover by the verified sender, recipient, exact quote and time window.
      if (!operationHash) {
        if (intent.operationHash) operationHash = intent.operationHash;
        else {
          const query = new URLSearchParams({ sender:intent.sender, target:intent.recipient, amount:intent.amountMutez,
            'timestamp.ge':intent.quotedAt, 'timestamp.le':intent.expiresAt, status:'applied', limit:'100', 'sort.asc':'id' });
          const txs = await chain(`/operations/transactions?${query}`);
          for (const tx of txs) {
            const claim = await claims.findOne({ _id:`tezos:mainnet:${tx.id}` });
            if (!claim || claim.intent === intent._id) { operationHash = tx.hash; break; }
          }
          if (!operationHash) return { ...publicIntent(intent), status:+now() > Date.parse(intent.expiresAt) ? 'expired' : 'quoted' };
        }
      }
      if (typeof operationHash !== 'string' || !/^o[1-9A-HJ-NP-Za-km-z]{50}$/.test(operationHash)) throw paymentError(400, 'Invalid Tezos operation hash');
      if (intent.operationHash && intent.operationHash !== operationHash) throw paymentError(409, 'This checkout already has a payment');
      const [transactions, head] = await Promise.all([chain(`/operations/transactions/${operationHash}`), chain('/head')]);
      const payment = matchingPayment(intent, transactions, head);
      if (!payment) {
        if (transactions.length) throw paymentError(422, 'Payment does not match this quote. No braincells were added.');
        return { ...publicIntent(intent), status:'confirming' };
      }
      if (!payment.confirmed) return { ...publicIntent(intent), status:'confirming' };
      const id = `tezos:mainnet:${payment.tx.id}`;
      try { await claims.insertOne({ _id:id, intent:intent._id, user:intent.user, operationHash, createdAt:now() }); }
      catch (error) { if (error.code !== 11000) throw error; }
      const claim = await claims.findOne({ _id:id });
      if (claim.intent !== intent._id || claim.user !== intent.user) throw paymentError(409, 'Payment already belongs to another checkout');
      // Claim the invoice too: two different payments cannot race to grant two
      // packs for one checkout. Extra payments require manual reconciliation.
      await intents.updateOne({ _id:intent._id, operationHash:{ $exists:false } }, { $set:{ operationHash, grantId:id } });
      const saved = await lookup(secret);
      if (saved.grantId !== id) throw paymentError(409, 'This checkout already has another payment');
      await fulfillGrant({ user:intent.user, id, credits:intent.credits }, wallets);
      await intents.updateOne({ _id:intent._id, grantId:id }, { $set:{ status:'credited', creditedAt:now() } });
      return publicIntent(await lookup(secret));
    },
  };
}
