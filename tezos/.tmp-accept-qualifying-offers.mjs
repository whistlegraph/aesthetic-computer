import fs from 'fs';
import path from 'path';
import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';

const CONTRACT = 'KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB';
const MARKETPLACE_DEFAULT = 'KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X';
const TOKENS = ['19', '20', '21', '22', '23', '24'];

const ASK_MUTEZ_BY_TOKEN = {
  '19': 8_500_000,
  '20': 9_000_000,
  '21': 10_000_000,
  '22': 10_500_000,
  '23': 12_000_000,
  '24': 13_000_000,
};

function parseEnv(filePath) {
  const out = {};
  const raw = fs.readFileSync(filePath, 'utf8');
  for (const line of raw.split(/\r?\n/)) {
    if (!line || line.trim().startsWith('#')) continue;
    const idx = line.indexOf('=');
    if (idx <= 0) continue;
    const k = line.slice(0, idx).trim();
    const v = line.slice(idx + 1).trim().replace(/^"|"$/g, '');
    out[k] = v;
  }
  return out;
}

async function gql(query, variables) {
  const res = await fetch('https://data.objkt.com/v3/graphql', {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ query, variables }),
  });
  if (!res.ok) throw new Error(`GraphQL HTTP ${res.status}`);
  const body = await res.json();
  if (body.errors?.length) throw new Error(`GraphQL: ${body.errors[0].message}`);
  return body.data;
}

async function ownsToken(address, tokenId) {
  const url = `https://api.tzkt.io/v1/tokens/balances?token.contract=${CONTRACT}&account=${address}&token.tokenId=${tokenId}&balance.gt=0&limit=1`;
  const res = await fetch(url);
  if (!res.ok) throw new Error(`TzKT HTTP ${res.status}`);
  const rows = await res.json();
  return Array.isArray(rows) && rows.length > 0;
}

async function main() {
  const envPath = path.join(process.cwd(), '../aesthetic-computer-vault/tezos/kidlisp/.env');
  const env = parseEnv(envPath);

  const walletAddress = env.AESTHETIC_ADDRESS;
  const secretKey = env.AESTHETIC_KEY;

  if (!walletAddress || !secretKey) {
    throw new Error('Missing AESTHETIC_ADDRESS or AESTHETIC_KEY in vault env');
  }

  const query = `
    query($contract:String!, $ids:[String!]) {
      offer_active(
        where:{_and:[{token:{fa_contract:{_eq:$contract}}},{token:{token_id:{_in:$ids}}}]}
        order_by:{price_xtz:desc}
        limit:100
      ) {
        id
        price_xtz
        buyer_address
        marketplace_contract
        amount_left
        timestamp
        token { token_id name }
      }
    }
  `;

  const data = await gql(query, { contract: CONTRACT, ids: TOKENS });
  const offers = Array.isArray(data.offer_active) ? data.offer_active : [];

  const qualifying = offers.filter((o) => {
    const tokenId = String(o?.token?.token_id ?? '');
    const ask = ASK_MUTEZ_BY_TOKEN[tokenId];
    const bid = Number(o?.price_xtz ?? 0);
    return Number.isFinite(ask) && bid > ask;
  });

  console.log(`Wallet: ${walletAddress}`);
  console.log(`Offers on #19-#24: ${offers.length}`);
  for (const o of offers) {
    const tokenId = String(o.token.token_id);
    const ask = ASK_MUTEZ_BY_TOKEN[tokenId];
    const bid = Number(o.price_xtz);
    console.log(`  offer#${o.id} token#${tokenId} ${o.token.name || ''} bid=${(bid/1e6).toFixed(3)} ask=${(ask/1e6).toFixed(3)} buyer=${o.buyer_address}`);
  }

  if (qualifying.length === 0) {
    console.log('No qualifying offers above ask.');
    return;
  }

  const tezos = new TezosToolkit('https://mainnet.api.tez.ie');
  tezos.setProvider({ signer: await InMemorySigner.fromSecretKey(secretKey) });

  const hashes = [];
  for (const o of qualifying) {
    const offerId = Number(o.id);
    const tokenId = Number(o.token.token_id);
    const marketplace = o.marketplace_contract || MARKETPLACE_DEFAULT;

    const ownerCheck = await ownsToken(walletAddress, tokenId);
    if (!ownerCheck) {
      console.log(`SKIP offer#${offerId}: wallet no longer owns token#${tokenId}`);
      continue;
    }

    console.log(`\nAccepting offer#${offerId} for token#${tokenId} at ${(Number(o.price_xtz)/1e6).toFixed(3)} ꜩ via ${marketplace}`);
    const market = await tezos.contract.at(marketplace);
    const op = await market.methodsObject.fulfill_offer({
      offer_id: offerId,
      token_id: tokenId,
      condition_extra: null,
    }).send();

    console.log(`  sent: ${op.hash}`);
    await op.confirmation(1);
    console.log(`  confirmed: ${op.hash}`);
    hashes.push({ offerId, tokenId, hash: op.hash, priceMutez: Number(o.price_xtz), buyer: o.buyer_address });
  }

  console.log('\nAccepted offers:');
  for (const h of hashes) {
    console.log(`  offer#${h.offerId} token#${h.tokenId} ${(h.priceMutez/1e6).toFixed(3)} ꜩ buyer=${h.buyer} hash=${h.hash}`);
  }
}

main().catch((err) => {
  console.error(`ERROR: ${err.message}`);
  process.exit(1);
});
