// tezos-revenue — tez that reached AC's wallets and the Keeps contract in a
// window, from public chain data (TzKT). Moves between AC's own wallets are
// left out. Sources come from TzKT's sender aliases: marketplaces (objkt,
// teia) mean sales and royalties; payments into the Keeps contract are mint
// fees held there until withdrawn; other payments into aesthetic.tez include
// Braincell tez checkouts, which the Braincell report counts on its own.
import { TEZOS, KEEPS } from "../mcp/tezos-wallets.mjs";

const TZKT = "https://api.tzkt.io/v1";
const NAMES = { ...Object.fromEntries(Object.entries(TEZOS).map(([name, address]) => [address, `${name}.tez`])), [KEEPS]: "Keeps contract" };
const round = (n, d = 2) => Math.round(n * 10 ** d) / 10 ** d;

export function revenueSource(tx) {
  const alias = String(tx.sender?.alias || "").toLowerCase();
  if (tx.target?.address === KEEPS) return "keeps mint fees";
  if (/objkt/.test(alias)) return "objkt sales and royalties";
  if (/teia|hic et nunc|hen\b/.test(alias)) return "teia / hic et nunc sales";
  if (tx.target?.address === TEZOS.aesthetic) return "direct to aesthetic.tez (includes Braincell tez checkouts)";
  return "direct payments";
}

export function foldTezosRevenue(transactions, { usdPerTez = null } = {}) {
  const ours = new Set([...Object.values(TEZOS), KEEPS]);
  const bySource = {}, byWallet = {}, senders = {}, buyers = {};
  let total = 0, count = 0;
  for (const tx of transactions) {
    if (ours.has(tx.sender?.address)) continue; // internal moves
    const xtz = Number(tx.amount) / 1e6;
    if (!(xtz > 0)) continue;
    total += xtz; count++;
    const source = revenueSource(tx), wallet = NAMES[tx.target?.address] || tx.target?.address;
    (bySource[source] ??= { xtz: 0, payments: 0 }).xtz += xtz; bySource[source].payments++;
    (byWallet[wallet] ??= { xtz: 0, payments: 0 }).xtz += xtz; byWallet[wallet].payments++;
    const who = tx.sender?.alias || `${tx.sender?.address?.slice(0, 8)}…`;
    (senders[who] ??= { xtz: 0, payments: 0 }).xtz += xtz; senders[who].payments++;
    // The initiator is the wallet that started the purchase: the buyer.
    if (tx.initiator?.address && !ours.has(tx.initiator.address)) {
      const buyer = tx.initiator.alias || `${tx.initiator.address.slice(0, 8)}…`;
      (buyers[buyer] ??= { xtz: 0, payments: 0 }).xtz += xtz; buyers[buyer].payments++;
    }
  }
  const rows = map => Object.entries(map).sort((a, b) => b[1].xtz - a[1].xtz)
    .map(([key, v]) => ({ key, xtz: round(v.xtz, 4), payments: v.payments, ...(usdPerTez ? { usd: round(v.xtz * usdPerTez) } : {}) }));
  return { xtz: round(total, 4), usd: usdPerTez ? round(total * usdPerTez) : null, usdPerTez, payments: count,
    bySource: rows(bySource), byWallet: rows(byWallet), topSenders: rows(senders).slice(0, 10), topBuyers: rows(buyers).slice(0, 10) };
}

export async function tezosRevenue({ hours = 24, fetch = globalThis.fetch } = {}) {
  const since = new Date(Date.now() - hours * 3600_000).toISOString();
  const targets = [...Object.values(TEZOS), KEEPS].join(",");
  const get = async url => { const r = await fetch(url, { signal: AbortSignal.timeout(30_000) }); if (!r.ok) throw new Error(`${r.status} ${url.split("?")[0]}`); return r.json(); };
  const transactions = await get(`${TZKT}/operations/transactions?target.in=${targets}&timestamp.ge=${since}&status=applied&amount.gt=0` +
    `&select=timestamp,amount,sender,target,initiator,hash&sort.desc=id&limit=1000`);
  // Marketplace contracts often carry no alias of their own; their creator
  // does ("objkt.com Issuer"), so name an unaliased contract after it.
  const unnamed = [...new Set(transactions.filter(tx => !tx.sender?.alias && tx.sender?.address?.startsWith("KT")).map(tx => tx.sender.address))].slice(0, 20);
  const creators = Object.fromEntries(await Promise.all(unnamed.map(async address => {
    try { const c = await get(`${TZKT}/contracts/${address}`); return [address, c.creator?.alias ? `${c.creator.alias} contract` : null]; }
    catch { return [address, null]; }
  })));
  for (const tx of transactions) if (!tx.sender?.alias && creators[tx.sender?.address]) tx.sender.alias = creators[tx.sender.address];
  let usdPerTez = null;
  try { usdPerTez = (await get("https://api.coingecko.com/api/v3/simple/price?ids=tezos&vs_currencies=usd")).tezos?.usd ?? null; } catch {}
  return { since, source: "TzKT public chain data; USD at today's CoinGecko price", truncated: transactions.length >= 1000,
    ...foldTezosRevenue(transactions, { usdPerTez }) };
}
