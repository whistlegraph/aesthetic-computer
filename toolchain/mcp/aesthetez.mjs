// aesthetez.mjs — reading the Tezos chain for AC: is the chain alive, is the
// art market alive, are our tokens selling, and does it pay for itself.
//
// Public data only (TzKT, the objkt indexer, CoinGecko). Nothing here signs.
// aesthetez-mcp.mjs serves these as tools; crypto-mcp borrows market() and
// tokens() so both servers count a sale the same way.

import { TEZOS, KEEPS, HEN } from "./tezos-wallets.mjs";
import { tezosRevenue } from "../analytics/tezos-revenue.mjs";

const TZKT = "https://api.tzkt.io/v1";
const OBJKT = "https://data.objkt.com/v3/graphql";
const HEN_MINTER = "KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9";

// Marketplaces that sell OBJKTs. Escrow markets take custody of a listed
// edition, so a transfer into one is a listing, not a sale.
export const MARKETS = {
  KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X: { name: "objkt", escrow: false }, // v6.2, the current one
  KT1Xjap1TwmDR1d8yEd8ErkraAj2mbdMrPZY: { name: "objkt", escrow: false }, // v4
  KT1CePTyk6fk4cFr6fasY5YXPGks6ttjSLp4: { name: "objkt", escrow: false }, // v3
  KT1WvzYHCNBvDSdwafTHv7nJ1dWmZ8GCYuuC: { name: "objkt", escrow: false }, // v2
  KT1FvqJwEDWb1Gwc55Jd1jjTHRVWbYKUUpyq: { name: "objkt", escrow: true }, // the old objkt.bid
  KT1PHubm9HtyQEJ4BBpMTVomq6mhbfNZ9z5w: { name: "teia", escrow: true },
  KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn: { name: "hic et nunc", escrow: true },
};
const OURS = new Set(Object.values(TEZOS));
const BURN = "tz1burnburnburnburnburnburnburjAYjjX";
const WALLET_NAME = Object.fromEntries(Object.entries(TEZOS).map(([name, address]) => [address, name]));

const get = async (url, opts = {}) => {
  const r = await fetch(url, { ...opts, signal: AbortSignal.timeout(opts.timeout ?? 45_000) });
  if (!r.ok) throw new Error(`${r.status} ${url.split("?")[0]}`);
  return r.json();
};
const objkt = async (query) => {
  const j = await get(OBJKT, { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify({ query }) });
  if (j.errors) throw new Error(`objkt: ${j.errors[0]?.message}`);
  return j.data;
};
const settle = async (p) => { try { return await p; } catch (e) { return { error: String(e.message || e) }; } };
const round = (n, d = 4) => Math.round(n * 10 ** d) / 10 ** d;
const tez = (mutez) => Number(mutez) / 1e6;
const short = (a) => a?.alias || (a?.address ? `${a.address.slice(0, 8)}…` : null);
const pct = (now, then) => (then ? round(((now - then) / then) * 100, 1) : null);

/** Run thunks a few at a time: TzKT is free and rate-limited. */
async function pool(thunks, size = 6) {
  const out = new Array(thunks.length);
  let next = 0;
  await Promise.all(Array.from({ length: size }, async () => {
    while (next < thunks.length) { const i = next++; out[i] = await settle(thunks[i]()); }
  }));
  return out;
}

async function xtzUsd() {
  const j = await settle(get("https://api.coingecko.com/api/v3/simple/price?ids=tezos&vs_currencies=usd"));
  return j.tezos?.usd ?? null;
}

// ⛓️ The chain

/**
 * Vitals and a month-by-month activity table. Monthly counts are contract
 * calls, read straight from the chain: all transactions, objkt purchases
 * (fulfill_ask on the two live marketplace versions), Teia collects and
 * hic et nunc mints (which is also how Teia mints).
 */
export async function chain({ months = 12 } = {}) {
  months = Math.min(36, Math.max(1, Number(months) || 12));
  const [stats, head, protocol, period, chart] = await Promise.all([
    get(`${TZKT}/statistics/current`), get(`${TZKT}/head`), settle(get(`${TZKT}/protocols/current`)),
    settle(get(`${TZKT}/voting/periods/current`)),
    settle(get("https://api.coingecko.com/api/v3/coins/tezos/market_chart?vs_currency=usd&days=365&interval=daily")),
  ]);

  const now = new Date();
  const monthStart = (back) => new Date(Date.UTC(now.getUTCFullYear(), now.getUTCMonth() - back, 1));
  const spans = Array.from({ length: months + 1 }, (_, i) => {
    const from = monthStart(months - i), to = monthStart(months - i - 1);
    return { month: from.toISOString().slice(0, 7), from: from.toISOString(), to: to.toISOString(), partial: to > now };
  });
  const count = (span, q) => () => get(`${TZKT}/operations/transactions/count?${q}&timestamp.ge=${span.from}&timestamp.lt=${span.to}`);
  const series = {
    transactions: "status=applied",
    objktV62: "target=KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X&entrypoint=fulfill_ask&status=applied",
    objktV4: "target=KT1Xjap1TwmDR1d8yEd8ErkraAj2mbdMrPZY&entrypoint=fulfill_ask&status=applied",
    teiaCollects: "target=KT1PHubm9HtyQEJ4BBpMTVomq6mhbfNZ9z5w&entrypoint=collect&status=applied",
    henMints: `target=${HEN_MINTER}&entrypoint=mint_OBJKT&status=applied`,
  };
  const keys = Object.keys(series);
  const counts = await pool(spans.flatMap((span) => keys.map((k) => count(span, series[k]))));
  const num = (v) => (typeof v === "number" ? v : null);
  const monthly = spans.map((span, i) => {
    const c = Object.fromEntries(keys.map((k, j) => [k, num(counts[i * keys.length + j])]));
    return {
      month: span.month, ...(span.partial ? { partial: true } : {}),
      transactions: c.transactions,
      objktPurchases: c.objktV62 === null && c.objktV4 === null ? null : (c.objktV62 || 0) + (c.objktV4 || 0),
      teiaCollects: c.teiaCollects, henMints: c.henMints,
    };
  });
  // Year over year compares the last complete month with the oldest one in
  // the table, so it is only a true year at the default twelve months.
  const complete = monthly.filter((m) => !m.partial);
  const latest = complete.at(-1), oldest = complete[0];
  const change = (k) => (latest && oldest && latest !== oldest ? pct(latest[k], oldest[k]) : null);

  const prices = chart.prices?.map(([, p]) => p) || [];
  const price = prices.length ? {
    usd: round(prices.at(-1), 4),
    change30dPct: pct(prices.at(-1), prices.at(-31)),
    change365dPct: pct(prices.at(-1), prices[0]),
    high365d: round(Math.max(...prices), 4), low365d: round(Math.min(...prices), 4),
  } : { error: chart.error || "no price history" };

  const supply = tez(stats.totalSupply);
  return {
    head: { level: head.level, cycle: head.cycle, timestamp: head.timestamp, synced: head.synced },
    protocol: protocol.error ? protocol : { number: protocol.code, hash: protocol.hash, upgradePending: head.nextProtocol !== head.protocol },
    governance: period.error ? period : { period: period.kind, status: period.status, ends: period.endTime },
    price,
    marketCapUsd: price.usd ? Math.round(tez(stats.circulatingSupply) * price.usd) : null,
    supplyXtz: Math.round(supply),
    stakedPct: round(((Number(stats.totalOwnStaked) + Number(stats.totalExternalStaked)) / Number(stats.totalSupply)) * 100, 1),
    bakers: stats.totalBakers, stakers: stats.totalStakers, delegators: stats.totalDelegators,
    monthly,
    compared: latest && oldest && latest !== oldest ? {
      from: oldest.month, to: latest.month,
      transactionsPct: change("transactions"), objktPurchasesPct: change("objktPurchases"),
      teiaCollectsPct: change("teiaCollects"), henMintsPct: change("henMints"),
    } : null,
  };
}

// 📈 The art market

/**
 * Sales on objkt's indexer (which also carries Teia and hic et nunc swaps)
 * over the last N days. Everything is valued in tez through price_xtz: a
 * sale's raw `price` is in the listing's own currency, and a listing priced
 * in a token with eight decimals reads as billions of tez.
 */
export async function market({ days = 7, top = 15 } = {}) {
  days = Number(days);
  if (!(days > 0 && days <= 30)) throw new Error("days must be 1..30");
  const since = new Date(Date.now() - days * 864e5).toISOString();
  const fields = "price price_xtz timestamp ophash buyer_address seller_address token{ token_id name creators{creator_address holder{alias}} fa{name contract} }";
  const sales = [];
  for (const table of ["listing_sale", "offer_sale"]) {
    for (let offset = 0; offset < 40_000; offset += 500) {
      const batch = (await objkt(`{ ${table}(where:{timestamp:{_gte:"${since}"}}, order_by:{id:asc}, limit:500, offset:${offset}){ ${fields} } }`))[table];
      sales.push(...batch);
      if (batch.length < 500) break;
    }
  }
  // A sale the indexer could not value in tez is counted, not guessed at.
  const unpriced = sales.filter((s) => s.price_xtz === null || s.price_xtz === undefined);
  const priced = sales.filter((s) => !unpriced.includes(s));
  const xtz = (s) => tez(s.price_xtz);

  // Wash filter: a creator whose repeated sales (3+, 500+ XTZ together) all
  // go to one buyer, or a sale where buyer and seller are the same wallet. A
  // collector buying a few cheap pieces from one artist is not wash.
  const byCreator = {};
  for (const s of priced) {
    const c = s.token?.creators?.[0]?.creator_address;
    if (!c) continue;
    (byCreator[c] ??= new Set()).add(s.buyer_address);
    byCreator[c].n = (byCreator[c].n || 0) + 1;
    byCreator[c].xtz = (byCreator[c].xtz || 0) + xtz(s);
  }
  const wash = new Set(Object.entries(byCreator).filter(([, b]) => b.size === 1 && b.n >= 3 && b.xtz >= 500).map(([c]) => c));
  const isWash = (s) => s.buyer_address === s.seller_address || wash.has(s.token?.creators?.[0]?.creator_address);
  const clean = priced.filter((s) => !isWash(s));
  const washed = priced.filter(isWash);
  const sum = (list) => list.reduce((a, s) => a + xtz(s), 0);

  const bands = [[0, 1], [1, 5], [5, 20], [20, 100], [100, 1000], [1000, Infinity]].map(([lo, hi]) => {
    const x = clean.filter((s) => xtz(s) >= lo && xtz(s) < hi);
    return { xtz: hi === Infinity ? `${lo}+` : `${lo}-${hi}`, sales: x.length, volume: round(sum(x), 0) };
  });
  const rank = (key) => {
    const m = {};
    for (const s of clean) {
      const k = key(s);
      if (!k) continue;
      const g = (m[k] ??= { sales: 0, volume: 0, buyers: new Set() });
      g.sales++; g.volume += xtz(s); g.buyers.add(s.buyer_address);
    }
    return Object.entries(m).sort((a, b) => b[1].volume - a[1].volume).slice(0, top)
      .map(([name, g]) => ({ name, sales: g.sales, volume: round(g.volume, 0), buyers: g.buyers.size, avg: round(g.volume / g.sales, 1) }));
  };
  const mine = {};
  for (const s of clean) {
    if (!s.token?.creators?.some((c) => OURS.has(c.creator_address))) continue;
    const g = (mine[s.token.fa?.name || s.token.fa?.contract] ??= { sales: 0, volume: 0 });
    g.sales++; g.volume = round(g.volume + xtz(s), 2);
  }
  // Weeks count back from now, so the newest bucket is always a full week.
  const weeks = {};
  for (const s of clean) {
    const ago = Math.floor((Date.now() - Date.parse(s.timestamp)) / (7 * 864e5));
    const w = (weeks[ago] ??= { sales: 0, volume: 0, buyers: new Set() });
    w.sales++; w.volume += xtz(s); w.buyers.add(s.buyer_address);
  }
  // Paid in something other than tez: the raw price differs from its tez value.
  const otherCurrency = clean.filter((s) => Number(s.price) !== Number(s.price_xtz));
  const volume = sum(clean);
  const usd = await xtzUsd();
  return {
    days, sales: clean.length, volumeXtz: round(volume, 0), volumeUsd: usd && round(volume * usd, 0),
    buyers: new Set(clean.map((s) => s.buyer_address)).size,
    excludedAsWash: { sales: washed.length, volumeXtz: round(sum(washed), 0) },
    unpriced: unpriced.length,
    paidInOtherTokens: {
      sales: otherCurrency.length, volumeXtz: round(sum(otherCurrency), 0),
      largest: [...otherCurrency].sort((a, b) => xtz(b) - xtz(a)).slice(0, 5).map((s) => ({ xtz: round(xtz(s), 2), token: s.token?.name, op: s.ophash })),
    },
    weekly: Object.entries(weeks).sort((a, b) => b[0] - a[0]).filter(([ago]) => (Number(ago) + 1) * 7 <= days)
      .map(([ago, w]) => ({ weeksAgo: Number(ago), sales: w.sales, volumeXtz: round(w.volume, 0), buyers: w.buyers.size })),
    priceBands: bands,
    topCollections: rank((s) => s.token?.fa?.name || s.token?.fa?.contract),
    topCreators: rank((s) => { const c = s.token?.creators?.[0]; return c && (c.holder?.alias || c.creator_address); }),
    topSales: [...clean].sort((a, b) => xtz(b) - xtz(a)).slice(0, 10).map((s) => ({
      xtz: round(xtz(s), 2), token: s.token?.name, collection: s.token?.fa?.name,
      creator: s.token?.creators?.[0]?.holder?.alias || s.token?.creators?.[0]?.creator_address,
    })),
    ourSalesByCollection: mine,
  };
}

// 🎟️ Our tokens

/**
 * Every hic et nunc / Teia OBJKT an AC wallet minted since a date, and what
 * became of each edition: held, listed (in a market's escrow or as an objkt
 * ask), or sold — to whom, where, for how much and how many days it took.
 */
export async function tokens({ limit = 60, since = "2026-09-26" } = {}) {
  limit = Math.min(500, Math.max(1, Number(limit) || 60));
  const minted = await get(`${TZKT}/tokens?contract=${HEN}&firstMinter.in=${[...OURS].join(",")}&firstTime.ge=${since}&sort.desc=id&limit=${limit}&select=tokenId,metadata.name%20as%20name,totalSupply,firstTime`);
  if (!minted.length) return { since, tokens: [], summary: { minted: 0 } };
  const ids = minted.map((t) => t.tokenId);
  const chunks = [];
  for (let i = 0; i < ids.length; i += 50) chunks.push(ids.slice(i, i + 50));
  const transfers = (await Promise.all(chunks.map((c) => get(`${TZKT}/tokens/transfers?token.contract=${HEN}&token.tokenId.in=${c.join(",")}&sort.asc=id&limit=5000&select=token.tokenId%20as%20id,from,to,amount,timestamp`)))).flat();
  const idList = (c) => c.map((i) => `"${i}"`).join(",");
  const where = (c) => `where:{token:{fa_contract:{_eq:"${HEN}"}, token_id:{_in:[${idList(c)}]}}}`;
  const indexed = await settle(Promise.all(chunks.map((c) => objkt(`{
    listing_sale(${where(c)}, limit:2000){ price_xtz timestamp marketplace_contract buyer_address seller_address token{token_id} }
    offer_sale(${where(c)}, limit:2000){ price_xtz timestamp marketplace_contract buyer_address seller_address token{token_id} }
    token(where:{fa_contract:{_eq:"${HEN}"}, token_id:{_in:[${idList(c)}]}}){ token_id listings_active{ price_xtz amount_left marketplace_contract seller_address } }
  }`))));
  const sales = indexed.error ? [] : indexed.flatMap((d) => [...d.listing_sale, ...d.offer_sale]);
  const listings = indexed.error ? {} : Object.fromEntries(indexed.flatMap((d) => d.token).map((t) => [t.token_id, t.listings_active.filter((l) => OURS.has(l.seller_address))]));

  const rows = minted.map((t) => {
    const moves = transfers.filter((x) => x.id === t.tokenId);
    // A burn leaves the supply figure alone, so burned editions come off here.
    const burned = moves.filter((x) => x.to?.address === BURN && OURS.has(x.from?.address)).reduce((a, x) => a + Number(x.amount), 0);
    const editions = Number(t.totalSupply) - burned;
    // An edition is sold once it reaches a wallet that is neither ours nor a
    // market, having come from us or out of a market's escrow.
    const out = moves.filter((x) => x.to && !OURS.has(x.to.address) && !MARKETS[x.to.address] && x.to.address !== BURN && (OURS.has(x.from?.address) || MARKETS[x.from?.address]));
    const sold = Math.min(editions, out.reduce((a, x) => a + Number(x.amount), 0));
    const primary = sales.filter((s) => s.token.token_id === t.tokenId && OURS.has(s.seller_address)).sort((a, b) => a.timestamp.localeCompare(b.timestamp));
    const resales = sales.filter((s) => s.token.token_id === t.tokenId && !OURS.has(s.seller_address));
    const open = listings[t.tokenId] || [];
    const first = out[0];
    const row = {
      objkt: Number(t.tokenId), name: t.name, minted: t.firstTime, editions, sold,
      ...(burned ? { burned } : {}),
      status: !editions ? "burned" : sold >= editions ? "sold" : open.length ? "listed" : sold ? "part sold" : "held",
    };
    if (open.length) row.listed = open.map((l) => ({ on: MARKETS[l.marketplace_contract]?.name || l.marketplace_contract, xtz: tez(l.price_xtz), left: l.amount_left }));
    if (first) {
      row.buyers = [...new Set(out.map((x) => short(x.to)))];
      row.soldAt = first.timestamp;
      row.daysToSell = round((Date.parse(first.timestamp) - Date.parse(t.firstTime)) / 864e5, 1);
      if (primary.length) {
        row.soldOn = MARKETS[primary[0].marketplace_contract]?.name || primary[0].marketplace_contract;
        row.xtz = round(primary.reduce((a, s) => a + tez(s.price_xtz), 0), 2);
      }
    }
    if (resales.length) row.resales = { count: resales.length, xtz: round(resales.reduce((a, s) => a + tez(s.price_xtz), 0), 2) };
    row.url = `https://teia.art/objkt/${t.tokenId}`;
    return row;
  });

  // Sell-through by the week a token was minted, newest week first.
  const weekOf = (iso) => { const d = new Date(iso); d.setUTCDate(d.getUTCDate() - ((d.getUTCDay() + 6) % 7)); return d.toISOString().slice(0, 10); };
  const weeks = {};
  for (const r of rows) {
    const w = (weeks[weekOf(r.minted)] ??= { minted: 0, sold: 0 });
    w.minted += r.editions; w.sold += r.sold;
  }
  const collectors = {};
  for (const r of rows) for (const b of r.buyers || []) {
    const c = (collectors[b] ??= { tokens: 0, lastBought: "" });
    c.tokens++; if (r.soldAt > c.lastBought) c.lastBought = r.soldAt;
  }
  const soldRows = rows.filter((r) => r.soldAt).sort((a, b) => b.soldAt.localeCompare(a.soldAt));
  const lastSale = soldRows[0]?.soldAt || null;
  const days = soldRows.map((r) => r.daysToSell).sort((a, b) => a - b);
  const editions = rows.reduce((a, r) => a + r.editions, 0), sold = rows.reduce((a, r) => a + r.sold, 0);
  return {
    since,
    summary: {
      minted: rows.length, editions, sold, unsold: editions - sold, sellThroughPct: editions ? round((sold / editions) * 100, 1) : null,
      lastSale, daysSinceLastSale: lastSale ? round((Date.now() - Date.parse(lastSale)) / 864e5, 1) : null,
      mintedSinceLastSale: lastSale ? rows.filter((r) => r.minted > lastSale).length : rows.length,
      medianDaysToSell: days.length ? days[Math.floor(days.length / 2)] : null,
      soldOn: soldRows.reduce((m, r) => { const k = r.soldOn || "direct or unknown"; m[k] = (m[k] || 0) + 1; return m; }, {}),
      ...(indexed.error ? { objktIndexer: indexed.error } : {}),
    },
    byMintWeek: Object.entries(weeks).sort((a, b) => b[0].localeCompare(a[0])).map(([week, w]) => ({ week, ...w, sellThroughPct: w.minted ? round((w.sold / w.minted) * 100, 0) : null })),
    collectors: Object.entries(collectors).sort((a, b) => b[1].tokens - a[1].tokens).map(([who, c]) => ({ who, ...c })),
    tokens: rows,
  };
}

// 🔎 One operation, read plainly

/**
 * What actually moved in an operation: tez between accounts and every token
 * transfer with its decimals applied. This is how a sale "for 35 billion"
 * turns out to be 8 tez routed through a swap into a memecoin.
 */
export async function trace({ op } = {}) {
  if (!/^o[1-9A-HJ-NP-Za-km-z]{50}$/.test(op || "")) throw new Error("op must be an operation hash (o…)");
  const txs = await get(`${TZKT}/operations/transactions/${op}`);
  if (!txs.length) throw new Error(`no transactions under ${op}`);
  const moved = await get(`${TZKT}/tokens/transfers?transactionId.in=${txs.map((t) => t.id).join(",")}&limit=500`);
  const scaled = (r) => {
    const d = Number(r.token.metadata?.decimals || 0);
    return d ? round(Number(r.amount) / 10 ** d, Math.min(d, 8)) : Number(r.amount);
  };
  const initiator = txs[0].sender;
  const calls = txs.map((t) => ({ from: short(t.sender), to: short(t.target), call: t.parameter?.entrypoint || "transfer", ...(t.amount ? { xtz: tez(t.amount) } : {}), ...(t.status !== "applied" ? { status: t.status } : {}) }));
  return {
    op, timestamp: txs[0].timestamp, status: txs.every((t) => t.status === "applied") ? "applied" : "failed",
    initiator: short(initiator),
    xtzSpentByInitiator: round(txs.filter((t) => t.sender.address === initiator.address).reduce((a, t) => a + tez(t.amount), 0), 6),
    feesXtz: round(txs.reduce((a, t) => a + tez((t.bakerFee || 0) + (t.storageFee || 0) + (t.allocationFee || 0)), 0), 6),
    markets: [...new Set(txs.map((t) => MARKETS[t.target?.address] && `${MARKETS[t.target.address].name} (${t.parameter?.entrypoint})`).filter(Boolean))],
    tokensMoved: moved.map((r) => ({
      token: r.token.metadata?.symbol?.trim() || r.token.metadata?.name || short(r.token.contract), id: r.token.tokenId,
      contract: short(r.token.contract), amount: scaled(r), from: r.from ? short(r.from) : "mint", to: r.to ? short(r.to) : "burn",
    })),
    calls: calls.length > 40 ? [...calls.slice(0, 40), { more: calls.length - 40 }] : calls,
  };
}

// 🧾 Does it pay for itself

/**
 * What Tezos earned AC and what it cost over a window: tez in (sales,
 * royalties, Keeps fees, direct payments), fees and storage burned by our
 * wallets, tez sent out to anyone else, and the sell-through of what was
 * minted in the window.
 */
export async function ledger({ days = 30 } = {}) {
  days = Math.min(365, Math.max(1, Number(days) || 30));
  const since = new Date(Date.now() - days * 864e5).toISOString();
  const [income, usd, minted, ...sent] = await Promise.all([
    settle(tezosRevenue({ hours: days * 24 })), xtzUsd(), settle(tokens({ limit: 500, since: since.slice(0, 10) })),
    ...Object.values(TEZOS).map((a) => get(`${TZKT}/operations/transactions?sender=${a}&timestamp.ge=${since}&limit=10000&select=sender,target,amount,bakerFee,storageFee,allocationFee,status,parameter`)),
  ]);
  const ours = new Set([...OURS, KEEPS]);
  const spend = {}, out = {};
  let fees = 0, burned = 0, paidOut = 0, ops = 0;
  for (const t of sent.flat()) {
    ops++;
    fees += tez(t.bakerFee || 0);
    burned += tez((t.storageFee || 0) + (t.allocationFee || 0));
    const w = (spend[WALLET_NAME[t.sender.address]] ??= { operations: 0, xtz: 0 });
    w.operations++; w.xtz += tez((t.bakerFee || 0) + (t.storageFee || 0) + (t.allocationFee || 0));
    if (t.status === "applied" && Number(t.amount) > 0 && !ours.has(t.target?.address)) {
      paidOut += tez(t.amount);
      const who = short(t.target);
      out[who] = round((out[who] || 0) + tez(t.amount), 4);
    }
  }
  const cost = fees + burned;
  const earned = income.error ? null : income.xtz;
  const inUsd = (x) => (usd && x !== null ? round(x * usd, 2) : null);
  const s = minted.summary;
  return {
    days, since, usdPerTez: usd,
    earned: income.error ? income : { xtz: earned, usd: inUsd(earned), payments: income.payments, bySource: income.bySource, topBuyers: income.topBuyers },
    cost: { xtz: round(cost, 4), usd: inUsd(cost), bakerFeesXtz: round(fees, 4), storageBurnXtz: round(burned, 4), operations: ops,
      byWallet: Object.fromEntries(Object.entries(spend).map(([k, v]) => [k, { operations: v.operations, xtz: round(v.xtz, 4) }])) },
    net: earned === null ? null : { xtz: round(earned - cost, 4), usd: inUsd(earned - cost) },
    sentOut: { xtz: round(paidOut, 4), usd: inUsd(paidOut), to: Object.fromEntries(Object.entries(out).sort((a, b) => b[1] - a[1]).slice(0, 10)) },
    minted: minted.error ? minted : {
      tokens: s.minted, editions: s.editions || 0, sold: s.sold || 0, sellThroughPct: s.sellThroughPct ?? null,
      costPerMintXtz: s.minted ? round(cost / s.minted, 4) : null,
      daysSinceLastSale: s.daysSinceLastSale ?? null, mintedSinceLastSale: s.mintedSinceLastSale ?? null, byMintWeek: minted.byMintWeek,
    },
  };
}
