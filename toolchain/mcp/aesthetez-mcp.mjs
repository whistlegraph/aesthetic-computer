#!/usr/bin/env node
// aesthetez-mcp.mjs — tracking the Tezos chain for AC.
//
// Five read-only tools over public data (TzKT, the objkt indexer, CoinGecko):
// the chain's vitals and monthly activity, the art market valued in tez, what
// became of every token AC minted, a plain reading of any one operation, and
// a ledger of what Tezos earned and cost. Together they answer whether the
// chain is still worth minting on. The work is in aesthetez.mjs.
//
// Wallets, transfers and the nightly mint stay in crypto-mcp.

import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";
import { chain, market, tokens, trace, ledger } from "./aesthetez.mjs";

const TOOLS = [
  {
    name: "aesthetez_chain",
    description: "Is Tezos alive: head, protocol and governance period, XTZ price with 30-day and one-year change, market cap, share of supply staked, bakers, stakers and delegators, and a month-by-month table of all transactions, objkt purchases, Teia collects and hic et nunc mints, with the change across the table. The transaction total swings with single bots (one wallet on a tzBTC pool multiplied October 2026 sevenfold); the art columns are the steadier reading. Read-only.",
    inputSchema: { type: "object", properties: { months: { type: "number", description: "Complete months of history (default 12, max 36)" } } },
  },
  {
    name: "aesthetez_market",
    description: "The Tezos art market over the last N days from objkt's indexer (which also carries Teia and hic et nunc swaps), every sale valued in tez: volume, buyers, week-by-week trend, price bands, top collections, creators and sales, sales paid in other tokens, and sales of AC's own work. Suspected wash trades are excluded and counted separately. Read-only.",
    inputSchema: {
      type: "object",
      properties: {
        days: { type: "number", description: "Window in days (default 7, max 30)" },
        top: { type: "number", description: "Rows per ranking (default 15)" },
      },
    },
  },
  {
    name: "aesthetez_tokens",
    description: "Every hic et nunc / Teia OBJKT an AC wallet minted since a date and what became of it: held, listed (and on which market, at what price) or sold (to whom, where, for how much, in how many days), with sell-through by mint week, the collectors, and how long since the last sale. A token sitting in a marketplace's escrow is listed, not sold. Read-only.",
    inputSchema: {
      type: "object",
      properties: {
        limit: { type: "number", description: "Most recent tokens to read (default 60, max 500)" },
        since: { type: "string", description: "Earliest mint date, YYYY-MM-DD (default 2026-09-26, the first daily token)" },
      },
    },
  },
  {
    name: "aesthetez_trace",
    description: "Read one Tezos operation plainly: who started it, the tez they spent, fees, which marketplace calls it made, and every token that moved with its decimals applied. Use it on a sale that looks wrong — a listing priced in another token reads as a huge number until its tez value is traced. Read-only.",
    inputSchema: { type: "object", properties: { op: { type: "string", description: "Operation hash (o…)" } }, required: ["op"] },
  },
  {
    name: "aesthetez_ledger",
    description: "Does Tezos pay for itself: over the last N days, tez that reached AC's wallets by source, baker fees and storage burned by AC's wallets, the net in XTZ and USD, tez sent out and to whom, and the sell-through and cost per mint of what was minted in the window. Read-only.",
    inputSchema: { type: "object", properties: { days: { type: "number", description: "Window in days (default 30, max 365)" } } },
  },
];

const RUN = { aesthetez_chain: chain, aesthetez_market: market, aesthetez_tokens: tokens, aesthetez_trace: trace, aesthetez_ledger: ledger };

async function callTool(name, args = {}) {
  const run = RUN[name];
  if (!run) throw new Error(`unknown tool ${name}`);
  return [{ type: "text", text: JSON.stringify(await run(args), null, 2) }];
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: params?.protocolVersion || "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "aesthetez-mcp", version: "1.0.0" },
            instructions: "Tracks the Tezos chain for Aesthetic Computer. aesthetez_chain and aesthetez_market read the chain and its art market; aesthetez_tokens and aesthetez_ledger read AC's own tokens and whether minting pays; aesthetez_trace explains a single operation. All read-only. A market total that looks impossible is usually a sale priced in another token: trace its op.",
          },
        };
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "ꜩ aesthetez-mcp shared daemon" });
else serveStdio({ handleMessage, banner: `ꜩ aesthetez-mcp started (${Object.keys(RUN).join(", ")})` });
