#!/usr/bin/env node
// domain.mjs — buy and wire up vanity domains for Aesthetic Computer pieces.
//
// A vanity domain (notepat.com, laklok.com, prompt.ac) is just two things:
//   1. DNS pointing the apex at lith (209.38.133.33)
//   2. A host block in lith/Caddyfile that rewrites "/" to the piece
//
// This tool owns step 0 (buying the domain via Porkbun) and hands you a
// ready-to-paste Caddy block for the rest. Cloudflare DNS automation lands
// once a CF token is wired in.
//
// Usage:
//   node toolchain/domains/domain.mjs ping                 test API keys
//   node toolchain/domains/domain.mjs price .games .com    price some TLDs
//   node toolchain/domains/domain.mjs check a.com b.games  availability + price
//   node toolchain/domains/domain.mjs buy nom.games        register (prompts)
//   node toolchain/domains/domain.mjs buy nom.games --yes  register (no prompt)
//   node toolchain/domains/domain.mjs caddy nom.games nom  print the Caddy block
//
// Keys: PORKBUN_API_KEY / PORKBUN_SECRET_API_KEY (env or vault .env).

import { createInterface } from "node:readline/promises";
import * as pb from "./porkbun.mjs";

const LITH_IP = "209.38.133.33";
const [cmd, ...rest] = process.argv.slice(2);
const flags = new Set(rest.filter((a) => a.startsWith("--")));
const args = rest.filter((a) => !a.startsWith("--"));

const money = (v) => (v == null ? "—" : `$${Number(v).toFixed(2)}`);

async function confirm(question) {
  if (flags.has("--yes")) return true;
  const rl = createInterface({ input: process.stdin, output: process.stdout });
  const answer = await rl.question(question + " ");
  rl.close();
  return /^y(es)?$/i.test(answer.trim());
}

async function cmdPing() {
  const r = await pb.ping();
  console.log(`✅ Porkbun keys valid. Your IP: ${r.yourIp}`);
}

async function cmdPrice(tlds) {
  const { pricing } = await pb.pricing();
  const wanted = tlds.map((t) => t.replace(/^\./, "").toLowerCase());
  const rows = wanted.length ? wanted : Object.keys(pricing).sort();
  for (const tld of rows) {
    const p = pricing[tld];
    if (!p) { console.log(`.${tld}  — not offered`); continue; }
    console.log(
      `.${tld.padEnd(12)} register ${money(p.registration).padStart(8)}` +
        `   renew ${money(p.renewal).padStart(8)}   transfer ${money(p.transfer).padStart(8)}`,
    );
  }
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Porkbun's checkDomain allows ~1 call per 10s. We space calls out and, if we
// still trip the limit, back off and retry once so a batch just completes.
async function checkOne(domain, retried = false) {
  try {
    const r = await pb.check(domain);
    const a = r.response || {};
    const avail = a.avail === "yes";
    const price = a.price ?? a.firstYearPromo ?? null;
    console.log(
      `${avail ? "🟢 available" : "🔴 taken    "}  ${domain.padEnd(22)} ${avail ? money(price) : ""}`,
    );
  } catch (e) {
    if (/within \d+ seconds/i.test(e.message) && !retried) {
      await sleep(11000);
      return checkOne(domain, true);
    }
    console.log(`⚠️  ${domain.padEnd(22)} ${e.message}`);
  }
}

async function cmdCheck(domains) {
  if (!domains.length) return console.error("Usage: check <domain> [domain…]");
  for (let i = 0; i < domains.length; i++) {
    if (i > 0) await sleep(11000); // stay under the 1-check-per-10s cap
    await checkOne(domains[i]);
  }
}

async function cmdBuy(domains) {
  if (!domains.length) return console.error("Usage: buy <domain> [--yes]");
  for (const domain of domains) {
    const r = await pb.check(domain);
    const a = r.response || {};
    if (a.avail !== "yes") { console.log(`🔴 ${domain} is not available — skipping.`); continue; }
    const price = Number(a.price ?? a.firstYearPromo ?? 0);
    const ok = await confirm(`Register ${domain} for ${money(price)}? [y/N]`);
    if (!ok) { console.log(`Skipped ${domain}.`); continue; }
    const cents = Math.round(price * 100);
    const result = await pb.register(domain, cents);
    console.log(`🎉 Registered ${domain}. ${result.message || ""}`);
    console.log(`   Next: point DNS at lith (${LITH_IP}) and add the Caddy block:`);
    printCaddy(domain, domain.split(".")[0]);
  }
}

function printCaddy(domain, piece) {
  const label = piece.replace(/[^a-z0-9]/gi, "");
  console.log(`
    # --- ${domain} ---
    @${label}root host ${domain} www.${domain}
    handle @${label}root {
        @${label}index path /
        handle @${label}index { rewrite * /${piece} }
        reverse_proxy localhost:8888
    }
    # …then add ${domain} www.${domain} to the @mainspa host list.`);
}

function cmdCaddy([domain, piece]) {
  if (!domain || !piece) return console.error("Usage: caddy <domain> <piece>");
  printCaddy(domain, piece);
}

const commands = {
  ping: cmdPing,
  price: () => cmdPrice(args),
  check: () => cmdCheck(args),
  buy: () => cmdBuy(args),
  caddy: () => cmdCaddy(args),
};

const run = commands[cmd];
if (!run) {
  console.error("Commands: ping | price | check | buy | caddy");
  process.exit(1);
}
Promise.resolve(run()).catch((e) => {
  console.error("❌ " + e.message);
  process.exit(1);
});
