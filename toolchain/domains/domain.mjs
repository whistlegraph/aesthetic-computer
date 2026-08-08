#!/usr/bin/env node
// domain.mjs — buy and wire up vanity domains for Aesthetic Computer pieces.
//
// A vanity domain (notepat.com, laklok.com, prompt.ac) is just two things:
//   1. DNS pointing the apex at lith (209.38.133.33)
//   2. A host block in lith/Caddyfile that rewrites "/" to the piece
//
// This tool owns buying the domain via Porkbun, adopting it into Cloudflare,
// adding hosts to it, and printing a ready-to-paste Caddy block. The registrar
// is only needed twice: once to buy, once to hand its nameservers to Cloudflare.
//
// Usage:
//   node toolchain/domains/domain.mjs ping                 test API keys
//   node toolchain/domains/domain.mjs balance              account credit
//   node toolchain/domains/domain.mjs price .games .com    price some TLDs
//   node toolchain/domains/domain.mjs check a.com b.games  availability + price
//   node toolchain/domains/domain.mjs buy nom.games        register (prompts)
//   node toolchain/domains/domain.mjs buy nom.games --yes  register (no prompt)
//   node toolchain/domains/domain.mjs caddy nom.games nom  print the Caddy block
//   node toolchain/domains/domain.mjs cf adopt nom.games   create the CF zone
//   node toolchain/domains/domain.mjs cf add nom.games api add a host to it
//   node toolchain/domains/domain.mjs cf list nom.games    show the CF records
//   node toolchain/domains/domain.mjs ns nom.games         registrar nameservers
//   node toolchain/domains/domain.mjs ns nom.games a.ns b.ns   repoint them
//   node toolchain/domains/domain.mjs dns nom.games        registrar DNS records
//
// Keys: PORKBUN_API_KEY / PORKBUN_SECRET_API_KEY for the registrar (env or
// vault .env), CLOUDFLARE_EMAIL / CLOUDFLARE_API_KEY for DNS (env or one of the
// vault env files — see cloudflare.mjs).

import { createInterface } from "node:readline/promises";
import * as cf from "./cloudflare.mjs";
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

// Registration spends account credit. Read it in dollars so callers can compare
// it against a price directly.
async function funds() {
  const r = await pb.balance();
  return Number(r.balance) / 100;
}

async function cmdBalance() {
  const dollars = await funds();
  console.log(`💰 Porkbun balance: ${money(dollars)}`);
  if (dollars < 10) {
    console.log("   Low — top up at https://porkbun.com/account/balance");
  }
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
  // Track credit as we spend it, so a batch stops at the wall instead of
  // failing mid-way with Porkbun's terse "No funds".
  let credit = await funds();
  for (const domain of domains) {
    const r = await pb.check(domain);
    const a = r.response || {};
    if (a.avail !== "yes") { console.log(`🔴 ${domain} is not available — skipping.`); continue; }
    const price = Number(a.price ?? a.firstYearPromo ?? 0);
    if (price > credit) {
      console.log(
        `💸 ${domain} costs ${money(price)} but the balance is ${money(credit)}.` +
          ` Top up at https://porkbun.com/account/balance`,
      );
      continue;
    }
    const ok = await confirm(`Register ${domain} for ${money(price)}? [y/N]`);
    if (!ok) { console.log(`Skipped ${domain}.`); continue; }
    const cents = Math.round(price * 100);
    const result = await pb.register(domain, cents);
    credit -= price;
    console.log(`🎉 Registered ${domain}. ${result.message || ""}`);
    console.log(`   Balance now ${money(credit)}.`);
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

// --- registrar-side DNS, for domains still on Porkbun's nameservers ---

async function cmdNs([domain, ...nameservers]) {
  if (!domain) return console.error("Usage: ns <domain> [ns1 ns2 …]");
  if (!nameservers.length) {
    const { ns } = await pb.getNs(domain);
    console.log(`${domain} nameservers:`);
    for (const server of ns) console.log(`   ${server}`);
    return;
  }
  const ok = await confirm(
    `Repoint ${domain} at ${nameservers.join(", ")}? This moves DNS. [y/N]`);
  if (!ok) return console.log("Left alone.");
  await pb.updateNs(domain, nameservers);
  console.log(`✅ ${domain} now points at ${nameservers.join(", ")}.`);
  console.log("   Propagation is usually minutes; the old zone keeps answering" +
    " until resolvers catch up.");
}

async function cmdDns([domain, action, ...rest]) {
  if (!domain) return console.error(
    "Usage: dns <domain> | dns <domain> add <type> <host> <answer> [ttl]");
  if (!action) {
    const { records } = await pb.dnsRetrieve(domain);
    for (const r of records) {
      console.log(`${String(r.id).padStart(12)}  ${r.type.padEnd(6)} ` +
        `${(r.name || "@").padEnd(28)} ${r.content}`);
    }
    if (!records.length) console.log("(no records)");
    return;
  }
  if (action === "add") {
    const [type, host, answer, ttl = "600"] = rest;
    if (!type || !host || !answer) return console.error(
      "Usage: dns <domain> add <type> <host> <answer> [ttl]");
    // Porkbun wants the label only, and an empty name for the apex.
    const name = host === "@" ? "" : host;
    await pb.dnsCreate(domain, { type: type.toUpperCase(), name,
      content: answer, ttl });
    console.log(`✅ ${type.toUpperCase()} ${host}.${domain} → ${answer}`);
    return;
  }
  console.error(`Unknown dns action "${action}".`);
}

// --- Cloudflare, where every AC domain ends up ---

async function cmdCf([action, domain, ...rest]) {
  if (action === "adopt") {
    if (!domain) return console.error("Usage: cf adopt <domain> [host…]");
    const zone = await cf.createZone(domain);
    console.log(`zone ${zone.id} · status ${zone.status}`);
    // The apex and www are what every AC domain wants; extra hosts are extra.
    for (const host of [domain, "www", ...rest]) {
      try {
        await cf.createRecord(zone.id, host, LITH_IP);
        console.log(`  A ${(host === domain ? "@" : host).padEnd(6)} → ${LITH_IP}`);
      } catch (error) {
        console.log(`  A ${(host === domain ? "@" : host).padEnd(6)} ${error.message}`);
      }
    }
    console.log("\nPoint the registrar's nameservers at:");
    for (const server of zone.name_servers || []) console.log(`   ${server}`);
    console.log(`\n   npm run domain ns ${domain} ` +
      `${(zone.name_servers || []).join(" ")}`);
    console.log("Nothing changes until that lands.");
    return;
  }
  if (action === "add") {
    const [host] = rest;
    if (!domain || !host) return console.error("Usage: cf add <domain> <host>");
    const zone = await cf.zone(domain);
    if (!zone) return console.error(`${domain} is not a Cloudflare zone yet` +
      ` — try: npm run domain cf adopt ${domain}`);
    await cf.createRecord(zone.id, host, LITH_IP);
    console.log(`✅ A ${host}.${domain} → ${LITH_IP} (dns-only)`);
    console.log("   Add the host to its Caddyfile block, then deploy. Caddy" +
      " gets the certificate on first request.");
    return;
  }
  if (action === "list") {
    if (!domain) return console.error("Usage: cf list <domain>");
    const zone = await cf.zone(domain);
    if (!zone) return console.error(`${domain} is not a Cloudflare zone.`);
    console.log(`${domain} · zone ${zone.id} · ${zone.status}`);
    for (const r of await cf.records(zone.id)) {
      console.log(`  ${r.type.padEnd(6)} ${r.name.padEnd(30)} ${r.content}` +
        `${r.proxied ? "  (proxied)" : ""}`);
    }
    return;
  }
  console.error("Usage: cf adopt <domain> [host…] | cf add <domain> <host> | " +
    "cf list <domain>");
}

const commands = {
  ping: cmdPing,
  balance: cmdBalance,
  price: () => cmdPrice(args),
  check: () => cmdCheck(args),
  buy: () => cmdBuy(args),
  caddy: () => cmdCaddy(args),
  ns: () => cmdNs(args),
  dns: () => cmdDns(args),
  cf: () => cmdCf(args),
};

const run = commands[cmd];
if (!run) {
  console.error(
    "Commands: ping | balance | price | check | buy | caddy | ns | dns | cf");
  process.exit(1);
}
Promise.resolve(run()).catch((e) => {
  console.error("❌ " + e.message);
  process.exit(1);
});
