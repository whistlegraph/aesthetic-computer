#!/usr/bin/env node
// Ask relational "vectors" of the social graph — the queryable counterpart
// to the static REPORT/broadside. Zero network: it only reads what the pull
// stages already fetched, so what it can answer grows as data lands.
//
// Vectors (jeffrey = the account, default whistlegraph):
//   mutuals            who I follow who ALSO follows me   (following ∩ followers)
//   idols              who I follow who does NOT follow back (following \ followers)
//   fans               who follows me that I don't follow back (followers \ following)
//   list               art-world nodes (filter with --type)
//   affiliations [h]   who runs / works at what (optionally for one handle)
//   also-follows <h>   of MY graph, who also follows <h>     (needs a target pull)
//   who-follows  <h>   everyone who follows <h>              (needs a target pull)
//   node <handle>      everything known about one account
//
// Filters: --art (art-world only) · --type gallery|museum|… · --relation mutual
//          --min-reach N · --limit N · --csv
//
// Target pulls (for also-follows / who-follows <h>) are an explicit, capped,
// account-risky opt-in — NOT run here:
//   bin/ig-social-graph.py whistlegraph --target <h> --cap 4000
// …which writes social/<acct>/targets/<h>.followers.jsonl that this reads.
//
// Usage:
//   node bin/ig-social-query.mjs whistlegraph mutuals --art --type gallery
//   node bin/ig-social-query.mjs whistlegraph also-follows simchowitzgallery --art

import { readFileSync, existsSync } from "fs";
import { join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const REPO = join(HERE, "..", "..", "..");

const argv = process.argv.slice(2);
const account = argv[0] && !argv[0].startsWith("--") ? argv[0] : "whistlegraph";
const verb = argv[1] && !argv[1].startsWith("--") ? argv[1] : "mutuals";
const posArg = argv[2] && !argv[2].startsWith("--") ? argv[2] : null;
const flag = (k) => argv.includes(k);
const opt = (k, d) => {
  const i = argv.indexOf(k);
  return i > -1 ? argv[i + 1] : d;
};
const dir = join(REPO, "portraits/jeffrey/social", account);

function jl(p) {
  const f = join(dir, p);
  if (!existsSync(f)) return [];
  return readFileSync(f, "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => {
      try {
        return JSON.parse(l);
      } catch {
        return null;
      }
    })
    .filter(Boolean);
}
const setOf = (rows) => new Set(rows.map((r) => r.username).filter(Boolean));

const following = jl("following.jsonl");
const followers = jl("followers.jsonl");
const fging = setOf(following);
const fgers = setOf(followers);
const graph = existsSync(join(dir, "graph.json"))
  ? JSON.parse(readFileSync(join(dir, "graph.json"), "utf8"))
  : { nodes: [], edges: [] };
const nodeBy = new Map(graph.nodes.map((n) => [n.id, n]));

function enrich(u) {
  const n = nodeBy.get(u) || {};
  const rel =
    fging.has(u) && fgers.has(u)
      ? "mutual"
      : fging.has(u)
        ? "idol"
        : fgers.has(u)
          ? "fan"
          : "—";
  return {
    handle: u,
    name: n.label || u,
    rel,
    type: n.type || "?",
    art: !!n.art_world,
    reach: n.reach ?? null,
    confirmed: (n.confirmed_by || []).join("+") || "—",
  };
}

function applyFilters(rows) {
  let r = rows;
  if (flag("--art")) r = r.filter((x) => x.art);
  const ty = opt("--type");
  if (ty) r = r.filter((x) => x.type === ty);
  const rel = opt("--relation");
  if (rel) r = r.filter((x) => x.rel === rel);
  const mr = +opt("--min-reach", 0);
  if (mr) r = r.filter((x) => (x.reach || 0) >= mr);
  r.sort((a, b) => (b.reach || 0) - (a.reach || 0) || a.handle.localeCompare(b.handle));
  const lim = +opt("--limit", 0);
  return lim ? r.slice(0, lim) : r;
}

function emit(title, rows) {
  rows = applyFilters(rows);
  if (flag("--csv")) {
    console.log("handle,name,rel,type,art,reach,confirmed");
    for (const x of rows)
      console.log(
        [x.handle, x.name, x.rel, x.type, x.art, x.reach ?? "", x.confirmed]
          .map((v) => {
            const s = String(v ?? "");
            return /[",]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
          })
          .join(","),
      );
    return;
  }
  console.error(`\n${title} — ${rows.length}\n${"─".repeat(60)}`);
  for (const x of rows.slice(0, +opt("--limit", 80))) {
    const r = x.reach
      ? x.reach >= 1000
        ? `${Math.round(x.reach / 1000)}k`
        : `${x.reach}`
      : "·";
    console.error(
      `  @${x.handle.padEnd(26)} ${String(r).padStart(5)}  ${x.type.padEnd(11)}` +
        ` ${x.art ? "▣" : " "} ${x.name.slice(0, 32)}`,
    );
  }
  if (rows.length > +opt("--limit", 80))
    console.error(`  …${rows.length - +opt("--limit", 80)} more (--csv for all)`);
}

function needTarget(h) {
  const tp = join(dir, "targets", `${h}.followers.jsonl`);
  if (existsSync(tp)) return jl(join("targets", `${h}.followers.jsonl`));
  console.error(
    `\nNo follower data for @${h} yet. That vector needs a capped, ` +
      `opt-in target pull (account-risky enumeration of an account you ` +
      `don't own):\n  bin/ig-social-graph.py ${account} --target ${h} --cap 5000\n` +
      `→ writes social/${account}/targets/${h}.followers.jsonl, then re-run this.`,
  );
  process.exit(2);
}

switch (verb) {
  case "mutuals": {
    if (!followers.length)
      console.error("⚠ followers.jsonl empty — followers pull not done yet; " +
        "mutuals will be partial/empty until it lands.");
    emit(
      "MUTUALS · I follow them AND they follow me",
      [...fging].filter((u) => fgers.has(u)).map(enrich),
    );
    break;
  }
  case "idols":
    emit(
      "IDOLS · I follow them, no follow-back",
      [...fging].filter((u) => !fgers.has(u)).map(enrich),
    );
    break;
  case "fans":
    emit(
      "FANS · they follow me, I don't follow back",
      [...fgers].filter((u) => !fging.has(u)).map(enrich),
    );
    break;
  case "list":
    emit(
      "ART-WORLD NODES",
      graph.nodes.filter((n) => n.art_world).map((n) => enrich(n.id)),
    );
    break;
  case "affiliations": {
    const af = graph.edges.filter((e) => e.kind === "affiliation");
    const rows = (posArg ? af.filter((e) => e.source === posArg) : af).slice(
      0,
      +opt("--limit", 200),
    );
    console.error(
      `\nWHO RUNS / WORKS AT WHAT${posArg ? ` · @${posArg}` : ""} — ${
        rows.length
      }\n${"─".repeat(60)}`,
    );
    for (const e of rows)
      console.error(
        `  @${e.source.padEnd(24)} →${e.role ? `[${e.role}]` : ""}→ ${e.target}` +
          `  {${e.via}}`,
      );
    break;
  }
  case "also-follows": {
    if (!posArg) {
      console.error("usage: also-follows <handle>");
      process.exit(64);
    }
    const tf = setOf(needTarget(posArg));
    emit(
      `OF MY GRAPH, ALSO FOLLOWS @${posArg}`,
      [...fging, ...fgers]
        .filter((u, i, a) => a.indexOf(u) === i && tf.has(u))
        .map(enrich),
    );
    break;
  }
  case "who-follows": {
    if (!posArg) {
      console.error("usage: who-follows <handle>");
      process.exit(64);
    }
    emit(`EVERYONE WHO FOLLOWS @${posArg}`, needTarget(posArg).map((r) =>
      enrich(r.username),
    ));
    break;
  }
  case "node": {
    if (!posArg) {
      console.error("usage: node <handle>");
      process.exit(64);
    }
    const e = enrich(posArg);
    const af = graph.edges.filter(
      (x) => x.kind === "affiliation" && x.source === posArg,
    );
    console.error(JSON.stringify({ ...e, affiliations: af }, null, 2));
    break;
  }
  default:
    console.error(
      `unknown vector "${verb}". try: mutuals | idols | fans | list | ` +
        `affiliations | also-follows <h> | who-follows <h> | node <h>`,
    );
    process.exit(64);
}
