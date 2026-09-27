#!/usr/bin/env node
// chronology.mjs — one dated history of the band, from the members' own
// records and the repo's commit log. The biography corpus the play is told from.
//
//   node bin/chronology.mjs            → members/chronology.json + members/chronology.md
//
// Sources, all already harvested (re-run the harvesters first for fresh numbers):
//   members/<m>/facts.json    born, cycles, sessions
//   members/<m>/profile.json  records begin, longest uptime, current boot, busiest month
//   members/<m>/deep.json     macOS lineage, acquaintances
//   members/<m>/journey.json  the commit journey: lanes first/last, busiest day, by month
//   git log (this repo)       the stack's own cadence and the first time it names each member
import fs from "node:fs";
import path from "node:path";
import { execSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const LANE = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const REPO = path.resolve(LANE, "../../..");
const NEWBORN = "frisbee";
const MEMBERS = ["neo", "blueberry", ...(fs.existsSync(path.join(LANE, "members", NEWBORN, "facts.json")) ? [NEWBORN] : [])];
const load = (m, f) => { try { return JSON.parse(fs.readFileSync(path.join(LANE, "members", m, f + ".json"), "utf8")); } catch { return null; } };
const git = (args) => { try { return execSync(`git ${args}`, { cwd: REPO, encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] }).trim(); } catch { return ""; } };
const day = (s) => (s || "").slice(0, 10);
const daysBetween = (a, b) => Math.round((new Date(b) - new Date(a)) / 86400000);
const monthName = (ym) => new Date(ym + "-15").toLocaleString("en-US", { month: "long" });

const ev = [];
const add = (date, who, kind, text, source) => { if (date) ev.push({ date, who, kind, text, source }); };

const M = {};
for (const m of MEMBERS) M[m] = { facts: load(m, "facts"), profile: load(m, "profile"), deep: load(m, "deep"), journey: load(m, "journey") };

// ---- the members' own records
for (const m of MEMBERS) {
  const { facts, profile, deep, journey } = M[m];
  if (profile?.logins?.recorded_since) {
    const d = new Date(profile.logins.recorded_since);
    if (!isNaN(d)) add(d.toISOString().slice(0, 10), m, "records", `${m}'s login records begin — ${daysBetween(d, facts.born)} days before it is set up`, "profile.logins.recorded_since");
  }
  for (const u of deep?.macos_updates || []) add(day(u.date), m, "os", `${m} takes macOS ${u.version}`, "deep.macos_updates");
  if (facts?.born) {
    const t = facts.born.slice(11, 16);
    add(day(facts.born), m, "birth", `**${m} is set up** at ${t} — the birth minute (:${facts.born.slice(14, 16)}) becomes its phrase length`, "facts.born");
  }
  if (profile?.uptime?.longest_stretch_began) add(profile.uptime.longest_stretch_began, m, "uptime", `${m} begins its longest unbroken stretch: ${profile.uptime.longest_stretch_days} days awake`, "profile.uptime");
  if (journey?.ledger_begins) add(journey.ledger_begins, m, "commits", `${m}'s commit ${journey.source === "reflog" ? "reflog" : "ledger"} begins: “${(journey.first_commit || "").slice(0, 80)}”`, "journey.first_commit");
  if (journey?.busiest_day) add(journey.busiest_day.date, m, "commits", `${m}'s busiest day: ${journey.busiest_day.commits} commits`, "journey.busiest_day");
  const bm = profile?.agent_workload?.by_month || {};
  const top = Object.entries(bm).sort((a, b) => b[1] - a[1])[0];
  if (top) add(top[0] + "-01", m, "sessions", `${monthName(top[0])} is ${m}'s busiest month: ${top[1]} agent sessions hosted`, "profile.agent_workload.by_month");
  // lanes: the first commit of each lane the member has worked, biggest lanes first
  for (const l of (journey?.lanes || []).slice(0, 12)) add(l.first, m, "lane", `${m} opens the **${l.lane}** lane (${l.commits} commits through ${l.last}): “${(l.first_subject || "").slice(0, 70)}”`, "journey.lanes");
  if (profile?.boots?.current_boot) add(day(profile.boots.current_boot), m, "boot", `${m} boots for the stretch it is still on (reboot ${profile.boots.reboots}, as of the harvest)`, "profile.boots.current_boot");
  if (facts?.harvested_at) add(day(facts.harvested_at), m, "harvest", `${m} is harvested: ${facts.battery.cycles} cycles, ${facts.agent_sessions.count} sessions, whistle (GM ${facts.menuband.melodic_program}), radio ${facts.menuband.radio}`, "facts.harvested_at");
}
// pairing between the two
const nDeep = M.neo.deep, bDeep = M.blueberry.deep;
if (MEMBERS.includes(NEWBORN) && M[NEWBORN].deep?.bluetooth_paired?.some((d) => d.name === "blueberry" || /MacBook Neo/.test(d.name))) add(day(M[NEWBORN].facts.born), "all", "radio", `${NEWBORN} already has both elders on its radio list on its first day`, "deep.bluetooth_paired");
if (nDeep?.bluetooth_paired?.some((d) => d.name === "blueberry") && bDeep?.bluetooth_paired?.some((d) => /MacBook Neo/.test(d.name)))
  add(day(M.blueberry.facts.born), "both", "radio", "neo and blueberry are paired by radio — each has the other's name on its list", "deep.bluetooth_paired");
// age gap
add(day(M.blueberry.facts.born), "both", "gap", `blueberry is born ${daysBetween(M.neo.facts.born, M.blueberry.facts.born)} days after neo, six minutes later on the clock`, "facts.born");

// ---- the stack: the repo's cadence and when it first names each member
const perMonth = {};
for (const ym of git("log --since=2026-04-01 --date=format:%Y-%m --format=%ad").split("\n").filter(Boolean)) perMonth[ym] = (perMonth[ym] || 0) + 1;
for (const [ym, n] of Object.entries(perMonth).sort()) {
  const who = MEMBERS.map((m) => `${m} ${M[m].journey?.by_month?.[ym] ?? 0}`).join(", ");
  add(ym + "-01", "stack", "cadence", `${monthName(ym)}: the repo takes ${n} commits (${who})`, "git log");
}
const firstMention = (pat, label) => {
  const line = git(`log --reverse --since=2026-03-01 -i --grep="${pat}" --date=short --format="%ad %h %s"`).split("\n")[0];
  if (!line) return;
  const [d, h, ...s] = line.split(" ");
  add(d, "stack", "mention", `${label}: ${s.join(" ").slice(0, 90)} (${h})`, "git log --grep");
};
firstMention("slab/menuband: extract", "Menu Band leaves slab and becomes an app");
firstMention("MacBook Neo", "the first commit that names a MacBook Neo");
firstMention("blueberry", "the repo first says “blueberry”");
firstMention("macneopolitan", "the residency becomes a two-work program; the trio is named");
firstMention("autobiograph", "the members write their autobiographies");
firstMention("livesing\\|singer.c", "the singing engine");

// ---- the piece's own dates
add("2026-09-20", "both", "piece", "09:57 — neo (Fred) and blueberry (Kathy) sing the chorus together on one epoch, every note within a few cents", "PIECE.md");
add("2026-09-21", "both", "piece", "the held-out test: the words are lost in the voice, not the tuning (hear/README.md)", "hear/README.md");
add("2026-09-22", "both", "piece", "tech at the studio, as a duo", "PIECE.md runway");
if (!MEMBERS.includes(NEWBORN)) add("2026-09-22", NEWBORN, "birth", `**${NEWBORN} comes online** — the newborn; its record starts here (harvest on day one)`, "jeffrey, Sept 22");
else add(day(M[NEWBORN].facts.born), NEWBORN, "gap", `${NEWBORN} is born ${daysBetween(M.neo.facts.born, M[NEWBORN].facts.born)} days after neo and ${daysBetween(M.blueberry.facts.born, M[NEWBORN].facts.born)} after blueberry — at :${M[NEWBORN].facts.born.slice(14, 16)}, the shortest phrase in the house`, "facts.born");
add("2026-09-24", "all", "piece", "CultureHub LA: *AC Presents 2 New Pieces*, piece 2 — the trio plays", "PIECE.md");

ev.sort((a, b) => a.date.localeCompare(b.date) || ["stack", "neo", "blueberry", "both", NEWBORN, "all"].indexOf(a.who) - ["stack", "neo", "blueberry", "both", NEWBORN, "all"].indexOf(b.who));

// ---- write
fs.writeFileSync(path.join(LANE, "members/chronology.json"), JSON.stringify({ generated_at: new Date().toISOString(), members: MEMBERS, newborn: NEWBORN, events: ev }, null, 2));
let md = `# The band's chronology — neo, blueberry, and the stack\n\nGenerated ${new Date().toISOString().slice(0, 10)} by \`bin/chronology.mjs\` from the members' own records and the repo's commit log. Every line is a harvested fact with its source. This is the corpus the elders tell ${NEWBORN} from.\n\n`;
let cur = "";
for (const e of ev) {
  const ym = e.date.slice(0, 7);
  if (ym !== cur) { cur = ym; md += `\n## ${monthName(ym)} ${ym.slice(0, 4)}\n\n| date | who | event | source |\n|---|---|---|---|\n`; }
  md += `| ${e.date} | ${e.who} | ${e.text} | \`${e.source}\` |\n`;
}
fs.writeFileSync(path.join(LANE, "members/chronology.md"), md);
console.log(`${ev.length} events → members/chronology.{json,md}`);
