#!/usr/bin/env node
// pulse — what is a named group of people saying, across Bluesky, X and Mastodon.
//
//   node social/bin/pulse.mjs groups
//   node social/bin/pulse.mjs find "tezos art" [--x] [--limit 30]
//   node social/bin/pulse.mjs find --pack at://did:plc:…/app.bsky.graph.starterpack/…
//   node social/bin/pulse.mjs run security "apple outage" --terms apple,outage --hours 12
//   node social/bin/pulse.mjs run tezos --all --no-x
//   node social/bin/pulse.mjs save tezos --title "Tezos art" --keywords tezos,objkt --add '[{"name":"…","bluesky":"…"}]'
//
// Groups, questions and runs are private and live in the vault (research/social/).

import { find, findReport, listGroups, pulse, report, saveGroup } from "../lib/pulse.mjs";

const [command, ...rest] = process.argv.slice(2);
const flags = {}, words = [];
for (let i = 0; i < rest.length; i++) {
  if (!rest[i].startsWith("--")) { words.push(rest[i]); continue; }
  const name = rest[i].slice(2);
  flags[name] = rest[i + 1] && !rest[i + 1].startsWith("--") ? rest[++i] : true;
}
const list = (value) => typeof value === "string" ? value.split(",").map((s) => s.trim()).filter(Boolean) : undefined;

try {
  if (command === "groups") {
    const groups = listGroups();
    console.log(groups.length ? groups.map((g) => `${g.slug} — ${g.title} · ${g.members} people · last look ${g.lastRun || "never"}${g.lastQuestion ? ` · "${g.lastQuestion}"` : ""}`).join("\n") : "no groups yet");
  } else if (command === "find") {
    console.log(findReport(await find({ topic: words.join(" ") || undefined, pack: flags.pack, x: flags.x === true,
      platforms: list(flags.platforms), limit: Number(flags.limit) || 20 })));
  } else if (command === "run") {
    const platforms = list(flags.platforms) || (flags["no-x"] ? ["bluesky", "mastodon"] : undefined);
    console.log(report(await pulse({ group: words[0], question: words.slice(1).join(" "), terms: list(flags.terms),
      hours: Number(flags.hours) || 24, platforms, all: flags.all === true, save: flags["no-save"] !== true })));
  } else if (command === "save") {
    const group = saveGroup({ slug: words[0], title: flags.title, about: flags.about, keywords: list(flags.keywords),
      add: flags.add ? JSON.parse(flags.add) : [], remove: list(flags.remove) || [] });
    console.log(`${group.slug}: ${group.members.length} people`);
  } else {
    console.log("usage: pulse groups | find <topic> | run <group> [question] | save <group> …");
    process.exit(command ? 1 : 0);
  }
} catch (error) {
  console.error(`pulse: ${error.message}`);
  process.exit(1);
}
