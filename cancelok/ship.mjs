// ship.mjs — put a certified pad in front of people.
//
// A pad nobody can reach is not a candidate, it's a file. Judgment is the whole
// product, so the last step of making something is publishing it: add it to the
// ^pads bag, commit, push, deploy. The machine gate is what earns it this — it
// has already proven it runs at 60fps without leaking or throwing, which is
// every question a script is able to ask.
//
// This is the ant colony's bargain (tests pass → commit) with a harder gate and
// a smaller blast radius: a pad is a LEAF. If a bad one lands, the cost is that
// someone presses cancel, which is the system working.
//
//   node cancelok/ship.mjs <pad> "<trait>"

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { REPO } from "./taste.mjs";

const BAGS = resolve(REPO, "system/public/aesthetic.computer/bags.json");
const git = (...a) => execFileSync("git", a, { cwd: REPO, encoding: "utf8" });

export function ship(code, trait = "") {
  // 1. Into the bag. Being in ^pads is what "kept" has always meant here — a
  // pad exists as a file, but it's only in the world once it's in the bag.
  const bags = JSON.parse(readFileSync(BAGS, "utf8"));
  const pads = bags.bags?.pads;
  if (!pads) throw new Error("no ^pads bag to ship into");
  if (pads.items.some((i) => i.code === code)) {
    console.log(`· ${code} is already in ^pads`);
  } else {
    pads.items.push({ type: "piece", code, name: code });
    pads.description = `${pads.items.length} self-running audiovisual instrument pads - tap or drag to play`;
    writeFileSync(BAGS, JSON.stringify(bags, null, 2) + "\n");
    console.log(`📦 ${code} → ^pads (${pads.items.length} pads)`);
  }

  // 2. Commit ONLY what this pad touched. A cron that commits a dirty tree will
  // one day ship whatever @jeffrey left open in his editor.
  const files = [`system/public/aesthetic.computer/disks/${code}.mjs`, "system/public/aesthetic.computer/bags.json"];
  git("add", ...files);
  const staged = git("diff", "--cached", "--name-only").trim();
  if (!staged) {
    console.log("· nothing to commit");
    return false;
  }
  git(
    "commit",
    "-m",
    `pads: ${code}${trait ? ` — ${trait}` : ""}\n\n` +
      `Made by the cancelok loop, certified by pad-doctor, unseen by human eyes\n` +
      `until someone presses cancel or ok.\n\n` +
      `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`,
  );
  console.log(`🌱 committed ${code}`);

  // 3. Push, then deploy — pushing alone doesn't put it in production.
  // --autostash because @jeffrey's tree is never clean, and a cron that demands
  // a clean tree is a cron that never runs.
  git("pull", "--rebase", "--autostash");
  git("push");
  console.log("⬆️  pushed");
  execFileSync("fish", [resolve(REPO, "lith/deploy.fish")], { cwd: REPO, stdio: "inherit" });
  console.log(`🚀 ${code} is live — aesthetic.computer/cancelok`);
  return true;
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const [code, trait] = process.argv.slice(2);
  if (!code) {
    console.error("usage: ship.mjs <pad> [trait]");
    process.exit(1);
  }
  ship(code, trait || "");
}
