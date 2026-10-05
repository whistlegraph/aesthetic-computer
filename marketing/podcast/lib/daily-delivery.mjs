import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { QUEUED_EXIT } from "./publishing-queue.mjs";

// Delivery failures never prevent the independent token stage. Non-billing
// errors still produce a failing exit status after that stage has run.
export function deliverDaily({ root, date, stage = false, mint = false, run = spawnSync, log = console.log }) {
  const slug = `daily-${date}`;
  const receipt = resolve(root, "out", `${slug}.buzzsprout.json`);
  const published = existsSync(receipt) ? JSON.parse(readFileSync(receipt, "utf8")) : null;
  const command = (args) => run(process.execPath, args, { cwd: root, stdio: "inherit" });
  let failed = false;
  if (!published) {
    const args = ["bin/buzzsprout.mjs", "enqueue", slug];
    if (stage) args.push("--private");
    if (command(args).status !== 0) { failed = true; log(`! ${slug}: upload could not be queued`); }
  }
  const upload = command(["bin/buzzsprout.mjs", "retry", "--limit=3"]);
  if (upload.status === QUEUED_EXIT) log(`! Buzzsprout uploads queued; daily production continues`);
  else if (upload.status !== 0) { failed = true; log(`! Buzzsprout delivery needs attention; continuing the token stage`); }
  // A private receipt must never become a public token on an ordinary rerun.
  if (mint && !stage && !published?.private) {
    const token = command(["bin/daily-token.mjs", "--date", date]);
    if (token.status !== 0) { failed = true; log(`! ${slug}: token failed; rerun bin/daily-token.mjs --date ${date}`); }
  }
  return failed ? 1 : 0;
}
