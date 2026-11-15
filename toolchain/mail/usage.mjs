#!/usr/bin/env node
// Who is using Aesthetic mail?  node toolchain/mail/usage.mjs [days=30]
//
// Read-only and content-free: handles, counts and dates — never letter text,
// subjects or outside addresses. The mailbox is only reachable from lith, so
// this file ships itself over ssh and runs its census half there.

import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const HOST = process.env.MAIL_USAGE_HOST || "root@lith.aesthetic.computer";

if (process.env.MAIL_USAGE_REMOTE) await census(Number(process.env.DAYS) || 30);
else relay(process.argv[2] === undefined ? 30 : Number(process.argv[2]));

// Local half: service health, then the census on lith.
function relay(days) {
  if (!Number.isInteger(days) || days < 1 || days > 400) {
    console.error("days must be 1–400");
    process.exit(2);
  }
  const ssh = (command, input) =>
    spawnSync("ssh", ["-o", "ConnectTimeout=10", HOST, command], {
      input,
      stdio: [input === undefined ? "ignore" : "pipe", "inherit", "inherit"],
    }).status;

  console.log("— service");
  ssh("systemctl is-active lith-mail | sed 's/^/lith-mail: /'; systemctl show lith-mail -p ActiveEnterTimestamp --value | sed 's/^/up since: /'");
  const status = ssh(
    `cd /opt/ac/system/backend && MAIL_USAGE_REMOTE=1 DAYS=${days} node --env-file=../.env --input-type=module -`,
    readFileSync(fileURLToPath(import.meta.url)),
  );
  process.exit(status ?? 1);
}

// Remote half: runs on lith from /opt/ac/system/backend.
async function census(days) {
  const { connect } = await import("./database.mjs");
  const { db } = await connect();
  const tells = db.collection("tells");
  const since = new Date(Date.now() - days * 864e5);
  const day = (date) => date.toISOString().slice(0, 10);
  // An inbound email carries the writer's display name in fromHandle; keep it on lith.
  const sender = (m) => (m.fromEmail || m.via === "smtp" ? "(outside address)" : m.fromHandle || "(no handle)");
  const recipient = (m) => (m.toEmail ? "(outside address)" : m.toHandle || "(no handle)");
  const kind = (m) => (m.via === "smtp" ? "email in" : m.via === "smtp-out" ? "email out" : m.subject ? "letter" : "tell");

  const rows = await tells
    .find({ when: { $gte: since } }, { projection: { text: 0, subject: 0, attachments: 0, auth: 0 } })
    .sort({ when: 1 })
    .toArray();
  // `subject` is projected away so nothing readable leaves the box; ask the
  // database which rows had one.
  const lettered = new Set(
    (await tells.find({ when: { $gte: since }, subject: { $exists: true } }, { projection: { _id: 1 } }).toArray()).map((m) => String(m._id)),
  );
  for (const m of rows) if (lettered.has(String(m._id))) m.subject = true;

  const tally = (keyOf) => {
    const counts = new Map();
    for (const m of rows) counts.set(keyOf(m), (counts.get(keyOf(m)) || 0) + 1);
    return [...counts].sort((a, b) => b[1] - a[1]);
  };
  const table = (title, pairs) => {
    console.log(`\n— ${title}`);
    for (const [key, n] of pairs) console.log(String(n).padStart(5), key);
    if (!pairs.length) console.log("    (none)");
  };

  const handles = new Set(rows.flatMap((m) => [m.fromEmail || m.via === "smtp" ? null : m.fromHandle, m.toEmail ? null : m.toHandle]).filter(Boolean));
  console.log(`\n— mailbox, last ${days} days`);
  console.log(`${rows.length} messages · ${handles.size} handles involved · ${await tells.countDocuments()} ever`);
  const newest = rows.at(-1);
  console.log(newest ? `newest: ${newest.when.toISOString().slice(0, 16)}Z ${sender(newest)} → ${recipient(newest)}` : "newest: none in window");

  table("by kind", tally(kind));
  table("senders", tally(sender));
  table("who writes to whom", tally((m) => `${sender(m)} → ${recipient(m)}`));
  table("by day", tally((m) => day(m.when)).sort((a, b) => a[0].localeCompare(b[0])).map(([d, n]) => [d, n]));

  const unread = new Map();
  for (const m of await tells.find({ read: false }, { projection: { toHandle: 1 } }).toArray()) {
    unread.set(m.toHandle || "(no handle)", (unread.get(m.toHandle || "(no handle)") || 0) + 1);
  }
  table("unread, all time", [...unread].sort((a, b) => b[1] - a[1]));

  const events = await db
    .collection("mail-events")
    .aggregate([
      { $match: { when: { $gte: since }, transport: { $ne: "smtp-in" } } },
      { $group: { _id: { transport: "$transport", action: "$action", event: "$event", status: "$status" }, n: { $sum: 1 } } },
      { $sort: { n: -1 } },
    ])
    .toArray();
  table(
    "app traffic (mail-events keeps 30 days)",
    events.map((e) => [[e._id.transport, e._id.action, e._id.event, e._id.status].filter((v) => v !== undefined && v !== null).join(" "), e.n]),
  );

  // Every send that did not come back 200, with what the same minutes held:
  // a success right after means the writer fixed the address and got through.
  const sends = await db
    .collection("mail-events")
    .find({ when: { $gte: since }, transport: { $in: ["api", "tell"] }, action: "send", event: { $in: ["request", "failed"] } },
      { projection: { when: 1, status: 1, reason: 1, shape: 1, error: 1, event: 1, transport: 1 } })
    .sort({ when: 1 })
    .toArray();
  console.log("\n— sends that failed");
  const failed = sends.filter((e) => e.status !== 200);
  for (const e of failed) {
    const retry = sends.find((o) => o.status === 200 && o.when > e.when && o.when - e.when < 10 * 60000);
    console.log(`${e.when.toISOString().slice(0, 19)}Z ${e.transport} ${e.status ?? e.event} ${e.reason || e.error || ""}${e.shape ? ` · typed: ${e.shape}` : ""}`.trimEnd(),
      retry ? `→ a send succeeded ${Math.round((retry.when - e.when) / 1000)}s later` : "→ no successful send within 10 min");
  }
  if (!failed.length) console.log("    (none)");
  process.exit(0);
}
