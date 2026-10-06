// Account deletion, 2026.09.26
// Deleting an account is a job, not a request. Asking locks the account at
// once and schedules the purge after a grace period; the purge then runs a
// fixed list of steps, recording each one in a ledger so a failure resumes
// where it stopped instead of leaving a half-deleted account. The sign-in
// identity goes last, so a stalled job can always be finished.
//
// Everything outside Node (Mongo, Redis, Spaces, Auth0, Stripe, mail, the
// chat servers) arrives through `deps`; see account-deletion-deps.mjs for the
// production wiring and system/tests/account-deletion.test.mjs for fakes.
//
// What is kept, and why, is written next to each step and in
// system/public/privacy-policy.html. The ledger never stores content; after
// the purge only a hash of the account ID remains, so a restored backup can
// be re-purged.

import { createHash, randomBytes } from "node:crypto";

const DAY = 24 * 60 * 60 * 1000;
export const GRACE_MS = 14 * DAY;
export const HANDLE_QUARANTINE_MS = 90 * DAY;
export const LEASE_MS = 10 * 60 * 1000;
const RETRY_MS = [5 * 60 * 1000, 30 * 60 * 1000, 2 * 60 * 60 * 1000, 12 * 60 * 60 * 1000];

export const LEDGER = "account-deletions";
export const TOMBSTONES = "account-tombstones";
export const QUARANTINE = "handle-quarantine";

export const hash = (value) => createHash("sha256").update(String(value)).digest("hex");
export const bareHandle = (handle) => String(handle || "").replace(/^@/, "").toLowerCase();
const escape = (text) => String(text).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
const unique = (values) => [...new Set(values.filter(Boolean))];

// 🧾 Ask. Locks the account, schedules the purge and mails a restore link.
// Asking again while a deletion is pending returns the same schedule.
export async function requestDeletion(deps, { user, now = new Date() }) {
  const sub = user?.sub;
  if (!sub) throw new Error("No account to delete.");
  const ledger = deps.db.collection(LEDGER);

  let job = await ledger.findOne({ _id: sub });
  if (!job || job.state === "cancelled") {
    const handleDoc = await deps.db.collection("@handles").findOne({ _id: sub });
    job = {
      _id: sub,
      state: "scheduled",
      requestedAt: now,
      purgeAfter: new Date(now.getTime() + GRACE_MS),
      attempts: 0,
      steps: {},
      mailed: false,
      snapshot: {
        handle: handleDoc?.handle || "",
        email: user.email || "",
        emailVerified: user.email_verified === true,
      },
    };
    await ledger.replaceOne({ _id: sub }, job, { upsert: true });
  }
  if (job.state !== "scheduled") return schedule(job);

  // Asking again repairs a request that failed halfway: the lock is
  // idempotent, and a link that never went out is issued again.
  await deps.lock(sub, job.purgeAfter);
  if (!job.mailed && job.snapshot.email) {
    const restoreToken = randomBytes(32).toString("base64url");
    await ledger.updateOne(
      { _id: sub, state: "scheduled" },
      { $set: { restoreTokenHash: hash(restoreToken) } },
    );
    const mailed = await deps.mail
      .scheduled({
        to: job.snapshot.email,
        handle: job.snapshot.handle,
        purgeAfter: job.purgeAfter,
        restoreToken,
      })
      .catch(() => false);
    job.mailed = mailed === true;
    await ledger.updateOne({ _id: sub }, { $set: { mailed: job.mailed } });
  }
  return schedule(job);
}

function schedule(job) {
  return {
    state: job.state,
    requestedAt: job.requestedAt,
    purgeAfter: job.purgeAfter,
    mailed: job.mailed === true,
  };
}

// 👀 Before asking: what deletion would remove, what would stay without the
// person, and the braincells that would be lost. Shown by the clients so
// nobody confirms blind.
export async function previewDeletion(deps, { user }) {
  const sub = user?.sub;
  if (!sub) throw new Error("No account to delete.");
  const handleDoc = await deps.db.collection("@handles").findOne({ _id: sub });
  const snap = {
    handle: handleDoc?.handle || "",
    email: user.email || "",
    emailVerified: user.email_verified === true,
  };
  const { inv, counts } = await survey(deps, sub, snap);
  const wallet = await deps.db.collection("ac-credit-wallets").findOne({ _id: sub });
  return {
    handle: snap.handle,
    email: snap.email,
    graceDays: Math.round(GRACE_MS / DAY),
    handleHoldDays: Math.round(HANDLE_QUARANTINE_MS / DAY),
    handleGoesToSotce: inv.sotceSister,
    braincells: Math.max(0, Number(wallet?.balance) || 0),
    counts,
  };
}

// 📦 Before asking: a copy of what the account made, so deleting it does not
// mean losing it. Records it created, and links to its uploaded files
// (public while the account exists).
const EXPORT = [
  ["paintings", "user"],
  ["pieces", "user"],
  ["kidlisp", "user"],
  ["moods", "user"],
  ["tapes", "user"],
  ["clocks", "user"],
  ["news-posts", "user"],
  ["news-comments", "user"],
  ["mimechan", "user"],
  ["chat-system", "user"],
  ["chat-clock", "user"],
  ["tells", "from"],
  ["calendar", "user"],
  ["walkieware-threads", "owner"],
  ["whistlegraph-roblox-rooms", "_id"],
];

// Export receipts, mint artwork and delivery state without Apple account
// tokens, signed payloads or mint capabilities. An allowlist also keeps
// future provider credentials out of this download.
const PRIVATE_EXPORT = [
  ["oskiewar-fighters", "owner", ["fighter", "acceptedAt", "expiresAt"]],
  ["nopaint-move-requests", "user", ["_id", "model", "braincells", "charged", "free", "paid", "status", "startedAt", "finishedAt"]],
  ["whistlegraph-speech-requests", "user", ["_id", "braincells", "charged", "free", "paid", "status", "startedAt", "finishedAt"]],
  ["whistlegraph-iap-accounts", "_id", ["createdAt"]],
  ["whistlegraph-iap-purchases", "user", [
    "_id", "transactionId", "productId", "credits", "environment", "createdAt",
    "deliveredAt", "refundedAt", "refundAppliedAt", "reconciliationRequired",
    ["state", ["signedDate", "refundedCredits", "type"]],
  ]],
  ["whistlegraph-iap-notifications", "user", [
    "_id", "type", "purchaseId", "signedDate", "refundedCredits", "receivedAt", "status", "appliedAt",
  ]],
  ["ac-credit-wallets", "_id", ["balance", "spent", "createdAt", "updatedAt"]],
  ["whistlegraph-mints", "user", [
    "code", "version", "source", "sourceHash", "density", "aspect", "title",
    "description", "editions", "royalties", "createdAt", "status", "sender",
    "artifactUri", "htmlUri", "zipUri", "coverUri", "thumbnailUri", "metadataUri",
    "artifactMimeType", "packageVersion", "operationHash", "tokenId", "mintedAt",
  ]],
];

function exportFields(doc, fields) {
  const selected = {};
  for (const field of fields) {
    const [key, nested] = Array.isArray(field) ? field : [field];
    if (!Object.hasOwn(doc, key)) continue;
    if (nested && (!doc[key] || typeof doc[key] !== "object" || Array.isArray(doc[key]))) continue;
    selected[key] = nested ? exportFields(doc[key], nested) : doc[key];
  }
  return selected;
}

export async function exportAccount(deps, { user, now = new Date() }) {
  const sub = user?.sub;
  if (!sub) throw new Error("No account to export.");
  const handleDoc = await deps.db.collection("@handles").findOne({ _id: sub });
  const records = {};
  for (const [name, field] of EXPORT) {
    const docs = await deps.db.collection(name).find({ [field]: sub }).toArray();
    if (docs.length) records[name] = docs;
  }
  for (const [name, field, fields] of PRIVATE_EXPORT) {
    const docs = await deps.db.collection(name).find({ [field]: sub }).toArray();
    if (docs.length) {
      records[name] = docs.map(doc => exportFields(doc, fields));
    }
  }
  const files = await deps.storage.list("user", `${sub}/`);
  return {
    exportedAt: now.toISOString(),
    account: { sub, handle: handleDoc?.handle || "", email: user.email || "" },
    records,
    files,
  };
}

// ↩️ Keep. The emailed token restores a scheduled account until the purge
// begins; after that there is nothing left to restore.
export async function restoreDeletion(deps, { token, now = new Date() }) {
  if (typeof token !== "string" || token.length < 32) return { restored: false };
  const ledger = deps.db.collection(LEDGER);
  // "restoring" keeps the runner away while the account unlocks; if the
  // unlock fails the job goes back to "scheduled" and the link still works.
  const job = await ledger.findOneAndUpdate(
    {
      restoreTokenHash: hash(token),
      state: { $in: ["scheduled", "restoring"] },
      purgeAfter: { $gt: now },
    },
    { $set: { state: "restoring", restoringSince: now } },
    { returnDocument: "after" },
  );
  if (!job) return { restored: false };
  try {
    await deps.unlock(job._id);
  } catch (error) {
    await ledger.updateOne({ _id: job._id, state: "restoring" }, { $set: { state: "scheduled" } });
    throw error;
  }
  await ledger.deleteOne({ _id: job._id, state: "restoring" });
  return { restored: true, handle: job.snapshot?.handle || "" };
}

// ⏰ Run whatever is due: scheduled purges past their grace period, failed
// jobs past their backoff, and running jobs whose worker died.
export async function runDueDeletions(deps, { now = new Date(), limit = 3 } = {}) {
  const ledger = deps.db.collection(LEDGER);
  const results = [];
  // A restore that died between its two unlocks is finished, never purged:
  // the person asked to keep the account.
  const stale = new Date(now.getTime() - LEASE_MS);
  for (const job of await ledger.find({ state: "restoring", restoringSince: { $lte: stale } }).toArray()) {
    try {
      await deps.unlock(job._id);
      await ledger.deleteOne({ _id: job._id, state: "restoring" });
      results.push({ state: "restored" });
    } catch (error) {
      deps.log?.(`🪦 Could not finish a restore: ${error?.message || error}`);
    }
  }
  for (let i = 0; i < limit; i += 1) {
    const owner = randomBytes(8).toString("hex");
    const job = await ledger.findOneAndUpdate(
      {
        $or: [
          { state: "scheduled", purgeAfter: { $lte: now } },
          { state: "failed", nextAttemptAt: { $lte: now } },
          { state: "running", leaseUntil: { $lte: now } },
        ],
      },
      {
        $set: { state: "running", leaseOwner: owner, leaseUntil: new Date(now.getTime() + LEASE_MS) },
        $unset: { restoreTokenHash: "" },
        $inc: { attempts: 1 },
      },
      { sort: { purgeAfter: 1 }, returnDocument: "after" },
    );
    if (!job) break;
    results.push(await purge(deps, job, now));
  }
  return results;
}

// 🧹 The purge. Steps run in order; a finished step is never repeated, and a
// failed one stops the job with a backoff so the next run resumes there.
// Every ledger write names the lease it holds, so a worker whose lease ran
// out cannot overwrite the one that took the job over.
class LostLease extends Error {}

export async function purge(deps, job, now = new Date()) {
  const ledger = deps.db.collection(LEDGER);
  const mine = { _id: job._id, leaseOwner: job.leaseOwner };
  const ctx = { deps, db: deps.db, sub: job._id, job, snap: job.snapshot || {}, now, mine };
  for (const step of STEPS) {
    if (job.steps?.[step.name]?.status === "done") continue;
    try {
      const detail = (await step.run(ctx)) ?? null;
      job.steps = { ...job.steps, [step.name]: { status: "done", at: new Date(), detail } };
      const held = await ledger.updateOne(
        mine,
        {
          $set: {
            [`steps.${step.name}`]: job.steps[step.name],
            leaseUntil: new Date(Math.max(Date.now(), now.getTime()) + LEASE_MS),
          },
        },
      );
      if (held && held.matchedCount === 0) throw new LostLease();
    } catch (error) {
      if (error instanceof LostLease) return { state: "handed-off", step: step.name };
      const wait = RETRY_MS[Math.min((job.attempts || 1) - 1, RETRY_MS.length - 1)];
      const message = String(error?.message || error).slice(0, 300);
      await ledger.updateOne(
        mine,
        {
          $set: {
            state: "failed",
            nextAttemptAt: new Date(now.getTime() + wait),
            [`steps.${step.name}`]: { status: "failed", at: new Date(), error: message },
          },
        },
      );
      deps.log?.(
        `🪦 Account deletion stopped at ${step.name} (attempt ${job.attempts || 1}): ${message}`,
      );
      return { state: "failed", step: step.name };
    }
  }
  await finish(ctx);
  return { state: "completed" };
}

async function finish({ deps, db, sub, snap, now }) {
  // A hash of the account ID is all that remains: enough to re-purge a
  // restored backup, not enough to say whose account it was.
  await db
    .collection(TOMBSTONES)
    .updateOne({ _id: hash(sub) }, { $setOnInsert: { completedAt: now } }, { upsert: true });
  if (snap.email) await deps.mail.completed({ to: snap.email }).catch(() => false);
  await deps.unlock(sub, { identityDeleted: true });
  await db.collection(LEDGER).deleteOne({ _id: sub });
}

// 🔒 A handle stays unclaimable for a while after its account is deleted, so
// no one can pick it up and pass as the person who left.
export async function handleQuarantined(db, handle, now = new Date()) {
  const held = await db.collection(QUARANTINE).findOne({ _id: hash(bareHandle(handle)) });
  return !!held && held.until > now;
}

// Records keyed to the account that are deleted outright.
// [collection, query(sub, ctx)]
const DELETE = [
  ["whistlegraph-speech-requests", (sub) => ({ user: sub })],
  ["nopaint-move-requests", (sub) => ({ user: sub })],
  ["paintings", (sub) => ({ user: sub })],
  ["pieces", (sub) => ({ user: sub })],
  ["moods", (sub) => ({ user: sub })],
  ["push-tokens", (sub) => ({ user: sub })],
  ["easel-transcripts-private", (sub) => ({ owner: sub })],
  ["walkieware-threads", (sub) => ({ owner: sub })],
  ["whistlegraph-roblox-rooms", (sub) => ({ _id: sub })],
  ["tells", (sub) => ({ $or: [{ to: sub }, { from: sub }] })],
  ["tapes", (sub) => ({ user: sub })],
  ["tape-drafts", (sub) => ({ user: sub })],
  ["oven-bakes", (sub, ctx) => ({ code: { $in: ctx.inv.tapeCodes } })],
  ["clocks", (sub) => ({ user: sub })],
  ["kidlisp", (sub, ctx) => ({ user: sub, code: { $in: ctx.inv.kidlispDelete } })],
  ["keep-jobs", (sub) => ({ user: sub })],
  ["news-posts", (sub) => ({ user: sub })],
  ["news-comments", (sub) => ({ user: sub })],
  ["news-votes", (sub) => ({ user: sub })],
  ["mimechan", (sub) => ({ user: sub })],
  ["hearts", (sub) => ({ user: sub })],
  ["piece-user-hits", (sub) => ({ user: sub })],
  ["account-activity", (sub) => ({ user: sub, tenant: "aesthetic" })],
  ["cal-feeds", (sub) => ({ user: sub })],
  ["calendar", (sub) => ({ user: sub })],
  ["laklok-themes", (sub) => ({ _id: sub })],
  ["nom-scores", (sub) => ({ user: sub })],
  ["cancelok", (sub) => ({ user: sub })],
  ["oskiewar-maps", (sub) => ({ owner: sub })],
  ["oskiewar-fighters", (sub) => ({ owner: sub })],
  ["oskiewar-generation-jobs", (sub) => ({ owner: sub })],
  ["easel-image-jobs", (sub) => ({ _id: { $regex: `^${escape(sub)}:` } })],
  ["easel-image-budget", (sub) => ({ _id: { $regex: `^${escape(sub)}:` } })],
  ["ai-usage", (sub, ctx) => ({ handle: { $in: ctx.handles } })],
  ["products", (sub) => ({ "source.user": sub })],
  ["mail-nudges", (sub) => ({ user: sub })],
  // Secrets the account saved for its own devices.
  ["device-creds", (sub) => ({ _id: sub })],
  ["device-tokens", (sub) => ({ _id: sub })],
  ["device-pairs", (sub) => ({ sub })],
  ["ac-machines", (sub) => ({ user: sub })],
  ["ac-machine-logs", (sub) => ({ user: sub })],
  ["ff1Devices", (sub) => ({ user: sub })],
  // Handle events ("hi @x", "@x is now @y") replay into public chat history.
  ["logs", (sub) => ({ users: sub })],
];

const CHATS = ["chat-system", "chat-clock"];

// What deleting this account would touch, by the same rules the purge
// uses. Codes and counts only; it never returns content.
async function survey(deps, sub, snap) {
  const db = deps.db;

  const user = await db.collection("users").findOne({ _id: sub });
  const tapes = await db.collection("tapes").find({ user: sub }).toArray();
  const moods = await db.collection("moods").find({ user: sub }).toArray();
  const news = await db.collection("news-posts").find({ user: sub }).toArray();
  const kidlisp = await db.collection("kidlisp").find({ user: sub }).toArray();

  // Minted KidLisp is on chain for good, and a $code someone else's
  // piece uses would break their work, so both lose their author
  // instead of being deleted.
  // Any trace of a keep counts: a confirmed token, a keep still waiting
  // on its transaction, or the legacy Tezos summary.
  const minted = kidlisp.filter(
    (k) => k.kept || k.pendingKeep || k.tezos?.minted || k.tezos?.tokenId != null || k.tezos?.contracts,
  );
  const rest = kidlisp.filter((k) => !minted.includes(k)).map((k) => k.code).filter(Boolean);
  const uses = (source, code) => new RegExp(`\\$${escape(code)}\\b`).test(source || "");
  const referenced = new Set();
  for (let i = 0; i < rest.length; i += 50) {
    const batch = rest.slice(i, i + 50);
    const pattern = `\\$(${batch.map(escape).join("|")})\\b`;
    const users = await db
      .collection("kidlisp")
      .find({ user: { $ne: sub }, source: { $regex: pattern } })
      .toArray();
    for (const other of users) {
      for (const code of batch) if (uses(other.source, code)) referenced.add(code);
    }
  }
  // What a kept piece uses must stay too, and what that uses, and so on.
  const bySource = new Map(kidlisp.map((k) => [k.code, k.source]));
  const kept = [...minted.map((k) => k.code), ...referenced];
  for (let i = 0; i < kept.length; i += 1) {
    for (const code of rest) {
      if (!referenced.has(code) && uses(bySource.get(kept[i]), code)) {
        referenced.add(code);
        kept.push(code);
      }
    }
  }

  let sotceSister = false;
  if (snap.handle && snap.email && snap.emailVerified && deps.auth0.sotceSister) {
    sotceSister = await deps.auth0.sotceSister(snap.email);
  }

  // IPFS bundles pinned for pieces that are deleted before anyone minted
  // them. A CID a kept piece also uses stays pinned.
  const cid = (uri) => String(uri || "").match(/^ipfs:\/\/([A-Za-z0-9]+)/)?.[1];
  const pinsOf = (k) =>
    [k.ipfsMedia?.artifactUri, k.ipfsMedia?.thumbnailUri, k.kept?.artifactUri, k.kept?.thumbnailUri,
      k.kept?.metadataUri, k.pendingKeep?.artifactUri, k.pendingKeep?.thumbnailUri, k.pendingKeep?.metadataUri]
      .map(cid)
      .filter(Boolean);
  const deleting = new Set(rest.filter((code) => !referenced.has(code)));
  const keptPins = new Set(kidlisp.filter((k) => !deleting.has(k.code)).flatMap(pinsOf));
  const ipfsCids = unique(kidlisp.filter((k) => deleting.has(k.code)).flatMap(pinsOf)).filter(
    (c) => !keptPins.has(c),
  );

  const inv = {
    userCode: user?.code || "",
    ipfsCids,
    tapeCodes: unique(tapes.map((t) => t.code)),
    moodRkeys: unique(moods.map((m) => m.bluesky?.rkey)),
    newsCodes: unique(news.map((n) => n.code)),
    kidlispDelete: rest.filter((code) => !referenced.has(code)),
    kidlispKeep: unique([...minted.map((k) => k.code), ...referenced]),
    sotceSister: sotceSister === true,
  };

  return {
    inv,
    counts: {
      paintings: await count(db, "paintings", { user: sub }),
      pieces: await count(db, "pieces", { user: sub }),
      whistlegraphs: await count(db, "walkieware-threads", { owner: sub }),
      moods: moods.length,
      tapes: inv.tapeCodes.length,
      news: inv.newsCodes.length,
      chat: (await count(db, "chat-system", { user: sub })) + (await count(db, "chat-clock", { user: sub })),
      kidlispDeleted: inv.kidlispDelete.length,
      kidlispKept: inv.kidlispKeep.length,
    },
  };
}

async function count(db, name, query) {
  const collection = db.collection(name);
  if (collection.countDocuments) return collection.countDocuments(query);
  return (await collection.find(query).toArray()).length;
}

export const STEPS = [
  {
    // Everything later steps need, read once while the records still exist.
    // Codes and counts only; the ledger never holds content.
    name: "inventory",
    async run(ctx) {
      const { db, sub, snap } = ctx;
      const { inv } = await survey(ctx.deps, sub, snap);
      await db.collection(LEDGER).updateOne({ _id: sub }, { $set: { inventory: inv } });
      ctx.job.inventory = inv;
      return {
        tapes: inv.tapeCodes.length,
        kidlispDeleted: inv.kidlispDelete.length,
        kidlispKept: inv.kidlispKeep.length,
      };
    },
  },
  {
    // Remove the Apple account mapping before touching the balance. The IAP
    // service also checks this deletion job/tombstone, so a late notification
    // cannot recreate the deleted wallet. Keep transaction IDs as immutable
    // anti-replay claims, with a hash instead of an account or Apple token.
    name: "whistlegraph-purchases",
    async run({ db, sub, now }) {
      await db.collection("whistlegraph-iap-accounts").deleteOne({ _id: sub });
      await db.collection("whistlegraph-iap-purchases").updateMany(
        { user: sub },
        {
          $set: { deletedAt: now, userHash: hash(sub) },
          $unset: { user: "", appAccountToken: "", signedPayload: "" },
        },
      );
      const notifications = await db.collection("whistlegraph-iap-notifications").deleteMany({ user: sub });
      return { notifications: notifications?.deletedCount || 0 };
    },
  },
  {
    // Mint links are bearer capabilities. Retire their hashed IDs so old links
    // cannot resume a mint or be reused, while removing drafts, wallet links,
    // personal metadata and previews. The artwork already on chain/IPFS stays.
    name: "whistlegraph-mints",
    async run({ db, sub, now }) {
      const collection = db.collection("whistlegraph-mints");
      const rows = await collection.find({ user: sub }).toArray();
      for (const row of rows) {
        const retired = { _id: row._id, status: "deleted", deletedAt: now };
        for (const key of ["operationHash", "tokenId"]) {
          if (typeof row[key] === "string") retired[key] = row[key];
        }
        await collection.replaceOne({ _id: row._id, user: sub }, retired);
      }
      return { retired: rows.length };
    },
  },
  {
    // Cancels monthly gifts and removes the Stripe customer (card details
    // and addresses). Stripe keeps the charges themselves as tax records.
    // Matched by email, so only a verified email is trusted.
    name: "payments",
    async run({ deps, snap }) {
      if (!deps.stripe) return { skipped: "not-configured" };
      if (!snap.email || !snap.emailVerified) return { skipped: "no-verified-email" };
      return { customers: await deps.stripe.forget(snap.email) };
    },
  },
  {
    name: "bluesky-mirror",
    async run(ctx) {
      const inv = inventory(ctx);
      let posts = 0;
      // Each deleted post leaves the list, so a retry picks up where this
      // one stopped instead of starting over.
      for (const rkey of [...inv.moodRkeys]) {
        if (!(await ctx.deps.bluesky.deletePost(rkey))) {
          throw new Error("Could not delete a mirrored Bluesky post.");
        }
        inv.moodRkeys = inv.moodRkeys.filter((r) => r !== rkey);
        await ctx.db.collection(LEDGER).updateOne(ctx.mine, { $set: { "inventory.moodRkeys": inv.moodRkeys } });
        posts += 1;
      }
      return { posts };
    },
  },
  {
    name: "storage-user",
    async run({ deps, sub }) {
      return { objects: await deps.storage.deletePrefix("user", `${sub}/`) };
    },
  },
  {
    name: "storage-tapes",
    async run(ctx) {
      const keys = inventory(ctx).tapeCodes.flatMap((c) => [`tapes/${c}.mp4`, `tapes/${c}-thumb.jpg`]);
      return { objects: await ctx.deps.storage.deleteKeys("tapes", keys) };
    },
  },
  {
    // Oven renders of deleted KidLisp, and the share cards of news posts.
    name: "storage-renders",
    async run(ctx) {
      const { db, deps } = ctx;
      const inv = inventory(ctx);
      let objects = 0;
      for (const code of inv.kidlispDelete) {
        objects += await deps.storage.deletePrefix("art", `oven/frozen/${code}-`);
        const cached = await db
          .collection("oven-cache")
          .find({ key: { $regex: `/\\$${escape(code)}-` } })
          .toArray();
        objects += await deps.storage.deleteKeys("art", cached.map((c) => `oven/${c.key}`));
        await db.collection("oven-cache").deleteMany({ key: { $in: cached.map((c) => c.key) } });
        const grabs = await db
          .collection("oven-grabs")
          .find({ piece: `$${code}` })
          .toArray();
        const grabKeys = grabs
          .map((g) => String(g.cdnUrl || "").match(/\/(oven\/grabs\/[^/?#]+)$/)?.[1])
          .filter(Boolean);
        objects += await deps.storage.deleteKeys("art", grabKeys);
        await db.collection("oven-grabs").deleteMany({ piece: `$${code}` });
      }
      objects += await deps.storage.deleteKeys("art", inv.newsCodes.map((c) => `og/news/${c}.png`));
      return { objects };
    },
  },
  {
    // Stops hosting bundles pinned for pieces that were never minted. Other
    // IPFS nodes that fetched them may keep copies; the policy says so.
    name: "ipfs",
    async run(ctx) {
      const cids = inventory(ctx).ipfsCids || [];
      if (!cids.length) return { unpinned: 0 };
      if (!ctx.deps.ipfs) return { skipped: "not-configured" };
      for (const c of cids) await ctx.deps.ipfs.unpin(c);
      return { unpinned: cids.length };
    },
  },
  {
    // The Datomic store keeps history, so deleted pieces are excised, not
    // just retracted. Skipped while KidLisp lives only in Mongo.
    name: "kidlisp-sidecar",
    async run(ctx) {
      if (!ctx.deps.sidecar?.enabled) return { skipped: "disabled" };
      const inv = inventory(ctx);
      return ctx.deps.sidecar.erase({
        sub: ctx.sub,
        deleteCodes: inv.kidlispDelete,
        anonymizeCodes: inv.kidlispKeep,
      });
    },
  },
  {
    name: "records",
    async run(ctx) {
      const handle = bareHandle(ctx.snap.handle);
      ctx.handles = handle ? [handle, `@${handle}`] : [];
      ctx.inv = inventory(ctx);
      const counts = {};
      for (const [name, query] of DELETE) {
        const result = await ctx.db.collection(name).deleteMany(query(ctx.sub, ctx));
        if (result?.deletedCount) counts[name] = result.deletedCount;
      }
      await ctx.db.collection("verifications").deleteOne({ _id: ctx.sub });
      return counts;
    },
  },
  {
    // Kept without the person: minted and shared KidLisp (on chain, or used
    // by other people's pieces), purchase ledgers (tax records), and
    // telemetry, whose visitor fields are dropped.
    name: "anonymize",
    async run(ctx) {
      const { db, sub } = ctx;
      const inv = inventory(ctx);
      const handle = bareHandle(ctx.snap.handle);
      const handles = handle ? [handle, `@${handle}`] : [];
      const gone = `deleted:${hash(sub).slice(0, 16)}`;

      await db.collection("kidlisp").updateMany(
        { user: sub },
        { $unset: { user: "", handle: "", "kept.keptBy": "", "pendingKeep.requestedBy": "" } },
      );
      await db.collection("kidlisp").updateMany(
        { "kept.keptBy": sub },
        { $unset: { "kept.keptBy": "" } },
      );
      await db.collection("kidlisp").updateMany(
        { "pendingKeep.requestedBy": sub },
        { $unset: { "pendingKeep.requestedBy": "" } },
      );

      const wallet = await db.collection("ac-credit-wallets").findOne({ _id: sub });
      if (wallet) {
        const { _id, ...rest } = wallet;
        await db.collection("ac-credit-wallets").replaceOne({ _id: gone }, rest, { upsert: true });
        await db.collection("ac-credit-wallets").deleteOne({ _id: sub });
      }
      await db.collection("ac-credit-purchases").updateMany({ user: sub }, { $set: { user: gone } });

      for (const name of ["boots", "piece-runs"]) {
        const visitor = {
          $or: [
            { "meta.user.sub": sub },
            { "meta.user": sub },
            ...(handles.length ? [{ "meta.handle": { $in: handles } }] : []),
          ],
        };
        await db.collection(name).updateMany(visitor, {
          $unset: { "meta.user": "", "meta.handle": "", "server.ip": "", "server.city": "" },
        });
      }
      if (handles.length) {
        await db
          .collection("os-install-reports")
          .updateMany({ handle: { $in: handles } }, { $unset: { handle: "", ip: "" } });
      }
      return { kidlisp: inv.kidlispKeep.length, wallet: !!wallet };
    },
  },
  {
    // Messages are deleted, not blanked: a blank row with the same ID and
    // time still says who spoke when.
    name: "chat",
    async run({ db, deps, sub }) {
      let messages = 0;
      for (const chat of CHATS) {
        messages += (await db.collection(chat).deleteMany({ user: sub }))?.deletedCount || 0;
        await db.collection(`${chat}-mutes`).deleteMany({ user: sub });
      }
      // The chat servers also hold recent messages in memory.
      const notified = await deps.chat.erase(sub).catch(() => false);
      return { messages, notified: notified === true };
    },
  },
  {
    name: "analytics",
    async run({ deps, sub }) {
      if (!deps.analytics) return { skipped: "not-configured" };
      return { persons: await deps.analytics.forget(sub) };
    },
  },
  {
    name: "atproto",
    async run({ deps, db, sub }) {
      const result = await deps.atproto.deleteAccount(db, sub);
      if (result.deleted) return { deleted: true };
      if (result.reason === "missing-did") return { deleted: false };
      throw new Error("Could not delete the linked ATProto account.");
    },
  },
  {
    // A sotce.net account that shares the handle keeps it; otherwise the
    // handle is released after its quarantine.
    name: "handle",
    async run(ctx) {
      const { db, sub, snap, now } = ctx;
      const handles = db.collection("@handles");
      const doc = await handles.findOne({ _id: sub });
      const handle = String(doc?.handle || snap.handle || "").replace(/^@/, "");
      if (!handle) return { handle: false };

      const inv = inventory(ctx);
      if (inv.sotceSister) {
        const sotceSub = await ctx.deps.auth0.sotceSub(snap.email);
        const sotceId = sotceSub && `sotce-${sotceSub}`;
        const sister = sotceId && (await handles.findOne({ _id: sotceId }));
        if (sister && bareHandle(sister.handle) === bareHandle(handle)) {
          await handles.deleteOne({ _id: sub });
          return { handle: "sotce" };
        }
        if (sotceId && !sister) {
          // Handles are unique, so the old row goes first. A retry after
          // the delete finds no row and rebuilds from the snapshot.
          const taken = await handles.findOne({ handle });
          if (!taken || taken._id === sub) {
            await handles.deleteOne({ _id: sub });
            await handles.insertOne({ _id: sotceId, handle });
            return { handle: "sotce" };
          }
        }
      }
      await db
        .collection(QUARANTINE)
        .updateOne(
          { _id: hash(bareHandle(handle)) },
          { $set: { until: new Date(now.getTime() + HANDLE_QUARANTINE_MS) } },
          { upsert: true },
        );
      await handles.deleteOne({ _id: sub });
      return { handle: "quarantined" };
    },
  },
  {
    name: "account-records",
    async run({ db, sub }) {
      await db.collection("users").deleteOne({ _id: sub });
      await db.collection("@users").deleteOne({ _id: sub });
      return {};
    },
  },
  {
    // After every record that could refill these caches is gone.
    name: "cache",
    async run(ctx) {
      const { deps, sub, snap } = ctx;
      const inv = inventory(ctx);
      const handle = bareHandle(snap.handle);
      const kv = deps.kv;
      await kv.connect();
      if (handle) {
        await kv.del("@handles", handle);
        await kv.del("@handles", `@${handle}`);
        await kv.del("lairk:positions", handle);
      }
      await kv.del("userIDs", sub);
      if (inv.userCode) await kv.del("permahandles", inv.userCode);
      // /api/user caches answers by email, handle and user code.
      const needles = unique([
        snap.email && `email:${snap.email}:*`,
        handle && `email:${handle}:*`,
        handle && `email:@${handle}:*`,
        inv.userCode && `code:${inv.userCode}:*`,
      ]);
      let fields = 0;
      for (const match of needles) {
        for (const field of await kv.scan("userCache", match)) {
          await kv.del("userCache", field);
          fields += 1;
        }
      }
      return { userCache: fields };
    },
  },
  {
    // Last: while the identity exists, a stalled job can still be finished.
    name: "identity",
    async run({ deps, sub }) {
      const removed = await deps.auth0.remove(sub);
      if (!removed) throw new Error("Could not delete the sign-in identity.");
      return {};
    },
  },
];

function inventory(ctx) {
  const inv = ctx.job.inventory;
  if (!inv) throw new Error("Inventory missing.");
  return inv;
}
