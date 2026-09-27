// Account deletion wiring, 2026.09.26
// The production `deps` for backend/account-deletion.mjs: Mongo, Redis,
// Spaces, Auth0, Stripe, PostHog, the chat servers and mail. Services that
// are not configured on this host are left out, and the steps that use
// them record "not-configured" instead of pretending to have run.

import {
  S3Client,
  ListObjectsV2Command,
  DeleteObjectsCommand,
} from "@aws-sdk/client-s3";
import * as KeyValue from "./kv.mjs";
import { email } from "./email.mjs";
import { deleteAtprotoAccount } from "./at.mjs";
import {
  deleteUser,
  setUserBlocked,
  userIDFromEmail,
  forgetAuthorizations,
} from "./authorization.mjs";
import { setAccountLock, clearAccountLock, holdDeletedAccountLock } from "./account-lock.mjs";
import { sidecar, kidlispDatomicEnabled } from "./kidlisp-sidecar.mjs";
import { shell } from "./shell.mjs";

const dev = process.env.CONTEXT === "dev";
const SITE = dev ? "https://localhost:8888" : "https://aesthetic.computer";

function spaces(endpoint, key, secret) {
  const host = String(endpoint || "sfo3.digitaloceanspaces.com").replace(/^https?:\/\//, "");
  return new S3Client({
    endpoint: `https://${host}`,
    region: "us-east-1",
    credentials: { accessKeyId: key, secretAccessKey: secret },
  });
}

function buckets() {
  const key = process.env.ART_KEY || process.env.DO_SPACES_KEY;
  const secret = process.env.ART_SECRET || process.env.DO_SPACES_SECRET;
  return {
    user: {
      client: spaces(process.env.USER_ENDPOINT, key, secret),
      name: process.env.USER_SPACE_NAME,
    },
    art: {
      client: spaces(process.env.ART_ENDPOINT, key, secret),
      name: process.env.ART_SPACE_NAME || "art-aesthetic-computer",
    },
    tapes: {
      client: spaces(process.env.DO_SPACES_ENDPOINT, key, secret),
      name: "at-blobs-aesthetic-computer",
    },
  };
}

async function deleteKeys({ client, name }, keys) {
  if (!name) throw new Error("Storage bucket is not configured.");
  let deleted = 0;
  for (let i = 0; i < keys.length; i += 1000) {
    const batch = keys.slice(i, i + 1000);
    const result = await client.send(
      new DeleteObjectsCommand({
        Bucket: name,
        Delete: { Objects: batch.map((Key) => ({ Key })), Quiet: true },
      }),
    );
    if (result.Errors?.length) throw new Error("Could not delete all stored files.");
    deleted += batch.length;
  }
  return deleted;
}

async function listPrefix(bucket, prefix) {
  if (!bucket.name) throw new Error("Storage bucket is not configured.");
  const files = [];
  let ContinuationToken;
  do {
    const listed = await bucket.client.send(
      new ListObjectsV2Command({ Bucket: bucket.name, Prefix: prefix, ContinuationToken }),
    );
    for (const { Key, Size, LastModified } of listed.Contents || []) {
      files.push({
        key: Key,
        size: Size,
        modified: LastModified,
        url: `https://${bucket.name}.sfo3.digitaloceanspaces.com/${Key.split("/").map(encodeURIComponent).join("/")}`,
      });
    }
    ContinuationToken = listed.IsTruncated ? listed.NextContinuationToken : undefined;
  } while (ContinuationToken);
  return files;
}

async function deletePrefix(bucket, prefix) {
  if (!bucket.name) throw new Error("Storage bucket is not configured.");
  let deleted = 0;
  // Deleting while listing shifts the listing, so each page restarts from
  // the top until nothing is left under the prefix.
  for (;;) {
    const listed = await bucket.client.send(
      new ListObjectsV2Command({ Bucket: bucket.name, Prefix: prefix, MaxKeys: 1000 }),
    );
    const keys = (listed.Contents || []).map(({ Key }) => Key);
    if (!keys.length) return deleted;
    deleted += await deleteKeys(bucket, keys);
  }
}

async function stripeClient() {
  const key = dev ? process.env.STRIPE_API_TEST_PRIV_KEY : process.env.STRIPE_API_PRIV_KEY;
  if (!key) return null;
  const { default: Stripe } = await import("stripe");
  const stripe = Stripe(key);
  return {
    async forget(address) {
      const customers = await stripe.customers.list({ email: address, limit: 100 });
      for (const customer of customers.data) {
        // Deleting a customer cancels its subscriptions and removes its
        // payment methods; the charges stay with Stripe for the books.
        await stripe.customers.del(customer.id);
      }
      return customers.data.length;
    },
  };
}

function posthog() {
  const key = process.env.POSTHOG_PERSONAL_API_KEY;
  const project = process.env.POSTHOG_PROJECT_ID;
  if (!key || !project) return null;
  const host = process.env.POSTHOG_API_HOST?.includes("eu.")
    ? "https://eu.posthog.com"
    : "https://us.posthog.com";
  const headers = { Authorization: `Bearer ${key}` };
  return {
    async forget(sub) {
      const base = `${host}/api/projects/${encodeURIComponent(project)}/persons`;
      const found = await fetch(`${base}/?distinct_id=${encodeURIComponent(sub)}`, { headers });
      if (!found.ok) throw new Error(`PostHog answered ${found.status}`);
      const { results = [] } = await found.json();
      for (const person of results) {
        const gone = await fetch(`${base}/${encodeURIComponent(person.id)}/?delete_events=true`, {
          method: "DELETE",
          headers,
        });
        if (!gone.ok && gone.status !== 404) throw new Error(`PostHog answered ${gone.status}`);
      }
      return results.length;
    },
  };
}

// Pinata, with the credentials keep-mint.mjs uses (the `secrets` collection).
function pinata(database) {
  return {
    async unpin(cid) {
      const creds = await database.db.collection("secrets").findOne({ _id: "pinata" });
      if (!creds?.apiKey) throw new Error("Pinata credentials not found.");
      const response = await fetch(`https://api.pinata.cloud/pinning/unpin/${encodeURIComponent(cid)}`, {
        method: "DELETE",
        headers: { pinata_api_key: creds.apiKey, pinata_secret_api_key: creds.apiSecret },
      });
      // Not pinned (any more) counts as unpinned, so a retry can finish.
      if (!response.ok && response.status !== 404) throw new Error(`Pinata answered ${response.status}`);
      return true;
    },
  };
}

// The chat servers keep recent messages in memory; this asks each one to
// drop the account's messages and handle (session-server/chat-manager.mjs).
async function eraseFromChat(sub) {
  const { got } = await import("got");
  const servers = dev
    ? ["https://localhost:8083/log", "https://localhost:8085/log"]
    : ["https://chat-system.aesthetic.computer/log", "https://chat-clock.aesthetic.computer/log"];
  let all = true;
  for (const url of servers) {
    try {
      await got.post(url, {
        json: { action: "account:erase", users: [sub], from: "account-deletion", when: new Date() },
        headers: { Authorization: `Bearer ${process.env.LOGGER_KEY}` },
        https: { rejectUnauthorized: !dev },
        timeout: { request: 10000 },
      });
    } catch (error) {
      shell.log(`🪦 Chat erase failed at ${url}: ${error.message}`);
      all = false;
    }
  }
  return all;
}

const date = (when) =>
  new Date(when).toLocaleDateString("en-US", { year: "numeric", month: "long", day: "numeric" });

const mail = {
  scheduled({ to, handle, purgeAfter, restoreToken }) {
    const name = handle ? `@${handle}` : "your Aesthetic Computer account";
    const link = `${SITE}/api/delete-erase-and-forget-me?restore=${encodeURIComponent(restoreToken)}`;
    return email({
      to,
      subject: `${name} will be deleted on ${date(purgeAfter)}`,
      text:
        `You asked to delete ${name}.\n\n` +
        `The account is locked now. On ${date(purgeAfter)} it will be deleted for good.\n\n` +
        `To keep it, open this link before then:\n${link}\n\n` +
        `If you did not ask for this, open the link and then change your password.\n`,
    });
  },
  completed({ to }) {
    return email({
      to,
      subject: "Your Aesthetic Computer account is deleted",
      text:
        "Your Aesthetic Computer account and its data have been deleted.\n\n" +
        "What we keep, and for how long, is at https://aesthetic.computer/privacy-policy.html\n",
    });
  },
};

export async function productionDeps(database) {
  return {
    db: database.db,
    log: (...args) => shell.log(...args),
    kv: {
      connect: () => KeyValue.connect(),
      del: (collection, key) => KeyValue.del(collection, key),
      scan: (collection, match) => KeyValue.scan(collection, match),
    },
    storage: (() => {
      const all = buckets();
      return {
        deleteKeys: (bucket, keys) => deleteKeys(all[bucket], keys),
        deletePrefix: (bucket, prefix) => deletePrefix(all[bucket], prefix),
        list: (bucket, prefix) => listPrefix(all[bucket], prefix),
      };
    })(),
    stripe: await stripeClient(),
    analytics: posthog(),
    chat: { erase: eraseFromChat },
    ipfs: pinata(database),
    bluesky: {
      async deletePost(rkey) {
        const { deleteMoodFromBluesky } = await import("./bluesky-mirror.mjs");
        return deleteMoodFromBluesky(database, rkey);
      },
    },
    sidecar: kidlispDatomicEnabled()
      ? { enabled: true, erase: (args) => sidecar.eraseUser(args) }
      : { enabled: false },
    atproto: { deleteAccount: deleteAtprotoAccount },
    auth0: {
      async sotceSister(address) {
        const found = await userIDFromEmail(address, "sotce");
        return !!(found?.userID && found?.email_verified);
      },
      async sotceSub(address) {
        return (await userIDFromEmail(address, "sotce"))?.userID || null;
      },
      async remove(sub) {
        return (await deleteUser(sub))?.success === true;
      },
    },
    async lock(sub, until) {
      await setAccountLock(sub, until);
      forgetAuthorizations(sub);
      await setUserBlocked(sub, true);
    },
    async unlock(sub, { identityDeleted = false } = {}) {
      if (identityDeleted) return holdDeletedAccountLock(sub);
      // Unblock first: a restore must not report success while Auth0 still
      // refuses the sign-in. Either failure leaves the restore retryable.
      if (!(await setUserBlocked(sub, false))) throw new Error("Could not unblock the sign-in.");
      await clearAccountLock(sub);
    },
    mail,
  };
}
