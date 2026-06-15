// device-creds.mjs — single source of truth for per-user OS provisioning
// secrets: the Claude OAuth token + GitHub PAT a flashed AC Native device
// needs. These are deliberately stored in their OWN collection, keyed by the
// Auth0 `sub`, and kept OFF the `@handles` / `users` documents so they never
// ride along on the handle/profile lookups that enrich public content
// (paintings, moods, tv, kidlisp feeds, etc.). Every place that needs the
// creds goes through this helper, so the storage location lives in exactly
// one file.

export const DEVICE_CREDS_COLLECTION = "device-creds";

// Fetch a user's device creds, or null. Returns the raw doc:
// { _id: sub, claudeToken, githubPat, claudeTokenUpdated, githubPatUpdated }
export async function getDeviceCreds(db, sub) {
  if (!sub) return null;
  return db.collection(DEVICE_CREDS_COLLECTION).findOne({ _id: sub });
}

// Upsert a user's device creds. Only writes the fields that are provided and
// pass their format check, stamping a matching *Updated time. Returns the list
// of fields actually saved.
export async function setDeviceCreds(db, sub, { claudeToken, githubPat } = {}) {
  if (!sub) return { saved: [] };
  const updates = {};
  if (claudeToken && claudeToken.startsWith("sk-ant-")) {
    updates.claudeToken = claudeToken;
    updates.claudeTokenUpdated = new Date();
  }
  if (githubPat && githubPat.startsWith("ghp_")) {
    updates.githubPat = githubPat;
    updates.githubPatUpdated = new Date();
  }
  if (Object.keys(updates).length === 0) return { saved: [] };
  await db
    .collection(DEVICE_CREDS_COLLECTION)
    .updateOne({ _id: sub }, { $set: updates }, { upsert: true });
  return { saved: Object.keys(updates).filter((k) => !k.endsWith("Updated")) };
}
