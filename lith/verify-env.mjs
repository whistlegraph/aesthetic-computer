// Verify a lith env file before it is allowed near production.
//
// `deploy.fish` copies the vault's env onto the server and restarts lith, so
// the env is the one thing in a deploy that ships without ever being tried
// first. On 2026-08-09 a deploy carrying a stale MongoDB password did exactly
// that: the restart at 05:14:53 UTC came back unable to authenticate, and
// every database-backed endpoint on aesthetic.computer and oskiewar.com
// answered 500 for the next fourteen hours. Nothing in the pipeline noticed,
// because the code was fine — only the credential was dead.
//
// So try the credential from here, before the upload. A password that cannot
// open the database on this machine will not open it on the server either.
//
// Usage: node lith/verify-env.mjs <path-to-env>
// Exit 0 when the env is safe to ship, 1 when it is not, and 0 with a loud
// warning when the check cannot be made at all — an unverifiable env is the
// state every deploy before this one was already in, so it must not become a
// new way to be unable to deploy.

import { readFile } from "node:fs/promises";
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";

const CONNECT_TIMEOUT_MS = 8000;
const red = (text) => `\x1b[0;31m${text}\x1b[0m`;
const green = (text) => `\x1b[0;32m${text}\x1b[0m`;
const yellow = (text) => `\x1b[1;33m${text}\x1b[0m`;

// Values may be bare or quoted, and a value can hold `=` — split on the first.
export function readEnv(text) {
  const env = {};
  for (const line of text.split("\n")) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith("#")) continue;
    const split = trimmed.indexOf("=");
    if (split < 1) continue;
    env[trimmed.slice(0, split)] = trimmed.slice(split + 1).trim()
      .replace(/^["']|["']$/g, "");
  }
  return env;
}

// Never print the password back out, not even to say it was wrong. Walked
// rather than pattern-matched, because the userinfo ends at the *last* `@` in
// the authority — a password carrying a raw one would otherwise have its tail
// printed by a regex that stopped at the first.
export function redactUri(uri) {
  const text = String(uri);
  const scheme = text.indexOf("://");
  if (scheme < 0) return text;
  const start = scheme + 3;
  const slash = text.indexOf("/", start);
  const authority = text.slice(start, slash < 0 ? text.length : slash);
  const at = authority.lastIndexOf("@");
  if (at < 0) return text;
  const colon = authority.slice(0, at).indexOf(":");
  if (colon < 0) return text;
  return text.slice(0, start + colon) + ":***" + text.slice(start + at);
}

// Which key names the env is expected to carry, if a manifest sits beside it.
// Names only: the manifest lists what production needs, never what it is.
export function missingKeys(env, manifest) {
  const wanted = manifest.split("\n")
    .map((line) => line.trim())
    .filter((line) => line && !line.startsWith("#"));
  return wanted.filter((key) => !env[key]);
}

// Only the CLI below runs the check. The pure helpers above are imported by
// the tests, and importing this file must not open a connection, print
// anything, or take the importing process down with it.
async function main(path) {
  if (!path) {
    console.error(red("verify-env: no env file given"));
    return 1;
  }

  let env;
  try {
    env = readEnv(await readFile(path, "utf8"));
  } catch (error) {
    console.error(red(`verify-env: cannot read ${path}: ${error.message}`));
    return 1;
  }

  // A stale env does not only hold the wrong password — it can also be a
  // *subset* of the real one. The plaintext on the deploy machine had quietly
  // lost ten keys (push, APNs, device provisioning, the nela OIDC client) and
  // deploy.fish uploaded that subset verbatim, so production ran without them
  // for weeks. Nothing failed loudly; those features simply were not there.
  try {
    const missing = missingKeys(env, await readFile(`${path}.keys`, "utf8"));
    if (missing.length) {
      console.error(red(`   env is missing ${missing.length} key(s) the ` +
        `manifest requires:`));
      for (const key of missing) console.error(red(`     ${key}`));
      console.error(yellow("   This env is a subset of what production needs. " +
        "Deploying it would silently switch those features off. Merge from " +
        `the encrypted copy, or update ${path}.keys if a key is truly gone.`));
      return 1;
    }
  } catch {
    // No manifest beside this env: nothing to compare against, carry on.
  }

  const uri = env.MONGODB_CONNECTION_STRING;
  const name = env.MONGODB_NAME;
  if (!uri || !name) {
    console.error(red(
      "verify-env: MONGODB_CONNECTION_STRING or MONGODB_NAME missing"));
    return 1;
  }

  let MongoClient;
  try {
    ({ MongoClient } = createRequire(import.meta.url)("mongodb"));
  } catch {
    console.warn(yellow("verify-env: mongodb driver not installed here; " +
      "shipping this env unverified."));
    return 0;
  }

  const client = new MongoClient(uri, {
    serverSelectionTimeoutMS: CONNECT_TIMEOUT_MS,
    connectTimeoutMS: CONNECT_TIMEOUT_MS,
  });
  try {
    await client.connect();
    await client.db(name).command({ ping: 1 });
    console.log(green(`   mongo: ${redactUri(uri)} authenticates`));
    return 0;
  } catch (error) {
    const failure = error?.codeName || error?.message || "unknown error";
    console.error(red(`   mongo: ${redactUri(uri)} -> ${failure}`));
    if (/auth/i.test(failure))
      console.error(yellow("   The password in this env is not the one the " +
        "database holds. Deploying it would restart lith into a total " +
        "database outage, which is how 2026-08-09 happened."));
    return 1;
  } finally {
    await client.close().catch(() => {});
  }
}

if (process.argv[1] &&
    import.meta.url === pathToFileURL(process.argv[1]).href)
  process.exit(await main(process.argv[2]));
