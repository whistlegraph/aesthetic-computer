// The deploy preflight's two pure parts: reading an env, and never repeating
// a password back out. The connection attempt itself needs a database and is
// exercised by the deploy.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import { readEnv, redactUri } from "../../lith/verify-env.mjs";

const source = await readFile(
  new URL("../../lith/verify-env.mjs", import.meta.url), "utf8");
const deploy = await readFile(
  new URL("../../lith/deploy.fish", import.meta.url), "utf8");

test("env values survive quotes, comments, and equals signs", () => {
  const env = readEnv([
    "# a comment",
    "",
    "MONGODB_NAME=aesthetic",
    'MONGODB_CONNECTION_STRING="mongodb://user:pa=ss@host:27017/db?authSource=db"',
    "  SPACED = value ",
    "NOT_AN_ENTRY",
    "=leading",
  ].join("\n"));
  assert.equal(env.MONGODB_NAME, "aesthetic");
  // A password can hold `=`, so only the first one separates key from value.
  assert.equal(env.MONGODB_CONNECTION_STRING,
    "mongodb://user:pa=ss@host:27017/db?authSource=db");
  assert.equal(env["SPACED "], "value");
  assert.equal("NOT_AN_ENTRY" in env, false);
  assert.equal("" in env, false);
});

test("the password is never repeated back, even to say it was wrong", () => {
  const secret = "hunter2SuperSecretPassword";
  const uri = `mongodb://aesthetic_app:${secret}@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic`;
  const shown = redactUri(uri);
  assert.doesNotMatch(shown, new RegExp(secret));
  assert.match(shown, /mongodb:\/\/aesthetic_app:\*\*\*@silo/);
  // The rest of the string still has to be readable, or the message cannot
  // tell you which database it failed against.
  assert.match(shown, /silo\.aesthetic\.computer:27017\/aesthetic/);
  // A raw `@` inside the password is malformed for a real URI, but an env can
  // still hold one — and the userinfo then ends at the *last* `@`, not the
  // first. Stopping at the first would print the tail of the password.
  assert.equal(redactUri("mongodb://user:pa@ss@host:27017/db"),
    "mongodb://user:***@host:27017/db");
  // Nothing to redact is left alone rather than mangled.
  assert.equal(redactUri("mongodb://host:27017/db"), "mongodb://host:27017/db");
  assert.equal(redactUri("not a uri"), "not a uri");
});

test("an env that cannot be verified is shipped, not blocked", () => {
  // Every deploy before this check existed shipped unverified. A machine
  // without the driver must not lose the ability to deploy.
  assert.match(source, /driver not installed here/);
  const missingDriver = source.slice(source.indexOf('("mongodb")'),
    source.indexOf("const client = new MongoClient"));
  assert.match(missingDriver, /shipping this env unverified[\s\S]{0,40}return 0;/);
});

test("deploy checks the credential before upload and the database after", () => {
  const check = deploy.indexOf("lith/verify-env.mjs");
  const upload = deploy.indexOf("scp -i $SSH_KEY $SERVICE_ENV");
  const restart = deploy.indexOf("systemctl restart lith");
  const purge = deploy.indexOf("Purging Cloudflare cache");
  const probe = deploy.indexOf("Verifying the database is answering");
  assert.ok(check > 0 && upload > check,
    "the credential is tried before the env is uploaded");
  assert.ok(probe > restart, "the database is probed after the restart");
  // And after the cache purge, so a failing probe cannot skip work that was
  // already earned. The probe reports; it does not gate.
  assert.ok(probe > purge, "the probe runs after the purge, not instead of it");
  assert.match(deploy, /Refusing to upload an environment that cannot reach its database/);
  assert.match(deploy, /Deployed, but the database is not answering/);
  // lith.aesthetic.computer is the SSH target and serves no vhost, so probing
  // it measures nothing. This cost one confusing red HTTP 000.
  assert.match(deploy, /set DB_PROBE_HOST "aesthetic\.computer"/);
  assert.doesNotMatch(deploy, /https:\/\/\$TARGET_HOST\/api\//);
});
