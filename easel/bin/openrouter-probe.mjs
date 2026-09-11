#!/usr/bin/env node
// openrouter-probe — answer the one question the OpenRouter memo could not.
//
// The whole per-handle tier rests on a single unverified claim: that a key
// minted with `limit` (USD) is *refused* by OpenRouter once it has spent that
// much, rather than merely accounted against it. The docs expose
// `limit_remaining` and say a credit limit is set per key, but never state what
// happens to a request that would exceed it. If the limit is only accounting,
// then a runaway or a hostile user spends real money and we find out later,
// which is a different product with different guarantees.
//
// So: mint a key with a limit small enough that overrunning it is cheap, spend
// past it deliberately, and watch what comes back. A refusal is the answer we
// want. A 200 is a finding we need before building anything on top.
//
// ANSWERED, 2026-09-11: it is a hard stop. The call past the cap returns
//
//   HTTP 403  {"error":{"message":"Key limit exceeded (total limit)","code":403}}
//
// with one wrinkle worth keeping: enforcement runs *ahead* of the number you can
// read. The refusal landed while GET /api/v1/key still reported $0.000198 of a
// $0.0005 cap remaining, because the usage counter is eventually consistent —
// it sits flat for a few calls and then jumps. So `limit_remaining` is an
// observation, never a prediction, and the error leans toward refusing early
// rather than overspending. For a cap that exists to bound a bill, early is the
// right direction to be wrong in.
//
// Reads credentials from the environment or from the vault's lith .env — never
// takes them on the command line, where they would land in shell history.
//
//   node easel/bin/openrouter-probe.mjs            # mint, overrun, report, revoke
//   node easel/bin/openrouter-probe.mjs --keep     # leave the probe key behind
//
// Costs well under a cent: the cap is $0.0005 and the probe stops the moment it
// has an answer, then revokes the key it minted.

import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = join(HERE, "..", "..");
const ENV_FILE = join(REPO, "vault", "lith", ".env");
const API = "https://openrouter.ai/api/v1";

// A limit small enough to actually reach. $0.02 was the first guess and it is
// far too generous: gpt-4o-mini at 500 max_tokens costs about $0.0001 a call, so
// forty calls spent a quarter of it and proved nothing. $0.0005 is crossed in
// four or five.
const LIMIT_USD = 0.0005;
// A model cheap enough that a handful of calls stays under a cent, and long
// enough output that the spend actually moves.
const MODEL = process.env.OPENROUTER_PROBE_MODEL || "openai/gpt-4o-mini";
const MAX_CALLS = 40;

function fromVault(name) {
  if (process.env[name]) return process.env[name];
  try {
    for (const line of readFileSync(ENV_FILE, "utf8").split("\n")) {
      const match = line.match(/^\s*(?:export\s+)?([A-Z0-9_]+)\s*=\s*(.*)$/);
      if (match && match[1] === name) {
        return match[2].trim().replace(/^["']|["']$/g, "");
      }
    }
  } catch {}
  return "";
}

const provisioning = fromVault("OPENROUTER_PROVISIONING_KEY");
if (!provisioning) {
  console.error(
    "No OPENROUTER_PROVISIONING_KEY.\n" +
      `Add it to ${ENV_FILE} (or export it) and run again.\n` +
      "It is the key from openrouter.ai/settings/provisioning-keys — NOT an ordinary API key.",
  );
  process.exit(1);
}

if (process.argv.includes("--help") || process.argv.includes("-h")) {
  // This spends real money and mints a real key, so an unrecognised flag must
  // not fall through into a live run — which is exactly what `--help` did once.
  console.log("openrouter-probe — mint a $0.0005 key, overrun it, report, revoke.\n");
  console.log("  node easel/bin/openrouter-probe.mjs         run it");
  console.log("  node easel/bin/openrouter-probe.mjs --keep  leave the probe key behind");
  process.exit(0);
}

const unknown = process.argv.slice(2).filter((a) => a !== "--keep");
if (unknown.length) {
  console.error(`unknown argument: ${unknown[0]} (see --help)`);
  process.exit(1);
}

const keep = process.argv.includes("--keep");
const auth = { Authorization: `Bearer ${provisioning}`, "Content-Type": "application/json" };

async function api(path, options = {}) {
  const response = await fetch(`${API}${path}`, { ...options, headers: { ...auth, ...options.headers } });
  const text = await response.text();
  let body;
  try { body = JSON.parse(text); } catch { body = text; }
  return { status: response.status, body };
}

console.log(`minting a probe key with a $${LIMIT_USD} limit…`);
const created = await api("/keys", {
  method: "POST",
  body: JSON.stringify({
    name: `easel-probe-${Date.now()}`,
    limit: LIMIT_USD,
    // Tagged so a key left behind by a crashed run is identifiable later.
    external: { user: "easel-probe" },
  }),
});

if (created.status !== 200 && created.status !== 201) {
  console.error(`could not mint a key (HTTP ${created.status}):`);
  console.error(JSON.stringify(created.body, null, 2).slice(0, 800));
  process.exit(1);
}

// The plaintext key is returned exactly once, at creation.
const secret = created.body?.key || created.body?.data?.key;
const keyHash = created.body?.data?.hash || created.body?.hash || "";
if (!secret) {
  console.error("minted, but no plaintext key came back — cannot probe.");
  console.error(JSON.stringify(created.body, null, 2).slice(0, 800));
  process.exit(1);
}
console.log(`minted. hash=${keyHash || "(none reported)"}`);

async function spend(n) {
  const response = await fetch(`${API}/chat/completions`, {
    method: "POST",
    headers: { Authorization: `Bearer ${secret}`, "Content-Type": "application/json" },
    body: JSON.stringify({
      model: MODEL,
      messages: [{ role: "user", content: `Write 200 words about the number ${n}.` }],
      max_tokens: 400,
    }),
  });
  const text = await response.text();
  let usage = null;
  try { usage = JSON.parse(text)?.usage || null; } catch {}
  return { status: response.status, usage, text };
}

let refusedAt = null;
let calls = 0;
let totalTokens = 0;

for (let n = 1; n <= MAX_CALLS; n += 1) {
  const result = await spend(n);
  calls = n;
  totalTokens += result.usage?.total_tokens || 0;

  if (result.status !== 200) {
    refusedAt = n;
    console.log(`\ncall ${n}: HTTP ${result.status} — refused`);
    console.log(String(result.text).slice(0, 400));
    break;
  }

  const check = await api(`/key`, { headers: { Authorization: `Bearer ${secret}` } });
  const usage = check.body?.data?.usage;
  const remaining = check.body?.data?.limit_remaining;
  console.log(
    `call ${n}: ok · usage=$${usage ?? "?"} remaining=$${remaining ?? "?"} tokens=${result.usage?.total_tokens ?? "?"}`,
  );

  if (typeof remaining === "number" && remaining <= 0) {
    // The interesting moment: the balance says empty. Does the next call go
    // through anyway? That single request is the whole experiment.
    console.log("\nlimit reported exhausted — testing one more call past it…");
    const past = await spend(999);
    if (past.status === 200) {
      console.log(`\n⚠️  ANSWER: the limit is ACCOUNTING ONLY.`);
      console.log(`    A call after limit_remaining <= 0 returned HTTP 200.`);
      console.log(`    Per-key limits cannot be relied on to stop spend.`);
    } else {
      console.log(`\n✅ ANSWER: the limit is a HARD STOP.`);
      console.log(`    The call past the limit returned HTTP ${past.status}.`);
      console.log(String(past.text).slice(0, 300));
    }
    refusedAt = past.status === 200 ? null : n + 1;
    break;
  }
}

if (refusedAt === null && calls === MAX_CALLS) {
  console.log(`\ninconclusive after ${MAX_CALLS} calls (~${totalTokens} tokens).`);
  console.log("The limit was never reported exhausted. Try a smaller limit or a pricier model.");
}

if (!keep && keyHash) {
  const deleted = await api(`/keys/${keyHash}`, { method: "DELETE" });
  console.log(`\nprobe key revoked (HTTP ${deleted.status}).`);
} else if (keep) {
  console.log("\n--keep: probe key left in place. Revoke it at openrouter.ai/settings/keys.");
} else {
  console.log("\n⚠️  no key hash returned — revoke the probe key by hand at openrouter.ai/settings/keys.");
}
