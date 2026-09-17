// The consent wall behind oskiewar's "add yourself" door.
//
// A player who wants to become a fighter hands over material — their face,
// how they move, their voice — and the thing that makes that a transaction
// rather than a surrender is that they say in advance which parts the game
// may use, for what, and for how long. This endpoint is where that answer is
// asked and recorded. It is the only path to a generation capability: no
// receipt, no capability, no job.
//
// REGARDE itself is not in this repository and must not be. The prover is
// client code under NDA, so what lives here is a bridge: it shapes the
// player's answer into the DATA_OPERATION × GRANT_CONSENT request the desk
// expects, forwards it, and returns the outcome. Every substantive decision
// is made on the other side of REGARDE_GATEWAY_URL.
//
// Three properties this file exists to hold:
//
//   1. Fail closed. An unset gateway URL, a timeout, a malformed answer, a
//      non-allow outcome — every one of them returns no capability. There is
//      no branch where a generation token is issued on a guess.
//   2. No raw identity crosses the wire. The desk keys grants by pseudonymous
//      sub, and its gateway refuses payloads carrying names, handles, or
//      emails outright. The player's Auth0 subject is hashed with a
//      purpose-scoped salt before it leaves this process, so REGARDE learns a
//      stable oskiewar pseudonym and never the person.
//   3. Nothing defaults on. Voice, movement, biography, marketing,
//      merchandise, and model training are each their own line in the scope.
//      A player who ticks "appearance" has not thereby agreed to be heard,
//      advertised, or trained on.
//
// See the working proposal (vault: regarde/proposals/oskiewar-regarde) for
// the pipeline this gates and the fighter bundle it eventually produces.

import { createHash } from "node:crypto";
import { authorize } from "../../backend/authorization.mjs";

const GATEWAY_TIMEOUT_MS = 6000;
const SCHEMA_VERSION = 1;
const PURPOSE = "oskiewar_fighter_generation";

// The scope vocabulary, split the way the player experiences it. These are
// closed sets on purpose: an unknown output or distribution is a refusal, not
// a pass-through, because a term REGARDE has not registered is a term nobody
// has decided the meaning of.
export const SOURCES = ["appearance", "movement", "voice", "biography"];
export const OUTPUTS = ["portrait", "fighter_mesh", "fighter_animation", "match_audio"];
export const DISTRIBUTION = ["private_preview", "local_gameplay", "online_play",
  "tournament_display"];
// Kept out of the lists above because they are not degrees of the same thing.
// A player can want to be playable online and still never want to be an
// advertisement, and the UI has to be able to say so in those words.
export const SEPARATE = ["marketing", "merchandise", "model_training"];

// Retention is chosen, never inherited. The proposal is explicit that neither
// source nor derived media may default to indefinite, so the absence of a
// choice here is an error rather than a fallback.
const RETENTION = ["bound_to_purpose_scope", "pilot_deadline"];

function fail(statusCode, message) {
  return {
    statusCode,
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ outcome: "refuse", capability: null, message }),
  };
}

// A subset check that treats "not a list" and "contains something unregistered"
// as the same failure, because both mean the caller and the schema disagree.
function subsetOf(values, vocabulary) {
  if (!Array.isArray(values)) return null;
  const picked = [...new Set(values.map((value) => String(value)))];
  return picked.every((value) => vocabulary.includes(value)) ? picked : null;
}

// The pseudonym. Salted so the same person is a different subject to a
// different purpose — an oskiewar grant cannot be correlated against any other
// REGARDE operation AC might one day run — and hashed so this side of the wire
// is the last place the Auth0 subject exists.
function pseudonym(sub, salt) {
  return "sub_" + createHash("sha256").update(`${salt}:${PURPOSE}:${sub}`)
    .digest("hex").slice(0, 32);
}

// The frozen fields are the grant. Once this object is hashed into a receipt
// it is what the player actually agreed to, so it is built in one place and
// never edited downstream — a generation worker reads it, it does not amend it.
export function frozenFields(scope) {
  return {
    data_class: "digital_replica_source",
    operation_kind: "GRANT_CONSENT",
    purpose_scope: {
      purpose: PURPOSE,
      source: scope.source,
      outputs: scope.outputs,
      distribution: scope.distribution,
      transform: "stylized",
      ...Object.fromEntries(SEPARATE.map((key) => [key, scope[key] === true])),
    },
    retention_constraint: scope.retention,
    schema_version: SCHEMA_VERSION,
  };
}

// Reading the player's answer. Everything is required and nothing is inferred:
// a missing field is a malformed ask, not an empty permission.
export function readScope(body) {
  const source = subsetOf(body?.source, SOURCES);
  if (!source?.length) return { error: "pick at least one kind of material" };
  const outputs = subsetOf(body?.outputs, OUTPUTS);
  if (!outputs?.length) return { error: "pick at least one thing to make" };
  const distribution = subsetOf(body?.distribution, DISTRIBUTION);
  if (!distribution?.length) return { error: "pick where it may appear" };
  if (!RETENTION.includes(body?.retention))
    return { error: "choose how long the material may be kept" };
  // The two couplings the proposal names by hand, because they are the ones a
  // player would reasonably assume and be wrong about. Appearance does not
  // imply voice; a voice output without the voice material is an ask for
  // something nobody offered.
  if (outputs.includes("match_audio") && !source.includes("voice"))
    return { error: "match audio needs a voice sample you have offered" };
  if ((outputs.includes("fighter_mesh") || outputs.includes("fighter_animation")) &&
      !source.includes("appearance"))
    return { error: "a body needs appearance references" };
  const scope = { source, outputs, distribution, retention: body.retention };
  for (const key of SEPARATE) scope[key] = body?.[key] === true;
  return { scope };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS")
    return { statusCode: 204, headers: {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Headers": "Authorization, Content-Type",
      "Access-Control-Allow-Methods": "POST, OPTIONS" } };
  if (event.httpMethod !== "POST") return fail(405, "POST only.");

  // Anonymous play is the front door of this game and always will be, but it
  // is not a door into a grant: a receipt has to belong to somebody who can
  // later come back and withdraw it.
  const user = await authorize(event.headers);
  if (!user?.sub) return fail(401, "Sign in before you give anything away.");

  let body;
  try { body = JSON.parse(event.body || "{}"); }
  catch { return fail(400, "Unreadable answer."); }

  const { scope, error } = readScope(body);
  if (error) return fail(400, error);

  const gateway = process.env.REGARDE_GATEWAY_URL;
  const salt = process.env.REGARDE_SUBJECT_SALT;
  const gatewayToken = process.env.REGARDE_GATEWAY_TOKEN;
  // Unconfigured is refused rather than waved through. This is the branch that
  // would otherwise quietly become "generation works, the wall is decorative."
  if (!gateway || !salt || !gatewayToken)
    return fail(503, "The consent desk is not reachable right now.");

  const request = {
    venue: "oskiewar",
    subject: pseudonym(user.sub, salt),
    operation_type: "DATA_OPERATION",
    frozen_fields: frozenFields(scope),
    // Idempotent per subject per exact scope: asking the same question twice
    // returns the same receipt instead of littering the chain with duplicates,
    // while any change to the scope is a genuinely new ask.
    idempotency_key: createHash("sha256")
      .update(JSON.stringify([pseudonym(user.sub, salt), frozenFields(scope)]))
      .digest("hex").slice(0, 32),
    operation_descriptor: {
      purpose: PURPOSE,
      description: "Generate an oskiewar fighter from player-supplied material.",
    },
  };

  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), GATEWAY_TIMEOUT_MS);
  let answer;
  try {
    const upstream = await fetch(gateway, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        // The gate treats a deployer relaying a player's choice as exactly
        // that: a deployer. The token proves which deployer is asking, never
        // that the person answered — the gate says so in its own standing
        // limits, and so does this comment, so neither of us can forget it.
        ...(process.env.REGARDE_GATEWAY_TOKEN
          ? { Authorization: `Bearer ${process.env.REGARDE_GATEWAY_TOKEN}` }
          : {}),
      },
      body: JSON.stringify(request),
      signal: controller.signal,
    });
    if (!upstream.ok) {
      console.log(`🚪 regarde gateway ${upstream.status}`);
      return fail(502, "The consent desk refused the ask.");
    }
    answer = await upstream.json();
  } catch (err) {
    console.log(`🚪 regarde gateway unreachable: ${err.message}`);
    return fail(504, "The consent desk did not answer.");
  } finally { clearTimeout(timer); }

  // "allow" is the only outcome that opens anything. "edit" means the desk
  // countered with narrower terms the player would have to accept as a fresh
  // ask, "deny" and "refuse" mean no. None of the three produces a capability
  // here, and the client is expected to show the counter rather than retry.
  const outcome = String(answer?.outcome || "refuse");
  const receipt = answer?.receipt
    ? {
        decision: answer.receipt.decision,
        hash: answer.receipt.hash ?? answer.receipt.jti ?? null,
        issued_at: answer.receipt.iat ?? null,
        expires_at: answer.receipt.exp ?? null,
      }
    : null;

  return {
    statusCode: 200,
    headers: { "Content-Type": "application/json",
      "Cache-Control": "no-store", "Access-Control-Allow-Origin": "*" },
    body: JSON.stringify({
      outcome,
      receipt,
      scope: request.frozen_fields.purpose_scope,
      counter: answer?.counter?.frozen_fields?.purpose_scope ?? null,
      // The desk mints this on an allow and only on an allow — an edit is an
      // offer the player has not taken, and authority on the strength of terms
      // nobody accepted is the failure this wall exists to prevent.
      //
      // It is narrow on purpose: the approved source categories and outputs,
      // and nothing about distribution, marketing, merchandise or training.
      // Those govern a finished bundle, not what a worker may touch. It
      // expires in minutes, because a capability is for one job run.
      //
      // No worker consumes it yet. It is passed through rather than withheld
      // because the next stage binds to it, and because a player who has just
      // been told "allowed, and recorded" should be able to see the shape of
      // what they permitted.
      capability: answer?.capability ?? null,
    }),
  };
}
