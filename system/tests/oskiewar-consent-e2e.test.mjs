// The consent wall, over the wire, as a signed-in player meets it.
//
// `oskiewar-consent-gateway.test.mjs` stubs auth and the desk to prove the
// branches. This file proves the deployed thing: a real Auth0 bearer token
// against a real host, asserting the contract that must hold no matter how
// REGARDE is configured on the other side.
//
// The invariant under test is deliberately not "the wall returns 503". That is
// only true while REGARDE_GATEWAY_URL is unset, and the point of the wall is
// that it is safe in both states. What is asserted instead is the promise the
// endpoint actually makes: **no request, in any configuration, comes back
// holding a capability**, and no response echoes the player's Auth0 subject.
// The suite reports which state it observed so a run doubles as a status check.
//
// Auth: `~/.ac-token` (written by `node tezos/ac-login.mjs`), or AC_TOKEN.
// Target: AC_CONSENT_URL, default production.
//
// Run: npm run test:consent:e2e
import assert from "node:assert/strict";
import test, { before } from "node:test";
import { readFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";

const URL_UNDER_TEST = process.env.AC_CONSENT_URL
  ?? "https://oskiewar.com/api/oskiewar-consent";

const minimal = {
  source: ["appearance"],
  outputs: ["portrait"],
  distribution: ["private_preview"],
  retention: "bound_to_purpose_scope",
};

let token;
let subject;

// The token file is JSON written by ac-login; tolerate a bare token too, since
// AC_TOKEN is likely to be pasted rather than generated.
async function loadToken() {
  if (process.env.AC_TOKEN) return process.env.AC_TOKEN.trim();
  try {
    const raw = await readFile(join(homedir(), ".ac-token"), "utf8");
    const trimmed = raw.trim();
    if (!trimmed.startsWith("{")) return trimmed;
    const parsed = JSON.parse(trimmed);
    return parsed.access_token ?? parsed.accessToken ?? parsed.token ?? null;
  } catch { return null; }
}

before(async () => {
  token = await loadToken();
  if (!token) {
    console.log("… no ~/.ac-token or AC_TOKEN — signed-in cases will skip.");
    console.log("  get one with: node tezos/ac-login.mjs");
    return;
  }
  // Resolve the subject so the "never echoed" assertion has something real to
  // look for rather than a guess at the shape of it.
  try {
    const info = await fetch("https://hi.aesthetic.computer/userinfo", {
      headers: { Authorization: `Bearer ${token}` },
    });
    if (info.ok) {
      subject = (await info.json())?.sub;
      console.log(`→ signed in as ${subject}`);
    } else {
      console.log(`… token did not resolve (userinfo ${info.status}); it may be stale.`);
      console.log("  refresh with: node tezos/ac-login.mjs");
      token = null;
    }
  } catch (err) {
    console.log(`… could not reach Auth0: ${err.message}`);
    token = null;
  }
});

const post = (body, headers = {}) => fetch(URL_UNDER_TEST, {
  method: "POST",
  headers: { "Content-Type": "application/json", ...headers },
  body: JSON.stringify(body),
});

const signed = (body) => post(body, { Authorization: `Bearer ${token}` });

test("the endpoint is deployed and answering", async () => {
  const response = await post(minimal);
  assert.notEqual(response.status, 404,
    `${URL_UNDER_TEST} is not routed — the function did not deploy`);
});

test("a stranger is turned away", async () => {
  const response = await post(minimal);
  assert.equal(response.status, 401);
  const answer = await response.json();
  assert.equal(answer.capability, null);
});

test("a garbled token is turned away", async () => {
  const response = await post(minimal, { Authorization: "Bearer not-a-token" });
  assert.equal(response.status, 401);
});

test("a signed-in player's malformed ask is refused", async (t) => {
  if (!token) return t.skip("no credentials");
  const response = await signed({ ...minimal, outputs: ["hologram"] });
  assert.equal(response.status, 400);
  const answer = await response.json();
  assert.equal(answer.capability, null);
  assert.match(answer.message, /thing to make/);
});

test("an empty ask is refused rather than read as blanket permission", async (t) => {
  if (!token) return t.skip("no credentials");
  const response = await signed({});
  assert.equal(response.status, 400);
  assert.equal((await response.json()).capability, null);
});

// The load-bearing one. Whatever REGARDE is doing, this must not hand back a
// capability — and it must not be a 500 either, because an unhandled throw is
// not a refusal, it is a wall with no one behind it.
test("a well-formed ask never yields a capability", async (t) => {
  if (!token) return t.skip("no credentials");
  const response = await signed(minimal);
  const answer = await response.json();

  assert.equal(answer.capability ?? null, null,
    "no configuration of REGARDE may return a generation capability yet");
  assert.ok(response.status < 500 || [502, 503, 504].includes(response.status),
    `unexpected server error ${response.status}: ${JSON.stringify(answer)}`);

  if (response.status === 503) {
    assert.equal(answer.outcome, "refuse");
    console.log("  state: fail-closed — REGARDE_GATEWAY_URL/SALT unset in this environment.");
  } else if (response.status === 200) {
    assert.ok(["allow", "deny", "edit", "refuse"].includes(answer.outcome),
      `unregistered outcome ${answer.outcome}`);
    console.log(`  state: desk reachable — outcome "${answer.outcome}".`);
  } else {
    console.log(`  state: desk configured but unhappy — ${response.status}.`);
  }
});

test("the response never echoes the player's Auth0 subject", async (t) => {
  if (!token) return t.skip("no credentials");
  if (!subject) return t.skip("subject unresolved");
  const body = await (await signed(minimal)).text();
  assert.ok(!body.includes(subject),
    "the raw Auth0 subject must never appear in a response");
  assert.doesNotMatch(body, /auth0\|/);
});

test("the browser wall can reach it cross-origin", async (t) => {
  if (!token) return t.skip("no credentials");
  // The wizard is served from the game's origin and posts here; a preflight
  // that forgets the header turns the whole wall into a console error.
  const response = await signed(minimal);
  assert.equal(response.headers.get("access-control-allow-origin"), "*");
});
