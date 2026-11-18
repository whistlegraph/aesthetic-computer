#!/usr/bin/env node
// install, 26.11.18
// Put the "Pick your handle" step into an Auth0 tenant: the flow that claims
// the name on AC, the form that asks for it, and the ask-handle Action.
//
//   node system/backend/auth0-forms/install.mjs                 # install, leave unbound
//   node system/backend/auth0-forms/install.mjs --only '^mail\+signuptest'   # ask test addresses only
//   node system/backend/auth0-forms/install.mjs --bind          # add to the post-login flow, last
//   node system/backend/auth0-forms/install.mjs --tenant dev-x.us.auth0.com
//
// Uses the Auth0 CLI's current login (`auth0 login`, a browser approval) and
// refuses when the CLI's active tenant is not the one named. The signup secret
// is derived from aesthetic-computer-vault/lith/.env — the copy production
// uses — and is never printed. Each piece is found by name and updated in
// place, so running it again is how a change here goes live.
//
// Nothing is asked of anyone until the Action is bound; `--only` keeps a bound
// Action to matching addresses, and running without it lifts that.

import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { signupSecret } from "../signup-handle.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = join(HERE, "../../..");
const args = process.argv.slice(2);
const option = (name) => { const i = args.indexOf(name); return i < 0 ? undefined : args[i + 1]; };
const TENANT = option("--tenant") || "aesthetic.us.auth0.com";
const ONLY = option("--only") || "";
const BIND = args.includes("--bind");
const ACTION = "ask-handle";

const auth0 = (...argv) => execFileSync("auth0", argv, { encoding: "utf8", timeout: 60_000, stdio: ["ignore", "pipe", "pipe"] });
const api = (method, path, data) => JSON.parse(auth0("api", method, path, ...(data ? ["--data", JSON.stringify(data)] : [])) || "null");
const listed = (value, key) => (Array.isArray(value) ? value : value?.[key] || []);

const active = listed(JSON.parse(auth0("tenants", "list", "--json")), "tenants").find((tenant) => tenant.active)?.name;
if (active !== TENANT) { console.error(`✗ the Auth0 CLI is on ${active || "no tenant"}, not ${TENANT}. Run: auth0 tenants use ${TENANT}`); process.exit(1); }

const env = readFileSync(join(ROOT, "aesthetic-computer-vault/lith/.env"), "utf8");
const root = env.match(/^AUTH0_M2M_SECRET=(.*)$/m)?.[1]?.replace(/^"|"$/g, "");
const secret = signupSecret({ AUTH0_M2M_SECRET: root });
if (!secret) { console.error("✗ AUTH0_M2M_SECRET is missing from aesthetic-computer-vault/lith/.env"); process.exit(1); }

// 1. The flow, with the secret in its header.
const flow = JSON.parse(readFileSync(join(HERE, "flow.json"), "utf8").replace("__SIGNUP_SECRET__", secret));
let flowID = listed(api("get", "flows"), "flows").find((f) => f.name === flow.name)?.id;
if (flowID) api("patch", `flows/${flowID}`, flow); else flowID = api("post", "flows", flow).id;
console.log(`→ flow   ${flow.name} (${flowID})`);

// 2. The form, pointed at the flow.
// form.css carries the sign-in page's typeface, logo and plain red errors
// into the form, which otherwise falls back to Auth0's stock look.
const form = JSON.parse(readFileSync(join(HERE, "form.json"), "utf8").replace("__FLOW_ID__", flowID));
form.style = { css: readFileSync(join(HERE, "form.css"), "utf8").replace(/\s*\n\s*/g, "") };
let formID = listed(api("get", "forms"), "forms").find((f) => f.name === form.name)?.id;
if (formID) api("patch", `forms/${formID}`, form); else formID = api("post", "forms", form).id;
console.log(`→ form   ${form.name} (${formID})`);

// 3. The Action.
const action = {
  name: ACTION, code: readFileSync(join(HERE, "ask-handle.js"), "utf8"), runtime: "node22", dependencies: [],
  secrets: [{ name: "SIGNUP_SECRET", value: secret }, { name: "FORM_ID", value: formID }, { name: "ONLY_EMAILS", value: ONLY }],
};
let actionID = listed(api("get", `actions/actions?actionName=${ACTION}`), "actions").find((a) => a.name === ACTION)?.id;
if (actionID) api("patch", `actions/actions/${actionID}`, action);
else actionID = api("post", "actions/actions", { ...action, supported_triggers: [{ id: "post-login", version: "v3" }] }).id;
let status;
for (let i = 0; i < 30 && status !== "built"; i++) {
  status = api("get", `actions/actions/${actionID}`).status;
  if (status === "failed") { console.error("✗ the Action failed to build"); process.exit(1); }
  if (status !== "built") execFileSync("sleep", ["2"]);
}
auth0("actions", "deploy", actionID);
console.log(`→ action ${ACTION} (${actionID}) deployed${ONLY ? `, asking only /${ONLY}/` : ", asking everyone without a handle"}`);

// 4. The post-login flow: ask-handle goes last, after any linking.
const bindings = listed(api("get", "actions/triggers/post-login/bindings"), "bindings");
const bound = bindings.some((b) => b.action?.id === actionID);
if (BIND && !bound) {
  api("patch", "actions/triggers/post-login/bindings", { bindings: [
    ...bindings.map((b) => ({ ref: { type: "action_id", value: b.action.id }, display_name: b.display_name })),
    { ref: { type: "action_id", value: actionID }, display_name: ACTION },
  ] });
}
const now = listed(api("get", "actions/triggers/post-login/bindings"), "bindings").map((b) => b.display_name);
console.log(`→ post-login: ${now.join(" → ") || "(empty)"}`);
console.log(now.includes(ACTION) ? "✓ installed and bound" : "✓ installed, not bound — nobody is asked yet (--bind to switch it on)");
