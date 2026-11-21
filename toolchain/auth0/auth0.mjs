#!/usr/bin/env node
// Auth0 CLI sessions from the vault's machine-to-machine keys, for both tenants.
//
//   node toolchain/auth0/auth0.mjs login [aesthetic|sotce|all]   machine login
//   node toolchain/auth0/auth0.mjs use aesthetic|sotce            switch the CLI's tenant
//   node toolchain/auth0/auth0.mjs grant-logs [aesthetic|sotce|all]
//       give the M2M app read:logs, so `auth0 logs list` and bansec checks work
//
// The keys stay in the vault (auth0:<tenant> in toolchain/vault/credentials.mjs);
// the CLI keeps only its session token, in the OS keychain.
import { spawnSync } from "node:child_process";
import { requireCredentials } from "../vault/credentials.mjs";

const TENANTS = {
  aesthetic: { domain: "aesthetic.us.auth0.com", id: "AUTH0_M2M_CLIENT_ID", secret: "AUTH0_M2M_SECRET" },
  sotce: { domain: "sotce.us.auth0.com", id: "SOTCE_AUTH0_M2M_CLIENT_ID", secret: "SOTCE_AUTH0_M2M_SECRET" },
};
const [command, which = "all"] = process.argv.slice(2);
const pick = (name) => {
  const names = name === "all" ? Object.keys(TENANTS) : [name];
  for (const n of names) if (!TENANTS[n]) throw new Error(`Unknown tenant: ${n}`);
  return names;
};
// Run the auth0 CLI; on failure, throw with what Auth0 said rather than a bare exit code.
const cli = (args) => {
  const r = spawnSync("auth0", args, { encoding: "utf8" });
  if (r.status !== 0) {
    const said = `${r.stderr || ""}${r.stdout || ""}`.trim();
    throw new Error(`auth0 ${args.slice(0, 2).join(" ")} failed: ${said || r.error?.message || `exit ${r.status}`}`);
  }
  return r.stdout;
};
const keys = (name) => {
  const { values } = requireCredentials(`auth0:${name}`);
  return { clientId: values[TENANTS[name].id], secret: values[TENANTS[name].secret] };
};

function login(name) {
  const { clientId, secret } = keys(name);
  const r = spawnSync("auth0", ["login", "--domain", TENANTS[name].domain, "--client-id", clientId,
    "--client-secret", secret, "--no-input"], { encoding: "utf8" });
  if (r.status !== 0) {
    const said = `${r.stdout || ""}${r.stderr || ""}`.split(secret).join("***").trim();
    throw new Error(`auth0 login ${name} failed: ${said || r.error?.message || `exit ${r.status}`}`);
  }
  console.log(`✅ auth0 CLI signed in to ${TENANTS[name].domain} (machine login)`);
}

function grantLogs(name) {
  const { clientId } = keys(name);
  cli(["tenants", "use", TENANTS[name].domain]);
  const grants = JSON.parse(cli(["api", "get", "client-grants", "--query", `client_id=${clientId}`]));
  const grant = grants.find((g) => g.audience === `https://${TENANTS[name].domain}/api/v2/`);
  if (!grant) throw new Error(`No Management API grant for the ${name} M2M app`);
  const want = ["read:logs", "read:logs_users"];
  const missing = want.filter((s) => !grant.scope.includes(s));
  if (!missing.length) return console.log(`✅ ${name}: already has ${want.join(", ")}`);
  cli(["api", "patch", `client-grants/${grant.id}`, "--data", JSON.stringify({ scope: [...grant.scope, ...missing] })]);
  console.log(`✅ ${name}: added ${missing.join(", ")}. Run \`login ${name}\` again to pick them up.`);
}

try {
  if (command === "login") pick(which).forEach(login);
  else if (command === "use") process.stdout.write(cli(["tenants", "use", TENANTS[pick(which)[0]].domain]));
  else if (command === "grant-logs") pick(which).forEach(grantLogs);
  else {
    console.log("usage: auth0.mjs login|use|grant-logs [aesthetic|sotce|all]");
    process.exit(command ? 1 : 0);
  }
} catch (err) {
  console.error(`❌ ${err.message}`);
  process.exit(1);
}
