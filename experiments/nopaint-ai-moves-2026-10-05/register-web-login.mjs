// Register only the production website; preserve every existing AC client URL.
// Requires an Auth0 CLI admin session. Default is a read-only preview.
import { execFileSync } from "node:child_process";
const tenant = "aesthetic.us.auth0.com";
const client = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
const origin = "https://nopaint.art";
const callback = "https://aesthetic.computer/api/nopaint-signin";
const fields = [
  "callbacks",
  "web_origins",
  "allowed_origins",
  "allowed_logout_urls",
];
const cli = (...args) =>
  JSON.parse(execFileSync("auth0", args, { encoding: "utf8" }));
const tenants = cli("tenants", "list", "--json");
if (!tenants.some((t) => t.active && t.name === tenant))
  throw Error(`Select ${tenant} in the Auth0 CLI first.`);
const path = `clients/${client}`;
const read = () =>
  cli(
    "api",
    "get",
    `${path}?fields=app_type,${fields.join(",")}&include_fields=true`,
  );
const before = read();
if (before.app_type !== "spa")
  throw Error("Expected the existing AC browser application.");
const patch = Object.fromEntries(
  fields
    .map((key) => [
      key,
      [
        ...new Set([
          ...(before[key] || []),
          origin,
          ...(key === "callbacks" ? [callback] : []),
        ]),
      ],
    ])
    .filter(([key, urls]) => urls.length !== (before[key] || []).length),
);
console.log(
  JSON.stringify({
    tenant,
    client,
    origin,
    addTo: Object.keys(patch),
    apply: process.argv.includes("--apply"),
  }),
);
if (process.argv.includes("--apply") && Object.keys(patch).length) {
  execFileSync(
    "auth0",
    ["api", "patch", path, "--data", JSON.stringify(patch)],
    { stdio: ["ignore", "pipe", "inherit"] },
  );
  const after = read();
  for (const key of fields) {
    for (const url of [
      ...(before[key] || []),
      origin,
      ...(key === "callbacks" ? [callback] : []),
    ]) {
      if (!(after[key] || []).includes(url))
        throw Error(`Missing ${key} URL after update.`);
    }
  }
  console.log("Verified nopaint.art and all previous AC application URLs.");
}
