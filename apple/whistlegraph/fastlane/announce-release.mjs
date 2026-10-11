// Tell lith that a Whistlegraph build is installable from TestFlight, so older
// copies offer the update when they open (POST /api/whistlegraph-release).
//
//   node announce-release.mjs <version> <build>
//
// Announces only once App Store Connect says testers can install the build
// (external state IN_BETA_TESTING); before beta review approves it, it exits 2
// and says to rerun `fastlane ios announce`. Signs in as you via ~/.ac-token.
import { execFileSync } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { loadTokens, UA } from "../../../toolchain/mcp/ac-token.mjs";

const REPO = join(dirname(fileURLToPath(import.meta.url)), "../../..");
const APP_ID = "6820297686";
const HOST = process.env.AC_HOST || "https://aesthetic.computer";
const [version, build] = process.argv.slice(2);
if (!version || !/^\d+$/.test(build || "")) {
  console.error("usage: announce-release.mjs <version> <build>");
  process.exit(64);
}

const asc = (path) => JSON.parse(execFileSync("node",
  [join(REPO, "slab/menuband/bin/asc.mjs"), "--app", "whistlegraph", "get", path], { encoding: "utf8" }));

const found = asc(`/v1/builds?filter[app]=${APP_ID}&filter[version]=${build}&filter[preReleaseVersion.version]=${version}&limit=1`).data?.[0];
if (!found) { console.error(`No build ${version} (${build}) in App Store Connect yet.`); process.exit(2); }
const state = asc(`/v1/builds/${found.id}/buildBetaDetail`).data?.attributes?.externalBuildState;
if (state !== "IN_BETA_TESTING") {
  console.error(`Build ${build} is ${state || "unknown"}, not installable yet. Rerun \`fastlane ios announce\` after beta review approves it.`);
  process.exit(2);
}

const { access_token: token } = await loadTokens();
const response = await fetch(`${HOST}/api/whistlegraph-release`, {
  method: "POST",
  headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}`, "User-Agent": UA },
  body: JSON.stringify({ version, build }),
});
const body = await response.json().catch(() => ({}));
if (!response.ok) { console.error(`lith refused: ${response.status} ${body.error || ""}`); process.exit(1); }
console.log(`lith now offers build ${body.build} (${body.version}) to older copies.`);
