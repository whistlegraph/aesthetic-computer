import fs from "fs";
import path from "path";
import process from "process";
import { execSync } from "child_process";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const repoRoot = path.resolve(__dirname, "..", "..");
const packagePath = path.join(repoRoot, "ac-electron", "package.json");

function getArg(flag) {
  const index = process.argv.indexOf(flag);
  if (index === -1) return null;
  const next = process.argv[index + 1];
  if (!next || next.startsWith("--")) return true;
  return next;
}

function parseVersion(version) {
  const parts = version.split(".").map((part) => Number(part));
  if (parts.length < 3 || parts.some((part) => Number.isNaN(part))) {
    throw new Error(`Invalid version: ${version}`);
  }
  return parts;
}

function bumpVersion(current, bumpType) {
  const [major, minor, patch] = parseVersion(current);
  if (bumpType === "major") return `${major + 1}.0.0`;
  if (bumpType === "minor") return `${major}.${minor + 1}.0`;
  return `${major}.${minor}.${patch + 1}`;
}

function run(command, options = {}) {
  execSync(command, { stdio: "inherit", cwd: repoRoot, ...options });
}

const versionArg = getArg("--version");
const bumpArg = getArg("--bump");
const dryRun = Boolean(getArg("--dry-run"));
const allowDirty = Boolean(getArg("--allow-dirty"));
const commitMessageArg = getArg("--message");

if (!fs.existsSync(packagePath)) {
  throw new Error(`Missing package.json at ${packagePath}`);
}

if (!allowDirty) {
  const status = execSync("git status --porcelain", { cwd: repoRoot })
    .toString()
    .trim();
  if (status.length > 0) {
    throw new Error("Working tree is dirty. Commit changes first or pass --allow-dirty.");
  }
}

const packageData = JSON.parse(fs.readFileSync(packagePath, "utf8"));
const currentVersion = packageData.version;

const nextVersion = versionArg
  ? String(versionArg).trim()
  : bumpVersion(currentVersion, bumpArg === true || !bumpArg ? "patch" : bumpArg);

packageData.version = nextVersion;

if (dryRun) {
  console.log(`[dry-run] Would update ac-electron/package.json from ${currentVersion} to ${nextVersion}`);
  console.log(`[dry-run] Would commit, tag v${nextVersion}, and push main + tag`);
  process.exit(0);
}

fs.writeFileSync(packagePath, JSON.stringify(packageData, null, 2) + "\n");

run("git add ac-electron/package.json");

const commitMessage =
  commitMessageArg && commitMessageArg !== true
    ? String(commitMessageArg)
    : `v${nextVersion}: electron desktop release`;

run(`git commit -m "${commitMessage}"`);
run(`git tag v${nextVersion}`);
run("git push origin main");
run(`git push origin v${nextVersion}`);

console.log(`Release tag v${nextVersion} pushed. GitHub Actions should publish the release.`);