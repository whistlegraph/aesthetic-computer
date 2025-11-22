#!/usr/bin/env node
import { existsSync, readFileSync } from "node:fs";
import path from "node:path";

const targetDir = process.argv[2];

if (!targetDir) {
  process.exit(0);
}

const pkgPath = path.join(targetDir, "package.json");
if (!existsSync(pkgPath)) {
  process.exit(0);
}

const nodeModulesPath = path.join(targetDir, "node_modules");
if (!existsSync(nodeModulesPath)) {
  console.log(
    JSON.stringify(
      {
        dir: targetDir,
        mismatches: [
          {
            name: "*",
            expected: "package.json",
            actual: null,
            reason: "node_modules-missing",
          },
        ],
      },
      null,
      2,
    ),
  );
  process.exit(1);
}

const pkg = JSON.parse(readFileSync(pkgPath, "utf8"));
const deps = {
  ...(pkg.dependencies || {}),
  ...(pkg.devDependencies || {}),
};

if (Object.keys(deps).length === 0) {
  process.exit(0);
}

const parseVersion = (version) => {
  return (version || "0.0.0")
    .replace(/^v/, "")
    .split(".")
    .map((part) => {
      const numeric = parseInt(part, 10);
      return Number.isFinite(numeric) ? numeric : 0;
    });
};

const compareVersions = (a, b) => {
  for (let i = 0; i < Math.max(a.length, b.length); i++) {
    const aVal = a[i] || 0;
    const bVal = b[i] || 0;
    if (aVal > bVal) return 1;
    if (aVal < bVal) return -1;
  }
  return 0;
};

const satisfies = (installed, expected) => {
  if (!expected || expected === "latest") return true;
  if (!installed) return false;

  const normalizedExpected = expected.trim();
  const normalizedInstalled = installed.trim();

  if (normalizedExpected === normalizedInstalled) {
    return true;
  }

  const cleanExpected = normalizedExpected.replace(/^[~^=]/, "");

  if (/^[~^]/.test(normalizedExpected)) {
    const expectedParts = parseVersion(cleanExpected);
    const installedParts = parseVersion(normalizedInstalled);

    if (normalizedExpected.startsWith("^")) {
      if (installedParts[0] !== expectedParts[0]) {
        return false;
      }
      return compareVersions(installedParts, expectedParts) >= 0;
    }

    if (normalizedExpected.startsWith("~")) {
      if (installedParts[0] !== expectedParts[0]) {
        return false;
      }
      if (installedParts[1] !== expectedParts[1]) {
        return false;
      }
      return compareVersions(installedParts, expectedParts) >= 0;
    }
  }

  if (/^[<>*]|\|\|/.test(normalizedExpected)) {
    // Complex ranges are hard to evaluate without semver; assume satisfied to avoid false alarms
    return true;
  }

  return normalizedInstalled === cleanExpected;
};

const mismatches = [];

for (const [name, expected] of Object.entries(deps)) {
  const dependencyPkgPath = path.join(nodeModulesPath, name, "package.json");

  if (!existsSync(dependencyPkgPath)) {
    mismatches.push({
      name,
      expected,
      actual: null,
      reason: "missing",
    });
    continue;
  }

  const dependencyPkg = JSON.parse(readFileSync(dependencyPkgPath, "utf8"));
  const installed = dependencyPkg.version || null;

  if (!satisfies(installed, expected)) {
    mismatches.push({
      name,
      expected,
      actual: installed,
      reason: "version-mismatch",
    });
  }
}

if (mismatches.length > 0) {
  console.log(
    JSON.stringify(
      {
        dir: targetDir,
        mismatches,
      },
      null,
      2,
    ),
  );
  process.exit(1);
}

process.exit(0);
