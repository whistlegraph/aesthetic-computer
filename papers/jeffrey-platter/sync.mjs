#!/usr/bin/env node

import { readFileSync, writeFileSync, mkdirSync } from "fs";
import { dirname, join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const REPO_ROOT = join(HERE, "..", "..");
const SOURCE = join(HERE, "manifest.json");
const TARGETS = [
  join(REPO_ROOT, "system/public/give.aesthetic.computer/jeffreys-manifest.json"),
];

const json = readFileSync(SOURCE, "utf8");
JSON.parse(json);

for (const target of TARGETS) {
  mkdirSync(dirname(target), { recursive: true });
  writeFileSync(target, json, "utf8");
  console.log(`wrote ${target.replace(REPO_ROOT + "/", "")}`);
}
