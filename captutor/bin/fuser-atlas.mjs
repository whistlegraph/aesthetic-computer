#!/usr/bin/env node

import { homedir } from "node:os";
import { join } from "node:path";
import { buildFuserSourceAtlas, compactFuserSourceAtlas } from "../app-intelligence/fuser-source-atlas.mjs";

const args = process.argv.slice(2);
const value = (flag, fallback) => {
  const index = args.indexOf(flag);
  return index < 0 ? fallback : args[index + 1];
};
if (args.includes("--help") || args.includes("-h")) {
  console.log("usage: node bin/fuser-atlas.mjs [--source ~/Developer/fuser] [--full]");
  process.exit(0);
}
const atlas = buildFuserSourceAtlas(value("--source", join(homedir(), "Developer", "fuser")));
console.log(JSON.stringify(args.includes("--full") ? atlas : compactFuserSourceAtlas(atlas), null, 2));

