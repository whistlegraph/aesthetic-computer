#!/usr/bin/env node
import { mkdir, symlink, realpath, chmod } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const bin = join(homedir(), ".local/bin");
const target = fileURLToPath(new URL("./ac-mail.mjs", import.meta.url));
await mkdir(bin, { recursive: true });
await chmod(target, 0o755);
try { await symlink(target, join(bin, "ac-mail")); }
catch (error) {
  if (error.code !== "EEXIST" || await realpath(join(bin, "ac-mail")) !== target) {
    console.error("An ac-mail command already exists. Nothing was replaced.");
    process.exit(1);
  }
}
console.log("Installed ~/.local/bin/ac-mail. Run ac-mail (or ac-mail login to sign in).");
